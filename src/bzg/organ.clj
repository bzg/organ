;; Copyright (c) Bastien Guerry
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bzg.organ
  "Parse Org-mode text into an AST (EDN data)."
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]))

;; Interop calls in the hot parsing paths must stay reflection-free;
;; this makes any regression visible at JVM compile time (no-op in bb).
(set! *warn-on-reflection* true)

(def ^:dynamic *parse-errors* nil)

(defn- add-parse-error! [line-num message]
  (when *parse-errors*
    (vswap! *parse-errors* conj {:line line-num :message message})))

(defn empty-value?
  "True if value is nil, empty collection, or blank string."
  [v]
  (or (nil? v)
      (and (coll? v) (empty? v))
      (and (string? v) (str/blank? v))))

(defn remove-empty-vals
  "Remove entries with empty values from a map."
  [m]
  (persistent!
   (reduce-kv (fn [acc k v] (if (empty-value? v) acc (assoc! acc k v)))
              (transient {}) m)))

(defn- make-node
  "Create an AST node, filtering out empty values."
  [type & {:as fields}]
  (assoc (remove-empty-vals fields) :type type))

(defn- split-lines
  "Same result as str/split-lines, faster: without \\r, String.split takes
   its single-character fast path and skips the regex engine."
  [^String s]
  (if (str/includes? s "\r")
    (str/split-lines s)
    (vec (.split s "\n"))))

(defn non-blank?
  "True if s is a non-nil, non-blank string."
  [s]
  (and (some? s) (not (str/blank? s))))

;; Regex Patterns
(def ^:private headline-pattern #"^(\*+)\s+(.*)$")
(def ^:private property-drawer-start-pattern #"(?i)^\s*:PROPERTIES:\s*$")
(def ^:private drawer-start-pattern #"^\s*:([A-Za-z][\w_-]*?):\s*$")
(def ^:private drawer-end-pattern #"(?i)^\s*:END:\s*$")
;; As in Org, a property name is any run of non-blank characters
;; (:header-args+:, :Foo.Bar:), closed by a colon then a blank or the end.
(def ^:private property-pattern #"^\s*:(\S+):(?:\s+(.*))?$")
;; A bullet alone on its line is an item too, its contents on the next
;; lines (group 3 is nil then), except for a star.
(def ^:private numeric-list-item-pattern
  #"^(\s*)(-|\+|\*|\d+[.)])(?:\s+(.*)|(?<!\*))$")
(def ^:private alphabetical-list-item-pattern
  #"^(\s*)(-|\+|\*|(?:\d+|[A-Za-z])[.)])(?:\s+(.*)|(?<!\*))$")
;; Rebound by parse-org when :alphabetical-lists? is set: single-letter
;; bullets (a. b) A.) are only list items then, as with Org's
;; org-list-allow-alphabetical.
(def ^:private ^:dynamic *list-item-pattern* numeric-list-item-pattern)
(def ^:private description-item-pattern #"^(.+?)\s+::(?:\s+(.*))?$")
;; Any line whose first non-blank character is | is a table line (the final
;; pipe is optional), as in Emacs org-element.
(def ^:private table-pattern #"^\s*\|.*$")
(def ^:private table-separator-pattern #"^\s*\|-.*$")
(def ^:private generic-block-begin-pattern #"(?i)^\s*#\+BEGIN_(\w+)(?:\s+(.*))?$")
(def ^:private metadata-pattern #"^\s*#\+(\w+):\s*(.*)$")
(def ^:private comment-pattern #"^\s*#(?:\s.*|$)")
(def ^:private html-line-pattern #"(?i)^\s*#\+html:\s*(.*)$")
(def ^:private latex-line-pattern #"(?i)^\s*#\+latex:\s*(.*)$")
(def ^:private block-begin-pattern #"(?i)^\s*#\+BEGIN.*$")
(def ^:private fixed-width-pattern #"^\s*:(?: (.*))?$")
;; No colon in the label: [fn:label:text] and [fn::text] starting a line
;; are inline footnotes in a paragraph, not definitions.
(def ^:private footnote-def-pattern #"^\[fn:([^\]:]+)\]\s*(.*)$")
(def ^:private link-type-pattern #"^(file|id|mailto|http|https|ftp|news|shell|elisp|doi):(.*)$")
;; The optional [...] is a short caption (#+CAPTION[short]: long) or a
;; cached result's hash (#+RESULTS[hash]: name); it is not kept.
(def ^:private affiliated-keyword-pattern
  #"(?i)^\s*#\+(attr_\w+|caption|name|header|results)(?:\[[^\]]*\])?:\s*(.*)$")

;; LaTeX fragment patterns (inline math).
(def ^:private latex-paren-pattern    #"^\\\(([\s\S]+?)\\\)")
(def ^:private latex-bracket-pattern  #"^\\\[([\s\S]+?)\\\]")
(def ^:private latex-dollars-pattern  #"^\$\$([\s\S]+?)\$\$")
;; Org's single-$ rule: content starts and ends with a non-whitespace,
;; non-punct char; no $ inside; up to 2 embedded newlines. The closing $
;; must be followed by whitespace, punctuation, or end of string.
(def ^:private latex-dollar1-pattern
  #"^\$([^ \t\r\n,;.$])\$(?=$|[- \t\r\n.,?;:'\")\\|^])")
(def ^:private latex-dollar-pattern
  #"^\$([^ \t\r\n,;.$](?:[^$\r\n]*?(?:\n[^$\r\n]*?){0,2})?[^ \t\r\n,.$])\$(?=$|[- \t\r\n.,?;:'\")\\|^])")

;; TODO keywords. The default set (TODO|DONE) can be replaced per file by
;; #+TODO:/#+SEQ_TODO:/#+TYP_TODO: directives — wherever they appear in the
;; file, they apply to the whole document, as in Emacs Org — or extended via
;; the :todo-keywords option of parse-org.
(def ^:private default-todo-keywords {:todo ["TODO"] :done ["DONE"]})

(def ^:private todo-directive-pattern
  #"(?im)^[ \t]*#\+(?:TODO|SEQ_TODO|TYP_TODO):[ \t]*(.*)$")

(defn- directive-candidate-lines
  "The lines of s holding \"#+\", found with indexOf: scanning the whole
   text with todo-directive-pattern is much slower. Matches never span a
   newline, so applying the pattern to these lines finds the same ones."
  [^String s]
  (loop [from 0 lines []]
    (let [i (.indexOf s "#+" (int from))]
      (if (neg? i)
        lines
        (let [start (inc (.lastIndexOf s "\n" (int i)))
              end   (let [e (.indexOf s "\n" (int i))] (if (neg? e) (.length s) e))]
          (recur (inc end) (conj lines (subs s start end))))))))

(defn- directive-words
  "Split a directive right-hand side into keywords, dropping fast-access
   selectors such as (t) or (w@/!) and stray | separators."
  [s]
  (->> (str/split (str/trim s) #"\s+")
       (map #(str/replace % #"\(.*\)$" ""))
       (remove str/blank?)
       (remove #{"|"})
       vec))

(defn- todo-sequence
  "{:todo [...] :done [...]} from one directive right-hand side. Words after
   a | separator are DONE states; without a separator the last word is the
   DONE state (Org convention)."
  [rhs]
  (if (str/includes? rhs "|")
    (let [[l r] (str/split rhs #"\|" 2)]
      {:todo (directive-words l) :done (directive-words r)})
    (let [ws (directive-words rhs)]
      {:todo (vec (butlast ws)) :done (vec (take-last 1 ws))})))

(defn- merge-todo-sequences [seqs]
  (-> (reduce (fn [acc m]
                (-> acc
                    (update :todo into (:todo m))
                    (update :done into (:done m))))
              {:todo [] :done []}
              seqs)
      (update :todo (comp vec distinct))
      (update :done (comp vec distinct))))

(defn parse-todo-keywords
  "Per-file TODO keywords from #+TODO:/#+SEQ_TODO:/#+TYP_TODO: directives,
   as {:todo [...] :done [...]}, or nil when the text has none (or only
   empty directives). Multiple directives accumulate, mirroring Emacs Org
   sequences."
  [org-content]
  (let [seqs (map second (mapcat #(re-seq todo-directive-pattern %)
                                 (directive-candidate-lines (or org-content ""))))
        m    (merge-todo-sequences (map todo-sequence seqs))]
    (when (or (seq (:todo m)) (seq (:done m))) m)))

(defn- normalize-todo-keywords
  "Coerce the :todo-keywords option into {:todo [...] :done [...]}: accepts
   that map directly, or a string in directive syntax (\"TODO NEXT | DONE\").
   Returns nil for an absent or empty option."
  [kws]
  (let [m (cond
            (map? kws)    {:todo (vec (:todo kws)) :done (vec (:done kws))}
            (string? kws) (todo-sequence kws)
            :else         nil)]
    (when (seq (concat (:todo m) (:done m))) m)))

(defn- headline-pattern-for
  "Full headline regex recognising the given {:todo [...] :done [...]}
   keywords (falls back to the default set when both are empty). The
   keyword and the priority may end the line (\"* TODO\": no title). The
   possessive quantifiers keep the regex from giving back what it took as
   a keyword, a priority or blanks so as to read a lone :tag: as tags: tags
   come after a title, as in Org. Tags are made of letters and digits of
   any script, and of _ @ # %."
  [{:keys [todo done]}]
  (let [kws (distinct (concat todo done))
        kws (if (seq kws) kws (concat (:todo default-todo-keywords)
                                      (:done default-todo-keywords)))]
    (re-pattern
     (str "^(\\*+)\\s++(?:("
          (str/join "|" (map #(java.util.regex.Pattern/quote %) kws))
          ")(?:\\s++|$))?+(?:\\[#([A-Z])\\](?:\\s++|$))?+(.*?)(?:\\s+(:[:\\p{L}\\p{N}_@#%]+:))?\\s*$"))))

(def ^:private ^:dynamic *headline-full-pattern*
  (headline-pattern-for default-todo-keywords))

;; LaTeX environment (block-level): \begin{env} ... \end{env}. The begin
;; line may go on (\begin{tabular}{ll}, \begin{equation}\label{eq}), but not
;; up to its own \end{env}: the environment ends on a later line.
(def ^:private latex-env-begin-pattern
  #"^\s*\\begin\{([^}]+)\}(?!.*\\end\{\1\}).*$")
(def ^:private latex-env-end-pattern-for
  "Memoized constructor for \\end{name} patterns."
  (memoize
   (fn [env]
     (re-pattern (str "^\\s*\\\\end\\{"
                      (java.util.regex.Pattern/quote env)
                      "\\}\\s*$")))))

;; Timestamp innards, loosely mirroring Emacs org-ts-regexp: date, then an
;; optional day name (which may not contain digits), an optional time or
;; time range, then anything else (repeater, warning/delay cookie) up to
;; the closer. Unlike Emacs (which normalises out-of-range hour/minute via
;; encode-time), hour/minute are bounded here (0-23 / 0-59) so an invalid
;; time degrades to a shorter timestamp instead of producing an illegal
;; ISO value downstream.
;; `closer` is the regex-escaped closing character (">" or "\\]").
(defn- ts-inner-pattern [closer]
  (str "(\\d{4})-(\\d{2})-(\\d{2})"
       "(?:\\s+[^\\s\\d+" closer "-]+)?"
       "(?:\\s+((?:[01]?\\d|2[0-3])):([0-5]\\d)"
       "(?:-((?:[01]?\\d|2[0-3])):([0-5]\\d))?)?"
       "[^\\n" closer "]*"))

;; Planning line (CLOSED, SCHEDULED, DEADLINE)
(def ^:private org-timestamp-pattern
  (re-pattern (str "<" (ts-inner-pattern ">") ">"
                   "|\\[" (ts-inner-pattern "\\]") "\\]")))
(def ^:private org-repeater-pattern #"(?:^|[\s])([.+]?\+\d+[hdwmy])")
(def ^:private planning-keyword-pattern #"^(CLOSED|SCHEDULED|DEADLINE):\s*")
(def ^:private planning-line-pattern #"^\s*((?:(?:CLOSED|SCHEDULED|DEADLINE):\s*(?:<[^>]+>|\[[^\]]+\])\s*)+)\s*$")

(defn- parse-org-timestamp
  "Parse an Org timestamp string into ISO 8601 format.
   <2025-01-15 Wed>             -> 2025-01-15
   <2025-01-15 Wed 10:30>       -> 2025-01-15T10:30
   <2025-01-15 Wed 9:30-12:00>  -> 2025-01-15T09:30/2025-01-15T12:00
   [2025-01-15 Wed 10:30]       -> 2025-01-15T10:30 (inactive timestamp)"
  [ts-str]
  (when-let [[_ & groups] (re-matches org-timestamp-pattern ts-str)]
    (let [pad2 #(if (= 1 (count %)) (str "0" %) %)
          ;; Active timestamp groups: 0-6, Inactive: 7-13
          [ay am ad ah amin aeh aemin
           iy im id ih imin ieh iemin] groups
          [y m d h min eh emin] (if ay [ay am ad ah amin aeh aemin]
                                  [iy im id ih imin ieh iemin])]
      (if h
        (let [start (str y "-" m "-" d "T" (pad2 h) ":" min)]
          (if eh
            (str start "/" y "-" m "-" d "T" (pad2 eh) ":" emin)
            start))
        (str y "-" m "-" d)))))

(defn- parse-org-repeater
  "Extract repeater cookie from an Org timestamp string.
   <2025-01-15 Wed 10:30 +1w>    -> \"+1w\"
   <2025-01-15 Wed .+2d>         -> \".+2d\"
   <2025-01-15 Wed ++1m>         -> \"++1m\"
   <2025-01-15 Wed 10:30>        -> nil"
  [ts-str]
  (when-let [[_ repeater] (re-find org-repeater-pattern ts-str)]
    repeater))

(def ^:private repeater-cookie-pattern #"^(\.\+|\+\+|\+)(\d+)([hdwmy])$")
(def ^:private repeater-kind {"+" :cumulate "++" :catch-up ".+" :restart})
(def ^:private repeater-unit {"h" :hour "d" :day "w" :week "m" :month "y" :year})

(defn parse-repeater
  "Parse a repeater/cookie string into a structured map, or nil.
   The :kind reflects Org's three repeater forms:
     +N  -> :cumulate  (shift by one interval)
     ++N -> :catch-up  (shift to the next future occurrence)
     .+N -> :restart   (shift relative to completion)
   Units: h :hour, d :day, w :week, m :month, y :year.
     +1w  -> {:n 1 :unit :week  :kind :cumulate}
     ++2d -> {:n 2 :unit :day   :kind :catch-up}
     .+1m -> {:n 1 :unit :month :kind :restart}
     nil / unparseable -> nil"
  [cookie]
  (when cookie
    (when-let [[_ k n u] (re-matches repeater-cookie-pattern cookie)]
      {:n (parse-long n) :unit (repeater-unit u) :kind (repeater-kind k)})))

(defn- parse-planning-line
  "Parse a planning information line into a map.
   Returns {:closed \"2025-01-15\" :scheduled \"2025-01-15T10:30\" :scheduled-repeat \"+1w\"} or nil."
  [line]
  (when (re-matches planning-line-pattern line)
    (let [remaining (str/trim line)]
      (loop [s remaining result {}]
        (if (str/blank? s)
          (when (seq result) result)
          (if-let [[_ kw] (re-find planning-keyword-pattern s)]
            (let [after-kw (str/replace-first s planning-keyword-pattern "")
                  ;; Extract the timestamp (active or inactive)
                  ts-match (or (re-find #"^<[^>]+>" after-kw)
                               (re-find #"^\[[^\]]+\]" after-kw))]
              (if ts-match
                (let [iso (parse-org-timestamp ts-match)
                      repeater (parse-org-repeater ts-match)
                      kw-key (keyword (str/lower-case kw))
                      rest-str (str/trim (subs after-kw (count ts-match)))]
                  ;; An unparseable timestamp yields no entry rather than a nil
                  ;; value (Emacs produces a planning element with nil values).
                  (recur rest-str (cond-> result
                                    iso (assoc kw-key iso)
                                    (and iso repeater)
                                    (assoc (-> kw str/lower-case (str "-repeat") keyword) repeater))))
                (when (seq result) result)))
            (when (seq result) result)))))))

;; Keywords that affect document rendering (used for affiliated/inline keywords)

(def ^:private affiliated-rendering-keywords
  #{"title" "author" "date" "subtitle" "email" "language"
    "html" "latex" "caption" "name" "header" "results" "options"})

(defn- rendering-keyword?
  "Check if a #+keyword line affects rendering.
   Returns true for known rendering keywords and attr_* patterns."
  [line]
  (when-let [[_ kw _] (re-matches metadata-pattern line)]
    (let [kw-lower (str/lower-case kw)]
      (or (contains? affiliated-rendering-keywords kw-lower)
          (str/starts-with? kw-lower "attr_")))))

(defn- ignored-keyword-line?
  "Check if a line is a #+ keyword that should be ignored (no rendering impact)."
  [line]
  (and (re-matches metadata-pattern line)
       (not (rendering-keyword? line))
       (not (re-matches block-begin-pattern line))))

;; Line Type Predicates
(defn- headline? [line] (re-matches headline-pattern line))
(defn- section-end?
  "True for a headline, which ends the current section: a block or LaTeX
   environment still open there is unterminated, as in Org. The starts-with?
   test spares the regex on the many lines that cannot be headlines."
  [^String line]
  (and (str/starts-with? line "*") (headline? line)))
(defn- block-begin? [line] (re-matches block-begin-pattern line))
(defn- planning-line? [line] (re-matches planning-line-pattern line))
(defn- html-line? [line] (re-matches html-line-pattern line))
(defn- latex-line? [line] (re-matches latex-line-pattern line))
(defn- drawer-terminated?
  "True if a :END: line occurs before the next headline or end of input."
  [indexed-lines]
  (->> indexed-lines
       (map :line)
       (take-while #(not (headline? %)))
       (some #(re-matches drawer-end-pattern %))
       boolean))
(defn- list-item? [line] (re-matches *list-item-pattern* line))
(defn- affiliated-keyword? [line] (re-matches affiliated-keyword-pattern line))

(def ^:private end-pattern-for
  "Memoized constructor for block end patterns."
  (memoize (fn [btype] (re-pattern (str "(?i)^\\s*#\\+END_" btype "\\s*$")))))
(defn- index-lines [lines] (map-indexed (fn [i line] {:line line :num (inc i)}) lines))

(defn- block-end-pattern
  "The pattern of the line that closes the block (#+BEGIN_X) or LaTeX
   environment (\\begin{env}) opened by line, or nil when line opens
   neither. Lines up to that end are opaque: kept as they are, never
   unwrapped or split."
  [line]
  (if-let [[_ btype] (re-matches generic-block-begin-pattern line)]
    (end-pattern-for btype)
    (when-let [[_ env] (re-matches latex-env-begin-pattern line)]
      (latex-env-end-pattern-for env))))

(defn- classify-line
  "Classify a line into a type keyword for fast dispatch.
   Uses first non-whitespace character to skip irrelevant regex tests."
  [line]
  (if (str/blank? line)
    :blank
    (let [trimmed (str/triml line)
          fc (when (seq trimmed) (.charAt ^String trimmed 0))]
      (cond
        (nil? fc) :blank

        (= fc \*)
        (if (re-matches headline-pattern line) :headline
          (if (re-matches *list-item-pattern* line) :list-item :text))

        (= fc \#)
        (cond
          (re-matches affiliated-keyword-pattern line) :affiliated
          (re-matches html-line-pattern line) :html-line
          (re-matches latex-line-pattern line) :latex-line
          (re-matches generic-block-begin-pattern line) :block-begin
          (re-matches comment-pattern line) :comment
          (re-matches metadata-pattern line) :metadata
          :else :text)

        (= fc \|)
        (if (re-matches table-pattern line) :table :text)

        (= fc \[)
        (if (re-matches footnote-def-pattern line) :footnote-def :text)

        (= fc \:)
        (cond
          (re-matches property-drawer-start-pattern line) :property-drawer-start
          (re-matches fixed-width-pattern line) :fixed-width
          (and (re-matches drawer-start-pattern line)
               (not (re-matches drawer-end-pattern line))) :drawer-start
          :else :text)

        (or (= fc \-) (= fc \+) (Character/isDigit (char fc)))
        (if (re-matches *list-item-pattern* line) :list-item :text)

        (= fc \\)
        (if (re-matches latex-env-begin-pattern line) :latex-env-begin :text)

        (and (or (= fc \C) (= fc \S) (= fc \D))
             (re-matches planning-line-pattern line))
        :planning

        ;; Letter bullet (a. or B)): only a list item with :alphabetical-lists?
        (and (Character/isLetter (char fc))
             (> (count trimmed) 1)
             (let [c2 (.charAt ^String trimmed 1)] (or (= c2 \.) (= c2 \))))
             (re-matches *list-item-pattern* line))
        :list-item

        :else :text))))

(defn- enrich-line
  "Add pre-computed :ltype to an indexed line map."
  [{:keys [line] :as m}]
  (assoc m :ltype (classify-line line)))

(def ^:private paragraph-break-types
  "Set of line types that break paragraph accumulation."
  #{:blank :headline :list-item :table :block-begin :property-drawer-start
    :drawer-start :comment :fixed-width :footnote-def :html-line :latex-line
    :latex-env-begin :affiliated :planning :metadata})

(defn- unescape-comma
  "Remove the first comma of a comma-escaped line inside a block.
   Mirrors Emacs org-unescape-code-in-string: un-escaping removes one comma
   from lines starting (after optional indentation) with ,* ,#+ ,,* or ,,#+."
  [line]
  (str/replace-first line #"^([ \t]*),(,*(?:\*|#\+))" "$1$2"))

(defn- escape-comma
  "Add a leading comma to escape lines that need it inside blocks.
   Mirrors Emacs org-escape-code-in-string: lines starting (after optional
   indentation) with * #+ ,* or ,#+ get one comma prepended."
  [line]
  (str/replace-first line #"^([ \t]*)(,*(?:\*|#\+))" "$1,$2"))

(defn- transform-block-lines
  "Apply a per-line transform to block content."
  [f content]
  (->> (str/split-lines content) (map f) (str/join "\n")))

(def escape-block-content
  "Add comma escapes to lines that need it in block content.
   Public helper for consumers writing block content back to Org text;
   the parser itself only unescapes."
  (partial transform-block-lines escape-comma))

(def ^:private unwrap-break-current-pattern
  "Union of the line patterns that break unwrapping when the line is the
   current one: blank, headline, metadata, table, comment, any `:key:` line
   (subsumes property lines, drawer start/end, :PROPERTIES:/:END:),
   fixed-width and planning. A single native regex match replaces a dozen
   predicate calls, which matters under SCI (Babashka) where each call is
   interpreted. Keep in sync with the individual patterns above."
  #"(?x)\*+\s.*                # headline (anchored at column 0)
    |\s*
     (?:\#\+\w+(?:\[[^\]]*\])?:.* # metadata, affiliated keyword
       |\|.*                   # table
       |\#(?:\s.*)?            # comment
       |:[\w_-]+:.*            # property line, drawer start/end
       |:(?:\ .*)?             # fixed width
       |(?:(?:CLOSED|SCHEDULED|DEADLINE):\s*(?:<[^>]+>|\[[^\]]+\])\s*)+ # planning
     )?                        # empty alternative: blank line
   ")

(def ^:private unwrap-break-next-pattern
  "Like unwrap-break-current-pattern, plus the patterns that only break
   unwrapping as the next line: list item, block begin, footnote
   definition and LaTeX environment begin."
  #"(?x)\*+\s.*                # headline (anchored at column 0)
    |\[fn:[^\]:]+\].*          # footnote definition (anchored at column 0)
    |\s*
     (?:\#\+\w+(?:\[[^\]]*\])?:.* # metadata, affiliated keyword
       |\|.*                   # table
       |\#(?:\s.*)?            # comment
       |:[\w_-]+:.*            # property line, drawer start/end
       |:(?:\ .*)?             # fixed width
       |(?:(?:CLOSED|SCHEDULED|DEADLINE):\s*(?:<[^>]+>|\[[^\]]+\])\s*)+ # planning
       |(?:[-+*]|\d+[.)])(?:\s.*|(?<!\*)) # list item
       |\#\+(?i:BEGIN).*       # block begin
       |\\begin\{([^}]+)\}(?!.*\\end\{\1\}).* # LaTeX environment begin
     )?                        # empty alternative: blank line
   ")

(defn- hard-break? [current-line next-line]
  (or (some? (re-matches unwrap-break-current-pattern current-line))
      (some? (re-matches unwrap-break-next-pattern next-line))
      ;; Letter bullets, recognised only with :alphabetical-lists?
      (and (identical? *list-item-pattern* alphabetical-list-item-pattern)
           (some? (re-matches *list-item-pattern* next-line)))))

;; Org entities - maps \name to Unicode/ASCII equivalent
(def ^:private org-entities
  {"\\alpha" "α"
   "\\beta" "β"
   "\\gamma" "γ"
   "\\delta" "δ"
   "\\epsilon" "ε"
   "\\zeta" "ζ"
   "\\eta" "η"
   "\\theta" "θ"
   "\\iota" "ι"
   "\\kappa" "κ"
   "\\lambda" "λ"
   "\\mu" "μ"
   "\\nu" "ν"
   "\\xi" "ξ"
   "\\pi" "π"
   "\\rho" "ρ"
   "\\sigma" "σ"
   "\\tau" "τ"
   "\\upsilon" "υ"
   "\\phi" "φ"
   "\\chi" "χ"
   "\\psi" "ψ"
   "\\omega" "ω"
   "\\Alpha" "Α"
   "\\Beta" "Β"
   "\\Gamma" "Γ"
   "\\Delta" "Δ"
   "\\Epsilon" "Ε"
   "\\Zeta" "Ζ"
   "\\Eta" "Η"
   "\\Theta" "Θ"
   "\\Iota" "Ι"
   "\\Kappa" "Κ"
   "\\Lambda" "Λ"
   "\\Mu" "Μ"
   "\\Nu" "Ν"
   "\\Xi" "Ξ"
   "\\Pi" "Π"
   "\\Rho" "Ρ"
   "\\Sigma" "Σ"
   "\\Tau" "Τ"
   "\\Upsilon" "Υ"
   "\\Phi" "Φ"
   "\\Chi" "Χ"
   "\\Psi" "Ψ"
   "\\Omega" "Ω"
   "\\rarr" "→"
   "\\larr" "←"
   "\\uarr" "↑"
   "\\darr" "↓"
   "\\harr" "↔"
   "\\rArr" "⇒"
   "\\lArr" "⇐"
   "\\uArr" "⇑"
   "\\dArr" "⇓"
   "\\hArr" "⇔"
   "\\to" "→"
   "\\gets" "←"
   "\\pm" "±"
   "\\times" "×"
   "\\div" "÷"
   "\\leq" "≤"
   "\\geq" "≥"
   "\\neq" "≠"
   "\\approx" "≈"
   "\\infty" "∞"
   "\\sum" "∑"
   "\\prod" "∏"
   "\\int" "∫"
   "\\partial" "∂"
   "\\nabla" "∇"
   "\\sqrt" "√"
   "\\in" "∈"
   "\\notin" "∉"
   "\\subset" "⊂"
   "\\supset" "⊃"
   "\\cap" "∩"
   "\\cup" "∪"
   "\\emptyset" "∅"
   "\\forall" "∀"
   "\\exists" "∃"
   "\\neg" "¬"
   "\\land" "∧"
   "\\lor" "∨"
   "\\oplus" "⊕"
   "\\otimes" "⊗"
   "\\dots" "…"
   "\\ldots" "…"
   "\\hellip" "…"
   "\\mdash" "—"
   "\\ndash" "–"
   "\\laquo" "«"
   "\\raquo" "»"
   "\\lsquo" "'"
   "\\rsquo" "'"
   "\\ldquo" "\""
   "\\rdquo" "\""
   "\\deg" "°"
   "\\pound" "£"
   "\\euro" "€"
   "\\yen" "¥"
   "\\copy" "©"
   "\\reg" "®"
   "\\trade" "™"
   "\\sect" "§"
   "\\para" "¶"
   "\\dagger" "†"
   "\\ddagger" "‡"
   "\\bull" "•"
   "\\nbsp" "\u00A0"
   "\\amp" "&"
   "\\lt" "<"
   "\\gt" ">"
   "\\checkmark" "✓"})

(def ^:private entity-pattern
  "Pre-compiled regex alternation of all org entity keys, sorted by descending length for safe matching.
   An entity ends with {} (which it consumes: \\alpha{}s) or where no
   alphabetic character follows (Org mode convention)."
  (->> (keys org-entities)
       (sort-by (comp - count))
       (map #(str (java.util.regex.Pattern/quote %) "(?:\\{\\}|(?![a-zA-Z]))"))
       (str/join "|")
       re-pattern))

(defn- replace-entities
  "Replace Org entities (\\name or \\name{}) with their Unicode equivalents in a single pass."
  [text]
  (if (str/includes? text "\\")
    (str/replace text entity-pattern
                 #(get org-entities (if (str/ends-with? % "{}") (subs % 0 (- (count %) 2)) %) %))
    text))

;; Inline Parsing
;; Parses Org inline markup into a vector of typed nodes.

(def ^:private emphasis-pre-punct #{\- \( \{ \' \"})

(def ^:private emphasis-post-punct #{\- \. \, \; \: \! \? \' \" \) \} \[})

(defn- emphasis-pre? [^String text ^long i]
  (or (zero? i)
      (let [c (.charAt text (dec i))]
        (or (Character/isSpaceChar c) (Character/isWhitespace c)
            (contains? emphasis-pre-punct c)))))

(defn- emphasis-post? [^String text ^long i]
  (or (= i (dec (.length text)))
      (let [c (.charAt text (inc i))]
        (or (Character/isSpaceChar c) (Character/isWhitespace c)
            (contains? emphasis-post-punct c)))))

(defn- find-close-marker
  "Find matching close position for an emphasis or literal marker.
   Returns index of closing marker, or nil.
   Jumps between marker occurrences with String.indexOf so the scan runs
   at host speed even when the surrounding code is interpreted (Babashka)."
  [^String text ^long open marker]
  (let [mc (int (char marker))
        len (.length text)
        content-start (inc open)]
    (when (and (< content-start len)
               (not (Character/isWhitespace (.charAt text content-start))))
      (loop [j (.indexOf text mc (inc content-start))]
        (when (pos? j)
          (if (and (not (Character/isWhitespace (.charAt text (dec j))))
                   (emphasis-post? text j))
            j
            (recur (.indexOf text mc (inc j)))))))))

(defn- classify-link-url
  "Classify a link URL into type and target."
  [url]
  (cond
    (str/starts-with? url "*")  {:link-type :heading :target (subs url 1)}
    (str/starts-with? url "#")  {:link-type :custom-id :target (subs url 1)}
    :else (if-let [[_ t target] (re-matches link-type-pattern url)]
            {:link-type (keyword t)
             :target (if (and (= t "file") (str/starts-with? target "//"))
                       (subs target 2)
                       target)}
            (if (or (str/starts-with? url "/")
                    (str/starts-with? url "~")
                    (str/starts-with? url "./")
                    (str/starts-with? url "../"))
              {:link-type :file :target url}
              {:link-type nil :target url}))))

(declare ^:private do-parse-inline)

(defn- re-find-at
  "Match pattern at position i in text, without creating a substring.
   Returns same format as re-find: the match string if no groups, or a vector
   [full group1 group2 ...] if there are groups. Returns nil if no match.
   The patterns all start with ^, which only matches at i: lookingAt tries
   that position alone, where find would retry up to the end of the text."
  [^java.util.regex.Pattern pattern ^String text ^long i]
  (let [m (doto (.matcher pattern text) (.region i (.length text)))]
    (when (.lookingAt m)
      ;; re-groups is compiled into bb, unlike a loop over the groups
      (re-groups m))))

(defn- link-node [url description]
  (let [{:keys [link-type target]} (classify-link-url url)]
    (cond-> {:type :link :url url}
      description (assoc :children (do-parse-inline description))
      link-type   (assoc :link-type link-type)
      target      (assoc :target target))))

(defn- try-parse-link
  "Parse the link opened by [[ at position i: [[url][description]] or
   [[url]], closed by the first ]] found. Scanned with indexOf: a regex
   alternation repeated over the link text recurses once per character
   and overflows the stack on a long paragraph with an unclosed [[."
  [^String text ^long i]
  (let [start (+ i 2)
        j     (.indexOf text "]" (int start))]
    (when-not (neg? j)
      (if (and (< (inc j) (.length text)) (= (.charAt text (inc j)) \[))
        (let [k (.indexOf text "]]" (int (+ j 2)))]
          (when-not (neg? k)
            [(link-node (subs text start j) (subs text (+ j 2) k)) (+ k 2)]))
        (let [k (.indexOf text "]]" (int j))]
          (when-not (neg? k)
            [(link-node (subs text start k) nil) (+ k 2)]))))))

(defn- matching-bracket
  "Index of the ] closing the [ at position open of text, brackets in
   between being balanced; nil when there is none."
  [^String text ^long open]
  (loop [from (inc open) depth 1]
    (let [close (.indexOf text "]" (int from))
          inner (.indexOf text "[" (int from))]
      (cond
        (neg? close)            nil
        (< -1 inner close)      (recur (inc inner) (inc depth))
        (= depth 1)             close
        :else                   (recur (inc close) (dec depth))))))

(defn- try-parse-footnote
  "Parse the footnote opened by [fn: at position i: an inline definition
   [fn:label:text] or [fn::text], whose text may hold brackets when they
   are balanced (a link), or a reference [fn:label]."
  [^String text ^long i]
  (or (when-let [[head label] (re-find-at #"^\[fn:([^\]\[:]*):" text i)]
        (let [start (+ i (count head))
              close (matching-bracket text i)]
          (when (and close (> (long close) start))
            [{:type :footnote-inline :label label
              :children (do-parse-inline (subs text start close))}
             (inc (long close))])))
      (when-let [[full label] (re-find-at #"^\[fn:([\w-]+)\]" text i)]
        [{:type :footnote-ref :label label}
         (+ i (count full))])))

(def ^:private active-timestamp-pattern
  (re-pattern (str "^(<" (ts-inner-pattern ">") ">)")))
(def ^:private inactive-timestamp-pattern
  (re-pattern (str "^(\\[" (ts-inner-pattern "\\]") "\\])")))

(defn- try-parse-timestamp [text i active?]
  (let [pat (if active? active-timestamp-pattern inactive-timestamp-pattern)]
    (when-let [[full raw] (re-find-at pat text i)]
      (let [parsed (parse-org-timestamp raw)
            repeater (parse-org-repeater raw)
            ;; For a time range (start/end), expose the two ends structurally
            ;; in addition to the joined :value string (backward compatible).
            range-parts (when (str/includes? parsed "/") (str/split parsed #"/"))]
        [(cond-> {:type :timestamp :raw raw :value parsed :active active?}
           range-parts (assoc :start (first range-parts) :end (second range-parts))
           repeater (assoc :repeater repeater))
         (+ i (count full))]))))

(def ^:private emphasis-type {\* :bold \/ :italic \_ :underline \+ :strike})
(def ^:private literal-type {\~ :code \= :verbatim})

(defn- match-latex-fragment
  "If `pattern` matches at position `i`, return a [node end] pair for a
   LaTeX fragment of the given `kind`. Otherwise nil."
  [text i pattern kind]
  (when-let [[full content] (re-find-at pattern text i)]
    [{:type :latex-fragment :kind kind :value content}
     (+ i (count full))]))

(defn- try-parse-latex-fragment
  "Try to parse a LaTeX math fragment starting at position i.
   Recognises \\(...\\), \\[...\\], $$...$$, and $...$ (Org's strict rule).
   Returns [node end] or nil."
  [^String text ^long i]
  (case (.charAt text i)
    \\ (or (match-latex-fragment text i latex-paren-pattern   :paren)
           (match-latex-fragment text i latex-bracket-pattern :bracket))
    \$ (or (match-latex-fragment text i latex-dollars-pattern :dollars)
           (when (or (zero? i) (not= (.charAt text (dec i)) \$))
             (or (match-latex-fragment text i latex-dollar1-pattern :dollar)
                 (match-latex-fragment text i latex-dollar-pattern  :dollar))))
    nil))

(defn- try-parse-macro [text i]
  (when-let [[full macro-name args] (re-find-at #"^\{\{\{([\w\-]+)(?:\((.+?)\))?\}\}\}" text i)]
    [(cond-> {:type :macro :args [] :name macro-name}
         args (assoc :args (mapv #(str/replace % #"\\," ",")
                                 (str/split args #"(?<!\\),"))))
       (+ i (count full))]))

(def ^:private inline-special-pattern
  "Characters that can start an inline node: links, footnotes and inactive
   timestamps ([), active timestamps (<), emphasis (* / _ +), literals (~ =),
   LaTeX fragments (\\ $), macros ({). Plain text between two special characters is
   copied in a single step, which keeps the scan at host speed under SCI."
  #"[\[{<*/_+~=$\\]")

(declare ^:private do-parse-inline)

(defn- parse-inline-at
  "Parse the inline construct opened by the special character c at
   position i of text. Returns [node end], or nil when c is plain text."
  [^String text ^long i c]
  (let [len (.length text)]
    (cond
      ;; macro: {{{
      (and (= c \{) (< (+ 2 i) len)
           (= (.charAt text (inc i)) \{)
           (= (.charAt text (+ 2 i)) \{))
      (try-parse-macro text i)

      ;; Link: [[
      (and (= c \[) (< (inc i) len) (= (.charAt text (inc i)) \[))
      (try-parse-link text i)

      ;; Footnote: [fn:
      (and (= c \[) (.startsWith text "[fn:" i))
      (try-parse-footnote text i)

      ;; Active timestamp: <YYYY-
      (and (= c \<) (<= (+ i 6) len)
           (Character/isDigit (.charAt text (inc i))))
      (try-parse-timestamp text i true)

      ;; Inactive timestamp: [YYYY-
      (and (= c \[) (<= (+ i 6) len)
           (Character/isDigit (.charAt text (inc i))))
      (try-parse-timestamp text i false)

      ;; Emphasis: * / _ +
      (and (contains? emphasis-type c) (emphasis-pre? text i))
      (when-let [close (find-close-marker text i c)]
        [{:type (emphasis-type c)
          :children (do-parse-inline (subs text (inc i) close))}
         (inc close)])

      ;; Code/verbatim: ~ =
      (and (contains? literal-type c) (emphasis-pre? text i))
      (when-let [close (find-close-marker text i c)]
        [{:type (literal-type c) :value (subs text (inc i) close)}
         (inc close)])

      ;; LaTeX fragment: \( \[ $ $$
      (or (= c \\) (= c \$))
      (try-parse-latex-fragment text i))))

(defn- do-parse-inline [^String text]
  (let [len (.length text)
        buf (StringBuilder.)
        matcher (.matcher ^java.util.regex.Pattern inline-special-pattern text)
        flush-buf! (fn [nodes]
                     (if (pos? (.length buf))
                       (let [t (replace-entities (.toString buf))]
                         (.setLength buf 0)
                         (conj nodes {:type :text :value t}))
                       nodes))]
    (loop [pos (long 0) nodes []]
      (if (>= pos len)
        (flush-buf! nodes)
        ;; Copy plain text up to the next special character in one call
        (let [i (if (.find matcher (int pos)) (long (.start matcher)) (long len))]
          (when (> i pos)
            (.append buf ^CharSequence text (int pos) (int i)))
          (if (>= i len)
            (flush-buf! nodes)
            (let [c (.charAt text i)]
              (if-let [[node end] (parse-inline-at text i c)]
                (recur (long end) (conj (flush-buf! nodes) node))
                (do (.append buf c) (recur (inc i) nodes))))))))))

(defn parse-inline
  "Parse Org inline markup into a vector of typed nodes.
   Returns [] for nil or blank input."
  [text]
  (if (or (nil? text) (str/blank? text))
    []
    (let [text (str/trim text)]
      ;; No special character, hence no node and no entity (\\name)
      (if (re-find inline-special-pattern text)
        (do-parse-inline text)
        [{:type :text :value text}]))))

(defn- do-inline-text [nodes bare-link]
  (when (seq nodes)
    (str/join
     (map (fn [node]
            (case (:type node)
              :text (:value node)
              :code (:value node)
              :verbatim (:value node)
              :timestamp (:raw node)
              :latex-fragment (case (:kind node)
                                :paren   (str "\\(" (:value node) "\\)")
                                :bracket (str "\\[" (:value node) "\\]")
                                :dollars (str "$$" (:value node) "$$")
                                :dollar  (str "$" (:value node) "$")
                                (:value node))
              :footnote-ref (str "[fn:" (:label node) "]")
              :footnote-inline ""
              :link (if (seq (:children node))
                      (do-inline-text (:children node) bare-link)
                      (bare-link node))
              (if (:children node) (do-inline-text (:children node) bare-link) "")))
          nodes))))

(defn- bare-link-fn
  "Resolve the :bare-link option into a node → string function."
  [v]
  (cond
    (= :omit v) (constantly "")
    (fn? v)     v
    :else       :url))                  ; :url, and any unknown value, keep the URL

(defn inline-text
  "Extract plain text from a vector of inline nodes.

   A link with a description always contributes its description; the
   option below only governs a link written without one, where there is
   no text to fall back on. Options:

   :bare-link  :url   the raw URL (default)
               :omit  nothing at all
               f      a function of the link node returning the text to
                      use (nil counts as none) — lets callers decide per
                      link, e.g. dropping image links, whose URL is not
                      visible in rendered output, while keeping the URL
                      of plain links, which is:
                        #(when-not (image-url? (:url %)) (:url %))

   Contributing nothing leaves the spaces that surrounded the link, so
   callers wanting tidy output should collapse whitespace afterwards."
  ([nodes] (do-inline-text nodes :url))
  ([nodes opts] (do-inline-text nodes (bare-link-fn (get opts :bare-link :url)))))

(defn- leading-indent
  "Width of the leading whitespace (as \\s) of a line, a tab reaching the
   next multiple of 8 as in Emacs."
  [^CharSequence line]
  (let [n (.length line)]
    (loop [i 0 col 0]
      (if (< i n)
        (let [c (int (.charAt line i))]
          (cond
            (= c 9)                      (recur (inc i) (* 8 (inc (quot col 8))))
            (or (= c 32) (<= 10 c 13))   (recur (inc i) (inc col))
            :else                        col))
        col))))

(defn- unwrap-text-indexed
  "Unwrap text while preserving original line numbers.
   Returns a vector of {:line string :num original-line-number} maps.
   A line being joined is held in a StringBuilder, which the tests below
   read as a CharSequence: building it with str at each step copied the
   whole joined line, quadratic in the length of a paragraph."
  [input]
  (let [indexed (index-lines (split-lines input))
        done    (fn [current]
                  (if (instance? StringBuilder (:line current))
                    {:line (str (:line current)) :num (:num current)}
                    current))
        numeric? (identical? *list-item-pattern* numeric-list-item-pattern)]
    (loop [result []
           remaining indexed
           end-pattern nil                ; set inside a block, environment or drawer
           bullets nil]                   ; indents of the enclosing list items, innermost first
      (if (empty? remaining)
        result
        (let [{:keys [line num] :as current} (first remaining)
              rest-lines (rest remaining)
              next-line (:line (first rest-lines))]
          (if (and end-pattern (not (section-end? line)))
            ;; Inside a block, keep lines as they are up to the end line (a
            ;; headline ends an unterminated block, then unwrapping resumes)
            (recur (conj result current) rest-lines
                   (when-not (re-matches end-pattern line) end-pattern)
                   bullets)
            (let [fc      (when (pos? (.length ^CharSequence line))
                            (.charAt ^CharSequence line 0))
                  ;; A line starting with a letter is plain text (unless
                  ;; letters are bullets): spare it the regexes below
                  plain?  (and numeric? fc (Character/isLetter (char fc)))
                  ;; Is the line (joined so far) a list item? (.matches
                  ;; tests it without copying the groups.) Never with a
                  ;; star in column 0, which makes a headline.
                  li?     (cond
                            (some? (:li? current))          (:li? current)
                            (or plain? (nil? fc) (= fc \*)) false
                            :else (.matches (re-matcher *list-item-pattern* line)))
                  ;; The items still open after this line: one indented no
                  ;; more than a bullet ends that item, two blank lines end
                  ;; them all.
                  bullets (cond
                            (and (not li?) (nil? bullets)) nil
                            (str/blank? line) (when-not (or (nil? next-line) (str/blank? next-line))
                                                bullets)
                            :else (let [indent (leading-indent line)
                                        open   (loop [b bullets]
                                                 (if (and b (>= (long (first b)) indent))
                                                   (recur (next b))
                                                   b))]
                                    (if li? (cons indent open) open)))]
              (if-let [end (when-not plain?
                             (or (block-end-pattern line)
                                 ;; A drawer is opaque too, when it is one: left
                                 ;; unterminated, it is a paragraph (see parse-drawer)
                                 (when (and (re-matches drawer-start-pattern line)
                                            (not (re-matches drawer-end-pattern line))
                                            (drawer-terminated? rest-lines))
                                   drawer-end-pattern)))]
                (recur (conj result (done current)) rest-lines end bullets)
                (if (or (nil? next-line) (hard-break? line next-line)
                        ;; Not a continuation line: it is not indented past
                        ;; the bullet of the item it follows
                        (and bullets
                             (<= (leading-indent next-line) (long (first bullets)))))
                  (recur (conj result (done current)) rest-lines nil bullets)
                  (let [trimmed-next    (str/trim next-line)
                        normalized-next (if li?
                                          (str/replace trimmed-next #"\s+" " ")
                                          trimmed-next)
                        ^StringBuilder sb (if (instance? StringBuilder line)
                                            line
                                            (StringBuilder. ^String line))]
                    (.append sb " ")
                    (.append sb ^String normalized-next)
                    (recur result
                           ;; A list item stays one while what gets appended
                           ;; has no line terminator, which . (in the
                           ;; pattern's (.*)$) rejects: testing the appended
                           ;; text alone keeps this linear.
                           (cons {:line sb :num num
                                  :li? (if li?
                                         (.matches (re-matcher #".*" normalized-next))
                                         (.matches (re-matcher *list-item-pattern* sb)))}
                                 (rest rest-lines))
                           nil
                           bullets)))))))))))


;; Parsing Helpers
(defn- parse-footnote-def [line]
  (when-let [[_ label content] (re-matches footnote-def-pattern line)]
    {:label label :content content}))

(defn- parse-attr-string
  "Parse an Org attribute string like ':width 300 :alt \"An image\" :class a b'
   into a map {:width \"300\" :alt \"An image\" :class \"a b\"}. As in Org
   (org-export-read-attribute), a value runs up to the next :key and may
   hold several words; a key without a value is dropped. Quotes around a
   whole value are removed."
  [s]
  (when (non-blank? s)
    (let [m   (re-matcher #"(?:^|\s+):([-\w]+)(?=\s|$)" s)
          add (fn [result k ^long start ^long end]
                (let [v          (str/trim (subs s start end))
                      [_ quoted] (re-matches #"\"([^\"]*)\"" v)]
                  (cond
                    (nil? k)       result
                    quoted         (assoc result (keyword k) quoted)
                    (str/blank? v) result
                    :else          (assoc result (keyword k) v))))]
      (loop [result {} k nil start 0]
        (if (.find m)
          (recur (add result k start (.start m)) (.group m 1) (.end m))
          (add result k start (count s)))))))

(defn- parse-affiliated-keywords
  "Collect consecutive affiliated keyword lines (#+attr_html, #+caption, etc.)
   Returns [affiliated-map remaining-lines] where affiliated-map has:
   {:caption \"...\" :name \"...\" :attr {:html {...} :org {...}}}"
  [indexed-lines]
  (loop [[{:keys [line]} & more :as remaining] indexed-lines
         result {:attr {}}]
    (if (or (empty? remaining) (not (affiliated-keyword? line)))
      [result remaining]
      (let [[_ kw-name value] (re-matches affiliated-keyword-pattern line)
            kw-lower (str/lower-case kw-name)]
        (cond
          ;; #+attr_html: :key val ... -> nested under :attr :html
          (str/starts-with? kw-lower "attr_")
          (let [attr-type (keyword (subs kw-lower 5)) ; "attr_html" -> :html
                parsed-attrs (parse-attr-string value)
                current-attrs (get-in result [:attr attr-type] {})]
            (recur more (assoc-in result [:attr attr-type] (merge current-attrs parsed-attrs))))

          ;; #+caption: ... -> :caption
          (= kw-lower "caption")
          (recur more (assoc result :caption (str/trim value)))

          ;; #+name: ... -> :name
          (= kw-lower "name")
          (recur more (assoc result :name (str/trim value)))

          ;; Other keywords (#+header, #+results, etc.)
          :else
          (recur more (assoc result (keyword kw-lower) (str/trim value))))))))

(defn- has-affiliated-keywords?
  "Check if the affiliated map contains any actual content."
  [affiliated]
  (some (fn [[k v]]
          (cond
            (= k :attr) (and (map? v) (seq v))  ; :attr is a nested map
            (string? v) (non-blank? v)
            :else (seq v)))
        affiliated))

(defn- attach-affiliated
  "Attach affiliated keywords to a node if present."
  [node affiliated]
  (if (has-affiliated-keywords? affiliated)
    (let [;; Clean up empty :attr map
          cleaned (if (empty? (:attr affiliated))
                    (dissoc affiliated :attr)
                    affiliated)]
      (assoc node :affiliated cleaned))
    node))

(defn- ordered-marker? [marker]
  (boolean (re-matches #"(?:\d+|[A-Za-z])[.)]" marker)))

(defn- alphabetical-case
  "For a letter bullet (a. or A)), :lower or :upper; nil otherwise."
  [marker]
  (let [c (.charAt ^String marker 0)]
    (cond (Character/isLowerCase c) :lower
          (Character/isUpperCase c) :upper)))

;; Parsing Functions
(defn- parse-headline [line]
  (when-let [[_ stars todo priority title tags] (re-matches *headline-full-pattern* line)]
    {:level (count stars)
     :title (str/trim title)
     :todo (when todo (keyword todo))
     :priority priority
     :tags (when tags
             (->> (str/split (str/replace tags #"^:|:$" "") #":")
                  (filter seq)
                  vec))}))

(defn- kvs->meta
  "Build the :meta map from [key value] pairs: an array map in first-
   occurrence order (array-map, unlike assoc, keeps that order past 8
   entries); a repeated key holds a vector of its values."
  [kvs]
  (let [by-key (reduce (fn [m [k v]] (update m k (fnil conj []) v)) {} kvs)]
    (apply array-map
           (mapcat (fn [k]
                     (let [vs (by-key k)]
                       [k (if (= 1 (count vs)) (first vs) vs)]))
                   (distinct (map first kvs))))))

(declare ^:private parse-property-drawer)

(defn- parse-metadata
  "Parse the document preamble: every #+KEY: value line before the first
   content line, whatever the keyword (#+TITLE:, #+TODO:, #+STARTUP:,
   #+HTML_HEAD:, ...), so consumers can re-emit it. Returns [meta remaining],
   meta mapping lower-cased keywords to values in file order (see kvs->meta).
   Blank lines are skipped. Comment lines (# -*- mode: org -*-) and one
   property drawer (the document's, as org-roam writes it) do not end the
   preamble: they are handed back at the head of remaining, to be parsed
   as content. Content directives (#+html:, #+latex:, #+BEGIN_*) and
   affiliated keywords (#+caption:, #+name:, #+attr_*: — they belong to
   the next element) end the preamble, as does any other line."
  [indexed-lines]
  (loop [[{:keys [line ltype] :as l} & more :as remaining] indexed-lines
         kvs     []
         skipped []                       ; comments, the drawer, blanks between
         drawer? false]
    (let [end #(vector (kvs->meta kvs) (concat skipped remaining))]
      (cond
        (nil? l)
        (end)

        (= ltype :blank)
        (recur more kvs (cond-> skipped (seq skipped) (conj l)) drawer?)

        (= ltype :comment)
        (recur more kvs (conj skipped l) drawer?)

        (= ltype :property-drawer-start)
        (if-let [[_ after] (and (not drawer?) (parse-property-drawer remaining))]
          (let [n (- (count remaining) (count after))]
            (recur after kvs (into skipped (take n remaining)) true))
          (end))

        (or (html-line? line) (latex-line? line) (block-begin? line)
            (affiliated-keyword? line))
        (end)

        :else
        (if-let [[_ key value] (re-matches metadata-pattern line)]
          (recur more (conj kvs [(keyword (str/lower-case key)) (str/trim value)]) skipped drawer?)
          (end))))))

(defn- parse-property-drawer
  "Parse a :PROPERTIES: drawer at the head of indexed-lines.
   Returns [properties remaining], or nil when the drawer is unterminated
   before the next headline — Emacs then treats the lines as a paragraph, so
   callers must fall back without consuming anything."
  [indexed-lines]
  (if (and (seq indexed-lines)
           (re-matches property-drawer-start-pattern (:line (first indexed-lines))))
    (loop [[{:keys [line]} & more :as remaining] (rest indexed-lines)
           properties {}]
      (cond
        (or (empty? remaining) (headline? line))
        nil

        (re-matches drawer-end-pattern line)
        [properties more]

        :else
        (if-let [[_ key value] (re-matches property-pattern line)]
          (recur more (assoc properties (keyword (str/lower-case key)) (str/trim (or value ""))))
          (recur more properties))))
    [{} indexed-lines]))

(defn- parse-drawer
  "Parse a generic drawer (:NAME: ... :END:).
   Returns [drawer-node remaining-lines], or nil when there is no :END: before
   the next headline or end of input — Emacs then treats the lines as a
   paragraph, so callers must fall back without consuming anything."
  [indexed-lines]
  (let [{:keys [line num]} (first indexed-lines)]
    (when-let [[_ drawer-name] (re-matches drawer-start-pattern line)]
      (when (not (re-matches property-drawer-start-pattern line))
        (loop [[{l :line} & more :as remaining] (rest indexed-lines)
               content []]
          (cond
            (or (empty? remaining) (headline? l))
            nil

            (re-matches drawer-end-pattern l)
            [(make-node :drawer :drawer-name (str/lower-case drawer-name)
                        :content (str/join "\n" content) :line num)
             more]

            :else
            (recur more (conj content l))))))))

(defn- parse-description-item
  "Check if content matches a description list item (term :: definition).
   Returns {:term term :definition definition} or nil."
  [^String content]
  (when-let [[_ term definition] (and (str/includes? content "::")
                                      (re-matches description-item-pattern content))]
    {:term (str/trim term) :definition (if definition (str/trim definition) "")}))

;; Forward declaration - parse-content is defined later but needed by parse-list-items
(declare ^:private parse-content)

(defn- normalize-marker
  "Normalize list markers for comparison.
   Ordered markers all normalize to :ordered.
   Unordered markers stay as-is (-, +, *)."
  [marker]
  (if (ordered-marker? marker)
    :ordered
    marker))

(defn- collect-list-item-body
  "Collect continuation lines for a list item until we hit another list item,
   a headline, or a non-indented line (outside of blocks). Returns [indexed-lines remaining].
   Continuation lines must be indented strictly more than min-indent (the list item's
   marker indent) — matching Org's rule that a line belongs to a list item only if it
   is indented past the item's bullet. The lines must carry their :ltype (see
   enrich-line), from which every decision here is taken."
  [indexed-lines min-indent]
  (loop [[{:keys [line ltype] :as current} & more :as remaining] indexed-lines
         collected []
         end-pattern nil]                 ; set inside a block, environment or drawer
    (cond
      (empty? remaining)
      [collected remaining]

      ;; A headline ends the item, even inside an unterminated block
      (= ltype :headline)
      [collected remaining]

      ;; Inside a block, environment or drawer, take every line up to its
      ;; end line, whatever its indentation
      end-pattern
      (recur more (conj collected current)
             (when-not (re-matches end-pattern line) end-pattern))

      ;; A block or LaTeX environment opened past the bullet belongs to the
      ;; item; opened at or left of it, it ends the list (the :else below)
      (and (#{:block-begin :latex-env-begin} ltype)
           (> (leading-indent line) min-indent))
      (recur more (conj collected current) (block-end-pattern line))

      ;; Drawer opened on a continuation line: the item extends to the
      ;; drawer's :END: even when the drawer contents are not indented
      (and (= ltype :drawer-start)
           (> (leading-indent line) min-indent)
           (drawer-terminated? more))
      (recur more (conj collected current) drawer-end-pattern)

      ;; A list item at the same or a lesser indent starts the next item
      (and (= ltype :list-item) (<= (leading-indent line) min-indent))
      [collected remaining]

      ;; A blank line belongs to the item only if a continuation follows: a
      ;; deeper list item or a line indented past the bullet. Otherwise leave
      ;; it unconsumed for parse-content to count as a trailing blank. Two
      ;; blank lines end the item, as in Org.
      (= ltype :blank)
      (let [{next-line :line next-type :ltype :as next} (first more)]
        (if (or (nil? next)
                (#{:blank :headline} next-type)
                (and (not= next-type :list-item)
                     (<= (leading-indent next-line) min-indent)))
          [collected remaining]
          (recur more (conj collected current) nil)))

      (> (leading-indent line) min-indent)
      (recur more (conj collected current) nil)

      :else
      [collected remaining])))

(def ^:private counter-set-pattern
  #"^\[@(?:start:)?(\d+|[A-Za-z])\]\s*(.*)$")

(defn- split-counter
  "Split an ordered item's leading counter-set cookie ([@5], [@start:5],
   [@c]) from its content, returning [counter content]. A letter counter
   counts from 1 for a/A, as in Org."
  [content]
  (if-let [[_ c rest] (re-matches counter-set-pattern content)]
    [(if (Character/isDigit (.charAt ^String c 0))
       (parse-long c)
       (- (int (Character/toUpperCase (.charAt ^String c 0))) 64))
     rest]
    [nil content]))

(defn- list-item-match
  "Parse a list item line, returning [indent marker content] or nil."
  [line]
  (when-let [[_ indent marker content] (re-matches *list-item-pattern* line)]
    [indent marker (or content "")]))

(defn- description-definition
  "For a description item `term ::` whose definition is empty, take the
   first content line of the body as the definition, every other line
   (#+KEY: lines included) staying in the body. Returns [definition body]."
  [desc-item body-lines]
  (if (and (str/blank? (:definition desc-item)) (seq body-lines))
    (let [skip?         #(or (str/blank? (:line %))
                             (ignored-keyword-line? (:line %)))
          first-content (first (drop-while skip? body-lines))]
      (if (and first-content (not (list-item? (:line first-content))))
        [(str/trim (:line first-content))
         (vec (remove #(identical? % first-content) body-lines))]
        ["" body-lines]))
    [(:definition desc-item) body-lines]))

(defn- parse-list-item
  "Build the item whose bullet line, at line num, holds content (the text
   after the bullet), reading its body from more. Returns [item remaining]."
  [content more indent ordered? num]
  (let [[counter content]  (if ordered? (split-counter content) [nil content])
        desc-item          (parse-description-item content)
        [body-lines rest-after-body] (collect-list-item-body more indent)
        [definition body]  (if desc-item
                             (description-definition desc-item body-lines)
                             [nil body-lines])
        children           (if (seq body) (first (parse-content body)) [])]
    [(make-node :list-item
                :counter counter
                :term (when desc-item (parse-inline (:term desc-item)))
                :definition (when desc-item (parse-inline definition))
                :content (when-not desc-item (parse-inline content))
                :children children
                :line num)
     rest-after-body]))

(defn- parse-list-items
  "Parse the items of the list starting at indexed-lines, whose lines must
   carry their :ltype (see enrich-line). Returns [items remaining]."
  [indexed-lines initial-indent initial-marker]
  (let [normalized-initial (normalize-marker initial-marker)
        ordered?           (= :ordered normalized-initial)
        ;; [indent marker content] of a line continuing this list, or nil.
        ;; Deeper list items never reach this loop: collect-list-item-body
        ;; consumes them into the current item's body, where parse-content
        ;; builds the sublist.
        sibling            (fn [line]
                             (when-let [[indent marker :as m] (list-item-match line)]
                               (when (and (= (leading-indent indent) initial-indent)
                                          (= (normalize-marker marker) normalized-initial))
                                 m)))]
    (loop [[{:keys [line ltype num]} & more :as remaining] indexed-lines
           items []]
      (cond
        (or (empty? remaining) (= ltype :headline))
        [items remaining]

        ;; A blank line stays in the list only before a sibling item; two
        ;; blank lines end it, as in Org. Otherwise leave it unconsumed for
        ;; parse-content to count as a trailing blank.
        (= ltype :blank)
        (let [{next-line :line next-type :ltype} (first more)]
          (if (and next-line
                   (not (#{:blank :headline} next-type))
                   (sibling next-line))
            (recur more items)
            [items remaining]))

        :else
        (if-let [[_ _ content] (sibling line)]
          (let [[item rest-lines] (parse-list-item content more initial-indent ordered? num)]
            (recur rest-lines (conj items item)))
          [items remaining])))))

(defn- process-list [indexed-lines]
  (let [{:keys [line num]} (first indexed-lines)]
    (if-let [[_ indent marker _] (re-matches *list-item-pattern* line)]
      (let [initial-indent     (leading-indent indent)
            [items rest-lines] (parse-list-items indexed-lines initial-indent marker)
            ordered            (ordered-marker? marker)
            is-description     (boolean (some :term items))]
        [(make-node :list :items items :ordered ordered
                    :alphabetical (alphabetical-case marker)
                    :description is-description :line num)
         rest-lines])
      [nil indexed-lines])))

(defn- parse-table [indexed-lines]
  ;; A leading separator (before any row) is a top border and suppresses
  ;; header interpretation — mirrors Emacs Org's ox-html behaviour.
  (let [start-line-num (:num (first indexed-lines))]
    (loop [[{:keys [line]} & more :as remaining] indexed-lines
           rows [] has-header false top-border? false]
      (cond
        (empty? remaining)
        [(make-node :table :rows rows :has-header has-header :line start-line-num) remaining]

        (re-matches table-separator-pattern line)
        (cond
          (empty? rows) (recur more rows has-header true)
          top-border?   (recur more rows has-header top-border?)
          :else         (recur more rows true       top-border?))

        (re-matches table-pattern line)
        (let [trimmed (str/trim line)
              cells (rest (str/split trimmed #"\|" -1))
              ;; A trailing | closes the last cell; without it the last
              ;; field is still a cell (Emacs accepts both).
              cells (if (str/ends-with? trimmed "|") (butlast cells) cells)
              row (mapv #(parse-inline (str/trim %)) cells)]
          (recur more (conj rows row) has-header top-border?))

        :else
        [(make-node :table :rows rows :has-header has-header :line start-line-num) remaining]))))

(defn- parse-block [indexed-lines]
  (let [{:keys [line num]} (first indexed-lines)]
    (if-let [[_ block-type args] (re-matches generic-block-begin-pattern line)]
      (let [block-type-lower (str/lower-case block-type)
            end-pattern (end-pattern-for block-type)
            make-block-node
            (fn [content & {:keys [warning]}]
              ;; Unescape comma-protected lines in the content
              (let [unescaped-content (map unescape-comma content)]
                (case block-type-lower
                  "src" (make-node
                         :src-block
                         :language (or (first (str/split (or args "") #"\s+")) "")
                         :args args :content (str/join "\n" unescaped-content) :line num
                         :warning warning :error-line (when warning num))
                  "quote" (let [content-str (str/join "\n" unescaped-content)
                                paras (->> (str/split content-str #"\n\n+")
                                           (remove str/blank?)
                                           (mapv #(make-node :paragraph :content (parse-inline %))))]
                            (make-node
                             :quote-block :children paras :line num
                             :warning warning :error-line (when warning num)))
                  (make-node :block :block-type (keyword block-type-lower)
                             :args (when args (str/trim args))
                             :content (str/join "\n" unescaped-content)
                             :line num :warning warning
                             :error-line (when warning num)))))]
        (loop [[{:keys [line]} & more :as remaining] (rest indexed-lines)
               content []]
          (cond
            ;; Unterminated: the block stops before the next headline, so
            ;; that the following sections survive
            (or (empty? remaining) (section-end? line))
            (do (add-parse-error! num (str "Unterminated " block-type " block"))
                [(make-block-node content :warning (str "Unterminated " block-type " block"))
                 remaining])
            (re-matches end-pattern line)
            [(make-block-node content) more]
            :else
            (recur more (conj content line)))))
      [nil indexed-lines])))

(defn- parse-consecutive-lines
  "Join the consecutive lines classified as node-type (their :ltype) into
   one node, each line passed through extract-fn."
  [indexed-lines extract-fn node-type]
  (let [start-line-num (:num (first indexed-lines))]
    (loop [[{:keys [line ltype]} & more :as remaining] indexed-lines
           content []]
      (if (or (empty? remaining) (not= ltype node-type))
        (when (seq content)
          [(make-node node-type :content (str/join "\n" content)
                      :line start-line-num) remaining])
        (recur more (conj content (extract-fn line)))))))

(defn- parse-comment [indexed-lines]
  (parse-consecutive-lines
   indexed-lines
   #(str/replace % #"^\s*#\s?" "") :comment))

(defn- parse-fixed-width [indexed-lines]
  (parse-consecutive-lines
   indexed-lines
   ;; A :fixed-width line is whitespace, ": ", then the content, or a
   ;; lone ":" (an empty line)
   #(or (second (re-matches fixed-width-pattern %)) "") :fixed-width))

(defn- parse-html-lines [indexed-lines]
  (parse-consecutive-lines
   indexed-lines
   #(second (re-matches html-line-pattern %)) :html-line))

(defn- parse-latex-lines [indexed-lines]
  (parse-consecutive-lines
   indexed-lines
   #(second (re-matches latex-line-pattern %)) :latex-line))

(defn- parse-latex-environment
  "Collect lines between \\begin{env} and the matching \\end{env}.
   Content is stored raw (including the \\begin and \\end lines)."
  [indexed-lines]
  (let [{:keys [line num]} (first indexed-lines)]
    (when-let [[_ env] (re-matches latex-env-begin-pattern line)]
      (let [end-pattern (latex-env-end-pattern-for env)]
        (loop [[{l :line} & more :as remaining] (rest indexed-lines)
               content [line]]
          (cond
            ;; Unterminated: stops before the next headline, like blocks
            (or (empty? remaining) (section-end? l))
            (let [warning (str "Unterminated \\begin{" env "} environment")]
              (add-parse-error! num warning)
              [(make-node :latex-environment :name env
                          :content (str/join "\n" content)
                          :line num :warning warning :error-line num)
               remaining])
            (re-matches end-pattern l)
            [(make-node :latex-environment :name env
                        :content (str/join "\n" (conj content l))
                        :line num)
             more]
            :else
            (recur more (conj content l))))))))

(defn- parse-footnote-definition [indexed-lines]
  (let [[{:keys [line num]} & more] indexed-lines]
    (when-let [{:keys [label content]} (parse-footnote-def line)]
      (loop [[{:keys [line]} & more2 :as remaining] more
             full-content [content]]
        (if (or (empty? remaining)
                (not (re-matches #"^\s+\S.*$" line)))
          [(make-node :footnote-def :label label
                      :content (parse-inline (str/join "\n" full-content)) :line num)
           remaining]
          (recur more2 (conj full-content (str/trim line))))))))

(defn- parse-paragraph [indexed-lines]
  (let [start-line-num (:num (first indexed-lines))]
    (loop [[{:keys [line ltype]} & more :as remaining] indexed-lines
           content []]
      (if (empty? remaining)
        (when (seq content)
          [(make-node :paragraph :content (parse-inline (str/join "\n" content))
                      :line start-line-num) []])
        (if (contains? paragraph-break-types ltype)
          (when (seq content)
            [(make-node :paragraph :content (parse-inline (str/join "\n" content))
                        :line start-line-num) remaining])
          (recur more (conj content line)))))))

(defn- parse-paragraph-forced
  "Parse a paragraph treating the first line as plain text even if its ltype
   would normally break a paragraph. Fallback for malformed input (e.g. an
   unterminated drawer), which Emacs parses as a paragraph."
  [indexed-lines]
  (parse-paragraph (cons (assoc (first indexed-lines) :ltype :text)
                         (rest indexed-lines))))

(defn- try-parse
  "Try a parser; return [rest-lines updated-nodes] or nil if parser returned nil."
  [parser remaining nodes]
  (when-let [[node rest-lines] (parser remaining)]
    [rest-lines (conj nodes node)]))

(defn- try-parse-with-affiliated
  "Try a parser with affiliated keywords; attach them to the resulting node."
  [parser remaining nodes affiliated]
  (when-let [[node rest-lines] (parser remaining)]
    [rest-lines (conj nodes (attach-affiliated node affiliated))]))

(defn- try-parse-maybe-affiliated
  "Try a parser, optionally attaching affiliated keywords to the result."
  [parser remaining nodes affiliated]
  (or (if affiliated
        (try-parse-with-affiliated parser remaining nodes affiliated)
        (try-parse parser remaining nodes))
      [(rest remaining) nodes]))

(defn- parse-content [indexed-lines]
  (loop [[{:keys [line ltype num]} & more :as remaining] indexed-lines
         nodes []
         pending-affiliated nil
         trailing-blanks 0]
    (if (empty? remaining)
      [nodes remaining trailing-blanks]
      (case ltype
        :blank
        (recur more nodes pending-affiliated (inc trailing-blanks))

        :headline
        [nodes remaining trailing-blanks]

        :affiliated
        (let [[affiliated rest-lines] (parse-affiliated-keywords remaining)]
          (recur rest-lines nodes affiliated 0))

        :property-drawer-start
        (if-let [[properties rest-lines] (parse-property-drawer remaining)]
          (let [drawer-node (make-node :property-drawer :properties properties
                                       :line (:num (first remaining)))]
            (recur rest-lines (conj nodes drawer-node) nil 0))
          ;; Unterminated: keep the lines as a paragraph (Emacs behavior)
          (let [{:keys [num]} (first remaining)
                _ (add-parse-error! num "Unterminated property drawer")
                [paragraph rest-lines] (parse-paragraph-forced remaining)]
            (recur rest-lines (conj nodes (attach-affiliated paragraph pending-affiliated)) nil 0)))

        :drawer-start
        (if-let [[drawer-node rest-lines] (parse-drawer remaining)]
          (recur rest-lines (conj nodes drawer-node) nil 0)
          ;; Unterminated: keep the lines as a paragraph (Emacs behavior)
          (let [{:keys [num]} (first remaining)
                [_ dname] (re-matches drawer-start-pattern line)
                _ (add-parse-error! num (str "Unterminated drawer " dname))
                [paragraph rest-lines] (parse-paragraph-forced remaining)]
            (recur rest-lines (conj nodes (attach-affiliated paragraph pending-affiliated)) nil 0)))

        ;; A #+KEY: value line outside the preamble (#+TODO:, #+STARTUP:,
        ;; #+PROPERTY:, a late #+TITLE:, ...) becomes a :keyword node so it
        ;; can be re-emitted; it has no rendering effect of its own.
        :metadata
        (let [[_ key value] (re-matches metadata-pattern line)]
          (recur more
                 (conj nodes (make-node :keyword
                                        :key (str/lower-case key)
                                        :value (str/trim value)
                                        :line num))
                 pending-affiliated 0))

        (:comment :fixed-width :html-line :latex-line :footnote-def)
        (let [parser (case ltype
                       :comment parse-comment
                       :fixed-width parse-fixed-width
                       :html-line parse-html-lines
                       :latex-line parse-latex-lines
                       :footnote-def parse-footnote-definition)
              [rest-lines nodes'] (or (try-parse parser remaining nodes)
                                      [more nodes])]
          (recur rest-lines nodes' nil 0))

        :list-item
        (let [[rest-lines nodes'] (try-parse-maybe-affiliated process-list remaining nodes pending-affiliated)]
          (recur rest-lines nodes' nil 0))

        :table
        (let [[rest-lines nodes'] (try-parse-maybe-affiliated parse-table remaining nodes pending-affiliated)]
          (recur rest-lines nodes' nil 0))

        :block-begin
        (let [[rest-lines nodes'] (try-parse-maybe-affiliated parse-block remaining nodes pending-affiliated)]
          (recur rest-lines nodes' nil 0))

        :latex-env-begin
        (let [[rest-lines nodes'] (try-parse-maybe-affiliated parse-latex-environment remaining nodes pending-affiliated)]
          (recur rest-lines nodes' nil 0))

        ;; :text, :planning, and any other type -> paragraph. A planning line
        ;; reaching this point is not right after a headline (those are
        ;; consumed by parse-sections): Emacs treats it as plain text.
        (if-let [[paragraph rest-lines] (if (= ltype :planning)
                                          (parse-paragraph-forced remaining)
                                          (parse-paragraph remaining))]
          (recur rest-lines (conj nodes (attach-affiliated paragraph pending-affiliated)) nil 0)
          (recur more nodes nil 0))))))

(defn- update-path-stack [path-stack new-level title]
  (let [current-level (count path-stack)]
    (cond
      (> new-level current-level) (conj path-stack title)
      (= new-level current-level) (-> path-stack butlast vec (conj title))
      :else (-> (take (dec new-level) path-stack) vec (conj title)))))

(defn- closes-section?
  "True when line is a headline at parent-level or above, which ends the
   section being parsed. Checked from the leading stars alone, so that the
   parent, which parses that headline, is the only one to run the full
   headline regex on it."
  [line parent-level]
  (when-let [stars (and parent-level (re-find #"^\*+" line))]
    (<= (count stars) parent-level)))

(defn- parse-sections
  ([indexed-lines current-path]
   (parse-sections indexed-lines current-path nil 0))
  ([indexed-lines current-path parent-level initial-blanks-before]
   (loop [[{:keys [line num]} & more :as remaining] indexed-lines
          sections [] path-stack current-path
          blanks-before initial-blanks-before]
     (if (empty? remaining)
       [sections remaining blanks-before]
       (if-let [headline-data (when-not (closes-section? line parent-level)
                                (parse-headline line))]
         (let [{:keys [level title todo priority tags]} headline-data]
           (if (and parent-level (<= level parent-level))
             ;; Return to parent level - pass back the blanks-before count
             ;; so parent can assign it to the next sibling
             [sections remaining blanks-before]
             (let [new-path-stack                       (update-path-stack path-stack level title)
                   ;; Parse optional planning line (CLOSED, SCHEDULED, DEADLINE)
                   [planning rest-after-planning]
                   (if (and (seq more) (planning-line? (:line (first more))))
                     [(parse-planning-line (:line (first more))) (rest more)]
                     [nil more])
                   ;; An unterminated property drawer falls through to
                   ;; parse-content, which keeps its lines as a paragraph.
                   [properties rest-after-props]
                   (or (parse-property-drawer rest-after-planning)
                       [{} rest-after-planning])
                   ;; Count blank lines immediately after headline/properties (before content)
                   [blanks-after-title rest-after-title-blanks]
                   (loop [lines rest-after-props n 0]
                     (if (and (seq lines) (str/blank? (:line (first lines))))
                       (recur (rest lines) (inc n))
                       [n lines]))
                   [content rest-after-content content-trailing-blanks] (parse-content rest-after-title-blanks)
                   ;; Use trailing blanks from content as blanks-before for subsections
                   [subsections rest-after-subsections sub-trailing-blanks]
                   (parse-sections rest-after-content new-path-stack level content-trailing-blanks)
                   new-section (make-node :section
                                          :level level
                                          :title (parse-inline title)
                                          :todo todo
                                          :priority priority
                                          :tags tags
                                          :planning planning
                                          :properties properties
                                          :path new-path-stack
                                          :line num
                                          :blank-lines-before blanks-before
                                          :blank-lines-after-title blanks-after-title
                                          :children (vec (concat content subsections)))
                   ;; Use trailing blanks from subsection parsing for next sibling
                   next-blanks sub-trailing-blanks]
               (recur rest-after-subsections (conj sections new-section) path-stack next-blanks))))
         [sections remaining blanks-before])))))

(defn parse-org
  "Parse Org text into an AST. Options:
   :unwrap?        join continuation lines (default true)
   :alphabetical-lists?
                   also recognise single-letter bullets (a. b) A.) as
                   ordered list items, like Org's
                   org-list-allow-alphabetical (default false). Such
                   lists carry :alphabetical (:lower or :upper, from
                   their first bullet).
   :todo-keywords  extra TODO keywords to recognise on headlines, as
                   {:todo [...] :done [...]} or a string in directive
                   syntax (\"TODO NEXT | DONE CANX\"). #+TODO:-style
                   directives found in the file replace the default
                   TODO/DONE set (as in Emacs Org); this option extends
                   whichever set is in effect. The effective set is
                   exposed as :todo-keywords on the document node."
  ([org-content] (parse-org org-content {}))
  ([org-content {:keys [unwrap? todo-keywords alphabetical-lists?]
                 :or {unwrap? true}}]
   (if (nil? org-content)
     (parse-org "" {})
     (let [file-kws (parse-todo-keywords org-content)
           opt-kws  (normalize-todo-keywords todo-keywords)
           kws      (merge-todo-sequences
                     (remove nil? [(or file-kws default-todo-keywords) opt-kws]))]
       (binding [*parse-errors* (volatile! [])
                 *headline-full-pattern* (headline-pattern-for kws)
                 *list-item-pattern* (if alphabetical-lists?
                                       alphabetical-list-item-pattern
                                       numeric-list-item-pattern)]
         (let [raw-lines (if unwrap?
                           (unwrap-text-indexed org-content)
                           (index-lines (split-lines org-content)))
               indexed-lines (mapv enrich-line raw-lines)
               [meta rest-after-meta] (parse-metadata indexed-lines)
               ;; Repeated #+TITLE: lines are joined with a space, as Org does
               title-raw (let [t (get meta :title "Untitled Document")]
                           (if (vector? t) (str/join " " t) t))
               [top-level-content rest-after-content content-trailing-blanks] (parse-content rest-after-meta)
               [sections _ _] (parse-sections rest-after-content [] nil content-trailing-blanks)
               errors @*parse-errors*
               doc (make-node :document
                              :title (parse-inline title-raw)
                              :meta meta
                              :todo-keywords kws
                              :children (vec (concat top-level-content sections)))]
           (if (seq errors)
             (assoc doc :parse-errors errors)
             doc)))))))

;; Timestamp collection
(defn- timestamp-entry
  "Build a uniform timestamp entry from a value string and metadata.
   Splits a start/end range, flags all-day timestamps, and parses the repeater."
  [{:keys [origin value repeater raw path title todo]}]
  (let [range-parts (when (str/includes? value "/") (str/split value #"/"))
        rep         (some-> repeater parse-repeater)]
    (cond-> {:origin origin
             :value value
             :start (if range-parts (first range-parts) value)
             :all-day (not (str/includes? value "T"))
             :active true}
      range-parts (assoc :end (second range-parts))
      raw         (assoc :raw raw)
      path        (assoc :path path)
      title       (assoc :title title)
      todo        (assoc :todo todo)
      rep         (assoc :repeater rep))))

(defn- inline-timestamp-entries
  "Active inline timestamp entries found anywhere within `nodes`, tagged with
   the enclosing section's `path`/`title`/`todo`."
  [nodes path title todo]
  (->> nodes
       (mapcat #(tree-seq coll? (fn [x] (if (map? x) (vals x) (seq x))) %))
       (filter #(and (map? %) (= :timestamp (:type %)) (:active %)))
       (map #(timestamp-entry {:origin :inline :value (:value %) :repeater (:repeater %)
                               :raw (:raw %) :path path :title title :todo todo}))))

(defn active-timestamps
  "Collect every active timestamp of the AST as a uniform sequence of entries.

   Sources:
   - inline active timestamps          -> :origin :inline
   - SCHEDULED: planning               -> :origin :scheduled
   - DEADLINE: planning                -> :origin :deadline
   CLOSED planning and inactive ([...]) timestamps are excluded.

   Each entry:
     {:origin   :inline|:scheduled|:deadline
      :value    \"2025-03-15T10:00/2025-03-15T11:00\"  ; raw ISO (compat)
      :start    \"2025-03-15T10:00\"
      :end      \"2025-03-15T11:00\"                    ; only for ranges
      :all-day  false                                  ; true when no time part
      :active   true
      :repeater {:n 1 :unit :week :kind :cumulate}     ; only when present
      :raw      \"<...>\"                               ; inline only
      :title    \"Subsection\"                          ; enclosing heading text
      :todo     :TODO                                   ; enclosing heading TODO state
      :path     [\"Section\" \"Subsection\"]}           ; enclosing section path}"
  [ast]
  (let [nodes    (tree-seq coll? #(if (map? %) (vals %) (seq %)) ast)
        sections (filter #(and (map? %) (= :section (:type %))) nodes)
        non-section #(not= :section (:type %))]
    (concat
     ;; top-level content (before the first heading)
     (inline-timestamp-entries (filter non-section (:children ast)) nil nil nil)
     (mapcat
      (fn [s]
        (let [pl (:planning s), path (:path s)
              title (inline-text (:title s)), todo (:todo s)]
          (concat
           (for [kw [:scheduled :deadline]
                 :let [v (get pl kw)] :when v]
             (timestamp-entry {:origin kw :value v
                               :repeater (get pl (keyword (str (name kw) "-repeat")))
                               :path path :title title :todo todo}))
           ;; inline timestamps in this section's own title + non-section children
           (inline-timestamp-entries
            (concat (:title s) (filter non-section (:children s)))
            path title todo))))
      sections))))

;; AST Filtering
(defn- section? [node] (= (:type node) :section))

(defn- section-matches? [section {:keys [level-limit title-pattern id-pattern]}]
  (let [level (:level section)]
    (and (or (nil? level-limit) (<= level level-limit))
         (or (nil? title-pattern) (when-let [title (inline-text (:title section))] (re-find title-pattern title)))
         (or (nil? id-pattern)
             (when-let [id (get-in section [:properties :id])] (re-find id-pattern id))
             (when-let [custom-id (get-in section [:properties :custom_id])] (re-find id-pattern custom-id))))))

(defn- filter-ast-node
  ([node opts] (filter-ast-node node opts []))
  ([node opts ancestors]
   (case (:type node)
     :document (assoc node :children (vec (keep #(filter-ast-node % opts ancestors) (:children node))))
     :section
     (let [{:keys [level-limit title-pattern id-pattern]} opts
           direct-match (section-matches? node opts)
           level-ok (or (nil? level-limit) (<= (:level node) level-limit))
           ancestor-title-ok
           (or (nil? title-pattern)
               (some #(when-let [t (inline-text (:title %))] (re-find title-pattern t)) ancestors))
           ancestor-id-ok
           (or (nil? id-pattern)
               (some #(or (when-let [id (get-in % [:properties :id])] (re-find id-pattern id))
                          (when-let [cid (get-in % [:properties :custom_id])] (re-find id-pattern cid)))
                     ancestors))
           ;; A section passes if it directly matches, OR if its ancestors
           ;; match title/id (keeping descendants) while respecting level-limit
           passes-filters (or direct-match (and level-ok ancestor-title-ok ancestor-id-ok))
           filtered-children (keep #(filter-ast-node % opts (conj ancestors node)) (:children node))]
       (cond
         passes-filters (assoc node :children (vec filtered-children))
         (some section? filtered-children) (assoc node :children (vec filtered-children))
         :else nil))
     node)))

(declare ^:private flatten-deep-sections-expand)

(defn- flatten-deep-sections
  "Transform sections deeper than level-limit-inclusive: convert their headings to bold
   paragraphs and inline their children into the parent's child list."
  [node level-limit-inclusive]
  (if (nil? level-limit-inclusive)
    node
    (case (:type node)
      :document (update node :children #(vec (mapcat (fn [c] (flatten-deep-sections-expand c level-limit-inclusive)) %)))
      :section (if (<= (:level node) level-limit-inclusive)
                 (update node :children #(vec (mapcat (fn [c] (flatten-deep-sections-expand c level-limit-inclusive)) %)))
                 node)
      node)))

(defn- flatten-deep-sections-expand
  "For a single child node: if it's a section beyond level-limit-inclusive, return
   a bold paragraph for its title followed by its children (recursively flattened).
   Otherwise return the node unchanged (wrapped in a vector)."
  [node level-limit-inclusive]
  (if (and (= (:type node) :section) (> (:level node) level-limit-inclusive))
    (let [title-nodes (or (:title node) [])
          todo-node (when (:todo node) {:type :text :value (str (name (:todo node)) " ")})
          priority-node (when (:priority node) {:type :text :value (str "[#" (:priority node) "] ")})
          tags-node (when (seq (:tags node)) {:type :text :value (str " :" (str/join ":" (:tags node)) ":")})
          bold-node {:type :bold :children title-nodes}
          content (filterv some? [todo-node priority-node bold-node tags-node])
          title-para (make-node :paragraph :content content
                                :blank-lines-before (:blank-lines-before node))
          children (mapcat #(flatten-deep-sections-expand % level-limit-inclusive) (:children node))]
      (into [title-para] children))
    [(flatten-deep-sections node level-limit-inclusive)]))

(defn filter-ast [ast opts]
  (-> (if (some (comp some? val) (dissoc opts :level-limit-inclusive))
        (filter-ast-node ast opts)
        ast)
      (flatten-deep-sections (:level-limit-inclusive opts))))

;; Content Cleaning
(defn- clean-properties [properties]
  (->> properties
       (remove (fn [[_ v]] (or (nil? v) (str/blank? (str v)))))
       (into {})
       not-empty))

(defn- content-blank? [node]
  (case (:type node)
    (:paragraph :footnote-def) (not (seq (:content node)))
    (:comment :fixed-width :src-block :block :html-line :latex-line :latex-environment :drawer) (str/blank? (:content node))
    :quote-block (not (seq (:children node)))
    :list (empty? (:items node))
    :table (empty? (:rows node))
    false))

(declare clean-node)
(defn- clean-children [children remove-blanks?]
  (let [cleaned (mapv clean-node children)]
    (if remove-blanks? (vec (remove content-blank? cleaned)) cleaned)))

(defn clean-node [node]
  (let [cleaned
        (case (:type node)
          :document (update node :children #(clean-children % true))
          :section (-> node (update :properties clean-properties) (update :children #(clean-children % true)))
          :list-item (update node :children #(clean-children % false))
          :list (update node :items #(mapv clean-node %))
          :table node
          :property-drawer (update node :properties clean-properties)
          :quote-block (update node :children #(clean-children % true))
          (:comment :fixed-width :block :html-line :latex-line :drawer) (update node :content #(when % (str/trim %)))
          ;; :paragraph, :footnote-def — :content is inline nodes, no trimming needed
          ;; :src-block — literal content, no trimming
          node)]
    (remove-empty-vals (dissoc cleaned :line))))

;; EDN output (no external deps needed)
(defn format-ast-as-edn
  "Pretty-print the AST as an EDN string."
  [ast]
  (with-out-str (binding [*print-length* nil *print-level* nil] (pprint/pprint ast))))
