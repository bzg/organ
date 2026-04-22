;; Copyright (c) Bastien Guerry
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: LICENSES/EPL-2.0.txt

(ns bzg.organ
  "Parse Org-mode text into an AST (EDN data)."
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]))

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
  (into {} (remove (comp empty-value? val) m)))

(defn- make-node
  "Create an AST node, filtering out empty values."
  [type & {:as fields}]
  (assoc (remove-empty-vals fields) :type type))

(defn non-blank?
  "True if s is a non-nil, non-blank string."
  [s]
  (and (some? s) (not (str/blank? s))))

;; Regex Patterns
(def ^:private headline-full-pattern #"^(\*+)\s+(?:(TODO|DONE)\s+)?(?:\[#([A-Z])\]\s+)?(.+?)(?:\s+(:[:\w@]+:))?\s*$")
(def ^:private headline-pattern #"^(\*+)\s+(.*)$")
(def ^:private property-drawer-start-pattern #"^\s*:PROPERTIES:\s*$")
(def ^:private property-drawer-end-pattern #"^\s*:END:\s*$")
(def ^:private drawer-start-pattern #"^\s*:([A-Za-z][\w_-]*?):\s*$")
(def ^:private drawer-end-pattern #"(?i)^\s*:END:\s*$")
(def ^:private property-pattern #"^\s*:([\w_-]+):\s*(.*)$")
(def ^:private list-item-pattern #"^(\s*)(-|\+|\*|\d+[.)])\s+(.*)$")
(def ^:private description-item-pattern #"^(.+?)\s+::(?:\s+(.*))?$")
(def ^:private table-pattern #"^\s*\|.*\|\s*$")
(def ^:private table-separator-pattern #"^\s*\|-.*\|\s*$")
(def ^:private generic-block-begin-pattern #"(?i)^\s*#\+BEGIN_(\w+)(?:\s+(.*))?$")
(def ^:private metadata-pattern #"^\s*#\+(\w+):\s*(.*)$")
(def ^:private comment-pattern #"^\s*#(?:\s.*|$)")
(def ^:private html-line-pattern #"(?i)^\s*#\+html:\s*(.*)$")
(def ^:private latex-line-pattern #"(?i)^\s*#\+latex:\s*(.*)$")
(def ^:private block-begin-pattern #"(?i)^\s*#\+BEGIN.*$")
(def ^:private fixed-width-pattern #"^\s*: (.*)$")
(def ^:private footnote-ref-pattern #"\[fn:([\w-]+)\]")
(def ^:private footnote-inline-pattern #"\[fn:([^\]:]*):([^\]]+)\]")
(def ^:private footnote-def-pattern #"^\[fn:([^\]]+)\]\s*(.*)$")
(def ^:private link-with-desc-pattern #"\[\[([^\]]+)\]\[((?:[^\]]|\](?!\]))+)\]\]")
(def ^:private link-without-desc-pattern #"\[\[((?:[^\]]|\](?!\]))+)\]\]")
(def ^:private link-type-pattern #"^(file|id|mailto|http|https|ftp|news|shell|elisp|doi):(.*)$")
(def ^:private affiliated-keyword-pattern #"(?i)^\s*#\+(attr_\w+|caption|name|header|results):\s*(.*)$")
(def ^:private list-item-simple-pattern #"^\s*(?:[-+*]|\d+[.)])\s+.*$")

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

;; LaTeX environment (block-level): \begin{env} ... \end{env}
(def ^:private latex-env-begin-pattern #"^\s*\\begin\{([^}]+)\}\s*$")
(def ^:private latex-env-end-pattern-for
  "Memoized constructor for \\end{name} patterns."
  (memoize
   (fn [env]
     (re-pattern (str "^\\s*\\\\end\\{"
                      (java.util.regex.Pattern/quote env)
                      "\\}\\s*$")))))

;; Planning line (CLOSED, SCHEDULED, DEADLINE)
(def ^:private org-timestamp-pattern #"<(\d{4})-(\d{2})-(\d{2})\s+\S+(?:\s+(\d{1,2}):(\d{2})(?:-(\d{1,2}):(\d{2}))?)?(?:\s+[.+]?[+]?\d+[hdwmy])*\s*>|\[(\d{4})-(\d{2})-(\d{2})\s+\S+(?:\s+(\d{1,2}):(\d{2})(?:-(\d{1,2}):(\d{2}))?)?(?:\s+[.+]?[+]?\d+[hdwmy])*\s*\]")
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
                  (recur rest-str (cond-> (assoc result kw-key iso)
                                    repeater (assoc (-> kw str/lower-case (str "-repeat") keyword) repeater))))
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
(defn- metadata-line? [line] (re-matches metadata-pattern line))
(defn- comment-line? [line] (re-matches comment-pattern line))
(defn- fixed-width-line? [line] (re-matches fixed-width-pattern line))
(defn- table-line? [line] (re-matches table-pattern line))
(defn- block-begin? [line] (re-matches block-begin-pattern line))
(defn- footnote-def? [line] (re-matches footnote-def-pattern line))
(defn- planning-line? [line] (re-matches planning-line-pattern line))
(defn- html-line? [line] (re-matches html-line-pattern line))
(defn- latex-line? [line] (re-matches latex-line-pattern line))
(defn- latex-env-begin? [line] (re-matches latex-env-begin-pattern line))
(defn- property-line? [line]
  (or (re-matches property-drawer-start-pattern line)
      (re-matches property-drawer-end-pattern line)
      (re-matches property-pattern line)))
(defn- drawer-start? [line]
  (and (re-matches drawer-start-pattern line)
       (not (re-matches property-drawer-start-pattern line))
       (not (re-matches drawer-end-pattern line))))
(defn- list-item? [line] (re-matches list-item-simple-pattern line))
(defn- affiliated-keyword? [line] (re-matches affiliated-keyword-pattern line))

(def ^:private end-pattern-for
  "Memoized constructor for block end patterns."
  (memoize (fn [btype] (re-pattern (str "(?i)^\\s*#\\+END_" btype "\\s*$")))))
(defn- index-lines [lines] (map-indexed (fn [i line] {:line line :num (inc i)}) lines))

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
          (if (re-matches list-item-simple-pattern line) :list-item :text))

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

        (or (= fc \-) (= fc \+) (Character/isDigit fc))
        (if (re-matches list-item-simple-pattern line) :list-item :text)

        (= fc \\)
        (if (re-matches latex-env-begin-pattern line) :latex-env-begin :text)

        (or (= fc \C) (= fc \S) (= fc \D))
        (if (re-matches planning-line-pattern line) :planning :text)

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
  "Remove leading comma escape from a line inside a block.
   A comma escapes lines starting with * or #+ inside blocks."
  [line]
  (if (re-matches #"^,([\*#]).*" line)
    (subs line 1)
    line))

(defn- escape-comma
  "Add leading comma to escape lines that need it inside blocks.
   Lines starting with * or #+ need escaping."
  [line]
  (if (re-matches #"^(?:\*|#\+).*" line)
    (str "," line)
    line))

(defn- transform-block-lines
  "Apply a per-line transform to block content."
  [f content]
  (->> (str/split-lines content) (map f) (str/join "\n")))

(def escape-block-content
  "Add comma escapes to lines that need it in block content."
  (partial transform-block-lines escape-comma))

(defn- hard-break? [current-line next-line in-block]
  (or in-block
      (str/blank? current-line)
      (str/blank? next-line)
      (headline? current-line)
      (headline? next-line)
      (metadata-line? current-line)
      (metadata-line? next-line)
      (list-item? next-line)
      (table-line? current-line)
      (table-line? next-line)
      (block-begin? next-line)
      (comment-line? current-line)
      (comment-line? next-line)
      (fixed-width-line? current-line)
      (fixed-width-line? next-line)
      (property-line? current-line)
      (property-line? next-line)
      (drawer-start? current-line)
      (drawer-start? next-line)
      (re-matches drawer-end-pattern current-line)
      (re-matches drawer-end-pattern next-line)
      (planning-line? current-line)
      (planning-line? next-line)
      (latex-env-begin? next-line)))

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
   "\\nbsp{}" "\u00A0"
   "\\amp" "&"
   "\\lt" "<"
   "\\gt" ">"
   "\\checkmark" "✓"})

(def ^:private entity-pattern
  "Pre-compiled regex alternation of all org entity keys, sorted by descending length for safe matching.
   Entities only match when NOT followed by an alphabetic character (Org mode convention)."
  (->> (keys org-entities)
       (sort-by (comp - count))
       (map #(str (java.util.regex.Pattern/quote %) "(?![a-zA-Z])"))
       (str/join "|")
       re-pattern))

(defn- replace-entities
  "Replace Org entities (\\name) with their Unicode equivalents in a single pass."
  [text]
  (if (str/includes? text "\\")
    (str/replace text entity-pattern #(get org-entities % %))
    text))

;; Inline Parsing
;; Parses Org inline markup into a vector of typed nodes.

(def ^:private emphasis-pre-punct #{\- \( \{ \' \"})

(def ^:private emphasis-post-punct #{\- \. \, \; \: \! \? \' \" \) \} \[})

(defn- emphasis-pre? [text i]
  (or (zero? i)
      (let [c (.charAt text (dec i))]
        (or (Character/isSpaceChar c) (Character/isWhitespace c)
            (contains? emphasis-pre-punct c)))))

(defn- emphasis-post? [text i]
  (or (= i (dec (.length text)))
      (let [c (.charAt text (inc i))]
        (or (Character/isSpaceChar c) (Character/isWhitespace c)
            (contains? emphasis-post-punct c)))))

(defn- find-close-marker
  "Find matching close position for an emphasis or literal marker.
   Returns index of closing marker, or nil."
  [text open marker]
  (let [len (.length text)
        content-start (inc open)]
    (when (and (< content-start len)
               (not (Character/isWhitespace (.charAt text content-start))))
      (loop [j content-start]
        (when (< j len)
          (if (and (= (.charAt text j) marker)
                   (> j content-start)
                   (not (Character/isWhitespace (.charAt text (dec j))))
                   (emphasis-post? text j))
            j
            (recur (inc j))))))))

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
  "Find regex match starting at position i in text, without creating a substring.
   Returns same format as re-find: the match string if no groups, or a vector
   [full group1 group2 ...] if there are groups. Returns nil if no match."
  [^java.util.regex.Pattern pattern ^String text ^long i]
  (let [m (doto (.matcher pattern text) (.region i (.length text)))]
    (when (.find m)
      (let [gc (.groupCount m)]
        (if (zero? gc)
          (.group m)
          (let [result (object-array (inc gc))]
            (dotimes [j (inc gc)]
              (aset result j (.group m (int j))))
            (vec result)))))))

(defn- try-parse-link [text i]
  (or (when-let [[full url desc] (re-find-at #"^\[\[([^\]]*)\]\[((?:[^\]]|\](?!\]))*)\]\]" text i)]
        (let [{:keys [link-type target]} (classify-link-url url)]
          [(cond-> {:type :link :url url :children (do-parse-inline desc)}
             link-type (assoc :link-type link-type)
             target    (assoc :target target))
           (+ i (count full))]))
      (when-let [[full url] (re-find-at #"^\[\[((?:[^\]]|\](?!\]))*)\]\]" text i)]
        (let [{:keys [link-type target]} (classify-link-url url)]
          [(cond-> {:type :link :url url}
             link-type (assoc :link-type link-type)
             target    (assoc :target target))
           (+ i (count full))]))))

(defn- try-parse-footnote [text i]
  (or (when-let [[full label content] (re-find-at #"^\[fn:([^\]:]*):([^\]]+)\]" text i)]
        [{:type :footnote-inline :label label :children (do-parse-inline content)}
         (+ i (count full))])
      (when-let [[full label] (re-find-at #"^\[fn:([\w-]+)\]" text i)]
        [{:type :footnote-ref :label label}
         (+ i (count full))])))

(def ^:private active-timestamp-pattern
  #"^(<\d{4}-\d{2}-\d{2}\s+\S+(?:\s+\d{1,2}:\d{2}(?:-\d{1,2}:\d{2})?)?(?:\s+[.+]?[+]?\d+[hdwmy])*\s*>)")
(def ^:private inactive-timestamp-pattern
  #"^(\[\d{4}-\d{2}-\d{2}\s+\S+(?:\s+\d{1,2}:\d{2}(?:-\d{1,2}:\d{2})?)?(?:\s+[.+]?[+]?\d+[hdwmy])*\s*\])")

(defn- try-parse-timestamp [text i active?]
  (let [pat (if active? active-timestamp-pattern inactive-timestamp-pattern)]
    (when-let [[full raw] (re-find-at pat text i)]
      (let [parsed (parse-org-timestamp raw)
            repeater (parse-org-repeater raw)]
        [(cond-> {:type :timestamp :raw raw :value parsed :active active?}
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
  [text i]
  (case (.charAt text i)
    \\ (or (match-latex-fragment text i latex-paren-pattern   :paren)
           (match-latex-fragment text i latex-bracket-pattern :bracket))
    \$ (or (match-latex-fragment text i latex-dollars-pattern :dollars)
           (when (or (zero? i) (not= (.charAt text (dec i)) \$))
             (or (match-latex-fragment text i latex-dollar1-pattern :dollar)
                 (match-latex-fragment text i latex-dollar-pattern  :dollar))))
    nil))

(defn- do-parse-inline [text]
  (let [len (.length text)
        buf (StringBuilder.)
        flush-buf! (fn [nodes]
                     (if (pos? (.length buf))
                       (let [t (replace-entities (.toString buf))]
                         (.setLength buf 0)
                         (conj nodes {:type :text :value t}))
                       nodes))
        skip! (fn [c] (.append buf c))]
    (loop [i 0 nodes []]
      (if (>= i len)
        (flush-buf! nodes)
        (let [c (.charAt text i)]
          (cond
            ;; Link: [[
            (and (= c \[) (< (inc i) len) (= (.charAt text (inc i)) \[))
            (if-let [[node end] (try-parse-link text i)]
              (recur end (conj (flush-buf! nodes) node))
              (do (skip! c) (recur (inc i) nodes)))

            ;; Footnote: [fn:
            (and (= c \[) (<= (+ i 4) len) (= (subs text i (+ i 4)) "[fn:"))
            (if-let [[node end] (try-parse-footnote text i)]
              (recur end (conj (flush-buf! nodes) node))
              (do (skip! c) (recur (inc i) nodes)))

            ;; Active timestamp: <YYYY-
            (and (= c \<) (<= (+ i 6) len)
                 (Character/isDigit (.charAt text (inc i))))
            (if-let [[node end] (try-parse-timestamp text i true)]
              (recur end (conj (flush-buf! nodes) node))
              (do (skip! c) (recur (inc i) nodes)))

            ;; Inactive timestamp: [YYYY- (not link, not footnote)
            (and (= c \[) (<= (+ i 6) len)
                 (not= (.charAt text (inc i)) \[)
                 (not= (.charAt text (inc i)) \f)
                 (Character/isDigit (.charAt text (inc i))))
            (if-let [[node end] (try-parse-timestamp text i false)]
              (recur end (conj (flush-buf! nodes) node))
              (do (skip! c) (recur (inc i) nodes)))

            ;; Emphasis: * / _ +
            (and (contains? emphasis-type c) (emphasis-pre? text i))
            (if-let [close (find-close-marker text i c)]
              (recur (inc close)
                     (conj (flush-buf! nodes)
                           {:type (emphasis-type c)
                            :children (do-parse-inline (subs text (inc i) close))}))
              (do (skip! c) (recur (inc i) nodes)))

            ;; Code/verbatim: ~ =
            (and (contains? literal-type c) (emphasis-pre? text i))
            (if-let [close (find-close-marker text i c)]
              (recur (inc close)
                     (conj (flush-buf! nodes)
                           {:type (literal-type c)
                            :value (subs text (inc i) close)}))
              (do (skip! c) (recur (inc i) nodes)))

            ;; LaTeX fragment: \( \[ $ $$
            (or (= c \\) (= c \$))
            (if-let [[node end] (try-parse-latex-fragment text i)]
              (recur end (conj (flush-buf! nodes) node))
              (do (skip! c) (recur (inc i) nodes)))

            ;; Plain character
            :else
            (do (skip! c) (recur (inc i) nodes))))))))

(defn parse-inline
  "Parse Org inline markup into a vector of typed nodes.
   Returns [] for nil or blank input."
  [text]
  (if (or (nil? text) (str/blank? text))
    []
    (do-parse-inline (str/trim text))))

(defn inline-text
  "Extract plain text from a vector of inline nodes."
  [nodes]
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
                      (inline-text (:children node))
                      (:url node))
              (if (:children node) (inline-text (:children node)) "")))
          nodes))))

(defn- unwrap-text-indexed
  "Unwrap text while preserving original line numbers.
   Returns a vector of {:line string :num original-line-number} maps."
  [input]
  (let [indexed (index-lines (str/split-lines input))]
    (loop [result []
           remaining indexed
           in-block false
           block-type nil
           block-end-pattern nil]
      (if (empty? remaining)
        result
        (let [{:keys [line num] :as current} (first remaining)
              rest-lines (rest remaining)
              next-line (:line (first rest-lines))]
          (cond
            ;; When inside a block, only check for the matching end
            (and in-block block-end-pattern (re-matches block-end-pattern line))
            (recur (conj result current) rest-lines false nil nil)

            ;; When inside a block, don't check for other patterns
            in-block
            (recur (conj result current) rest-lines true block-type block-end-pattern)

            ;; Not in a block - check for block start
            :else
            (if-let [[_ btype] (re-matches generic-block-begin-pattern line)]
              (recur (conj result current) rest-lines true btype
                     (end-pattern-for btype))
              (if-let [[_ env] (re-matches latex-env-begin-pattern line)]
                (recur (conj result current) rest-lines true env
                       (latex-env-end-pattern-for env))
                (if (or (nil? next-line) (hard-break? line next-line false))
                  (recur (conj result current) rest-lines false nil nil)
                  (let [trimmed-next    (str/trim next-line)
                        normalized-next (if (list-item? line)
                                          (str/replace trimmed-next #"\s+" " ")
                                          trimmed-next)
                        new-current     {:line (str line " " normalized-next) :num num}]
                    (recur result (cons new-current (rest rest-lines)) false nil nil)))))))))))


;; Parsing Helpers
(defn- parse-footnote-def [line]
  (when-let [[_ label content] (re-matches footnote-def-pattern line)]
    {:label label :content content}))

(defn- parse-attr-string
  "Parse an Org attribute string like ':width 300 :alt \"An image\" :class my-class'
   into a map {:width \"300\" :alt \"An image\" :class \"my-class\"}"
  [s]
  (when (non-blank? s)
    (loop [remaining (str/trim s)
           result {}]
      (if (str/blank? remaining)
        result
        ;; Match :key followed by either "quoted value" or unquoted-value
        (if-let [[_ key quoted unquoted rest]
                 (re-matches #"^:(\w+)\s+(?:\"([^\"]*)\"|(\S+))(.*)$" remaining)]
          (recur (str/trim rest)
                 (assoc result (keyword key) (or quoted unquoted)))
          ;; No match, skip to next potential key or end
          (if-let [[_ rest] (re-matches #"^\S+\s*(.*)$" remaining)]
            (recur rest result)
            result))))))

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

(defn- flush-item [items current-item]
  (if current-item (conj items current-item) items))

(defn- ordered-marker? [marker]
  (boolean (re-matches #"\d+[.)]" marker)))

;; Parsing Functions
(defn- parse-headline [line]
  (if-let [[_ stars todo priority title tags] (re-matches headline-full-pattern line)]
    {:level (count stars)
     :title (str/trim title)
     :todo (when todo (keyword todo))
     :priority priority
     :tags (when tags
             (->> (str/split (str/replace tags #"^:|:$" "") #":")
                  (filter seq)
                  vec))}
    (when-let [[_ stars title] (re-matches headline-pattern line)]
      {:level (count stars) :title (str/trim title)})))

;; Keyword set for metadata parsing (document preamble).
(def ^:private preamble-metadata-keywords
  #{"title" "author" "date" "subtitle" "email" "language" "description" "keywords" "options"})

(defn- parse-metadata [indexed-lines]
  (loop [[{:keys [line] :as l} & more :as remaining] indexed-lines
         meta {} order [] raw []]
    (if (nil? l)
      [(assoc meta :_order order :_raw raw) remaining]
      (if-let [[_ key value] (re-matches metadata-pattern line)]
        (let [kw       (keyword (str/lower-case key))
              kw-str   (str/lower-case key)]
          (cond
            ;; Known preamble metadata: collect it
            (contains? preamble-metadata-keywords kw-str)
            (let [existing (get meta kw)
                  v        (str/trim value)
                  new-val  (cond
                             (nil? existing) v
                             (vector? existing) (conj existing v)
                             :else [existing v])]
              (recur more
                     (assoc meta kw new-val)
                     (if (some #{kw} order) order (conj order kw))
                     (conj raw line)))
            ;; Content directives (#+html:, #+latex:, #+BEGIN_*): stop metadata parsing
            (or (html-line? line) (latex-line? line) (block-begin? line))
            [(assoc meta :_order order :_raw raw) remaining]
            ;; Other unknown keywords: skip
            :else
            (recur more meta order raw)))
        (if (str/blank? line)
          (recur more meta order raw)
          [(assoc meta :_order order :_raw raw) remaining])))))

(defn- parse-property-drawer [indexed-lines]
  (if (and (seq indexed-lines)
           (re-matches property-drawer-start-pattern (:line (first indexed-lines))))
    (let [start-line-num (:num (first indexed-lines))]
      (loop [[{:keys [line]} & more :as remaining] (rest indexed-lines)
             properties {}]
        (cond
          (empty? remaining)
          (do (add-parse-error! start-line-num "Unterminated property drawer")
              [nil properties []])

          (re-matches property-drawer-end-pattern line)
          [nil properties more]

          :else
          (if-let [[_ key value] (re-matches property-pattern line)]
            (recur more (assoc properties (keyword (str/lower-case key)) (str/trim value)))
            (recur more properties)))))
    [nil {} indexed-lines]))

(defn- parse-drawer
  "Parse a generic drawer (:NAME: ... :END:).
   Returns [drawer-node remaining-lines] or nil."
  [indexed-lines]
  (let [{:keys [line num]} (first indexed-lines)]
    (when-let [[_ drawer-name] (re-matches drawer-start-pattern line)]
      (when (not (re-matches property-drawer-start-pattern line))
        (loop [[{:keys [line]} & more :as remaining] (rest indexed-lines)
               content []]
          (cond
            (empty? remaining)
            (do (add-parse-error! num (str "Unterminated drawer " drawer-name))
                [(make-node :drawer :drawer-name (str/lower-case drawer-name)
                            :content (str/join "\n" content) :line num
                            :warning (str "Unterminated drawer " drawer-name))
                 []])

            (re-matches drawer-end-pattern line)
            [(make-node :drawer :drawer-name (str/lower-case drawer-name)
                        :content (str/join "\n" content) :line num)
             more]

            :else
            (recur more (conj content line))))))))

(defn- parse-description-item
  "Check if content matches a description list item (term :: definition).
   Returns {:term term :definition definition} or nil."
  [content]
  (when-let [[_ term definition] (re-matches description-item-pattern content)]
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

(defn- leading-indent
  "Count leading whitespace characters on a line."
  [^String line]
  (count (re-find #"^\s*" line)))

(defn- collect-list-item-body
  "Collect continuation lines for a list item until we hit another list item,
   a headline, or a non-indented line (outside of blocks). Returns [indexed-lines remaining].
   Continuation lines must be indented strictly more than min-indent (the list item's
   marker indent) — matching Org's rule that a line belongs to a list item only if it
   is indented past the item's bullet."
  [indexed-lines min-indent]
  (loop [[{:keys [line]} & more :as remaining] indexed-lines
         collected []
         in-block false
         block-type nil
         block-end-pattern nil]
    (let [block-begin-match (when (and line (not in-block))
                              (re-matches generic-block-begin-pattern line))
          li-match (when (and line (not in-block))
                     (re-matches list-item-pattern line))]
      (cond
        (empty? remaining)
        [collected remaining]

        (nil? line)
        [collected remaining]

        ;; Stop at headlines (but only outside blocks)
        (and (not in-block) (headline? line))
        [collected remaining]

        ;; Track block start
        block-begin-match
        (let [[_ btype] block-begin-match]
          (recur more (conj collected (first remaining)) true btype
                 (end-pattern-for btype)))

        ;; Track block end
        (and in-block
             block-end-pattern
             (re-matches block-end-pattern line))
        (recur more (conj collected (first remaining)) false nil nil)

        ;; Inside a block - always include the line
        in-block
        (recur more (conj collected (first remaining)) true block-type block-end-pattern)

        ;; Stop at list items (same or less indent) - only outside blocks
        (and li-match
             (let [[_ indent _ _] li-match]
               (<= (count indent) min-indent)))
        [collected remaining]

        ;; Blank lines: only consume if a continuation follows (line indented past the
        ;; item's marker, or a deeper list item). If what follows is a headline, a shallow
        ;; list item, or content not indented past min-indent, stop and leave blanks
        ;; unconsumed so parse-content can count them as trailing-blanks.
        (str/blank? line)
        (let [next-non-blank (first (drop-while #(str/blank? (:line %)) more))
              next-line      (:line next-non-blank)]
          (if (or (nil? next-non-blank)
                  (headline? next-line)
                  (and (not (re-matches list-item-pattern next-line))
                       (<= (leading-indent next-line) min-indent)))
            [collected remaining]
            (recur more (conj collected (first remaining)) false nil nil)))

        ;; Indented continuation lines (strictly deeper than min-indent) are included
        (> (leading-indent line) min-indent)
        (recur more (conj collected (first remaining)) false nil nil)

        ;; Non-indented / shallow-indent non-blank line ends the item (only outside blocks)
        :else
        [collected remaining]))))

(defn- list-item-match
  "Parse a list item line, returning [indent marker content] or nil."
  [line]
  (when-let [[_ indent marker content] (re-matches list-item-pattern line)]
    [indent marker content]))

(defn- parse-list-items [indexed-lines initial-indent initial-marker]
  (let [normalized-initial (normalize-marker initial-marker)]
    (loop [[{:keys [line num]} & more :as remaining] indexed-lines
           items [] current-item nil after-blank false]
      (if (or (empty? remaining) (nil? line))
        [(flush-item items current-item) remaining]
        (let [li-match (list-item-match line)]
          (cond
            ;; Headlines always end the list
            (headline? line)
            [(flush-item items current-item) remaining]

            ;; New list item at same indent level with same marker type
            (and li-match
                 (let [[indent marker _] li-match]
                   (and (= (count indent) initial-indent)
                        (= (normalize-marker marker) normalized-initial)
                        (not (and after-blank (= initial-indent 0) (= marker "*"))))))
            (let [[_indent _marker content] li-match
                  desc-item (parse-description-item content)
                  [body-lines rest-after-body] (collect-list-item-body more initial-indent)
                  [definition body-for-parsing]
                  (if (and desc-item
                           (str/blank? (:definition desc-item))
                           (seq body-lines))
                    (let [first-content (->> body-lines
                                             (drop-while #(or (str/blank? (:line %))
                                                              (ignored-keyword-line? (:line %))))
                                             first)]
                      (if (and first-content
                               (not (list-item? (:line first-content))))
                        [(str/trim (:line first-content))
                         (rest (drop-while #(or (str/blank? (:line %))
                                                (ignored-keyword-line? (:line %))) body-lines))]
                        ["" body-lines]))
                    [(or (:definition desc-item) "") body-lines])
                  [children _] (if (seq body-for-parsing)
                                 (parse-content body-for-parsing)
                                 [[] []])
                  new-item (if desc-item
                             (make-node :list-item
                                        :term (parse-inline (:term desc-item))
                                        :definition (parse-inline definition)
                                        :children children
                                        :line num)
                             (make-node :list-item
                                        :content (parse-inline content)
                                        :children children
                                        :line num))]
              (recur rest-after-body
                     (flush-item items current-item)
                     new-item
                     false))

            ;; Nested list item (deeper indent) - starts a new sublist with its own marker
            (and li-match
                 (let [[indent _ _] li-match]
                   (> (count indent) initial-indent)))
            (if current-item
              (let [[indent marker _] li-match
                    sub-is-ordered (ordered-marker? marker)
                    [sublist-items rest-lines] (parse-list-items remaining (count indent) marker)
                    sublist (make-node :list :items sublist-items :ordered sub-is-ordered)
                    updated-item (update current-item :children conj sublist)]
                (recur rest-lines (flush-item items updated-item) nil false))
              (recur more items current-item after-blank))

            ;; Blank line - peek ahead: if the next non-blank line would end the list,
            ;; leave blanks unconsumed so parse-content can count them as trailing-blanks.
            (str/blank? line)
            (let [next-non-blank (first (drop-while #(str/blank? (:line %)) more))]
              (if (or (nil? next-non-blank)
                      (headline? (:line next-non-blank))
                      (not (when-let [[indent marker _] (list-item-match (:line next-non-blank))]
                             (and (= (count indent) initial-indent)
                                  (= (normalize-marker marker) normalized-initial)))))
                [(flush-item items current-item) remaining]
                (recur more items current-item true)))

            ;; End of list (different marker, less indent, or non-continuation line)
            :else
            [(flush-item items current-item) remaining]))))))

(defn- process-list [indexed-lines]
  (let [{:keys [line num]} (first indexed-lines)]
    (if-let [[_ indent marker _] (re-matches list-item-pattern line)]
      (let [initial-indent     (count indent)
            [items rest-lines] (parse-list-items indexed-lines initial-indent marker)
            ordered            (ordered-marker? marker)
            is-description     (some :term items)]
        [(make-node :list :items items :ordered ordered :description is-description :line num) rest-lines])
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
        (let [row (->> (str/split (str/trim line) #"\|" -1)
                       rest
                       butlast
                       (mapv #(parse-inline (str/trim %))))]
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
            (empty? remaining)
            (do (add-parse-error! num (str "Unterminated " block-type " block"))
                [(make-block-node content :warning (str "Unterminated " block-type " block")) []])
            (re-matches end-pattern line)
            [(make-block-node content) more]
            :else
            (recur more (conj content line)))))
      [nil indexed-lines])))

(defn- parse-consecutive-lines [indexed-lines pred extract-fn node-type]
  (let [start-line-num (:num (first indexed-lines))]
    (loop [[{:keys [line]} & more :as remaining] indexed-lines
           content []]
      (if (or (empty? remaining) (not (pred line)))
        (when (seq content)
          [(make-node node-type :content (str/join "\n" content)
                      :line start-line-num) remaining])
        (recur more (conj content (extract-fn line)))))))

(defn- parse-comment [indexed-lines]
  (parse-consecutive-lines
   indexed-lines comment-line?
   #(str/replace % #"^\s*#\s?" "") :comment))

(defn- parse-fixed-width [indexed-lines]
  (parse-consecutive-lines
   indexed-lines fixed-width-line?
   #(second (re-matches fixed-width-pattern %)) :fixed-width))

(defn- parse-html-lines [indexed-lines]
  (parse-consecutive-lines
   indexed-lines html-line?
   #(second (re-matches html-line-pattern %)) :html-line))

(defn- parse-latex-lines [indexed-lines]
  (parse-consecutive-lines
   indexed-lines latex-line?
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
            (empty? remaining)
            (let [warning (str "Unterminated \\begin{" env "} environment")]
              (add-parse-error! num warning)
              [(make-node :latex-environment :name env
                          :content (str/join "\n" content)
                          :line num :warning warning :error-line num)
               []])
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

(def ^:private simple-line-parsers
  [[comment-line?     parse-comment]
   [fixed-width-line? parse-fixed-width]
   [html-line?        parse-html-lines]
   [latex-line?       parse-latex-lines]
   [footnote-def?     parse-footnote-definition]])

(defn- find-simple-parser
  "Return the parser fn for a line matched by simple-line-parsers, or nil."
  [line]
  (some (fn [[pred parser]] (when (pred line) parser)) simple-line-parsers))

(defn- parse-content [indexed-lines]
  (loop [[{:keys [line ltype]} & more :as remaining] indexed-lines
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
        (let [[_ properties rest-lines] (parse-property-drawer remaining)
              drawer-node (make-node :property-drawer :properties properties
                                     :line (:num (first remaining)))]
          (recur rest-lines (conj nodes drawer-node) nil 0))

        :drawer-start
        (if-let [[drawer-node rest-lines] (parse-drawer remaining)]
          (recur rest-lines (conj nodes drawer-node) nil 0)
          (recur more nodes pending-affiliated 0))

        :metadata
        (if (ignored-keyword-line? line)
          (recur more nodes pending-affiliated 0)
          (if-let [[paragraph rest-lines] (parse-paragraph remaining)]
            (recur rest-lines (conj nodes (attach-affiliated paragraph pending-affiliated)) nil 0)
            (recur more nodes nil 0)))

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

        ;; :text, :planning, and any other type -> paragraph
        (if-let [[paragraph rest-lines] (parse-paragraph remaining)]
          (recur rest-lines (conj nodes (attach-affiliated paragraph pending-affiliated)) nil 0)
          (recur more nodes nil 0))))))

(defn- update-path-stack [path-stack new-level title]
  (let [current-level (count path-stack)]
    (cond
      (> new-level current-level) (conj path-stack title)
      (= new-level current-level) (-> path-stack butlast vec (conj title))
      :else (-> (take (dec new-level) path-stack) vec (conj title)))))

(defn- parse-sections
  ([indexed-lines current-path]
   (parse-sections indexed-lines current-path nil 0))
  ([indexed-lines current-path parent-level initial-blanks-before]
   (loop [[{:keys [line num]} & more :as remaining] indexed-lines
          sections [] path-stack current-path
          blanks-before initial-blanks-before]
     (if (empty? remaining)
       [sections remaining blanks-before]
       (if-let [headline-data (parse-headline line)]
         (let [{:keys [level title todo priority tags]} headline-data
               title-inline (parse-inline title)]
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
                   [_ properties rest-after-props]      (parse-property-drawer rest-after-planning)
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
                                          :title title-inline
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
  ([org-content] (parse-org org-content {}))
  ([org-content {:keys [unwrap?] :or {unwrap? true}}]
   (if (nil? org-content)
     (parse-org "" {})
     (binding [*parse-errors* (volatile! [])]
       (let [raw-lines (if unwrap?
                         (unwrap-text-indexed org-content)
                         (index-lines (str/split-lines org-content)))
             indexed-lines (mapv enrich-line raw-lines)
             [meta-raw rest-after-meta] (parse-metadata indexed-lines)
             meta (dissoc meta-raw :_order :_raw)
             title-raw (get meta :title "Untitled Document")
             [top-level-content rest-after-content content-trailing-blanks] (parse-content rest-after-meta)
             [sections _ _] (parse-sections rest-after-content [] nil content-trailing-blanks)
             errors @*parse-errors*
             doc (make-node :document
                            :title (parse-inline title-raw)
                            :meta meta
                            :children (vec (concat top-level-content sections)))]
         (if (seq errors)
           (assoc doc :parse-errors errors)
           doc))))))

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
