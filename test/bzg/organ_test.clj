(ns bzg.organ-test
  (:require [bzg.organ :as organ]
            [clojure.string :as str]))

(defn- assert= [label expected actual]
  (if (= expected actual)
    (do (println (str "  OK " label)) :pass)
    (do (println (str "FAIL " label))
        (println (str "  expected: " (pr-str expected)))
        (println (str "  actual:   " (pr-str actual)))
        :fail)))

(defn- find-section
  "Find a top-level or nested section by title text in the AST."
  [ast title]
  (letfn [(walk [node]
            (when (map? node)
              (if (and (= (:type node) :section)
                       (= (organ/inline-text (:title node)) title))
                node
                (some walk (:children node)))))]
    (walk ast)))

(defn- find-node
  "Find a node by type in the AST (depth-first)."
  [ast node-type]
  (letfn [(walk [node]
            (when (map? node)
              (if (= (:type node) node-type)
                node
                (some walk (or (:children node) (:items node))))))]
    (walk ast)))

(defn- find-nodes
  "Find all nodes of a given type in the AST (depth-first)."
  [ast node-type]
  (letfn [(walk [node]
            (when (map? node)
              (concat
               (when (= (:type node) node-type) [node])
               (mapcat walk (or (:children node) (:items node))))))]
    (walk ast)))

(defn test-all []
  (let [results
        [(assert= "parse empty string"
                  :document
                  (:type (organ/parse-org "")))

         (assert= "parse title"
                  "Hello"
                  (organ/inline-text (:title (organ/parse-org "#+TITLE: Hello\n"))))

         (assert= "section level"
                  1
                  (-> (organ/parse-org "* Heading\nBody text")
                      :children first :level))

         (assert= "section title"
                  "Heading"
                  (-> (organ/parse-org "* Heading\nBody text")
                      :children first :title organ/inline-text))

         ;; --- Inline parsing of paragraph content ---
         (assert= "paragraph content has inline nodes"
                  :bold
                  (-> (organ/parse-org "* Heading\nThis is *bold* text")
                      :children first :children first :content
                      second :type))

         (assert= "paragraph inline text"
                  "This is bold text"
                  (-> (organ/parse-org "* Heading\nThis is *bold* text")
                      :children first :children first :content organ/inline-text))

         (assert= "nested sections"
                  2
                  (-> (organ/parse-org "* L1\n** L2\nBody")
                      :children first :children first :level))

         (assert= "TODO keyword"
                  :TODO
                  (-> (organ/parse-org "* TODO My task")
                      :children first :todo))

         (assert= "tags"
                  ["work" "urgent"]
                  (-> (organ/parse-org "* Heading :work:urgent:")
                      :children first :tags))

         (assert= "src block"
                  :src-block
                  (-> (organ/parse-org "#+BEGIN_SRC clojure\n(+ 1 2)\n#+END_SRC")
                      :children first :type))

         (assert= "src block language"
                  "clojure"
                  (-> (organ/parse-org "#+BEGIN_SRC clojure\n(+ 1 2)\n#+END_SRC")
                      :children first :language))

         (assert= "list detection"
                  :list
                  (-> (organ/parse-org "- item 1\n- item 2")
                      :children first :type))

         (assert= "table detection"
                  :table
                  (-> (organ/parse-org "| a | b |\n| 1 | 2 |")
                      :children first :type))

         (assert= "filter-ast level-limit"
                  0
                  (-> (organ/parse-org "* L1\n** L2\n*** L3")
                      (organ/filter-ast {:level-limit 1})
                      :children first :children count))

         ;; --- filter-ast title-pattern ---
         (assert= "filter-ast title-pattern keeps match"
                  "Alpha"
                  (-> (organ/parse-org "* Alpha\nBody\n* Beta\nBody")
                      (organ/filter-ast {:title-pattern #"Alpha"})
                      :children first :title organ/inline-text))

         (assert= "filter-ast title-pattern removes non-match"
                  1
                  (-> (organ/parse-org "* Alpha\nBody\n* Beta\nBody")
                      (organ/filter-ast {:title-pattern #"Alpha"})
                      :children count))

         (assert= "filter-ast title-pattern keeps descendants"
                  1
                  (-> (organ/parse-org "* Alpha\n** Child\n* Beta")
                      (organ/filter-ast {:title-pattern #"Alpha"})
                      :children first :children count))

         (assert= "clean-node paragraph content"
                  "Hello"
                  (-> (organ/parse-org "* Heading\n  Hello  ")
                      organ/clean-node
                      :children first :children first :content organ/inline-text))

         (assert= "format-ast-as-edn returns string"
                  true
                  (string? (organ/format-ast-as-edn (organ/parse-org "* Hi"))))

         ;; --- DONE keyword ---
         (assert= "DONE keyword"
                  :DONE
                  (-> (organ/parse-org "* DONE Finished task")
                      :children first :todo))

         ;; --- CLOSED planning ---
         (let [ast (organ/parse-org "* DONE Task\nCLOSED: [2025-02-20 Thu 14:30]")]
           (assert= "CLOSED planning"
                    "2025-02-20T14:30"
                    (-> ast :children first :planning :closed)))

         ;; --- DEADLINE planning ---
         (let [ast (organ/parse-org "* TODO Task\nDEADLINE: <2025-02-28 Fri>")]
           (assert= "DEADLINE planning"
                    "2025-02-28"
                    (-> ast :children first :planning :deadline)))

         ;; --- Combined CLOSED + DEADLINE ---
         (let [ast (organ/parse-org "* DONE Task\nCLOSED: [2025-02-20 Thu 14:30] DEADLINE: <2025-02-28 Fri>")]
           (assert= "combined CLOSED and DEADLINE"
                    true
                    (let [p (-> ast :children first :planning)]
                      (and (some? (:closed p)) (some? (:deadline p))))))

         ;; --- Repeater on timestamp ---
         (let [ast (organ/parse-org "* TODO Task\nSCHEDULED: <2025-03-15 Sat +1w>")]
           (assert= "repeater on scheduled"
                    "+1w"
                    (-> ast :children first :planning :scheduled-repeat)))

         ;; --- Priority ---
         (assert= "priority"
                  "A"
                  (-> (organ/parse-org "* TODO [#A] Task")
                      :children first :priority))

         ;; --- Multiple metadata (#+AUTHOR twice) ---
         (let [ast (organ/parse-org "#+AUTHOR: Alice\n#+AUTHOR: Bob\n")]
           (assert= "multiple authors"
                    ["Alice" "Bob"]
                    (-> ast :meta :author)))

         ;; --- Subtitle metadata ---
         (let [ast (organ/parse-org "#+SUBTITLE: My Sub\n")]
           (assert= "subtitle metadata"
                    "My Sub"
                    (-> ast :meta :subtitle)))

         ;; --- Email metadata ---
         (let [ast (organ/parse-org "#+EMAIL: a@b.com\n")]
           (assert= "email metadata"
                    "a@b.com"
                    (-> ast :meta :email)))

         ;; --- Language metadata ---
         (let [ast (organ/parse-org "#+LANGUAGE: fr\n")]
           (assert= "language metadata"
                    "fr"
                    (-> ast :meta :language)))

         ;; --- Quote block ---
         (assert= "quote block"
                  :quote-block
                  (-> (organ/parse-org "#+BEGIN_QUOTE\nHello\n#+END_QUOTE")
                      :children first :type))

         (assert= "quote block has paragraph children"
                  :paragraph
                  (-> (organ/parse-org "#+BEGIN_QUOTE\nHello\n#+END_QUOTE")
                      :children first :children first :type))

         ;; --- Generic block (CENTER) ---
         (let [ast (organ/parse-org "#+BEGIN_CENTER\nCentered\n#+END_CENTER")]
           (assert= "generic block type"
                    :center
                    (-> ast :children first :block-type)))

         (let [ast (organ/parse-org "#+BEGIN_CENTER\nCentered\n#+END_CENTER")]
           (assert= "generic block node type"
                    :block
                    (-> ast :children first :type)))

         ;; --- Comment ---
         (assert= "comment"
                  :comment
                  (-> (organ/parse-org "# A comment\n# Another")
                      :children first :type))

         ;; --- Fixed width ---
         (assert= "fixed width"
                  :fixed-width
                  (-> (organ/parse-org ": fixed width line")
                      :children first :type))

         ;; --- HTML line ---
         (assert= "html line"
                  :html-line
                  (-> (organ/parse-org "* S\n#+html: <div>hello</div>")
                      :children first :children first :type))

         (assert= "html line content"
                  "<div>hello</div>"
                  (-> (organ/parse-org "* S\n#+html: <div>hello</div>")
                      :children first :children first :content))

         ;; --- LaTeX line ---
         (assert= "latex line"
                  :latex-line
                  (-> (organ/parse-org "* S\n#+latex: \\newpage")
                      :children first :children first :type))

         (assert= "latex line content"
                  "\\newpage"
                  (-> (organ/parse-org "* S\n#+latex: \\newpage")
                      :children first :children first :content))

         ;; --- LaTeX environment (block) ---
         (assert= "latex environment type"
                  :latex-environment
                  (-> (organ/parse-org "\\begin{equation}\nx = y + 1\n\\end{equation}")
                      :children first :type))

         (assert= "latex environment name"
                  "equation"
                  (-> (organ/parse-org "\\begin{equation}\nx = y + 1\n\\end{equation}")
                      :children first :name))

         (assert= "latex environment content preserved"
                  "\\begin{equation}\nx = y + 1\n\\end{equation}"
                  (-> (organ/parse-org "\\begin{equation}\nx = y + 1\n\\end{equation}")
                      :children first :content))

         (assert= "latex environment align with multiple lines"
                  "\\begin{align}\na &= b \\\\\nc &= d\n\\end{align}"
                  (-> (organ/parse-org "\\begin{align}\na &= b \\\\\nc &= d\n\\end{align}")
                      :children first :content))

         (assert= "latex environment starred (equation*)"
                  "equation*"
                  (-> (organ/parse-org "\\begin{equation*}\nx\n\\end{equation*}")
                      :children first :name))

         (assert= "mismatched end does not terminate"
                  "\\begin{equation}\n\\end{align}\nstill inside\n\\end{equation}"
                  (-> (organ/parse-org "\\begin{equation}\n\\end{align}\nstill inside\n\\end{equation}")
                      :children first :content))

         (assert= "unterminated latex environment is flagged"
                  true
                  (-> (organ/parse-org "\\begin{equation}\nx = y")
                      :children first :warning some?))

         (assert= "latex environment breaks paragraph"
                  [:paragraph :latex-environment :paragraph]
                  (->> (organ/parse-org "Before.\n\\begin{equation}\nx\n\\end{equation}\nAfter.")
                       :children (map :type)))

         (assert= "latex env lines not unwrapped into preceding paragraph"
                  "Before."
                  (-> (organ/parse-org "Before.\n\\begin{equation}\nx\n\\end{equation}")
                      :children first :content organ/inline-text))

         (assert= "latex environment with affiliated #+NAME"
                  "eq1"
                  (-> (organ/parse-org "* S\n#+NAME: eq1\n\\begin{equation}\nx\n\\end{equation}")
                      :children first :children first :affiliated :name))

         (assert= "latex environment preserves blank line inside"
                  "\\begin{align}\na &= b\n\nc &= d\n\\end{align}"
                  (-> (organ/parse-org "\\begin{align}\na &= b\n\nc &= d\n\\end{align}")
                      :children first :content))

         (assert= "latex environment before headline ends at section boundary"
                  [:latex-environment :section]
                  (->> (organ/parse-org "\\begin{equation}\nx\n\\end{equation}\n* Next")
                       :children (map :type)))

         ;; --- Footnote definition ---
         (assert= "footnote definition"
                  :footnote-def
                  (-> (organ/parse-org "[fn:1] My footnote.")
                      :children first :type))

         (assert= "footnote definition label"
                  "1"
                  (-> (organ/parse-org "[fn:1] My footnote.")
                      :children first :label))

         ;; --- Inline footnote in paragraph ---
         (assert= "inline footnote in paragraph"
                  :footnote-inline
                  (-> (organ/parse-org "Text with[fn:x:inline def].")
                      :children first :content
                      second :type))

         ;; --- Description list ---
         (let [ast (organ/parse-org "- Term :: Definition")]
           (assert= "description list item term"
                    "Term"
                    (organ/inline-text (-> ast :children first :items first :term))))

         (let [ast (organ/parse-org "- Term :: Definition")]
           (assert= "description list item definition"
                    "Definition"
                    (organ/inline-text (-> ast :children first :items first :definition))))

         ;; --- Ordered list detection ---
         (let [ast (organ/parse-org "1. First\n2. Second")]
           (assert= "ordered list"
                    true
                    (-> ast :children first :ordered)))

         ;; --- Unordered list detection ---
         (let [ast (organ/parse-org "- First\n- Second")]
           (assert= "unordered list"
                    false
                    (-> ast :children first :ordered)))

         ;; --- Table with header ---
         (let [ast (organ/parse-org "| a | b |\n|---+---|\n| 1 | 2 |")]
           (assert= "table has-header"
                    true
                    (-> ast :children first :has-header)))

         ;; --- Table without header ---
         (let [ast (organ/parse-org "| a | b |\n| 1 | 2 |")]
           (assert= "table without header"
                    false
                    (-> ast :children first :has-header)))

         ;; --- Table cells are inline nodes ---
         (let [ast (organ/parse-org "| *bold* | b |\n| 1 | 2 |")]
           (assert= "table cell inline nodes"
                    :bold
                    (-> ast :children first :rows first first first :type)))

         ;; --- Affiliated keywords: caption ---
         (let [ast (organ/parse-org "* S\n#+CAPTION: My caption\n| a |\n| 1 |")]
           (assert= "affiliated caption"
                    "My caption"
                    (-> ast :children first :children first :affiliated :caption)))

         ;; --- Affiliated keywords: name ---
         (let [ast (organ/parse-org "* S\n#+NAME: my-table\n| a |\n| 1 |")]
           (assert= "affiliated name"
                    "my-table"
                    (-> ast :children first :children first :affiliated :name)))

         ;; --- Affiliated keywords: attr_html ---
         (let [ast (organ/parse-org "* S\n#+ATTR_HTML: :width 80%\n| a |\n| 1 |")]
           (assert= "affiliated attr_html"
                    "80%"
                    (-> ast :children first :children first :affiliated :attr :html :width)))

         ;; --- Org entities ---
         (assert= "org entity alpha"
                  true
                  (str/includes?
                   (organ/inline-text (-> (organ/parse-org "\\alpha here") :children first :content))
                   "α"))

         (assert= "org entity arrow"
                  true
                  (str/includes?
                   (organ/inline-text (-> (organ/parse-org "\\rarr here") :children first :content))
                   "→"))

         ;; --- Comma escaping in blocks ---
         (let [ast (organ/parse-org "#+BEGIN_SRC org\n,* Escaped headline\n,#+TITLE: Escaped\n#+END_SRC")]
           (assert= "comma unescape in src block"
                    true
                    (let [content (-> ast :children first :content)]
                      (and (str/includes? content "* Escaped headline")
                           (not (str/includes? content ",*"))))))

         ;; --- Property drawer as standalone ---
         (let [ast (organ/parse-org "* Sec\n:PROPERTIES:\n:ID: abc-123\n:END:\nBody")]
           (assert= "property drawer in section"
                    "abc-123"
                    (-> ast :children first :properties :id)))

         ;; --- Nested list ---
         (let [ast (organ/parse-org "- Parent\n  - Child")]
           (assert= "nested list"
                    :list
                    (-> ast :children first :items first :children first :type)))

         ;; --- Nested list: paragraph after sublist belongs to outer item ---
         ;; A line re-indented to the outer item's content column (not past the
         ;; sublist's marker) must attach to the outer item, not be absorbed by
         ;; the last sublist item.
         (let [ast (organ/parse-org
                    "- outer\n  1. inner\n  2. last inner\n\n  tail paragraph\n")
               outer (-> ast :children first :items first)
               sublist (-> outer :children first)
               last-inner (-> sublist :items last)]
           (assert= "tail paragraph not absorbed by last inner item"
                    nil
                    (:children last-inner))
           (assert= "tail paragraph is outer item's child"
                    :paragraph
                    (-> outer :children second :type)))

         ;; --- Src block args ---
         (let [ast (organ/parse-org "#+BEGIN_SRC python :results output\nprint(1)\n#+END_SRC")]
           (assert= "src block args"
                    "python :results output"
                    (-> ast :children first :args)))

         ;; --- Parse errors for unterminated block ---
         (let [ast (organ/parse-org "#+BEGIN_SRC clojure\n(+ 1 2)")]
           (assert= "unterminated block has parse-errors"
                    true
                    (boolean (seq (:parse-errors ast)))))

         ;; --- Generic drawer (LOGBOOK) ---
         (let [ast (organ/parse-org "* S\n:LOGBOOK:\nCLOCK: [2025-01-15 Wed 10:00]\n:END:\nBody")]
           (assert= "drawer type"
                    :drawer
                    (-> ast :children first :children first :type)))

         (let [ast (organ/parse-org "* S\n:LOGBOOK:\nCLOCK: [2025-01-15 Wed 10:00]\n:END:\nBody")]
           (assert= "drawer name"
                    "logbook"
                    (-> ast :children first :children first :drawer-name)))

         (let [ast (organ/parse-org "* S\n:LOGBOOK:\nCLOCK: [2025-01-15 Wed 10:00]\n:END:\nBody")]
           (assert= "drawer content"
                    "CLOCK: [2025-01-15 Wed 10:00]"
                    (-> ast :children first :children first :content)))

         (let [ast (organ/parse-org "* S\n:LOGBOOK:\nEntry\n:END:\nBody")]
           (assert= "body after drawer"
                    "Body"
                    (organ/inline-text (-> ast :children first :children second :content))))

         ;; --- Unterminated drawer ---
         (let [ast (organ/parse-org "* S\n:LOGBOOK:\nEntry")]
           (assert= "unterminated drawer has parse-errors"
                    true
                    (boolean (seq (:parse-errors ast)))))

         ;; --- RESULTS drawer ---
         (let [ast (organ/parse-org "* S\n:RESULTS:\nsome output\n:END:")]
           (assert= "results drawer"
                    "results"
                    (-> ast :children first :children first :drawer-name)))

         ;; --- Tags with @ ---
         (assert= "tags with @"
                  ["@home" "work"]
                  (-> (organ/parse-org "* Heading :@home:work:")
                      :children first :tags))

         ;; --- #hashtag is not a comment ---
         (assert= "#hashtag not a comment"
                  :paragraph
                  (-> (organ/parse-org "* S\n#hashtag")
                      :children first :children first :type))

         ;; --- Entity not replaced in src block ---
         (assert= "entity preserved in src block"
                  false
                  (str/includes?
                   (-> (organ/parse-org "#+BEGIN_SRC python\nx = \"\\alpha\"\n#+END_SRC")
                       :children first :content)
                   "α"))

         ;; --- Entity not matched as word prefix ---
         (assert= "entity not partial-matched"
                  "The \\integers are nice"
                  (organ/inline-text (-> (organ/parse-org "The \\integers are nice")
                                         :children first :content)))

         ;; --- Inactive timestamp time range ---
         (let [ast (organ/parse-org "* DONE Task\nCLOSED: [2025-02-20 Thu 09:00-12:00]")]
           (assert= "inactive timestamp time range"
                    "2025-02-20T09:00/2025-02-20T12:00"
                    (-> ast :children first :planning :closed)))

         ;; --- Table with leading separator ---
         (let [ast (organ/parse-org "|---+---|\n| a | b |")]
           (assert= "table with leading separator"
                    "a"
                    (organ/inline-text (-> ast :children first :rows first first))))

         ;; ============================
         ;; Inline parsing: parse-inline
         ;; ============================

         ;; --- Nil and blank input ---
         (assert= "parse-inline nil"
                  []
                  (organ/parse-inline nil))

         (assert= "parse-inline blank"
                  []
                  (organ/parse-inline "   "))

         ;; --- Plain text ---
         (assert= "parse-inline plain text"
                  [{:type :text :value "hello world"}]
                  (organ/parse-inline "hello world"))

         ;; --- Bold ---
         (assert= "inline bold"
                  {:type :bold :children [{:type :text :value "bold"}]}
                  (second (organ/parse-inline "text *bold* more")))

         ;; --- Italic ---
         (assert= "inline italic"
                  {:type :italic :children [{:type :text :value "italic"}]}
                  (second (organ/parse-inline "text /italic/ more")))

         ;; --- Underline ---
         (assert= "inline underline"
                  {:type :underline :children [{:type :text :value "underline"}]}
                  (second (organ/parse-inline "text _underline_ more")))

         ;; --- Strikethrough ---
         (assert= "inline strikethrough"
                  {:type :strike :children [{:type :text :value "strike"}]}
                  (second (organ/parse-inline "text +strike+ more")))

         ;; --- Code ---
         (assert= "inline code"
                  {:type :code :value "code"}
                  (second (organ/parse-inline "text ~code~ more")))

         ;; --- Verbatim ---
         (assert= "inline verbatim"
                  {:type :verbatim :value "verbatim"}
                  (second (organ/parse-inline "text =verbatim= more")))

         ;; --- Code/verbatim preserve content literally ---
         (assert= "code preserves entities"
                  "\\alpha"
                  (:value (first (organ/parse-inline "~\\alpha~"))))

         (assert= "verbatim preserves entities"
                  "\\beta"
                  (:value (first (organ/parse-inline "=\\beta="))))

         ;; --- Emphasis boundary: not in middle of words ---
         (assert= "star in middle of word is not bold"
                  [{:type :text :value "a*b*c"}]
                  (organ/parse-inline "a*b*c"))

         (assert= "slash path is not italic"
                  [{:type :text :value "/path/to/file"}]
                  (organ/parse-inline "/path/to/file"))

         ;; --- Emphasis boundary: requires non-whitespace inside ---
         (assert= "star with space inside is not bold"
                  [{:type :text :value "* not bold *"}]
                  (organ/parse-inline "* not bold *"))

         ;; --- Emphasis with punctuation boundaries ---
         (assert= "bold after paren"
                  :bold
                  (-> (organ/parse-inline "(*bold*)") second :type))

         ;; --- Nested emphasis: bold containing italic ---
         (let [nodes (organ/parse-inline "*bold /italic/ text*")]
           (assert= "nested bold>italic"
                    :italic
                    (-> nodes first :children second :type)))

         (let [nodes (organ/parse-inline "*bold /italic/ text*")]
           (assert= "nested bold>italic text extraction"
                    "bold italic text"
                    (organ/inline-text nodes)))

         ;; --- Link with description ---
         (let [link (second (organ/parse-inline "See [[https://example.com][Example]]"))]
           (assert= "link with desc type" :link (:type link))
           (assert= "link with desc url" "https://example.com" (:url link))
           (assert= "link with desc link-type" :https (:link-type link))
           (assert= "link with desc children"
                    "Example"
                    (organ/inline-text (:children link))))

         ;; --- Link without description ---
         (let [link (first (organ/parse-inline "[[https://example.com]]"))]
           (assert= "link without desc type" :link (:type link))
           (assert= "link without desc url" "https://example.com" (:url link))
           (assert= "link without desc no children"
                    nil
                    (seq (:children link))))

         ;; --- Link type classification ---
         (assert= "link type file"
                  :file
                  (:link-type (first (organ/parse-inline "[[file:notes.org]]"))))

         (assert= "link type id"
                  :id
                  (:link-type (first (organ/parse-inline "[[id:abc-123]]"))))

         (assert= "link target for id"
                  "abc-123"
                  (:target (first (organ/parse-inline "[[id:abc-123]]"))))

         (assert= "link type custom-id"
                  :custom-id
                  (:link-type (first (organ/parse-inline "[[#my-section]]"))))

         (assert= "link type heading"
                  :heading
                  (:link-type (first (organ/parse-inline "[[*Some Heading]]"))))

         (assert= "link target for heading"
                  "Some Heading"
                  (:target (first (organ/parse-inline "[[*Some Heading]]"))))

         ;; --- Link description with emphasis ---
         (let [link (first (organ/parse-inline "[[https://x.com][*bold* desc]]"))]
           (assert= "link desc with bold"
                    :bold
                    (-> link :children first :type)))

         ;; --- Footnote reference ---
         (assert= "inline footnote ref"
                  {:type :footnote-ref :label "1"}
                  (second (organ/parse-inline "text[fn:1] more")))

         ;; --- Footnote inline ---
         (let [fn-node (second (organ/parse-inline "text[fn:x:inline def] more"))]
           (assert= "inline footnote type" :footnote-inline (:type fn-node))
           (assert= "inline footnote label" "x" (:label fn-node))
           (assert= "inline footnote content"
                    "inline def"
                    (organ/inline-text (:children fn-node))))

         ;; --- Active timestamp ---
         (let [ts (second (organ/parse-inline "Due <2025-03-27 Thu>."))]
           (assert= "active timestamp type" :timestamp (:type ts))
           (assert= "active timestamp active" true (:active ts))
           (assert= "active timestamp value" "2025-03-27" (:value ts)))

         ;; --- Active timestamp with time ---
         (let [ts (first (organ/parse-inline "<2025-03-27 Thu 10:30>"))]
           (assert= "timestamp with time"
                    "2025-03-27T10:30"
                    (:value ts)))

         ;; --- Active timestamp with repeater ---
         (let [ts (first (organ/parse-inline "<2025-03-27 Thu +1w>"))]
           (assert= "timestamp with repeater"
                    "+1w"
                    (:repeater ts)))

         ;; --- Inactive timestamp ---
         (let [ts (first (organ/parse-inline "[2025-03-27 Thu]"))]
           (assert= "inactive timestamp type" :timestamp (:type ts))
           (assert= "inactive timestamp active" false (:active ts))
           (assert= "inactive timestamp value" "2025-03-27" (:value ts)))

         ;; --- Entity replacement in text nodes ---
         (assert= "entity in text node"
                  [{:type :text :value "α here"}]
                  (organ/parse-inline "\\alpha here"))

         ;; --- Entity NOT replaced in verbatim ---
         (assert= "entity in verbatim preserved"
                  [{:type :verbatim :value "\\alpha"}]
                  (organ/parse-inline "=\\alpha="))

         ;; --- LaTeX fragments ---
         (assert= "latex fragment $...$"
                  [{:type :latex-fragment :kind :dollar :value "x^2"}]
                  (organ/parse-inline "$x^2$"))
         (assert= "latex fragment $$...$$"
                  [{:type :latex-fragment :kind :dollars :value "E=mc^2"}]
                  (organ/parse-inline "$$E=mc^2$$"))
         (assert= "latex fragment \\(...\\)"
                  [{:type :latex-fragment :kind :paren :value "a+b"}]
                  (organ/parse-inline "\\(a+b\\)"))
         (assert= "latex fragment \\[...\\]"
                  [{:type :latex-fragment :kind :bracket :value "\\sum_i x_i"}]
                  (organ/parse-inline "\\[\\sum_i x_i\\]"))
         (assert= "latex fragment entities preserved inside"
                  [{:type :latex-fragment :kind :paren :value "\\alpha\\beta"}]
                  (organ/parse-inline "\\(\\alpha\\beta\\)"))
         (assert= "dollar currency not a fragment"
                  [{:type :text :value "I have $5 and $10"}]
                  (organ/parse-inline "I have $5 and $10"))
         (assert= "latex fragment surrounded by text"
                  [:text :latex-fragment :text]
                  (mapv :type (organ/parse-inline "before $a+b$ after")))
         (assert= "inline-text reconstructs fragment"
                  "see $x^2$"
                  (organ/inline-text (organ/parse-inline "see $x^2$")))

         ;; --- Multiple inline elements in sequence ---
         (let [nodes (organ/parse-inline "*bold* and ~code~ and [[https://x.com][link]]")]
           (assert= "multi-element count"
                    5
                    (count nodes))
           (assert= "multi-element types"
                    [:bold :text :code :text :link]
                    (mapv :type nodes)))

         ;; --- inline-text extraction ---
         (assert= "inline-text from emphasis"
                  "bold text"
                  (organ/inline-text (organ/parse-inline "*bold* text")))

         (assert= "inline-text from code"
                  "use func() here"
                  (organ/inline-text (organ/parse-inline "use ~func()~ here")))

         (assert= "inline-text from link with desc"
                  "See Example"
                  (organ/inline-text (organ/parse-inline "See [[https://x.com][Example]]")))

         (assert= "inline-text from link without desc"
                  "https://x.com"
                  (organ/inline-text (organ/parse-inline "[[https://x.com]]")))

         (assert= "inline-text nil input"
                  nil
                  (organ/inline-text nil))

         (assert= "inline-text empty"
                  nil
                  (organ/inline-text []))

         ;; --- Orphan :END: is not a drawer start ---
         (let [ast (organ/parse-org "* S\n:END:\nBody text")]
           (assert= "orphan :END: does not start drawer"
                    :paragraph
                    (-> ast :children first :children first :type)))

         (let [ast (organ/parse-org "* S\n:END:\nBody text")]
           (assert= "orphan :END: body not swallowed"
                    false
                    (boolean (seq (:parse-errors ast)))))

         ;; --- #+KEYWORD: lines don't leak into paragraphs ---
         (let [ast (organ/parse-org "* S\nSome text\n#+PROPERTY: foo bar\nMore text")]
           (assert= "metadata line not in paragraph"
                    "Some text"
                    (organ/inline-text (-> ast :children first :children first :content))))

         ;; --- parse-org nil input ---
         (assert= "parse-org nil"
                  :document
                  (:type (organ/parse-org nil)))

         ;; --- Test from test.org file if available ---
         (let [test-file "test/bzg/test.org"]
           (if (.exists (java.io.File. test-file))
             (let [ast (organ/parse-org (slurp test-file))]
               (assert= "test.org parses as document"
                        :document (:type ast)))
             (do (println "  SKIP test.org (not found)") :skip)))]]

    (let [{:keys [pass fail skip]} (merge {:pass 0 :fail 0 :skip 0} (frequencies results))]
      (println (str "\n" pass "/" (count results) " passed"
                    (when (pos? fail) (str ", " fail " failed"))
                    (when (pos? skip) (str ", " skip " skipped"))))
      (when (pos? fail) (System/exit 1)))))
