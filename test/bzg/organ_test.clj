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
  "Find a top-level or nested section by title in the AST."
  [ast title]
  (letfn [(walk [node]
            (when (map? node)
              (if (and (= (:type node) :section) (= (:title node) title))
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
                  (:title (organ/parse-org "#+TITLE: Hello\n")))

         (assert= "section level"
                  1
                  (-> (organ/parse-org "* Heading\nBody text")
                      :children first :level))

         (assert= "section title"
                  "Heading"
                  (-> (organ/parse-org "* Heading\nBody text")
                      :children first :title))

         (assert= "paragraph content preserves org markup"
                  "This is *bold* text"
                  (-> (organ/parse-org "* Heading\nThis is *bold* text")
                      :children first :children first :content))

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

         (assert= "clean-node trims content"
                  "Hello"
                  (-> (organ/parse-org "* Heading\n  Hello  ")
                      organ/clean-node
                      :children first :children first :content))

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

         ;; --- Footnote definition ---
         (assert= "footnote definition"
                  :footnote-def
                  (-> (organ/parse-org "[fn:1] My footnote.")
                      :children first :type))

         (assert= "footnote definition label"
                  "1"
                  (-> (organ/parse-org "[fn:1] My footnote.")
                      :children first :label))

         ;; --- Inline footnote in paragraph text ---
         (assert= "inline footnote in paragraph"
                  true
                  (let [content (-> (organ/parse-org "Text with[fn:x:inline def].")
                                    :children first :content)]
                    (boolean (re-find organ/footnote-inline-pattern content))))

         ;; --- Description list ---
         (let [ast (organ/parse-org "- Term :: Definition")]
           (assert= "description list item term"
                    "Term"
                    (-> ast :children first :items first :term)))

         (let [ast (organ/parse-org "- Term :: Definition")]
           (assert= "description list item definition"
                    "Definition"
                    (-> ast :children first :items first :definition)))

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
                  (str/includes? (-> (organ/parse-org "\\alpha here") :children first :content) "α"))

         (assert= "org entity arrow"
                  true
                  (str/includes? (-> (organ/parse-org "\\rarr here") :children first :content) "→"))

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
                    (-> ast :children first :children second :content)))

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
                  (-> (organ/parse-org "The \\integers are nice")
                      :children first :content))

         ;; --- Inactive timestamp time range ---
         (let [ast (organ/parse-org "* DONE Task\nCLOSED: [2025-02-20 Thu 09:00-12:00]")]
           (assert= "inactive timestamp time range"
                    "2025-02-20T09:00/2025-02-20T12:00"
                    (-> ast :children first :planning :closed)))

         ;; --- Table with leading separator ---
         (let [ast (organ/parse-org "|---+---|\n| a | b |")]
           (assert= "table with leading separator"
                    [["a" "b"]]
                    (-> ast :children first :rows)))

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
