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
        ;; flatten: a form may return a vector of several assert= results
        (flatten
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

         (assert= "description flag is a boolean"
                  [true false]
                  (mapv :description
                        (:children (organ/parse-org "- Term :: Def\n+ Plain"))))

         ;; --- Alphabetical lists (opt-in, like org-list-allow-alphabetical) ---
         (assert= "letter bullets are text by default"
                  [:paragraph]
                  (mapv :type (:children (organ/parse-org "a. x\nb. y"))))

         (let [alpha #(:children (organ/parse-org % {:alphabetical-lists? true}))
               [lst] (alpha "a. x\nb) [@d] y\n   c. sub")
               item2 (second (:items lst))]
           [(assert= "lower-case letter list"
                     [:list true :lower 2]
                     [(:type lst) (:ordered lst) (:alphabetical lst) (count (:items lst))])
            (assert= "letter counter-set and content"
                     [4 "y"]
                     [(:counter item2) (organ/inline-text (:content item2))])
            (assert= "nested letter list"
                     :lower
                     (-> item2 :children first :alphabetical))
            (assert= "upper-case letter list"
                     :upper
                     (:alphabetical (first (alpha "A) x\nB) y"))))
            (assert= "numeric list has no :alphabetical"
                     [true nil]
                     ((juxt :ordered :alphabetical) (first (alpha "1. x\n2. y"))))
            (assert= "letter bullet ends a paragraph"
                     [:paragraph :list]
                     (mapv :type (alpha "Some text\nb. y")))
            (assert= "only single-letter bullets"
                     [:paragraph]
                     (mapv :type (alpha "ab. x")))
            (assert= "S/C/D bullets are list items, planning still parsed"
                     [[:list] "2026-11-05"]
                     (let [sec (first (:children (organ/parse-org
                                                  "* H\nSCHEDULED: <2026-11-05 Thu>\nS. x"
                                                  {:alphabetical-lists? true})))]
                       [(mapv :type (:children sec)) (-> sec :planning :scheduled)]))])

         ;; --- Blocks and LaTeX environments in list items ---
         (let [types #(mapv :type (:children (organ/parse-org %)))]
           [(assert= "block at the bullet's column ends the list"
                     [:list :quote-block]
                     (types "- item\n#+begin_quote\nx\n#+end_quote"))
            (assert= "same with a blank line before the block"
                     [:list :quote-block]
                     (types "- item\n\n#+begin_quote\nx\n#+end_quote"))
            (assert= "block indented past the bullet stays in the item"
                     [:list]
                     (types "- item\n  #+begin_quote\n  x\n  #+end_quote"))])

         (let [ast (organ/parse-org "- a\n  \\begin{align}\n  x\n\n\n  y\n  \\end{align}\n- b")]
           [(assert= "two blank lines inside a LaTeX environment keep the item"
                     [[:latex-environment] []]
                     (mapv #(mapv :type (:children %)) (-> ast :children first :items)))
            (assert= "LaTeX environment in an item is not flagged unterminated"
                     nil
                     (:parse-errors ast))])

         ;; --- An unterminated block stops before the next headline ---
         (let [section-titles #(mapv (comp organ/inline-text :title)
                                     (:children (organ/parse-org %)))]
           [(assert= "unterminated block keeps the next section"
                     ["A" "B"]
                     (section-titles "* A\n#+begin_src\nx\n* B\ny"))
            (assert= "unterminated block content stops at the headline"
                     "x"
                     (-> (organ/parse-org "* A\n#+begin_src\nx\n* B\ny")
                         :children first :children first :content))
            (assert= "unterminated block in a list item keeps the next section"
                     ["A" "B"]
                     (section-titles "* A\n- item\n  #+begin_src\n* B\ntext"))
            (assert= "unterminated LaTeX environment keeps the next section"
                     ["A" "B"]
                     (section-titles "* A\n\\begin{align}\nx\n* B\ny"))
            (assert= "unwrapping resumes after an unterminated block"
                     "Para a para b"
                     (-> (organ/parse-org "#+begin_note\nstray\n* H\nPara a\npara b")
                         :children second :children first :content organ/inline-text))])

         ;; --- Two blank lines end a list ---
         (let [list-sizes #(mapv (comp count :items) (:children (organ/parse-org %)))]
           [(assert= "one blank line keeps the list"
                    [2]
                    (list-sizes "- a\n\n- b"))
           (assert= "two blank lines end the list"
                    [2 1]
                    (list-sizes "- a\n- b\n\n\n- c"))
           (assert= "two blank lines end nested lists too"
                    [1 1 1]
                    (list-sizes "- a\n  - a1\n\n\n  - a2\n- b"))
           (assert= "two blank lines inside a block keep the item"
                    [2]
                    (list-sizes "- a\n  #+begin_src\n  x\n\n\n  y\n  #+end_src\n- b"))])

         (assert= "two blank lines end an item before indented text"
                  [:list :paragraph]
                  (mapv :type (:children (organ/parse-org "- a\n\n\n  para"))))

         ;; --- Ordered list detection ---
         (let [ast (organ/parse-org "1. First\n2. Second")]
           (assert= "ordered list"
                    true
                    (-> ast :children first :ordered)))

         ;; --- Ordered list counter-set cookie ---
         (let [items (-> (organ/parse-org "1. a\n2. b\n5. [@5] e\n6. f")
                         :children first :items)]
           [(assert= "counter-set on ordered item"
                    [nil nil 5 nil]
                    (mapv :counter items))
           (assert= "counter-set stripped from content"
                    "e"
                    (organ/inline-text (:content (nth items 2))))])

         (let [items (-> (organ/parse-org "1) [@start:3] x\n2) [@c] y\n3. [@4] t :: d")
                         :children first :items)]
           [(assert= "counter-set start: prefix and letter counter"
                    [3 3 4]
                    (mapv :counter items))
           (assert= "counter-set before description term"
                    "t"
                    (organ/inline-text (:term (nth items 2))))])

         (let [item (-> (organ/parse-org "- [@3] z") :children first :items first)]
           (assert= "counter-set ignored on unordered item"
                    [nil "[@3] z"]
                    [(:counter item) (organ/inline-text (:content item))]))

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
           [(assert= "tail paragraph not absorbed by last inner item"
                     nil
                     (:children last-inner))
            (assert= "tail paragraph is outer item's child"
                     :paragraph
                     (-> outer :children second :type))])

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

         ;; --- Drawer in a list item, contents not indented ---
         (let [ast (organ/parse-org "* S\n- item\n  :data:\nNot indented\n\nStill inside\n:END:\n- next")]
           (assert= "drawer attached to its list item"
                    :drawer
                    (-> ast :children first :children first :items first :children first :type)))

         (let [ast (organ/parse-org "* S\n- item\n  :data:\nNot indented\n\nStill inside\n:END:\n- next")]
           (assert= "unindented drawer content"
                    "Not indented\n\nStill inside"
                    (-> ast :children first :children first :items first :children first :content)))

         (let [ast (organ/parse-org "* S\n- item\n  :data:\nNot indented\n\nStill inside\n:END:\n- next")]
           (assert= "list continues after item drawer"
                    2
                    (-> ast :children first :children first :items count)))

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

         ;; --- Leading separator suppresses header ---
         (let [ast (organ/parse-org "|---+---|\n| a | b |\n| c | d |\n|---+---|\n| e | f |")]
           (assert= "leading separator suppresses header"
                    false
                    (-> ast :children first :has-header)))

         ;; --- Separator after rows without leading border = header ---
         (let [ast (organ/parse-org "| a | b |\n| c | d |\n|---+---|\n| e | f |")]
           (assert= "mid separator without top border marks header"
                    true
                    (-> ast :children first :has-header)))

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
           [(assert= "link with desc type" :link (:type link))
            (assert= "link with desc url" "https://example.com" (:url link))
            (assert= "link with desc link-type" :https (:link-type link))
            (assert= "link with desc children"
                     "Example"
                     (organ/inline-text (:children link)))])

         ;; --- Link without description ---
         (let [link (first (organ/parse-inline "[[https://example.com]]"))]
           [(assert= "link without desc type" :link (:type link))
            (assert= "link without desc url" "https://example.com" (:url link))
            (assert= "link without desc no children"
                     nil
                     (seq (:children link)))])

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
           [(assert= "inline footnote type" :footnote-inline (:type fn-node))
            (assert= "inline footnote label" "x" (:label fn-node))
            (assert= "inline footnote content"
                     "inline def"
                     (organ/inline-text (:children fn-node)))])

         ;; --- Active timestamp ---
         (let [ts (second (organ/parse-inline "Due <2025-03-27 Thu>."))]
           [(assert= "active timestamp type" :timestamp (:type ts))
            (assert= "active timestamp active" true (:active ts))
            (assert= "active timestamp value" "2025-03-27" (:value ts))])

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
           [(assert= "inactive timestamp type" :timestamp (:type ts))
            (assert= "inactive timestamp active" false (:active ts))
            (assert= "inactive timestamp value" "2025-03-27" (:value ts))])

         ;; --- Timestamp range exposes structured :start/:end ---
         (let [ts (first (organ/parse-inline "<2025-03-27 Thu 09:00-12:00>"))]
           [(assert= "range timestamp :start" "2025-03-27T09:00" (:start ts))
            (assert= "range timestamp :end" "2025-03-27T12:00" (:end ts))
            (assert= "range timestamp value preserved"
                     "2025-03-27T09:00/2025-03-27T12:00" (:value ts))])
         (let [ts (first (organ/parse-inline "<2025-03-27 Thu 10:30>"))]
           (assert= "non-range timestamp has no :end" nil (:end ts)))

         ;; --- parse-repeater ---
         (assert= "parse-repeater +1w" {:n 1 :unit :week :kind :cumulate}
                  (organ/parse-repeater "+1w"))
         (assert= "parse-repeater ++2d" {:n 2 :unit :day :kind :catch-up}
                  (organ/parse-repeater "++2d"))
         (assert= "parse-repeater .+1m" {:n 1 :unit :month :kind :restart}
                  (organ/parse-repeater ".+1m"))
         (assert= "parse-repeater nil" nil (organ/parse-repeater nil))
         (assert= "parse-repeater garbage" nil (organ/parse-repeater "weekly"))

         ;; --- active-timestamps: unified inline + planning ---
         (let [ast (organ/parse-org
                    "* Meeting\nSCHEDULED: <2025-03-15 Sat 10:00-11:00 +1w>\nNotes <2025-03-20 Thu>")
               ts  (organ/active-timestamps ast)]
           [(assert= "active-timestamps count" 2 (count ts))
            (assert= "active-timestamps origins" #{:scheduled :inline}
                     (set (map :origin ts)))
            (let [sched (first (filter #(= :scheduled (:origin %)) ts))]
              [(assert= "scheduled :start" "2025-03-15T10:00" (:start sched))
               (assert= "scheduled :end" "2025-03-15T11:00" (:end sched))
               (assert= "scheduled repeater parsed" {:n 1 :unit :week :kind :cumulate}
                        (:repeater sched))
               (assert= "scheduled :path" ["Meeting"] (:path sched))
               (assert= "scheduled not all-day" false (:all-day sched))])
            (let [inl (first (filter #(= :inline (:origin %)) ts))]
              [(assert= "inline all-day" true (:all-day inl))
               (assert= "inline value" "2025-03-20" (:value inl))
               (assert= "inline :path" ["Meeting"] (:path inl))])])

         ;; --- active-timestamps: DEADLINE origin ---
         (let [ast (organ/parse-org "* Task\nDEADLINE: <2025-02-28 Fri>")
               ts  (organ/active-timestamps ast)]
           (assert= "deadline origin" :deadline (:origin (first ts))))

         ;; --- active-timestamps excludes CLOSED + inactive ---
         (let [ast (organ/parse-org
                    "* DONE Task\nCLOSED: [2025-02-20 Thu 14:30]\nSee [2025-02-21 Fri]")]
           (assert= "active-timestamps excludes closed/inactive"
                    0 (count (organ/active-timestamps ast))))

         ;; --- active-timestamps carries :title/:todo ---
         (let [ast (organ/parse-org "* TODO Ship it\nSCHEDULED: <2025-03-15 Sat>")
               e   (first (organ/active-timestamps ast))]
           [(assert= "entry :title" "Ship it" (:title e))
            (assert= "entry :todo" :TODO (:todo e))])

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
           [(assert= "multi-element count"
                     5
                     (count nodes))
            (assert= "multi-element types"
                     [:bold :text :code :text :link]
                     (mapv :type nodes))])

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

         (assert= "inline-text :bare-link :omit drops a bare URL"
                  "see  here"
                  (organ/inline-text (organ/parse-inline "see [[https://x.com]] here")
                                     {:bare-link :omit}))

         (assert= "inline-text :bare-link :omit keeps a link description"
                  "see Example here"
                  (organ/inline-text (organ/parse-inline "see [[https://x.com][Example]] here")
                                     {:bare-link :omit}))

         (assert= "inline-text :bare-link :omit recurses into nested nodes"
                  "bold  end"
                  (organ/inline-text (organ/parse-inline "*bold [[https://x.com]]* end")
                                     {:bare-link :omit}))

         (assert= "inline-text :bare-link :omit drops a bare file: link"
                  ""
                  (organ/inline-text (organ/parse-inline "[[file:~/photo.jpg]]")
                                     {:bare-link :omit}))

         (assert= "inline-text unknown :bare-link value keeps the default"
                  "https://x.com"
                  (organ/inline-text (organ/parse-inline "[[https://x.com]]")
                                     {:bare-link :nonsense}))

         (assert= "inline-text :bare-link as a function decides per link"
                  ["see https://x.com here" "see  here"]
                  (let [drop-images #(when-not (re-find #"\.jpg$" (:url %)) (:url %))]
                    [(organ/inline-text (organ/parse-inline "see [[https://x.com]] here")
                                        {:bare-link drop-images})
                     (organ/inline-text (organ/parse-inline "see [[https://x.com/a.jpg]] here")
                                        {:bare-link drop-images})]))

         (assert= "inline-text :bare-link function still yields to a description"
                  "see Example here"
                  (organ/inline-text (organ/parse-inline "see [[https://x.com/a.jpg][Example]] here")
                                     {:bare-link (constantly "NOPE")}))

         (assert= "inline-text empty opts keeps the default"
                  "https://x.com"
                  (organ/inline-text (organ/parse-inline "[[https://x.com]]") {}))

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

         ;; --- Warning/delay cookies in timestamps ---
         (let [ast (organ/parse-org "* TODO Task\nDEADLINE: <2025-02-28 Fri -2d>")]
           (assert= "deadline with warning cookie"
                    "2025-02-28"
                    (-> ast :children first :planning :deadline)))

         (let [ast (organ/parse-org "* T\nSCHEDULED: <2025-03-15 Sat 10:00 +1w -2d>")]
           (assert= "scheduled with repeater and warning"
                    ["2025-03-15T10:00" "+1w"]
                    (let [p (-> ast :children first :planning)]
                      [(:scheduled p) (:scheduled-repeat p)])))

         (let [ts (first (organ/parse-inline "<2025-03-15 Sat -1d>"))]
           (assert= "inline timestamp with warning cookie"
                    ["2025-03-15" :timestamp]
                    [(:value ts) (:type ts)]))

         ;; --- Day name is optional in timestamps ---
         (assert= "timestamp without day name"
                  "2025-01-15"
                  (:value (first (organ/parse-inline "<2025-01-15>"))))

         (assert= "inactive timestamp without day name, with time"
                  "2025-01-15T10:30"
                  (:value (first (organ/parse-inline "[2025-01-15 10:30]"))))

         ;; --- Footnote definition right after a paragraph ---
         (assert= "footnote def not merged into preceding paragraph"
                  [:paragraph :footnote-def]
                  (->> (organ/parse-org "Para text.\n[fn:1] A note.")
                       :children (mapv :type)))

         ;; --- Unterminated property drawer keeps content (Emacs: paragraph) ---
         (let [ast (organ/parse-org "* S\n:PROPERTIES:\n:ID: x\nBody\n* Next\nCorps")]
           (assert= "unterminated property drawer keeps content and next section"
                    [:paragraph "Next" true]
                    [(-> ast :children first :children first :type)
                     (-> ast :children second :title organ/inline-text)
                     (boolean (seq (:parse-errors ast)))]))

         ;; --- Unterminated generic drawer keeps content (Emacs: paragraph) ---
         (let [ast (organ/parse-org "Avant.\n\n:ARCHIVE:\n\nTexte.\n\n* Titre\nCorps.")]
           (assert= "unterminated drawer becomes paragraph, headline survives"
                    [[:paragraph :paragraph :paragraph :section] "Titre"]
                    [(mapv :type (:children ast))
                     (-> ast :children last :title organ/inline-text)]))

         ;; --- Affiliated keywords before content at document top ---
         (let [ast (organ/parse-org "#+CAPTION: My cap\n| a |\n| 1 |")]
           (assert= "caption at document top"
                    "My cap"
                    (-> ast :children first :affiliated :caption)))

         ;; --- Stray planning line is kept as a paragraph ---
         (let [ast (organ/parse-org "Text\n\nSCHEDULED: <2025-01-01 Wed>\n\nAfter")]
           (assert= "stray planning line kept as paragraph"
                    [3 "SCHEDULED: <2025-01-01 Wed>"]
                    [(count (:children ast))
                     (organ/inline-text (-> ast :children second :content))]))

         ;; --- Invalid planning timestamp leaves no nil in the AST ---
         (let [ast (organ/parse-org "* T\nSCHEDULED: <someday>")]
           (assert= "invalid planning timestamp yields no planning key"
                    nil
                    (-> ast :children first :planning)))

         ;; --- Out-of-range end hour is dropped, start time is kept ---
         (let [ast (organ/parse-org "* T\nSCHEDULED: <2026-07-24 Fri 12:00-25:30>")]
           (assert= "out-of-range end hour drops the range, keeps start time"
                    "2026-07-24T12:00"
                    (-> ast :children first :planning :scheduled)))

         ;; --- Out-of-range start hour falls back to date-only ---
         (let [ast (organ/parse-org "* T\nSCHEDULED: <2026-07-24 Fri 25:30>")]
           (assert= "out-of-range start hour falls back to date-only"
                    "2026-07-24"
                    (-> ast :children first :planning :scheduled)))

         ;; --- Out-of-range minute falls back to date-only ---
         (let [ast (organ/parse-org "* T\nSCHEDULED: <2026-07-24 Fri 12:65>")]
           (assert= "out-of-range minute falls back to date-only"
                    "2026-07-24"
                    (-> ast :children first :planning :scheduled)))

         ;; --- Table row without trailing pipe ---
         (let [ast (organ/parse-org "| a | b |\n| c | d")]
           (assert= "row without trailing pipe is a table row with all cells"
                    [2 "d"]
                    [(-> ast :children first :rows count)
                     (organ/inline-text (-> ast :children first :rows second second))]))

         ;; --- Comma escaping follows org-src rules ---
         (let [ast (organ/parse-org "#+BEGIN_SRC org\n,* t\n,,* deep\n,# literal\n  ,#+kw\n#+END_SRC")]
           (assert= "comma unescape: one comma removed, ,# kept literal"
                    "* t\n,* deep\n,# literal\n  #+kw"
                    (-> ast :children first :content)))

         (assert= "escape-block-content escapes * #+ and ,*"
                  ",* a\n,,* b\n,#+kw\ntext"
                  (organ/escape-block-content "* a\n,* b\n#+kw\ntext"))

         ;; --- Custom TODO keywords ---
         (let [ast (organ/parse-org "#+TODO: TODO NEXT WAIT | DONE CANX\n* WAIT [#A] Task :work:")]
           (assert= "directive keyword recognised, priority and tags parsed"
                    [:WAIT "A" "Task" ["work"]]
                    (-> ast :children first
                        ((juxt :todo :priority
                               #(organ/inline-text (:title %)) :tags)))))

         (assert= "effective keywords exposed on the document node"
                  {:todo ["TODO" "NEXT" "WAIT"] :done ["DONE" "CANX"]}
                  (:todo-keywords
                   (organ/parse-org "#+TODO: TODO NEXT WAIT | DONE CANX\n* T")))

         (assert= "default set without directive"
                  [{:todo ["TODO"] :done ["DONE"]} nil]
                  (let [ast (organ/parse-org "* WAIT Task")]
                    [(:todo-keywords ast) (-> ast :children first :todo)]))

         (assert= "directive replaces the default set"
                  nil
                  (-> (organ/parse-org "#+TODO: OPEN | CLOSED\n* TODO Task")
                      :children first :todo))

         (assert= "directive applies to headings before it"
                  :NEXT
                  (-> (organ/parse-org "* NEXT Task\n#+TODO: TODO NEXT | DONE")
                      :children first :todo))

         (assert= "directive without bar: last word is DONE"
                  {:todo ["OPEN"] :done ["CLOSED"]}
                  (organ/parse-todo-keywords "#+SEQ_TODO: OPEN CLOSED"))

         (assert= "fast-access selectors are stripped"
                  {:todo ["TODO" "WAIT"] :done ["DONE"]}
                  (organ/parse-todo-keywords "#+TODO: TODO(t) WAIT(w@/!) | DONE(d!)"))

         (assert= "multiple directives accumulate"
                  {:todo ["TODO" "OPEN"] :done ["DONE" "CLOSED"]}
                  (organ/parse-todo-keywords
                   "#+TODO: TODO | DONE\n#+TYP_TODO: OPEN | CLOSED"))

         (assert= ":todo-keywords option as directive-syntax string"
                  :STRT
                  (-> (organ/parse-org "* STRT Task" {:todo-keywords "STRT |"})
                      :children first :todo))

         (assert= ":todo-keywords option extends the default set"
                  [:NEXT :TODO]
                  (let [ast (organ/parse-org "* NEXT A\n* TODO B"
                                             {:todo-keywords {:todo ["NEXT"]}})]
                    (mapv :todo (:children ast))))

         (let [ast (organ/parse-org "#+TODO: TODO WAIT | DONE\n* WAIT Task\nSCHEDULED: <2026-08-03 Mon>")]
           (assert= "active-timestamps carries custom keyword"
                    :WAIT
                    (-> (organ/active-timestamps ast) first :todo)))

         (assert= "keywords are deduplicated across sources"
                  {:todo ["TODO"] :done ["DONE" "CANX"]}
                  (:todo-keywords
                   (organ/parse-org "* T" {:todo-keywords {:done ["DONE" "CANX"]}})))

         (assert= "malformed directive with several bars yields no | keyword"
                  {:todo ["A"] :done ["B" "C"]}
                  (organ/parse-todo-keywords "#+TODO: A | B | C"))

         (assert= "empty directive falls back to the default set"
                  [{:todo ["TODO"] :done ["DONE"]} :TODO]
                  (let [ast (organ/parse-org "#+TODO:\n* TODO Task")]
                    [(:todo-keywords ast) (-> ast :children first :todo)]))

         ;; --- Preamble: every keyword kept, in file order, past 8 keys ---
         (let [ast (organ/parse-org (str "#+TITLE: T\n#+TODO: TODO | DONE\n#+STARTUP: overview\n"
                                         "#+FILETAGS: :a:\n#+PROPERTY: x 1\n#+HTML_HEAD: <a>\n"
                                         "\n#+html_head: <b>\n#+LANGUAGE: fr\n#+AUTHOR: A\n"
                                         "#+DATE: 2026\n\nText"))]
           (assert= "preamble keeps unknown keywords in file order"
                    [[:title :todo :startup :filetags :property :html_head :language :author :date]
                     "TODO | DONE" ["<a>" "<b>"] :paragraph]
                    [(vec (keys (:meta ast)))
                     (-> ast :meta :todo)
                     (-> ast :meta :html_head)
                     (-> ast :children first :type)]))

         (assert= "preamble stops at content directives and affiliated keywords"
                  [{:title "T"} :html-line {:title "T"} :table]
                  (let [a (organ/parse-org "#+TITLE: T\n#+html: <hr>\n#+AUTHOR: X\n")
                        b (organ/parse-org "#+TITLE: T\n#+NAME: tbl\n| a |\n")]
                    [(:meta a) (-> a :children first :type)
                     (:meta b) (-> b :children first :type)]))

         (assert= "repeated #+TITLE: lines are joined for the document title"
                  ["A B" ["A" "B"]]
                  (let [ast (organ/parse-org "#+TITLE: A\n#+TITLE: B\n\nx")]
                    [(organ/inline-text (:title ast)) (-> ast :meta :title)]))

         (assert= "no preamble yields no :meta"
                  nil
                  (:meta (organ/parse-org "Text\n#+TITLE: Late")))

         ;; --- #+KEY: lines outside the preamble become :keyword nodes ---
         (let [ast (organ/parse-org "Text\n#+TITLE: Late\n#+STARTUP: overview\n\nAfter\n* H\n#+PROPERTY: foo bar\nBody")]
           (assert= "body keyword lines become :keyword nodes, in place"
                    [[:paragraph :keyword :keyword :paragraph :section]
                     {:type :keyword :key "title" :value "Late" :line 2}
                     [{:type :keyword :key "property" :value "foo bar" :line 7}
                      :paragraph]]
                    [(mapv :type (:children ast))
                     (-> ast :children second)
                     (let [[k p] (-> ast :children last :children)] [k (:type p)])]))

         (assert= "late #+TODO: is both a :keyword node and the keyword source"
                  [:keyword :NEXT]
                  (let [ast (organ/parse-org "* NEXT Task\n#+TODO: TODO NEXT | DONE")]
                    [(-> ast :children first :children first :type)
                     (-> ast :children first :todo)]))

         (assert= "keyword line inside a list item"
                  [:keyword :paragraph]
                  (->> (organ/parse-org "- item\n  #+STARTUP: x\n  more")
                       :children first :items first :children (mapv :type)))

         (assert= "keyword line between `- term ::` and its definition is kept"
                  ["definition" [:keyword]]
                  (let [item (-> (organ/parse-org "- term ::\n  #+FOO: bar\n  definition")
                                 :children first :items first)]
                    [(organ/inline-text (:definition item))
                     (mapv :type (:children item))]))

         ;; --- A long paragraph with an unclosed [[ must not overflow the stack ---
         (assert= "unclosed [[ before a long text"
                  [1 2]
                  (let [long-text (apply str (repeat 50000 "x "))]
                    [(count (organ/parse-inline (str "see [[ " long-text)))
                     (count (organ/parse-inline (str "[[u][" long-text "]] end")))]))

         ;; --- Comments and a property drawer do not end the preamble ---
         (assert= "preamble after a mode-line comment"
                  ["Foo" {:title "Foo" :author "me"} [:comment :paragraph]]
                  (let [ast (organ/parse-org "# -*- mode: org -*-\n#+TITLE: Foo\n#+AUTHOR: me\n\nText")]
                    [(organ/inline-text (:title ast)) (:meta ast) (mapv :type (:children ast))]))

         (assert= "preamble after a document property drawer (org-roam)"
                  ["Note" [:property-drawer :paragraph] {:id "abc"}]
                  (let [ast (organ/parse-org ":PROPERTIES:\n:ID: abc\n:END:\n#+title: Note\n\nText")]
                    [(organ/inline-text (:title ast))
                     (mapv :type (:children ast))
                     (-> ast :children first :properties)]))

         (assert= "comments separated by a blank line stay separate"
                  [[:comment :comment :paragraph] {:title "T"}]
                  (let [ast (organ/parse-org "# a\n\n# b\n#+TITLE: T\nText")]
                    [(mapv :type (:children ast)) (:meta ast)]))

         ;; --- Drawer contents are literal: never unwrapped ---
         (assert= "drawer lines are not joined"
                  ["CLOCK: [2025-01-01 Wed 10:00]--[2025-01-01 Wed 11:00] =>  1:00\nCLOCK: [2025-01-02 Thu 10:00]--[2025-01-02 Thu 11:00] =>  1:00"
                   "- State \"DONE\"   from \"TODO\"\n  more   text\nfree text"
                   "Body text"]
                  (let [ast (organ/parse-org (str "* H\n:LOGBOOK:\n"
                                                  "CLOCK: [2025-01-01 Wed 10:00]--[2025-01-01 Wed 11:00] =>  1:00\n"
                                                  "CLOCK: [2025-01-02 Thu 10:00]--[2025-01-02 Thu 11:00] =>  1:00\n"
                                                  ":END:\n:NOTE:\n- State \"DONE\"   from \"TODO\"\n  more   text\nfree text\n:END:\n"
                                                  "Body\ntext"))
                        [d1 d2 p] (-> ast :children first :children)]
                    [(:content d1) (:content d2) (organ/inline-text (:content p))]))

         (assert= "an unterminated drawer is still unwrapped as a paragraph"
                  ":NOTE:\none two"
                  (-> (organ/parse-org ":NOTE:\none\ntwo") :children first :content organ/inline-text))

         ;; --- A line not indented past the bullet ends the item, unwrapped or not ---
         (assert= "text right after a list is not joined to its last item"
                  (repeat 2 [[:list :paragraph] "item" "not part of the list at all"])
                  (for [opts [{} {:unwrap? false}]]
                    (let [[l p :as nodes] (:children (organ/parse-org "- item\nnot part of the list\nat all" opts))]
                      [(mapv :type nodes)
                       (str/replace (organ/inline-text (-> l :items first :content)) "\n" " ")
                       (str/replace (organ/inline-text (:content p)) "\n" " ")])))

         (assert= "text after a sublist goes back to the outer item"
                  ["b" [:list :paragraph] "back to a still a" "c"]
                  (let [[a c] (-> (organ/parse-org "- a\n  - b\n  back to a\n  still a\n- c") :children first :items)]
                    [(-> a :children first :items first :content organ/inline-text)
                     (mapv :type (:children a))
                     (-> a :children second :content organ/inline-text)
                     (organ/inline-text (:content c))]))

         (assert= "after two blank lines, an indented paragraph is unwrapped freely"
                  "indented and back"
                  (-> (organ/parse-org "- a\n\n\n  indented\nand back") :children second :content organ/inline-text))

         (assert= "a tab indents a continuation line up to column 8"
                  ["one two" [:list]]
                  (let [nodes (:children (organ/parse-org "      1. one\n\t two"))]
                    [(-> nodes first :items first :content organ/inline-text)
                     (mapv :type nodes)]))

         ;; --- :PROPERTIES: and :END: in any case, as in Org ---
         (assert= "lower-case property drawer"
                  [{:id "abc"} [:paragraph] {:id "def"}]
                  (let [[a b] (:children (organ/parse-org "* A\n:properties:\n:id: abc\n:end:\nText\n* B\n:PROPERTIES:\n:ID: def\n:end:"))]
                    [(:properties a) (mapv :type (:children a)) (:properties b)]))

         ;; --- Property names are any non-blank run (header-args+, Foo.Bar) ---
         (assert= "property names with + and ."
                  {:header-args+ ":x y" :foo.bar "1" :header-args:python ":session s" :empty ""}
                  (-> (organ/parse-org "* H\n:PROPERTIES:\n:header-args+: :x y\n:Foo.Bar: 1\n:header-args:python: :session s\n:EMPTY:\n:END:")
                      :children first :properties))

         ;; --- Attribute values run up to the next :key, as in Org ---
         (assert= "multi-word, quoted, dashed and valueless attributes"
                  {:style "color: red; width: 50%" :alt "A nice image" :data-x "1" :title "" :class "a b"}
                  (-> (organ/parse-org (str "#+ATTR_HTML: :style color: red; width: 50% :alt \"A nice image\""
                                            " :controls :data-x 1 :title \"\" :class a b\n[[file:x.png]]"))
                      :children first :affiliated :attr :html))

         ;; --- Affiliated keywords with an optional part: #+CAPTION[short]:, #+RESULTS[hash]: ---
         (assert= "short caption and cached results"
                  [[:paragraph :table] {:caption "long caption"} {:results "tbl"}]
                  (let [[p f :as nodes] (:children (organ/parse-org "#+CAPTION[short]: long caption\n[[file:x.png]]\n\n#+RESULTS[abc123]: tbl\n| 42 |"))]
                    [(mapv :type nodes) (:affiliated p) (:affiliated f)]))

         ;; --- A TODO keyword or a priority may end the headline ---
         (assert= "headlines without a title"
                  [[:TODO nil nil nil] [:DONE nil nil nil] [:TODO "A" nil nil] [nil "B" nil nil]
                   [nil nil "TODOS" nil] [:TODO nil ":tag:" nil] [nil nil ":tag:" nil] [nil nil nil nil]
                   [:TODO "A" "Do it" ["x" "y"]]]
                  (->> (organ/parse-org "* TODO\n* DONE  \n* TODO [#A]\n* [#B]\n* TODOS\n* TODO :tag:\n*   :tag:\n* \n* TODO [#A] Do it :x:y:")
                       :children
                       (mapv (juxt :todo :priority #(some-> (:title %) organ/inline-text) :tags))))

         ;; --- Tags: letters and digits of any script, and _ @ # % ---
         (assert= "non-ASCII tags, # and %"
                  [["Titre" ["été" "à_faire"]] ["T2" ["a#b" "50%" "@home"]] ["T3 :a-b:" nil]]
                  (->> (organ/parse-org "* Titre :été:à_faire:\n* T2 :a#b:50%:@home:\n* T3 :a-b:")
                       :children
                       (mapv (juxt #(organ/inline-text (:title %)) :tags))))

         ;; --- Inline footnote holding a link ---
         (assert= "inline footnote with a link, then text"
                  [[:text :footnote-inline :text] "see x ok" " end [fn::open"]
                  (let [[_ f t :as nodes] (organ/parse-inline "Text[fn::see [[https://x.org][x]] ok] end [fn::open")]
                    [(mapv :type nodes) (organ/inline-text (:children f)) (:value t)]))

         ;; --- An inline footnote starting a line is not a definition ---
         (assert= "paragraphs starting with an inline footnote"
                  [[:paragraph :paragraph :footnote-def]
                   [:text :footnote-inline :text]
                   [:footnote-inline :text]]
                  (let [[p1 p2 :as nodes] (:children (organ/parse-org "Some text\n[fn::An anonymous note] goes on.\n\n[fn:lbl:Inline def] another.\n\n[fn:1] A definition."))]
                    [(mapv :type nodes) (mapv :type (:content p1)) (mapv :type (:content p2))]))

         ;; --- A \begin{env} line may carry arguments or a label ---
         (assert= "LaTeX environments whose begin line goes on"
                  [[:latex-environment :latex-environment :paragraph]
                   ["tabular" "\\begin{tabular}{ll}\na & b\n\\end{tabular}"]
                   ["equation" "\\begin{equation}\\label{eq:1}\nx\n\\end{equation}"]
                   nil]
                  (let [ast (organ/parse-org "\\begin{tabular}{ll}\na & b\n\\end{tabular}\n\n\\begin{equation}\\label{eq:1}\nx\n\\end{equation}\n\n\\begin{math} x \\end{math} on one line")
                        [e1 e2 :as nodes] (:children ast)]
                    [(mapv :type nodes)
                     ((juxt :name :content) e1)
                     ((juxt :name :content) e2)
                     (:parse-errors ast)]))

         ;; --- A lone colon is an empty fixed-width line ---
         (assert= "empty line inside a fixed-width area"
                  [[:fixed-width] "a\n\n  b"]
                  (let [nodes (:children (organ/parse-org ": a\n:\n:   b"))]
                    [(mapv :type nodes) (:content (first nodes))]))

         ;; --- \name{} is the entity, braces consumed ---
         (assert= "entities closed by {}"
                  "a αb, πr, a b and ⇒ c \\alphabet{}"
                  (organ/inline-text (organ/parse-inline "a \\alpha{}b, \\pi{}r, a\\nbsp{}b and \\rArr{} c \\alphabet{}")))

         ;; --- A bullet alone on its line is an item ---
         (assert= "empty items and items whose contents start on the next line"
                  [[:list] [nil nil "c d" nil] [[] [:keyword :paragraph] [] [:paragraph]]
                   [:paragraph]]
                  (let [nodes (:children (organ/parse-org "- \n-\n  #+vindex: x\n  text\n- c\n  d\n-\n\n  para"))
                        items (-> nodes first :items)]
                    [(mapv :type nodes)
                     (mapv #(some-> (:content %) organ/inline-text) items)
                     (mapv #(mapv :type (:children %)) items)
                     (mapv :type (:children (organ/parse-org "a star\n*\nalone")))]))

         ;; --- Macros ---
         (let [macro (first (organ/parse-inline "{{{my-macro}}}"))]
           (assert= "macro without arguments"
                    {:type :macro :name "my-macro" :args []} macro))

         (let [macro (first (organ/parse-inline "{{{my-macro(1,2)}}}"))]
           (assert= "macro with arguments"
                    {:type :macro :name "my-macro" :args ["1" "2"]} macro))

         (let [macro (first (organ/parse-inline "{{{my-macro(1,2\\,3)}}}"))]
           (assert= "macro arguments with comma" ["1" "2,3"] (:args macro)))

         (let [macro (first (organ/parse-inline "{{{my-macro((with parens))}}}"))]
           (assert= "macro with parens in argument"
                    {:type :macro :name "my-macro" :args ["(with parens)"]} macro))

         ;; ensure the matching regexp is non greedy
         (let [[macro1 space macro2] (organ/parse-inline "{{{my-macro((1))}}} {{{second-macro(2)}}}")]
           [(assert= "first macro"
                     {:type :macro :name "my-macro" :args ["(1)"]} macro1)
            (assert= "space between macros" {:type :text, :value " "} space)
            (assert= "second macro"
                     {:type :macro, :args ["2"], :name "second-macro"} macro2)])

         ;; --- Test from test.org file if available ---
         (let [test-file "test/bzg/test.org"]
           (if (.exists (java.io.File. test-file))
             (let [ast (organ/parse-org (slurp test-file))]
               (assert= "test.org parses as document"
                        :document (:type ast)))
             (do (println "  SKIP test.org (not found)") :skip)))])]

    (let [{:keys [pass fail skip]} (merge {:pass 0 :fail 0 :skip 0} (frequencies results))]
      (println (str "\n" pass "/" (count results) " passed"
                    (when (pos? fail) (str ", " fail " failed"))
                    (when (pos? skip) (str ", " skip " skipped"))))
      (when (pos? fail) (System/exit 1)))))
