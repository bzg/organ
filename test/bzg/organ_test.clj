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

         ;; Test from test.org file if available
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
