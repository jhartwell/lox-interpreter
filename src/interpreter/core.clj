(ns interpreter.core
  (:require [clojure.java.io :as io]
            [interpreter.evaluator :as evaluator]
            [interpreter.lox :as lox]
            [interpreter.scanner :as scanner]
            [interpreter.parser :as parser])
  (:gen-class))

(defn parse [file-contents]
  (let [rdr (-> file-contents char-array io/reader slurp)
        tokens (scanner/tokenize rdr)
        parser (parser/create-parser tokens)
        ast (parser/->ast parser)]
    (when-not (lox/error?)
      (parser/display-ast ast))
    (System/exit (lox/exit-code))))

(defn tokenize [file-contents]
  (let [rdr (-> file-contents char-array io/reader slurp)
        tokens (scanner/tokenize rdr)]
    (doseq [t tokens]
      (println (.toString t)))
    (System/exit (lox/exit-code))))

(defn evaluate [file-contents]
  (let [results (-> file-contents
                    char-array
                    io/reader
                    slurp
                    scanner/tokenize
                    parser/create-parser
                    parser/->ast
                    evaluator/evaluate-all)]
    (doseq [r results]
      (println r))))

(defn -main [& args]
  ;; You can use print statements as follows for debugging, they'll be visible when running tests.
  ;;  (println "Logs from your program will appear here!")
  (let [action (first args)
        filename (second args)
        file-contents (slurp filename)]
    (if (zero? (count file-contents))
      (println "EOF  null")
      (condp = action
        "parse" (parse file-contents)
        "tokenize" (tokenize file-contents)
        "evaluate" (evaluate file-contents)))))
