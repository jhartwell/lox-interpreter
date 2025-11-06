(ns interpreter.core
  (:require [clojure.java.io :as io]
            [interpreter.lox :as lox]
            [interpreter.scanner :as scanner])
  (:gen-class))

(defn -main [& args]
  ;; You can use print statements as follows for debugging, they'll be visible when running tests.
  ;;  (println "Logs from your program will appear here!")
  (let [filename (second args)
        file-contents (slurp filename)]
    (if (zero? (count file-contents))
      (println "EOF  null")
      (let [rdr (-> file-contents char-array io/reader slurp)
            tokens (scanner/tokenize rdr)]
        (doseq [t tokens]
          (println (.toString t)))
          (System/exit (lox/exit-code))))))
