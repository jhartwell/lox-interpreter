(ns interpreter.evaluator)

(defmulti evaluate :ast/type)

(defmethod evaluate :literal [this] (:ast/value this))


(defn evaluate-all [ast]
  (map evaluate ast))

(comment
  (require '[interpreter.scanner :as scanner]
           '[interpreter.parser :as parser])

  (def a (-> "true"
             scanner/tokenize
             parser/create-parser
             parser/->ast))

  a
  (evaluate (first a))

  (eval a)

  a


  )
