(ns interpreter.parser
  (:require [clojure.spec.alpha :as s]
            [interpreter.lox :as lox]
            [interpreter.scanner :as scanner]))

(declare expression)

(def value? (s/or :some some? :nil nil?))
(s/def :ast/type #{:literal :binary :unary :grouping})
(s/def :ast/value value?)
(s/def :ast/op string?)
(s/def :ast/lhs value?)
(s/def :ast/rhs value?)
(s/def :ast/literal (s/keys :req [:ast/type :ast/value]))
(s/def :ast/binary (s/keys :req [:ast/type :ast/lhs :ast/rhs :ast/op]))
(s/def :ast/unary (s/keys :req [:ast/type :ast/rhs :ast/op]))
(s/def :ast/grouping (s/keys :req [:ast/type :ast/value]))
(s/def :ast/node (s/or :literal :ast/literal
                       :binary :ast/binary
                       :unary :ast/unary
                       :grouping :ast/grouping))

(defprotocol Parsing
  (match? [this types])
  (advance [this])
  (at-end? [this])
  (peek-token [this])
  (previous [this])
  (consume [this token msg]))

(defrecord Parser [tokens index]
  Parsing
  (match? [this types]
    (-> this peek-token :type (= types)))
  (advance [this]
    (update this :index inc))
  (at-end? [this]
    (-> this peek-token :type (= :eof)))
  (previous [this]
    (nth (:tokens this) (dec (:index this))))
  (peek-token [this]
    (nth (:tokens this) (:index this)))
  (consume [this token msg]
    (if (-> this peek-token :type (= token))
      (advance this)
      (do
        (lox/error msg)
        this))))

(defn primary [parser]
  (cond
    (match? parser :true) [(advance parser) #:ast{:type :literal
                                             :value true}]
    (match? parser :false) [(advance parser) #:ast{:type :literal
                                              :value false}]
    (match? parser :nil) [(advance parser) #:ast{:type :literal
                                                 :value nil}]
    (or (match? parser :number)
        (match? parser :string)) [(advance parser) #:ast{:type :literal
                                                         :value (-> parser peek-token :literal)}]
    (match? parser :left-paren) (let [[parser* expr] (expression (advance parser))
                                      parser* (consume parser* :right-paren "Expect ) after expression.")]
                                  [parser* #:ast{:type :grouping
                                                 :value expr}])
    :else (do
            (lox/error "Expect expression.")
            [(advance parser) nil])))

(defn unary [parser]
  (if (or (match? parser :bang)
          (match? parser :minus))
    (let [op (peek-token parser)
          parser* (advance parser)
          [parser* expr] (unary parser*)]
      [parser* #:ast{:type :unary
                     :op (:lexeme op)
                     :rhs expr}])
    (primary parser)))

(defn parse-binary-expr
  "Generic function to handle the parsing of binary expressions.
  * branch-fn - a function to be called on the parser, such as factor,term, expression, etc
  * match-fn - a function that returns a true if the next token matches what is passed in
  * parser - a Parser record"
  [branch-fn match-fn parser]
  (let [[parser* expr] (branch-fn parser)]
    (if (match-fn parser*)
      (loop [expr expr
             parser** parser*]
        (let [op (-> parser** peek-token :lexeme)
              [parser** rhs] (branch-fn (advance parser**))]
          (if (match-fn parser**)
            (recur #:ast{:type :binary
                         :op op
                         :lhs expr
                         :rhs rhs}
                   parser**)
            [parser** #:ast{:type :binary
                            :op op
                            :lhs expr
                            :rhs rhs}])))
      [parser* expr])))

(defn factor [parser]
  (parse-binary-expr unary
                     (fn [p] (or (match? p :slash)
                                 (match? p :star)))
                     parser))


(defn term [parser]
  (parse-binary-expr factor
                     (fn [p] (or (match? p :plus)
                                 (match? p :minus)))
                     parser))


(defn comparison [parser]
  (parse-binary-expr term
                     (fn [p] (or (match? p :greater)
                                 (match? p :greater-equal)
                                 (match? p :less)
                                 (match? p :less-equal)))
                     parser))

(defn equality [parser]
  (parse-binary-expr comparison
                     (fn [p] (or (match? p :equal-equal)
                                 (match? p :bang-equal)))
                     parser))

(defn expression [parser]
  (equality parser))

(defn create-parser [tokens]
  (->Parser tokens 0))

(defn ->ast [parser]
  (if (at-end? parser)
    []
    (loop [[parser* node] (expression parser)
           ast []]
      (if-not (at-end? parser*)
        (recur (expression parser*) (conj ast node))
        (conj ast node)))))

(defmulti fmt :ast/type)

(defmethod fmt :literal [this] (if (nil? (:ast/value this))
                                 "nil"
                                 (str (:ast/value this))))
(defmethod fmt :grouping [this] (format "(group %s)" (fmt (:ast/value this))))

(defmethod fmt :unary [this] (format "(%s %s)" (:ast/op this) (-> this :ast/rhs fmt)))

(defmethod fmt :binary [this] (format "(%s %s %s)" (:ast/op this) (-> this :ast/lhs fmt) (-> this :ast/rhs fmt)))

(defn display-ast [ast]
  (doseq [n ast]
    (println (fmt n))))

(comment
  (def a (scanner/tokenize "(foo"))
  (def p (create-parser a))
  (def ast (->ast p))
  (display-ast ast))
