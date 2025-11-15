(ns interpreter.scanner
  (:require [clojure.java.io :as io]
            [clojure.string :as cstr]
            [clojure.walk :as w]
            [interpreter.lox :as lox]
            [interpreter.protocols :as p]
            [interpreter.util :as util])
  (:import java.lang.Character))

(def state (atom {:line 1
                  :tokens []}))

(defrecord Token [type lexeme literal]
  Object
  (toString [_] (format "%s %s %s" (-> type name (cstr/replace #"-" "_") cstr/upper-case) (p/fmt lexeme) (if (nil? literal) "null" literal))))

(defn handle-string [contents line]
  (let [#_ #_[s line] (reduce (fn [[s l] c]
                    [(str s c) (if (= c \newline) (inc l) l)])
                  ["" line]
                  (take-while #(not= % \") contents))
#_ #_        remainder (->> contents
                            (drop-while #(not= % \")))
        [s remainder] (split-with #(not= % \") contents)
        string-value (cstr/join s)
        terminated? (= (first remainder) \")]
    {:token (->Token :string (format "\"%s\"" string-value)  string-value)
     :remainder (rest remainder)
     :line line
     :terminated? terminated?}))


(defn handle-comment
  "For a comment we want to ignore everything until the next newline. Using drop-while will
  get us what we need, however, the newline will still be in the sequence so we want to drop
  1 more item in order to ensure that we don't keep the newline"
  [contents]
  (drop 1 (drop-while #(not= % \newline) contents)))

(defn handle-number [ch contents]
  (let [[lhs remainder] (split-with Character/isDigit contents)
        rhs? (= \. (first remainder))
        [rhs remainder] (if rhs?
                          (split-with Character/isDigit (rest remainder))
                          ["" remainder])
        number (cond-> (str ch (cstr/join lhs))
                 rhs? (str "." (cstr/join rhs)))]
    {:remainder remainder
     :number number}))

(def reserved-words #{"and"
                      "class"
                      "else"
                      "false"
                      "for"
                      "fun"
                      "if"
                      "nil"
                      "or"
                      "print"
                      "return"
                      "super"
                      "this"
                      "true"
                      "var"
                      "while"})

(defn handle-identifier [ch contents]
  (let [is-letter? (fn [c] (and (not= c \space)
                                (or (Character/isLetter c)
                                    (Character/isDigit c)
                                    (= c \_))))
        [ident remainder] (split-with is-letter? contents)]
    {:identifier (str ch (cstr/join ident))
     :remainder remainder}))

(defn create-identifier-token [ident]
  (if-let [reserved (get reserved-words ident)]
    (->Token (keyword reserved) reserved nil)
    (->Token :identifier ident nil)))

(defn tokenize [contents]
  (loop [[ch & rst] contents
         tokens []
         line 1]
    (let [nxt (first rst)]
      (cond
        (and (= ch \=)
             (= nxt \=)) (recur (rest rst) (conj tokens (->Token :equal-equal "==" nil)) line)
        (and (= ch \!)
             (= nxt \=)) (recur (rest rst) (conj tokens (->Token :bang-equal "!=" nil)) line)
        (and (= ch \<)
             (= nxt \=)) (recur (rest rst) (conj tokens (->Token :less-equal "<=" nil)) line)
        (and (= ch \>)
             (= nxt \=)) (recur (rest rst) (conj tokens (->Token :greater-equal ">=" nil)) line)
        (and (= ch \/)
             (= nxt \/)) (let [remainder (handle-comment rst)]
                           (recur remainder tokens (inc line)))
        (= ch \newline) (recur rst tokens (inc line))
        (or (= ch \space)
            (= ch \tab)) (recur rst tokens line)
        (= ch \=) (recur rst (conj tokens (->Token :equal "=" nil)) line)
        (= ch \() (recur rst (conj tokens (->Token :left-paren (str ch) nil)) line)
        (= ch \)) (recur rst (conj tokens (->Token :right-paren (str ch) nil)) line)
        (= ch \{) (recur rst (conj tokens (->Token  :left-brace (str ch) nil)) line)
        (= ch \}) (recur rst (conj tokens (->Token  :right-brace (str ch) nil)) line)
        (= ch \*) (recur rst (conj tokens (->Token  :star (str ch) nil)) line)
        (= ch \.) (recur rst (conj tokens (->Token :dot (str ch) nil)) line)
        (= ch \,) (recur rst (conj tokens (->Token :comma (str ch) nil)) line)
        (= ch \+) (recur rst (conj tokens (->Token  :plus (str ch) nil)) line)
        (= ch \-) (recur rst (conj tokens (->Token :minus (str ch) nil)) line)
        (= ch \/) (recur rst (conj tokens (->Token :slash (str ch) nil)) line)
        (= ch \;) (recur rst (conj tokens (->Token :semicolon (str ch) nil)) line)
        (= ch \!) (recur rst (conj tokens (->Token :bang (str ch) nil)) line)
        (= ch \<) (recur rst (conj tokens (->Token :less (str ch) nil)) line)
        (= ch \>) (recur rst (conj tokens (->Token :greater (str ch) nil)) line)
        (= ch \") (let [{:keys [token remainder line terminated?]} (handle-string rst line)]
                    (if terminated?
                      (recur remainder (conj tokens token) line)
                      (do
                        (lox/error (format "[line %s] Error: Unterminated string." line))
                        (recur remainder tokens line))))
        (nil? ch) (conj tokens (->Token :eof "" nil))
        :else (cond
                (Character/isDigit ch) (let [{:keys [number remainder]} (handle-number ch rst)]
                                         (recur remainder (conj tokens (->Token :number number (Double/parseDouble number))) line))
                (or (Character/isLetter ch)
                    (= ch \_)) (let [{:keys [identifier remainder]} (handle-identifier ch rst)]
                                 (recur remainder (conj tokens (create-identifier-token identifier)) line))
                :else (do (lox/error (format "[line %s] Error: Unexpected character: %s" line ch)) (recur rst tokens line)))))))
