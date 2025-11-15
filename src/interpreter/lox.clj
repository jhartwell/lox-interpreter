(ns interpreter.lox)

(def has-error? (atom false))


(defn error [msg]
  (reset! has-error? true)
  (binding [*out* *err*]
    (println msg)))

(defn error? [] @has-error?)

(defn exit-code []
  (if @has-error?
    65
    0))
