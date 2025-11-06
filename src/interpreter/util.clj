(ns interpreter.util)

(defn eprintln [msg]
  (binding [*out* *err*]
    (println msg)))
