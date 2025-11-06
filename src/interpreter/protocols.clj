(ns interpreter.protocols
  (:import java.text.DecimalFormat))

(defprotocol PrettyPrint
  (fmt [this]))

(extend java.lang.Double
  PrettyPrint
  {:fmt (fn [this] (let [fmt (DecimalFormat. "0.0###########")]
                     (.format fmt this)))})

(extend java.lang.String
  PrettyPrint
  {:fmt (fn [this] this)})

