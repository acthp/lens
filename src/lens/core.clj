(ns lens.core
  (:refer-clojure :exclude  [set]))

(defn lens [getter setter]
  (fn [inj]
    (fn [a]
      ((:fmap (inj (getter a))) (fn [b] (setter a b))))))

(defn constant [c]
  {:fmap (fn [_] (constant c))
   :value c})

(defn id [c]
  {:fmap (fn [f] (id (f c)))
   :value c})

(defn view [lens x]
  (:value ((lens constant) x)))

(defn set [lens x v]
  (:value ((lens (fn [_] (id v))) x)))

(defn over [lens x f]
  (:value ((lens (fn [y] (id (f y)))) x)))
