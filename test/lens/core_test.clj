(ns lens.core-test
  (:refer-clojure :exclude  [set])
  (:require [clojure.test :refer :all]
            [lens.core :refer :all])
  (:require [clojure.test.check :as tc])
  (:require [clojure.test.check.generators :as gen])
  (:require [clojure.test.check.clojure-test :as tcct :refer [defspec]])
  (:require [clojure.test.check.properties :as prop]))

(def ^:dynamic *verbose* false)

(defmacro is= [a b]
  `(let [a# ~a
         b# ~b
         r# (= a# b#)]
    (when (and (not r#) *verbose*)
      (println "expected (=" ~(str a) ~(str b) ")")
      (print "actual (not (=")
      (clojure.pprint/write a#)
      (print " ")
      (clojure.pprint/write b#)
      (println "))"))
     r#))

(defn index-lens [k]
  (lens
    (fn [x] (get x k))         ; getter
    (fn [x v] (assoc x k v)))) ; setter

;
; lens method unit tests.
;

(deftest lens-methods
  (testing "should get"
    (let [foo-lens (index-lens :foo)]
      (is (= (view foo-lens {:foo 5 :bar 7}) 5))
      (is (= (view foo-lens {:foo ["hello"] :bar 7}) ["hello"]))))

  (testing "should set"
    (let [foo-lens (index-lens :foo)]
      (is (= (set foo-lens
                  {:foo 5 :bar 7}
                  ["hello"])
             {:foo ["hello"] :bar 7}))
      (is (= (set foo-lens
                  {:foo ["hello"] :bar 7}
                  12)
             {:foo 12 :bar 7}))))

  (testing "should compose"
    (let [foo-lens (index-lens :foo)
          third-lens (index-lens 2)
          third-foo-lens (comp third-lens foo-lens)]
      (is (= (view third-foo-lens
                   [{:foo 12} {:foo 2} {:foo "bingo"}])
             "bingo"))
      (is (= (set third-foo-lens
                  [{:foo 12} {:foo 2} {:foo "bingo"}]
                  "hork")
             [{:foo 12} {:foo 2} {:foo "hork"}])))))

(def ^:dynamic *test-runs* 100)

(defn gen-max [generator max-size]
  (gen/sized
    (fn [size]
      (gen/resize (min size max-size) generator))))

;
; Lens laws for a sample lens.
; 

; set followed by get returns the set value.
; (get (set y x)) = x, for all x, y
(defspec set-get
  *test-runs*
  (let [first-lens (index-lens 0)] ; lens over 1st element of a vector
    (prop/for-all
      [big (gen/such-that not-empty (gen/vector (gen-max gen/any 40)))
       small (gen-max gen/any 40)]
      (is= (view first-lens (set first-lens big small)) small))))

; get followed by set is noop
; (set y (get y)) = y, for all y
(defspec get-set
  *test-runs*
  (let [first-lens (index-lens 0)]
    (prop/for-all
      [big (gen/such-that not-empty (gen/vector (gen-max gen/any 40)))]
      (is= (set first-lens big (view first-lens big)) big))))

; a second set overwrites the first.
; (get (set (set x y) z)) = z, for all x, y, z
(defspec set-set
  *test-runs*
  (let [first-lens (index-lens 0)]
    (prop/for-all
      [big (gen/such-that not-empty (gen/vector (gen-max gen/any 40)))
       small1 (gen-max gen/any 40)
       small2 (gen-max gen/any 40)]
      (is= (view first-lens (set first-lens (set first-lens big small1) small2))
           small2))))

;
; example of a lens over an arbitrary transform.
;

(defn string-join-lens [c]
  (let [pat (re-pattern (str "[" c "]"))]
    (lens
      (fn [arr] (clojure.string/join c arr))
      (fn [arr v] (clojure.string/split v pat)))))

(deftest split-lens
  (testing "should join on get"
      (let [csv-lens (string-join-lens \,)]
        (is (= (view csv-lens ["a" "b" "c"]) "a,b,c"))))
  (testing "should split on set"
      (let [csv-lens (string-join-lens \,)]
        (is (= (set csv-lens ["a" "b" "c"] "x,y,z") ["x" "y" "z"])))))

;
; Showing a reusable component w/o knowledge of how or where
; its state is stored in the global state.
;

(def to-int-lens
  (lens (fn [strs] (mapv #(Integer/parseInt %) strs))
        (fn [_ ints-] (mapv (fn [x] (.toString x)) ints-))))

(defn string-split-lens [c]
  (let [pat (re-pattern (str "[" c "]"))]
    (lens
      (fn [arr] (clojure.string/split arr pat))
      (fn [arr v] (clojure.string/join c v)))))

(defn keys-to-vec-lens [keys]
  (lens
    (fn [hmap] ((apply juxt keys) hmap))
    (fn [hmap v] (into hmap (map vector keys v)))))

(defn mean-normalize [vs]
  (let [mean (/ (reduce + vs) (count vs))]
    (mapv #(- % mean) vs)))

; A reusable component which normalizes a vector of numbers in the
; global state. The lens can be bound to the global state in
; various ways. Here we've used a side-effecting lens (hence not really
; a lens) which ignores the "current state" param, to which we pass nil.
(defn mean-normalize-component [lens]
  (over lens nil (fn [vs] (mean-normalize vs))))

(deftest component-reuse
  (testing "normalizes vector in global state"
    (let [state (atom [1 2 3])
          ; Lens to update global state. Not technically a lens, because side-effects.
          state-lens (lens (fn [_] @state) (fn [_ v] (swap! state (fn [_] v))))]
      (mean-normalize-component state-lens)
      (is (= @state [-1 0 1]))))

  (testing "normalizes vector in csv in global state"
    (let [state (atom "1,2,3")
          state-lens (lens (fn [_] @state) (fn [_ v] (swap! state (fn [_] v))))
          csv-lens (string-split-lens \,)]
      (mean-normalize-component (comp state-lens csv-lens to-int-lens))
      (is (= @state "-1,0,1"))))

  (testing "normalizes vector split across global state"
      (let [state (atom {:a 1 :b 2 :c 3})
            state-lens (lens (fn [_] @state) (fn [_ v] (swap! state (fn [_] v))))
            vec-lens (keys-to-vec-lens [:a :b :c])]
        (mean-normalize-component (comp state-lens vec-lens))
        (is (= @state {:a -1 :b 0 :c 1})))))
