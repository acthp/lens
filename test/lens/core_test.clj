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

(defn string-join-lens [c]
  (let [pat (re-pattern (str "[" c "]"))]
    (lens
      (fn [arr] (clojure.string/join c arr))
      (fn [arr v] (clojure.string/split v pat)))))

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

(defn spy [msg x]
  (println msg x)
  x)

(defn gen-max [generator max-size]
  (gen/sized
    (fn [size]
      (gen/resize (min size max-size) generator))))

;
; Lens laws.
; 

; set followed by get returns the set value.
; (get (set y x)) = x, for all x, y
(defspec set-get
  *test-runs*
  (let [first-lens (index-lens 0)]
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

(deftest split-lens
  (testing "should join on get"
      (let [csv-lens (string-join-lens \,)]
        (is (= (view csv-lens ["a" "b" "c"]) "a,b,c"))))
  (testing "should split on set"
      (let [csv-lens (string-join-lens \,)]
        (is (= (set csv-lens ["a" "b" "c"] "x,y,z") ["x" "y" "z"])))))
