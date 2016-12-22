(ns spec-bnf.core-test
  (:require [clojure.spec :as s]
            [clojure.spec.test :as st]
            [clojure.test :refer :all]
            [juxt.iota :refer [given]]
            [spec-bnf.core :as c :refer :all]))

(st/instrument)

(s/def ::int int?)

(s/def ::point (s/cat :x int? :y int?))

(deftest rule-test
  (testing "Rule with one predicate"
    (given (rule ::int)
      ::c/type := :rule
      ::c/lhs := ::int
      [::c/rhs ::c/type] := :identifier))

  (testing "Rule with cat"
    (given (rule ::point)
      ::c/type := :rule
      ::c/lhs := ::point
      [::c/rhs ::c/type] := :concatenation)))

(deftest rhs-test
  (testing "keyword -> identifier"
    (given (rhs :foo 0)
      ::c/type := :identifier
      ::c/id :foo))

  (testing "symbol -> identifier"
    (given (rhs `int? 0)
      ::c/type := :identifier
      ::c/id := `int?))

  (testing "empty set results in error"
    (is (thrown-with-msg? Exception #"Empty set" (rhs #{} 0))))

  (testing "set -> alternation"
    (given (rhs #{"a" "b"} 0)
      ::c/type := :alternation
      [::c/rhss 0 ::c/type] := :terminal
      [::c/rhss 0 ::c/term] := "a"
      [::c/rhss 1 ::c/type] := :terminal
      [::c/rhss 1 ::c/term] := "b"))

  (testing "cat form -> concatenation"
    (given (rhs `(s/cat :x int? :y int?) 0)
      ::c/type := :concatenation
      [::c/rhss 0 ::c/type] := :terminal
      [::c/rhss 0 ::c/term] := "["
      [::c/rhss 1 ::c/type] := :identifier
      [::c/rhss 1 ::c/id] := `int?
      [::c/rhss 2 ::c/type] := :identifier
      [::c/rhss 2 ::c/id] := `int?
      [::c/rhss 3 ::c/type] := :terminal
      [::c/rhss 3 ::c/term] := "]"))

  (testing "only the first item of an and form is used"
    (given (rhs `(s/and int? string?) 0)
      ::c/type := :identifier
      ::c/id := `int?))

  (testing "or form -> alternation"
    (given (rhs `(s/or :kw keyword? :str string?) 0)
      ::c/type := :alternation
      [::c/rhss 0 ::c/type] := :identifier
      [::c/rhss 0 ::c/id] := `keyword?
      [::c/rhss 1 ::c/type] := :identifier
      [::c/rhss 1 ::c/id] := `string?))

  (testing "? form -> optional"
    (given (rhs `(s/? keyword?) 0)
      ::c/type := :optional
      [::c/rhs ::c/type] := :identifier
      [::c/rhs ::c/id] := `keyword?))

  (testing "* form -> repetition"
    (given (rhs `(s/* keyword?) 0)
      ::c/type := :repetition
      [::c/rhs ::c/type] := :identifier
      [::c/rhs ::c/id] := `keyword?))

  (testing "+ form -> concatenation of single rhs and repetition"
    (given (rhs `(s/+ keyword?) 0)
      ::c/type := :concatenation
      [::c/rhss 0 ::c/type] := :identifier
      [::c/rhss 0 ::c/id] := `keyword?
      [::c/rhss 1 ::c/type] := :repetition
      [::c/rhss 1 ::c/rhs ::c/type] := :identifier
      [::c/rhss 1 ::c/rhs ::c/id] := `keyword?))

  (testing "spec form uses the form inside"
    (given (rhs `(s/spec keyword?) 0)
      ::c/type := :identifier
      ::c/id := `keyword?)))

(deftest spec-kws-test
  (is (some #{::int} (spec-kws "spec-bnf.core-test")))
  (is (some #{::point} (spec-kws "spec-bnf.core-test"))))

(comment
  (println (emit-grammar (apply grammar (spec-kws "spec-bnf.core-test"))))
  )
