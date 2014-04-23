(ns source-counter.core-test
  (:require [clojure.test :refer :all]
            [source-counter.core :refer :all]))

(deftest test-check-clojure-line
  (is (= :code (check-clojure-line "(defn add [x y] (+ x y))")))
  (is (= :comment (check-clojure-line ";This is a single line comment")))
  (is (= :blank (check-clojure-line "\n"))))

(deftest test-check-c-ish-line
  (is (= :code (check-c-ish-line "System.out.println('Hello, world!');")))
  (is (= :comment (check-c-ish-line "/** This is a ")))
  (is (= :comment (check-c-ish-line "* multiline comment */")))
  (is (= :comment (check-c-ish-line "//This is single line comment")))
  (is (= :blank (check-c-ish-line "\n"))))

(deftest test-check-py-line
  (is (= :code (check-py-line "print 'Hello, world!'")))
  (is (= :comment (check-py-line "#This is a single line comment")))
  (is (= :blank (check-py-line "\n"))))
  
(deftest test-check-html-line
  (is (= :code (check-html-line "<h1>Hello, world!</h1>")))
  (is (= :comment (check-html-line "<!-- This is a single line comment -->")))
  (is (= :blank (check-html-line "\n"))))
