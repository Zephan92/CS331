(ns bst.core-spec
;  (:refer-clojure :exclude [])
  (:require [speclj.core :refer :all]
            [bst.core :refer :all])
;  (:import [bst.core ])
  )

;; # The Tests
;;
;; We are going to use [spelj](https://github.com/slagyr/speclj) for our tests.


(describe "The spec file"
          (it "should have some tests."
              (should true))

	  (it "should have size 0 when it is empty tree."  
	      (should= 0 (size (make-tree))))

	  (it "should have size 3 when the tree has three elements."  
	      (let [x  (make-tree)] (do
	      	   (add x 1 3)
		   (add x 2 4)
		   (add x 3 5))
	      (should= 3 (size x))))




)
(run-specs)
