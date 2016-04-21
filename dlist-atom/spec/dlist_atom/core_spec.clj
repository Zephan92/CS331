(ns dlist-atom.core-spec
;  (:refer-clojure :exclude [])
  (:require [speclj.core :refer :all]
            [dlist-atom.core :refer :all])
;  (:import [dlist-atom.core ])
  )

;; # The Tests
;;
;; We are going to use [spelj](https://github.com/slagyr/speclj) for our tests.

(describe "insert-front"
	  (it "should insert an element into the front of a Dlist"
	      (let [xx (dlist)]
		       (do (insert-front xx 4)
		       	   (should= '(4) (show-dlist xx))))))

(describe "insert-front"
	  (it "should increase the size of the dlist"
	      (let [xx (dlist)]
		       (do (insert-front xx 4)
		       	   (should= 1 (d-size xx))))))

(describe "insert-front"
	  (it "should increase the size of the dlist"
	      (let [xx (dlist)]
		       (do (insert-front xx 4)
		       	   (insert-front xx 6)
		       	   (should= 2 (d-size xx))))))

(describe "insert-front"
	  (it "should be able to insert multiple times"
	      (let [xx (dlist)]
		       (do (insert-front xx 5)
		       	   (insert-front xx 4)
		       	   (should= '(4 5) (show-dlist xx))))))

(describe "insert-front"
	  (it "should return nil when an empty list is inserted into front"
	      (let [xx (dlist)]
		       (do (insert-front xx 5)
		       	   (should= '(5) (show-dlist xx))))))

(describe "insert-last"
	  (it "should insert an element into the back of a Dlist."
	      (let [xx (dlist)]
		       (do (insert-last xx 4)
		       	   (should= '(4) (show-dlist xx))))))

(describe "insert-last"
	  (it "should be able to insert multiple times"
	      (let [xx (dlist)]
		       (do (insert-last xx 4)
		       	   (insert-last xx 5)
		       	   (should= '(4 5) (show-dlist xx))))))

(describe "insert-last"
	  (it "should insert an element in sorrted order into a nonempty Dlist"
	      (let [xx (dlist)]
		       (do (insert-last xx 5)
		       	   (should= '(5) (show-dlist xx))))))

(describe "insert-last"
	  (it "should increase the size of the dlist"
	      (let [xx (dlist)]
		       (do (insert-last xx 4)
		       	   (should= 1 (d-size xx))))))

(describe "insert-last"
	  (it "should increase the size of the dlist"
	      (let [xx (dlist)]
		       (do (insert-last xx 4)
		       	   (insert-last xx 6)
		       	   (should= 2 (d-size xx))))))


(describe "insert-sorted"
	  (it "should insert an element in sorted order into a nonempty Dlist"
	      (let [xx (dlist)]
		       (do (insert-front xx 6)
			   (insert-front xx 4)
			   (insert-sorted xx 5)
			   (should= '(4 5 6) (show-dlist xx))))))

(describe "insert-sorted"
	  (it "should insert into the front of the list if elt is smaller than the rest of the list"
	      (let [xx (dlist)]
	      	       (do (insert-front xx 6)
		       	   (insert-front xx 4)
		       	   (insert-sorted xx 2)
		       	   (should= '(2 4 6) (show-dlist xx))))))

(describe "insert-sorted"
	  (it "should insert into the back of the list if elt is larger than the rest of the list"
	      (let [xx (dlist)]
		       (do (insert-front xx 6)
		       	   (insert-front xx 4)
		       	   (insert-sorted xx 8)
		       	   (should= '(4 6 8) (show-dlist xx))))))

(describe "insert-sorted"
	  (it "should be able to insert multiple times."
	      (let [xx (dlist)]
		       (do (insert-sorted xx 10)
			   (insert-sorted xx 8)
			   (insert-sorted xx 12)
		       	   (should= '(8 10 12) (show-dlist xx))))))

(describe "insert-sorted"
	  (it "should be able to work with other functions."
	      (let [xx (dlist)]
	      	       (do (insert-front xx 4)
		       	   (insert-last xx 6)
			   (insert-sorted xx 5)
			   (should= '(4 5 6) (show-dlist xx))))))

(describe "insert-sorted"
	  (it "should increase the size of the dlist"
	      (let [xx (dlist)]
		       (do (insert-sorted xx 4)
		       	   (should= 1 (d-size xx))))))

(describe "insert-front"
	  (it "should increase the size of the dlist"
	      (let [xx (dlist)]
		       (do (insert-sorted xx 4)
		       	   (insert-sorted xx 6)
		       	   (should= 2 (d-size xx))))))

(describe "index-forward"
	  (it "should give the correct index."
	      (let [xx (dlist)]
	      	       (do (insert-sorted xx 4)
		       	   (insert-sorted xx 5)
		       	   (insert-sorted xx 6)
		       	   (insert-sorted xx 7)
		       	   (insert-sorted xx 8)
		       	   (insert-sorted xx 9)
			   (insert-sorted xx 10)
			   (should= 3 (index-forward xx 7))))))

(describe "index-forward"
	  (it "should be able to deal with searching an empty list."
	      (let [xx (dlist)]
	      	       (do
			   (should= nil (index-forward xx 7))))))

(describe "index-forward"
	  (it "should return 0 if it is the first element in the list."
	      (let [xx (dlist)]
	      	       (do (insert-front xx 7)
		       	   (insert-front xx 6)
			   (insert-front xx 5)
			   (should= 0 (index-forward xx 5))))))

(describe "index-backward"
	  (it "should give the correct index."
	      (let [xx (dlist)]
	      	       (do (insert-sorted xx 4)
		       	   (insert-sorted xx 5)
		       	   (insert-sorted xx 6)
		       	   (insert-sorted xx 7)
		       	   (insert-sorted xx 8)
		       	   (insert-sorted xx 9)
			   (insert-sorted xx 10)
			   (should= -4 (index-backward xx 7))))))

(describe "index-backward"
	  (it "should be able to deal with searching an empty list."
	      (let [xx (dlist)]
	      	       (do
			   (should= nil (index-backward xx 7))))))

(describe "index-backward"
	  (it "should return -1 if it is the last element in the list."
	      (let [xx (dlist)]
	      	       (do (insert-front xx 7)
		       	   (insert-front xx 6)
		       	   (insert-front xx 5)
			   (should= -1 (index-backward xx 7))))))

(describe "delete"
	  (it "should delete the correct element in an unsorted dlist."
	      (let [xx (dlist)]
	      	   (do  (insert-front xx 4)
		       	(insert-last xx 5)
		       	(insert-front xx 6)
		       	(insert-last xx 7)
		       	(insert-front xx 8)
		       	(insert-last xx 9)
			(insert-front xx 10)
			(delete xx 4)
		   (should= '(10 8 6 5 7 9) (show-dlist xx))))))

(describe "reverse"
	  (it "should return the same list if show-dlist is used."
	      (let [xx (dlist)]
	      	   (do (insert-sorted xx 1)
		       (insert-sorted xx 2)
		       (insert-sorted xx 3)
		       (insert-sorted xx 4)
		       (insert-sorted xx 5)
		       (reverse xx)
		   (should= '(5 4 3 2 1) (show-dlist xx))))))

(run-specs)
