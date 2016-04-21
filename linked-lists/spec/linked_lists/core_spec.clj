(ns linked_lists.core-spec
  (:require [speclj.core :refer :all]
            [linked_lists.core :refer :all])
  (:import [linked_lists.core Cons]))

;; # The Tests
;;
;; We are going to use [spelj](https://github.com/slagyr/speclj) for our tests.


(describe "The record declaration"
          (it "should create something"
              (should (Cons. 10 20)))

          (it "should have a car"
              (should= 10 (:car (Cons. 10 20))))

          (it "should have a cdr"
              (should= 20 (:cdr (Cons. 10 20))))

          (it "should be chainable"
              (should= 40 (-> (Cons. 10 (Cons. 20 (Cons. 30 40))) :cdr :cdr :cdr))))

(describe "insert-at-beginning"
          (it "creates a cons cell"
              (should-not= nil (insert-at-beginning 10 nil)))

          (it "should work with empty lists"
              (should= (Cons. 10 nil) (insert-at-beginning 10 nil) ))
          
          (it "should work with lists that have data"
              (let [xx (Cons. 10 (Cons. 20 (Cons. 30 nil)))]
                (should= (Cons. 5 xx) (insert-at-beginning 5 xx) )))
)

(describe "insert-at-end"
          (it "creates a cons cell"
              (should-not= nil (insert-at-end 10 nil) ))

	  (it "shold work with empty lists"
	      (should= (Cons. 10 nil) (insert-at-end 10 nil) ))
	  
	  (it "should work with lists that have data"
	      (let [xx (Cons. 10 (Cons. 20 (Cons. 30 nil)))]
	      	(should= (Cons. 10 (Cons. 20 (Cons. 30 (Cons. 5 nil)))) (insert-at-end 5 xx) )))
)

(describe "sorted-insert"
          (it "creates a cons cell"
              (should-not= nil (sorted-insert 10 nil) ))

	  (it "should work with empty lists"
	      (should= (Cons. 10 nil) (sorted-insert 10 nil) ))

	  (it "should work with lists that have data"
	      (let [xx (Cons. 10 (Cons. 20 (Cons. 30 nil)))]
	        (should= (Cons. 5 xx) (sorted-insert 5 xx) )))

	  (it "should insert into the middle of the list"
	      (let [xx (Cons. 10 (Cons. 20 (Cons. 40 nil)))] 
	      	(should= (Cons. 10 (Cons. 20 (Cons. 30 (Cons. 40 nil))))(sorted-insert 30 xx) )))
)

(describe "search"
          (it "should return nil if xx is empty."
              (should= nil (search 5 nil)))

	  (it "should return nil if elt is not found."
	      (let [xx (Cons. 10 (Cons. 20 (Cons. 30 nil)))]
              (should= nil (search 5 xx))))
	      
	  (it "should return elt if elt is found."
	      (let [xx (Cons. 10 (Cons. 20 (Cons. 30 nil)))]
              (should= 20 (search 20 xx))))
)

(describe "delete"
          (it "should return nil if xx is empty."
              (should= nil (delete 5 nil)))

	  (it "should return the list if elt is not found."
	      (let [xx (Cons. 10 (Cons. 20 (Cons. 30 nil)))]
              (should= (Cons. 10 (Cons. 20 (Cons. 30 nil))) (delete 5 xx))))
	      
	  (it "should return the modified list if elt is found."
	      (let [xx (Cons. 10 (Cons. 20 (Cons. 30 nil)))]
              (should= (Cons. 10 (Cons. 30 nil)) (delete 20 xx))))
)

(describe "delete-all"
          (it "should return nil if xx is empty."
              (should= nil (delete-all 5 nil)))

	  (it "should return the list if elt is not found."
	      (let [xx (Cons. 10 (Cons. 20 (Cons. 20 (Cons. 30 nil))))]
              (should= (Cons. 10 (Cons. 20 (Cons. 20 (Cons. 30 nil)))) (delete-all 5 xx))))

	  (it "should return the modified list if elt found."
	      (let [xx (Cons. 10 (Cons. 20 (Cons. 20 (Cons. 30 nil))))]
              (should= (Cons. 10 (Cons. 30 nil)) (delete-all 20 xx))))
)

(describe "efficient-delete"
          (it "should return nil if xx is empty."
              (should= nil (efficient-delete 5 nil)))

	  (it "should return the same list if elt is not found."
	      (let [xx (Cons. 10 (Cons. 20 (Cons. 30 nil)))]
              (should= xx (efficient-delete 5 xx))))
	      
	  (it "should return the modified list if elt is found."
	      (let [xx (Cons. 10 (Cons. 20 (Cons. 30 nil)))]
              (should= (Cons. 10 (Cons. 30 nil)) (efficient-delete 20 xx))))
)
(run-specs)
