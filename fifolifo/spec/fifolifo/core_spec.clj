(ns fifolifo.core-spec
  (:refer-clojure :exclude [pop peek])
  (:require [speclj.core :refer :all]
            [fifolifo.core :refer :all])
  (:import [fifolifo.core Stack Queue]))

;; # The Tests
;;
;; We are going to use [spelj](https://github.com/slagyr/speclj) for our tests.


(describe "The stack declaration"

          (it "should create something."
              (should (make-stack)))

          (it "should have empty components."
              (should= (Stack. nil 0) (make-stack)))
          
          (it "should have a size of zero."  (should= 0 (stack-size
              (make-stack))))

          (it "should have a size of one when there is 1 element."
              (should= 1 (stack-size (push (make-stack) 7)) ))

          (it "should have a size of three when there is 3 elements."
              (should= 3 (stack-size (push (push (push (make-stack) 7)
              8) 9))))

          (it "should have a FILO order."
	      (let [xx (push (push (push (make-stack) 7) 8) 9)]
              (should= '(9 8 7) (:top xx))))

          (it "should push 1 element to the top of the list."
	      (let [xx (push (make-stack) 9)]
              (should= (:top xx) '(9))))

          (it "should return size of 1 when an element is pushed to the top of the list."
	      (let [xx (push (make-stack) 9)]
              (should= (stack-size xx) 1)))

          (it "should return size of 3 when three elements are  pushed to the top of the list."
	      (let [xx (push (push (push (make-stack) 9)8)7)]
              (should= (stack-size xx) 3)))

          (it "should pop the first element on the list."
	      (let [xx (push (push (push (make-stack) 7) 8) 9)]
              (should= '(8 7) (:top (pop  xx)))))

          (it "should pop two elements on the list."
	      (let [xx (push (push (push (make-stack) 7) 8) 9)]
              (should= '(7) (:top (pop (pop xx))))))

          (it "should return an empty list if all elements in the list are popped."
	      (let [xx (push (push (push (make-stack) 7) 8) 9)]
              (should= () (:top (pop(pop(pop  xx)))))))

          (it "should return size 0 if 1 element is popped from a list with 1."
	      (let [xx (push (make-stack) 9)]
              (should= (stack-size (pop xx)) 0)))

          (it "should return size 0 if all elements are popped from a list."
	      (let [xx (push(push(push (make-stack) 9)8)7)]
              (should= (stack-size (pop(pop(pop xx)))) 0)))

          (it "should return size 2 if 1 element is popped from a list with 3."
	      (let [xx (push(push(push(make-stack) 9)8)7)]
              (should= (stack-size (pop xx)) 2)))
	  
	  (it "should return the top of the list of 3 elements."
	      (let [xx (push(push(push(make-stack)9)8)7)]
	      (should= 7 (top xx))))
	  
	  (it "should return the top of the list of 1 elements."
	      (let [xx (push(make-stack)9)]
	      (should= 9 (top xx))))
	  
	  (it "should return stack if it is an empty list of elements."
	      (let [xx (make-stack)]
	      (should= nil (top xx))))

	  (it "should be able to pop right after pushing."
	      (let [xx (push(push(make-stack)9)8)]
	      (should= 9 (top (pop xx))))) 

	  (it "should be able to push right after popping."
	      (let [xx (push(push(make-stack)9)8)]
	      (should= 8 (top (push (pop xx)8)))))

	  (it "should not be negative if an empty stack is popped."
	      (should-not= -1 (stack-size (pop (make-stack)))))

          )


(describe "The queue declaration"

          (it "should create something."
              (should (make-queue)))

          (it "should have empty components."
              (should= (Queue. nil nil 0) (make-queue)))
          
          (it "should have a size of zero."
              (should= 0 (queue-size (make-queue))))

          (it "should have a size of one when there is 1 element."
              (should= 1 (queue-size (enqueue (make-queue) 7)) ))

          (it "should have a size of three when there is 3 elements."
              (should= 3 (queue-size (enqueue (enqueue (enqueue (make-queue) 7) 8) 9))))

          (it "should have a FIFO order."
	      (let [xx (enqueue (enqueue (enqueue (make-queue) 7) 8) 9)]
              (should= 7 (peek xx))))

	  (it "should the back of the queue should be back to front."
	      (let [xx (enqueue (enqueue (enqueue (make-queue) 7) 8) 9)]
	      (should= '(9 8 7) (:back xx))))

	  (it "should not be negative if an empty list is dequeued."
	      (should-not= -1 (dequeue(make-queue))))

	  (it "should not have elements in the back after dequeuing."
	      (let [xx (enqueue (enqueue (enqueue (make-queue) 7) 8) 9)]
	      (should= nil (:back (dequeue xx)))))

	  (it "should dequeue the front of the list by one."
	      (let [xx (enqueue (enqueue (enqueue (make-queue) 7) 8) 9)]
	      (should= '(8 9)(:front (dequeue xx)))))
	      
	  (it "should be able to dequeue after enqueuing."
	      (let [xx (enqueue (enqueue (enqueue (make-queue) 7) 8) 9)]
	      (should= 8 (peek (dequeue xx)))))

	  (it "should be able to enqueue after dequeuing."
	      (let [xx (enqueue (enqueue (enqueue (make-queue) 7) 8) 9)]
	      (should= '(10) (:back (enqueue (dequeue xx)10)))))

	  (it "should be able to keep track of their size."
	      (let [xx (make-queue)]
	      (should= 3 (queue-size (enqueue (dequeue (enqueue (enqueue (enqueue (make-queue) 7) 8) 9))10)))))

	  (it "should not return a negative number after dequeing from an empty queue."
	      (should-not= -1 (queue-size (dequeue (make-queue)))))
          )

(run-specs)
