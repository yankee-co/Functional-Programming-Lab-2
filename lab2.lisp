;;; Variant 8
;;; Task 1: reverse-and-nest-tail
;;; Reverses input list and creates nested structure

(defun reverse-and-nest-tail (lst)
  "Reverses list and creates nested structure from its elements.
   Uses tail recursion with accumulator."
  (reverse-and-nest-helper lst nil))

(defun reverse-and-nest-helper (lst acc)
  "Helper function with tail recursion.
   lst - remainder of input list
   acc - accumulator for building result"
  (cond 
    ((null lst) acc)  ; base case - list is empty
    ((null (cdr lst)) ; if one element remains
     (cons (car lst) acc))
    (t ; recursive case
     (reverse-and-nest-helper 
      (cdr lst) 
      (cons (car lst) (cons acc nil))))))

;;; Alternative implementation without tail recursion
(defun reverse-and-nest-simple (lst)
  "Reverses list and creates nested structure.
   Uses simple recursion."
  (cond
    ((null lst) nil)
    ((null (cdr lst)) (car lst))
    (t (cons (car lst) 
             (cons (reverse-and-nest-simple (cdr lst)) nil)))))

;;; Task 2: compress-list

(defun compress-list (lst)
  "Compresses sequences of identical elements to format (element count).
   Uses simple recursion."
  (cond
    ((null lst) nil)  ; base case - empty list
    (t ; recursive case
     (compress-with-count lst))))

(defun compress-with-count (lst)
  "Helper function for counting identical elements."
  (cond
    ((null lst) nil)
    (t (let ((count-result (count-consecutive (car lst) lst 0)))
         (cons (list (car lst) (car count-result))
               (compress-with-count (cdr count-result)))))))

(defun count-consecutive (elem lst count)
  "Counts consecutive occurrences of element.
   Returns (count . remainder-list)."
  (cond
    ((null lst) (cons count nil))
    ((equal elem (car lst))
     (count-consecutive elem (cdr lst) (+ count 1)))
    (t (cons count lst))))

;;; Alternative implementation with tail recursion
(defun compress-list-tail (lst)
  "Compresses list using tail recursion."
  (compress-tail-helper lst nil))

(defun compress-tail-helper (lst acc)
  "Helper function with tail recursion and accumulator."
  (cond
    ((null lst) (reverse-list acc))
    (t (let ((count-result (count-consecutive (car lst) lst 0)))
         (compress-tail-helper 
          (cdr count-result)
          (cons (list (car lst) (car count-result)) acc))))))

(defun reverse-list (lst)
  "Reverses list using tail recursion."
  (reverse-helper lst nil))

(defun reverse-helper (lst acc)
  "Helper function for reversing list."
  (cond
    ((null lst) acc)
    (t (reverse-helper (cdr lst) (cons (car lst) acc)))))

;;; TESTING

(defun run-tests ()
  "Runs all tests for both functions."
  (format t "~%===== TESTING reverse-and-nest-tail =====~%")
  (test-reverse-and-nest)
  (format t "~%===== TESTING compress-list =====~%")
  (test-compress))

(defun test-reverse-and-nest ()
  "Tests reverse-and-nest-tail function."
  
  ;; Test 1: Basic example from task
  (format t "~%Test 1: '(a b c)~%")
  (format t "Expected: (C (B (A)))~%")
  (format t "Result:   ~A~%" (reverse-and-nest-tail '(a b c)))
  
  ;; Test 2: Empty list
  (format t "~%Test 2: '()~%")
  (format t "Expected: NIL~%")
  (format t "Result:   ~A~%" (reverse-and-nest-tail '()))
  
  ;; Test 3: One element
  (format t "~%Test 3: '(x)~%")
  (format t "Expected: X~%")
  (format t "Result:   ~A~%" (reverse-and-nest-tail '(x)))
  
  ;; Test 4: Two elements
  (format t "~%Test 4: '(1 2)~%")
  (format t "Expected: (2 (1))~%")
  (format t "Result:   ~A~%" (reverse-and-nest-tail '(1 2)))
  
  ;; Test 5: Longer list
  (format t "~%Test 5: '(1 2 3 4 5)~%")
  (format t "Expected: (5 (4 (3 (2 (1)))))~%")
  (format t "Result:   ~A~%" (reverse-and-nest-tail '(1 2 3 4 5)))
  
  ;; Comparison with alternative implementation
  (format t "~%Alternative implementation for '(a b c): ~A~%" 
          (reverse-and-nest-simple '(a b c))))

(defun test-compress ()
  "Tests compress-list function."
  
  ;; Test 1: Basic example
  (format t "~%Test 1: '(a a a b b c c c c d)~%")
  (format t "Expected: ((A 3) (B 2) (C 4) (D 1))~%")
  (format t "Result:   ~A~%" (compress-list '(a a a b b c c c c d)))
  
  ;; Test 2: Empty list
  (format t "~%Test 2: '()~%")
  (format t "Expected: NIL~%")
  (format t "Result:   ~A~%" (compress-list '()))
  
  ;; Test 3: All elements different
  (format t "~%Test 3: '(a b c d)~%")
  (format t "Expected: ((A 1) (B 1) (C 1) (D 1))~%")
  (format t "Result:   ~A~%" (compress-list '(a b c d)))
  
  ;; Test 4: All elements same
  (format t "~%Test 4: '(x x x x x)~%")
  (format t "Expected: ((X 5))~%")
  (format t "Result:   ~A~%" (compress-list '(x x x x x)))
  
  ;; Test 5: Numbers
  (format t "~%Test 5: '(1 1 2 2 2 3 1 1)~%")
  (format t "Expected: ((1 2) (2 3) (3 1) (1 2))~%")
  (format t "Result:   ~A~%" (compress-list '(1 1 2 2 2 3 1 1)))
  
  ;; Test 6: Single element
  (format t "~%Test 6: '(a)~%")
  (format t "Expected: ((A 1))~%")
  (format t "Result:   ~A~%" (compress-list '(a)))
  
  ;; Comparison with tail recursion
  (format t "~%Tail recursion for '(a a b b b c): ~A~%" 
          (compress-list-tail '(a a b b b c))))

;;; Run tests
(run-tests)