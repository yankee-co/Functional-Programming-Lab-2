; (defun reverse-and-nest-tail (lst)
;   (defun reverse-helper (lst acc)
;     (cond
;       ((null lst) acc)
;       ((null acc) (reverse-helper (cdr lst) (list (car lst))))
;       (t (reverse-helper (cdr lst) (list (car lst) acc)))))
;   (reverse-helper lst nil))

; (defun compress-list (lst)
;   (defun compress-helper (lst current count)
;     (cond
;       ((null lst) (list (list count current)))
;       ((eq (car lst) current)
;        (compress-helper (cdr lst) current (+ count 1)))
;       (t
;        (cons (list count current)
;              (compress-helper (cdr lst) (car lst) 1)))))
;   (if (null lst)
;       nil
;       (compress-helper (cdr lst) (car lst) 1)))

(defun test-fn (fn args expected)
  (let ((result (apply fn args)))
    (format t "Expected: ~A~%" expected)
    (format t "Result:   ~A~%" result)
    (format t "=> ~A~%" (if (equal result expected) "PASSED" "FAILED"))))

(defun compress-helper (lst current count)
  (cond
    ((null lst)
     (list (list count current)))                 ; кінець списку
    ((equal (car lst) current)
     (compress-helper (cdr lst) current (+ count 1))) ; продовжуємо рахунок
    (t
     (cons (list count current)                   ; додаємо поточну пару
           (compress-helper (cdr lst) (car lst) 1))))) ; нова послідовність

(defun compress-list (lst)
  (if (null lst)
      nil
      (compress-helper (cdr lst) (car lst) 1)))

(defun reverse-and-nest-helper (lst acc)
  (cond
    ((null lst) acc)
    ((null acc)
     (reverse-and-nest-helper (cdr lst) (list (car lst))))
    (t
     (reverse-and-nest-helper (cdr lst) (list (car lst) acc)))))

(defun reverse-and-nest-tail (lst)
  (reverse-and-nest-helper lst nil))
