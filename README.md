 <p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
 <p align="center">
 <b>Звіт з лабораторної роботи 2</b><br/>
 "Рекурсія"<br/>
 дисципліни "Вступ до функціонального програмування"
 </p>
 <p align="right"><b>Студент(-ка)</b>: Землянський Едуард КВ-22</p>
 <p align="right"><b>Рік</b>: 2025</p>

## Варіант 8

<img width="798" height="404" alt="image" src="https://github.com/user-attachments/assets/7049478b-04e3-4198-9583-1a7a3bb32387" />

 ## Лістинг функції reverse-and-nest-tail
 ```lisp
(defun reverse-and-nest-helper (lst acc)
  (cond
    ((null lst) acc)
    ((null acc)
     (reverse-and-nest-helper (cdr lst) (list (car lst))))
    (t
     (reverse-and-nest-helper (cdr lst) (list (car lst) acc)))))

(defun reverse-and-nest-tail (lst)
  (reverse-and-nest-helper lst nil))
 ```
 
 ## Лістинг функції compress-list
 ```lisp
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
 ```

### Тестові набори та утиліти
 ```lisp
(defun test-fn (fn args expected)
  (let ((result (apply fn args)))
    (format t "Expected: ~A~%" expected)
    (format t "Result:   ~A~%" result)
    (format t "=> ~A~%" (if (equal result expected) "PASSED" "FAILED"))))
 ```

 ### Тестування
 ```lisp
CL-USER > (test-fn #'reverse-and-nest-tail '((a b c)) '(C (B (A))))
Expected: (C (B (A)))
Result:   (C (B (A)))
=> PASSED

CL-USER > (test-fn #'reverse-and-nest-tail '((1 2 3 4)) '(4 (3 (2 (1)))))
Expected: (4 (3 (2 (1))))
Result:   (4 (3 (2 (1))))
=> PASSED

CL-USER > (test-fn #'compress-list '((1 a a 3 3 3 b)) '((1 1) (2 A) (3 3) (1 B)))
Expected: ((1 1) (2 A) (3 3) (1 B))
Result:   ((1 1) (2 A) (3 3) (1 B))
=> PASSED

CL-USER > (test-fn #'compress-list '((x x x y z z)) '((3 X) (1 Y) (2 Z)))
Expected: ((3 X) (1 Y) (2 Z))
Result:   ((3 X) (1 Y) (2 Z))
=> PASSED
 ```



