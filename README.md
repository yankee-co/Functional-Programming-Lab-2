 <p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
 <p align="center">
 <b>Звіт з лабораторної роботи 2</b><br/>
 "Рекурсія"<br/>
 дисципліни "Вступ до функціонального програмування"
 </p>
 <p align="right"><b>Студент(-ка)</b>: Землянський Едуард КВ-22</p>
 <p align="right"><b>Рік</b>: рік</p>
 ## Загальне завдання
 <!-- Зазначається загальне завдання -->
 ## Варіант <номер варіанту>
 <!-- Зазначається завдання за варіантом -->
 ## Лістинг функції reverse-and-nest-tail
 ```lisp
(defun reverse-and-nest-tail (lst)
  (defun reverse-helper (lst acc)
    (cond
      ((null lst) acc)
      ((null acc) (reverse-helper (cdr lst) (list (car lst))))
      (t (reverse-helper (cdr lst) (list (car lst) acc)))))
  (reverse-helper lst nil))
 ```
 ### Тестові набори та утиліти
 ```lisp
 (defun test-fn (fn args expected)
  (let ((result (apply fn args)))
    (format t "Expected: ~A~%" expected)
    (format t "Result:   ~A~%" result)
    (format t "=> ~A~%" (if (equal result expected) "PASSED" "FAILED"))))```
 ### Тестування
```lisp
(test-fn #'reverse-and-nest-tail '((a b c)) '(C (B (A))))
(test-fn #'reverse-and-nest-tail '((1 2 3 4)) '(4 (3 (2 (1)))))
 ```
 ## Лістинг функції compress-list
 ```lisp
(defun compress-list (lst)
  (defun compress-helper (lst current count)
    (cond
      ((null lst) (list (list count current)))
      ((eq (car lst) current)
       (compress-helper (cdr lst) current (+ count 1)))
      (t
       (cons (list count current)
             (compress-helper (cdr lst) (car lst) 1)))))
  (if (null lst)
      nil
      (compress-helper (cdr lst) (car lst) 1)))
 ```
 ### Тестові набори та утиліти
 ```lisp
 ;; використовується та сама test-fn
 ```
 ### Тестування
 ```lisp
(test-fn #'compress-list '((1 a a 3 3 3 b)) '((1 1) (2 A) (3 3) (1 B)))
(test-fn #'compress-list '((x x x y z z)) '((3 X) (1 Y) (2 Z)))
 ```