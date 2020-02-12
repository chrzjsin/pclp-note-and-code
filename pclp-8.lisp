;;;; 这是第八章编写的示例宏 do-primes ，类似于 DOTIMES 的循环构造，但是是在素数上循环

;;; 工具函数
;; 检测数是否为素数，是素数返回T，否则返回NIL
(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))
;;返回大于或等于给定实参的下一个素数
(defun next-prime (number)
  (loop for n from number when (primep n) return n))

;;;宏参数解构
;;手工解构版本
;(defmacro do-primes (var-and-range &rest body)
;  (let ( (var (first var-and-range))
;	 (start (second var-and-range))
;	 (end (third var-and-range)))
;    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
;	 ((> ,var ,end))
;       ,@body)))
;;自动解构版本
;(defmacro do-primes ((var start end) &body body)
;  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
;       ((> ,var ,end))
 ;    ,@body))

;;处理了 Leaky Abstractions 问题的版本
(defmacro do-primes ((var start end) &body body)
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
	  (,ending-value-name ,end))
	 ((> ,var ,ending-value-name))
       ,@body)))
