;;;; ���ǵڰ��±�д��ʾ���� do-primes �������� DOTIMES ��ѭ�����죬��������������ѭ��

;;; ���ߺ���
;; ������Ƿ�Ϊ����������������T�����򷵻�NIL
(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))
;;���ش��ڻ���ڸ���ʵ�ε���һ������
(defun next-prime (number)
  (loop for n from number when (primep n) return n))

;;;������⹹
;;�ֹ��⹹�汾
;(defmacro do-primes (var-and-range &rest body)
;  (let ( (var (first var-and-range))
;	 (start (second var-and-range))
;	 (end (third var-and-range)))
;    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
;	 ((> ,var ,end))
;       ,@body)))
;;�Զ��⹹�汾
;(defmacro do-primes ((var start end) &body body)
;  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
;       ((> ,var ,end))
 ;    ,@body))

;;������ Leaky Abstractions ����İ汾
(defmacro do-primes ((var start end) &body body)
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
	  (,ending-value-name ,end))
	 ((> ,var ,ending-value-name))
       ,@body)))
