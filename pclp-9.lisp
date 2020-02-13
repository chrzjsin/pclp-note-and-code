;;;;第九章实践，一个单元测试框架，可以组织测试函数，编写测试用例，返回测试结果，追溯测试失败的用例

(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(defun report-result (result form)
  (format t "~:[FAIL~;pass~]...~a: ~a~%" result *test-name* form)
  result)

(defmacro with-gensyms ( (&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))
(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))
