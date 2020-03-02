;;;; 第 15 章代码，使用读取期条件化的手法，构建了在各种 Common Lisp 实现上的可移植路径库
;;;; 包括列出路径列表，判断路径是否存在，以及文件夹上的一个迭代器

;; 辅助函数
(defun component-present-p (value)
  (and value (not (eql value :unspecific))))
(defun directory-pathname-p (p)
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))
   p))
(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
	(make-pathname
	 :directory (append (or (pathname-directory pathname) (list :relative))
			    (list (file-namestring pathname)))
	 :name nil
	 :type nil
	 :defaults pathname)
	pathname)))

(defun directory-wildcard (dirname)
  (make-pathname 
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))

;;list-directory 函数，列出文件路径下所有文件夹与文件

(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "can only list concreate directory names."))
  (let ((wildcard (directory-wildcard dirname)))
    
   #+(or sbcl cmu lispworks)
   (directory wildcard)

   #+openmcl
   (directory wildcard :directories t)

   #+allegro
   (directory wildcard :directories-are-files nil)

   #+clisp
   (nconc
    (directory wildcard)
    (directory (clisp-subdirectories-wildcard wildcard)))

   #-(or sbcl cmu lispworks openmcl allegro clisp)
   (error "lisp-directory not implemented")))

(defun pathname-as-file (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathname"))
    (if (directory-pathname-p name)
	(let* ((directory (pathname-directory pathname))
	       (name-and-type (pathname (first (last directory)))))
	  (make-pathname
	   :directory (butlast directory)
	   :name (pathname-name name-and-type)
	   :type (pathname-type name-and-type)
	   :defaults pathname))
	pathname)))

;;file-exists-p 函数，判断路径下文件是否存在

(defun file-exists-p (pathname)
  #+(or sbcl lispworks openmcl)
  (probe-file pathname)

  #+(or allegro cmu)
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))

  #+clisp
  (or (ignore-errors
	(probe-file (pathname-as-file pathaname)))
      (ignore-errors
	(let ((directory-form (pathname-as-directory pathname)))
	  (when (ext:probe-directory directory-form)
	    directory-form))))

  #-(or sbcl cmu lispworks openmcl allegro clisp)
  (error "file-exists-p not implemented"))

;;walk-directory 函数，在文件路径下做迭代

(defun walk-directory (dirname fn &kay directories (test (constantly t)))
  (labels
      ((walk (name)
	 (cond
	   ((directory-pathname-p name)
	    (when (and directories (funcall test name))
	      (funcall fn name))
	    (dolist (x (list-directory name)) (walk x)))
	   ((funcall test name) (funcall fn name)))))
    (walk (pathanme-as-directory dirname))))

