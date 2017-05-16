;;;; soc.lisp

(in-package #:soc)

;;; specials
(defparameter *tabs* (make-array 0 :fill-pointer 0 :adjustable 't :element-type 'character))

;;; utils

(defun inc-tab ()
  (vector-push-extend #\Tab *tabs*))

(defun dec-tab()
  (vector-pop *tabs*))

(defun indent ()
  (format t *tabs*))

(defun key-string (key)
  (string-downcase (symbol-name key)))

;; branched (not (atom (car tree)))


;;; tags
;; why does macro call work here??

(defmacro element (&body tree)
  "new html element"
  `(progn
     (format t "tag: ~a~%" (key-string (car ,@tree)))
     (walk (cdr ,@tree))))

(defmacro code (&body tree)
  "code string flexibility"
  `(progn
     (format t "code: ~a~%" (car ,@tree))
     (walk (cdr ,@tree))))

(defmacro attrib (&body tree)
  "atttribute: key/value pairs"
  `(progn
     (format t "key: ~a~%value: ~a~%" (car ,@tree) (cadr ,@tree))
     (call-walk (cddr ,@tree))))

(defmacro content (&body tree)
  "content string (inner html, paragraph text)"
  `(progn
     (format t "content: ~a~%" (car ,@tree))
     (call-walk (cdr ,@tree))))

(defmacro attrib-content (&body tree)
  "key/value pair or content string dispatch"
  `(if (keywordp (car ,@tree))
       (attrib ,@tree)
       (content ,@tree)))


;;; traversal

(defmacro branch (&body tree)
  "new element or bare string"
  `(let ((name (car ,@tree)))
     (if (keywordp name) ; TODO check nil
         (element ,@tree)
         (code ,@tree))))

;; remeber to cal on both car and cdr in some cases (branching)
(defmacro walk (&body tree)
  "attrib pairs, content, branch"
  `(cond ((not ,@tree) ; nil
          (format t "~a~%" "close"))
         ((atom (car ,@tree)) ; key/value, content
          (attrib-content ,@tree))
         ('t
          (call-branch (car ,@tree))
          (call-walk (cdr ,@tree)))))

;; functions to expand car/cdr calls within walk
(defun call-branch (tree)
  (branch tree))

(defun call-walk (tree)
  (walk tree))


;;; call this

(defmacro soc (&body tree)
  `(branch ',@tree))
