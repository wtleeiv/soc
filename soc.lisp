;;;; soc.lisp

(in-package #:soc)

;;; specials
(defparameter *soc* *standard-output*)
(defparameter *tabs* (make-array 0 :fill-pointer 0 :adjustable 't :element-type 'character))

;;; utils

(defmacro write-out (&rest args)
  `(format *soc* ,@args))

(defun inc-tab ()
  (vector-push-extend #\Tab *tabs*))

(defun dec-tab()
  (vector-pop *tabs*))

(defun indent ()
  (write-out *tabs*))

(defun newline ()
  (write-out "~%"))

(defun key-string (key)
  (string-downcase (symbol-name key)))

(defmacro no-attributes (&body tree)
  "next item is nil or a list"
  `(or (not (cadr ,@tree)) (not (atom (cadr ,@tree)))))

(defmacro no-content (&body tree)
  "item after value is nil or a list"
  `(or (not (atom (caddr ,@tree))) (not (caddr ,@tree))))

;; branched (not (atom (car tree)))


;;; tags
;; why does macro call work here??

;; will need to remember tag and call walk with it to close later
(defmacro element (&body tree)
  "new html element"
  `(let ((tag-name (key-string (car ,@tree))))
     (write-out "<~a" tag-name)
     (when (no-attributes ,@tree) ; close opening tag
       (write-out ">"))
     (walk tag-name (cdr ,@tree))))

(defmacro code (&body tree)
  "code string flexibility"
  `(progn
     (write-out "~a" (car ,@tree))
     (walk nil (cdr ,@tree))))

(defmacro attrib (tag &body tree)
  "atttribute: key/value pairs"
  `(let ((key (key-string (car ,@tree)))
         (value (cadr ,@tree)))
     (when (symbolp value)
       (setf value (string-downcase value)))
     (write-out " ~a=\"~a\"" key value)
     (when (no-content ,@tree) ; TODO wrap
       (write-out ">"))
     (call-walk ,tag (cddr ,@tree))))

(defmacro content (tag &body tree)
  "content string (inner html, paragraph text)"
  `(progn
     (write-out ">~a" (car ,@tree))
     (call-walk ,tag (cdr ,@tree))))

(defmacro attrib-content (tag &body tree)
  "key/value pair or content string dispatch"
  `(if (keywordp (car ,@tree))
       (attrib ,tag ,@tree)
       (content ,tag ,@tree)))

(defun self-closing (tag)
  (let ((return-this nil))
    (dolist (x '(area base br col embed hr img input link meta param source track wbr))
      (when (equal tag (string-downcase x))
        (setf return-this tag)))
    return-this))

(defun close-tag (tag)
  (when (and tag (not (self-closing tag)))
    (newline) ; TODO remove this once attrib/content sets close location
    (indent)
    (write-out "</~a>" tag))
  (when (> (fill-pointer *tabs*) 0)
      (dec-tab)))


;;; traversal

(defmacro branch (&body tree)
  "new element or bare string"
  `(let ((name (car ,@tree)))
     (if (keywordp name) ; TODO check nil
         (element ,@tree)
         (code ,@tree))))

;; remeber to cal on both car and cdr in some cases (branching)
(defmacro walk (tag &body tree)
  "attrib pairs, content, branch"
  `(cond ((not ,@tree) ; nil
          (close-tag ,tag))
         ((atom (car ,@tree)) ; key/value, content
          (attrib-content ,tag ,@tree))
         ('t
          (call-branch (car ,@tree))
          (call-walk ,tag (cdr ,@tree)))))

;; functions to expand car/cdr calls within walk
(defun call-branch (tree)
  (newline)
  (inc-tab)
  (indent)
  (branch tree))

(defun call-walk (tag tree)
  (walk tag tree))


;;; call this

(defmacro soc (&body tree)
  `(branch ',@tree))
