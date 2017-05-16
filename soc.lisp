;;;; soc.lisp

(in-package #:soc)

;;; specials
(defparameter *tabs* (make-array 0 :fill-pointer 0 :adjustable 't :element-type 'character))

;;; utils
;; branched (not (atom (car tree)))
(defun key-string (key)
  (string-downcase (symbol-name key)))


;; tags

(defmacro element (&body tree)
  `(progn
     (print (key-string (car ,@tree)))
     (walk (cdr ,@tree))))

(defmacro code (&body tree)
  `(progn
     (print "codeses time")
     (walk (cdr ,@tree))))



;;; traversal

(defmacro branch (&body tree)
  "new element or bare string"
  `(let ((name (car ,@tree)))
     (if (keywordp name) ; TODO check nil
         (element ,@tree)
         (code ,@tree))))

(defun call-branch (tree)
  (branch tree))

;; remeber to cal on both car and cdr in some cases (branching)
(defmacro walk (&body tree)
  "attrib pairs, content, branch"
  `(cond ((not ,@tree)
          (print "done"))
         ((atom (car ,@tree))
          (print (car ,@tree))
          (call-walk (cdr ,@tree)))
         ('t
          (call-branch (car ,@tree))
          (call-walk (cdr ,@tree)))))

(defun call-walk (tree)
  (walk tree))

;; `(cond ((not ,@tree)                  ; nil
;;         (lex:echo-line "done"))
;;        ((atom (car ,@tree))           ; key/value, content
;;         (walk (cdr ,@tree)))
;;        ('t
;;         (branch (car ,@tree))
;;         (walk (cdr ,@tree))))

;;; call this

(defmacro soc (&body tree)
  `(branch ',@tree))
