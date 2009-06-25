;;;; -*- mode: lisp -*-
;;;;
;;;; xl-expectations.l
;;;;
;;;; Author: bowbow99 <bowbow99@gmail.com>
;;;; Created: 2009-06-17 10:44:29
;;;; Updated: 2009-06-25 08:42:38
;;;;

#|
(eval-when (:execute :load-toplevel :compile-toplevel)
  (unless (find-package "xl-expectations")
    (defpackage "xl-expectations"
      (:use "lisp" "editor")
      (:nicknames "expect"))))

(in-package "xl-expectations")
;|#


;;;;
;;;; * utilities
;;;;

(defun keyword (name)
  "Returns a keyword symbol of <name>."
  (if (keywordp name) name
    (intern (string name) (find-package "keyword"))))

(defun values-of (object &rest places)
  "Returns eache <places> of <object> as multiple values."
  (apply #'values
         (mapcar (lambda (place)
                   (funcall place object))
           places)))

(defun uncons (object)
  "Return car and cdr of <object> as multiple value if it is cons,
otherwise return <object>."
  (if (consp object) (values-of object #'car #'cdr) object))

(defun find-doc (body)
  "Return documentation and rest as multiple values."
  (if (stringp (car body))
      (uncons body)
    (values nil body)))

(defun condition-name (condition)
  "Returns name of <condition>."
  (si:*structure-definition-name
   (si:*structure-definition condition)))

(defmacro quote! (object)
  "Force quoting symbol."
  ;; NOTE: only for symbol?
  (if (symbolp object) `(quote ,object) object))

(defmacro unquote! (object)
  "Force no quoting."
  (if (and (consp object)
           (eql (car object) 'quote))
      `,(cadr object)
    object))

(defmacro lambda-bind (vars vals &body body)
  "Evaluate <body> with binding <vals> to <vars>.
u can use lambda-list-keywords in vars, and it works just like function call."
  `(apply (lambda ,vars ,@body) ,vals))

(setf (get 'lambda-bind 'lisp-indent-hook) 2)

(defmacro save (item list &key (key #'identity) (test #'eql))
  "Save <item> in <list> with removing if save <item> already contained."
  `(car (setf ,list
              (cons ,item
                    (delete ,item ,list
                            :test ,test :key ,key)))))

(defun slot-accessor (struct slot &optional conc-name)
  "Returns name of slot accessor function."
  (intern (concat (or conc-name (format nil "~S-" struct)) (string slot))
          (symbol-package struct)))

(defun arrow-p (object)
  (and (symbolp object)
       (member (string object) '("=>" "->") :test #'string=)))

(defun get-temp-buffer (name &key (erase t))
  (let ((buffer (get-buffer-create name)))
    (setup-temp-buffer buffer)
    (when erase
      (erase-buffer buffer))
    buffer))


;;;;
;;;; * value predication
;;;;
;;;; value predication is a way to specify a value partially.
;;;; for example:
;;;;    "a number that is greater than 4, but less than 8."
;;;;    "a hash-table that has a list with 'key."
;;;;
(defvar *type/adjectives-table* (make-hash-table))

(defun adjectives-table (type)
  "Return a hash-table that holds adjectives for <type>."
  (or #1=(gethash type *type/adjectives-table*)
      (setf #1# (make-hash-table))))

(defmacro define-adjectives (type/options &rest definitions)
  (let ((table (gensym)))
    (multiple-value-bind (type options)
        (uncons type/options)
      (setf options
            (cons (list* :include :value (cdr (assoc :include options)))
                  (delete :include options :key #'car)))
      `(let ((,table (adjectives-table ,(keyword type))))
         (labels ((save (key value)
                    (setf (gethash key ,table) value)))
           ,@(mapcar (lambda (option)
                       `(save
                         ,(keyword (concat "option-" (string (car option))))
                         ,(case (car option)
                            (:predicate `(lambda ,@(cdr option)))
                            (:include
                             `',(mapcar #'keyword (cdr option)))
                            (t (error "invalid option: ~S" option)))))
               options)
           ,@(mapcar (lambda (def)
                       (lambda-bind (adjective args &rest body) def
                         `(save
                           ,(keyword adjective)
                           ,(if (arrow-p args)
                                `(list ,@(mapcar #'keyword body))
                              `(lambda (,type ,@args) ,@body)))))
               definitions))))))

(setf (get 'define-adjectives 'lisp-indent-hook) 1)

;;; adjectives

(define-adjectives (value
                    (:predicate (value) t))
  (:or (&rest preds)
   `(find-if (lambda (pred) (meet pred ,value)) (list ,@preds))))

(define-adjectives (non-nil
                    (:predicate (non-nil) non-nil)))

(define-adjectives list
  (:contains (&rest items)
   `(every (lambda (item)
             (member item ,list :test #'meet))
      (list ,@items)))
  (:includes -> list contains)
  (:longer-than (n)
   `(> (length ,list) ,n))
  (:shorter-than (n)
   `(> ,n (length ,list)))
  (:length-of (n)
   `(= (length ,list) ,n))
  (:has (item &key at in as)
   (let ((place (or at in as)))
     (if place
         `(meet ,item (funcall ,place ,list))
       `(member ,item ,list :test #'meet)))))

(define-adjectives symbol
  (:keyword ()
   `(keywordp ,symbol))
  (:named (name)
   `(string= (string ,symbol) ,name)))

(define-adjectives number
  (:greater-than (n)
   `(> ,number ,n))
  (:less-than (n)
   `(< ,number ,n)))

(define-adjectives string
  (:longer-than -> list :longer-than)
  (:shorter-than -> list :shorter-than)
  (:length-of -> list :length-of)
  (:match-with (regexp)
   `(string-match ,regexp ,string))
  (:contains (substring)
   `(string-match (regexp-quote ,substring) ,string))
  (:start-with (head)
   `(let ((head ,head))
      (string= (subseq ,string 0 (length head)) head)))
  (:end-with (tail)
   `(let ((tail ,tail)
          (string ,string))
      (string= (subseq string (apply #'- (mapcar #'length
                                           (list string tail))))
               tail))))

(define-adjectives hash-table
  (:has-key (key)
   `(gethash ,key ,hash-table))
  (:has-value (item)
   `(with-hash-table-iterator (iter ,hash-table)
      (loop
        (multiple-value-bind (f key value)
            (iter)
          (cond ((not f) (return f))
                ((meet ,item value) (return value)))))))
  (:has (item &key with in at as)
   (let ((key (or with in at as)))
     `(meet ,item (gethash ,key ,hash-table))))
  (:has-items-at-least (n)
   `(> (hash-table-count ,hash-table) ,n))
  (:has-items-at-most (n)
   `(> ,n (hash-table-count ,hash-table))))

(define-adjectives (alist (:predicate (alist)
                           (and (listp alist)
                                (every #'consp alist)))
                          (:include list))
  (:has-key (key)
   `(find ,key ,alist :test #'meet :key #'car))
  (:has-value (item)
   `(find ,item ,alist :test #'meet :key #'cdr))
  (:has (value &key with in at as)
   (let ((key (or with in at as)))
     `(meet ,value (cdr (assoc ,key ,alist))))))

(define-adjectives (non-nil (:predicate (non-nil) non-nil)))

;;; predict, expand, and validate

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defun get-adjective-expander (type adjective)
    "Returns a function that generate a validation form for <type> and <adjective>."
    (let* ((table (or (adjectives-table (keyword type))
                      (error "unknown type: ~S" type)))
           (found (gethash (keyword adjective) table)))
      (cond ((functionp found) found)
            ((consp found) (apply #'get-adjective-expander found))
            (t (or (let ((parents (gethash :option-include table)))
                     (while (and parents (not found))
                       (setq found (get-adjective-expander (car parents) adjective)
                             parents (cdr parents)))
                     found)
                   (error "maybe wrong data stored as adjective-expander: ~S ~S"
                          type adjective)))))))

(defstruct predicate
  source doc thunk)

(defmacro a (&whole form type &rest adjective)
  "Predict a value."
  (labels ((expand-adjective (adj)
             (apply (get-adjective-expander type (car adj))
                    type (cdr adj))))
    `(make-predicate
      :source ',form
      :doc nil
      :thunk (lambda (,type)
               (and ,(or (let ((pred (gethash :option-predicate
                                              (adjectives-table (keyword type)))))
                           (when pred `(funcall ,pred ,type)))
                         `(typep ,type ',type))
                    ,@(mapcar #'expand-adjective adjective))))))

(defun meet (pred value)
  "Return non-nil if <value> meet or equal to <pred>."
  (if (predicate-p pred)
      (funcall (predicate-thunk pred) value)
    (equal pred value)))


;;;;
;;;; * verification frame
;;;;
;;;; macro do-verify will do:
;;;;    - make a verification object: which holds result
;;;;    - execute verify-form
;;;;       - if it execute another do-verify, then result of
;;;;         sub do-verify will be stored in -details slot of
;;;;         current verification object.
;;;;         and -passed-p will depend on each sub verification
;;;;         result(s).
;;;;       - otherwise, assume return value of verify-form
;;;;         represent its passed or not.
;;;;    - if current do-verify is inside of another do-verify,
;;;;      then store current verification object in parent one.
;;;;    - otherwise, pass it to *vr-handler*
;;;;

(defstruct (verification (:conc-name "vr-"))
  reports details (passed-p t))

(defparameter *current-vr* nil)
(defparameter *parent-vr* nil)

(defparameter *report-destination* nil)

(defun report-verification (ver &key (out t))
  (format (or *report-destination* *standard-output*)
          "~:[~;~:*~A~&~]" (vr-reports ver))
  (dolist (d (reverse (vr-details ver))
             (vr-passed-p ver)) ;returning passed-p
    (report-verification d :out out)))

(defparameter *vr-handler* 'report-verification)

(defmacro do-verify (verify-form &rest report-forms)
  (with-gensyms (passed?)
    `(let ((*parent-vr* *current-vr*)
           (*current-vr* (make-verification)))
       (let ((,passed? ,verify-form))
         ;; NOTE: if verify-form contains another do-verify, it
         ;; modifies *current-vr* (it is *parent-vr* in sub
         ;; do-verify form), and -passed-p is already set.
         ;; otherwise, verify-form must return boolean whether
         ;; its passed or not.
         (unless (vr-details *current-vr*)
           (setf (vr-passed-p *current-vr*) ,passed?))
         ;; get reports if its failed
         (unless (vr-passed-p *current-vr*)
           (setf (vr-reports *current-vr*)
                 (with-output-to-string (*standard-output*)
                   ,@report-forms))))
       (if *parent-vr*
           ;; there is *parent-vr* means, this do-verify form
           ;; is inside another do-verify form.
           ;; in this case, inner do-verify form do:
           ;;    - push current in -details of parent
           ;;    - make sure -passed-p of parent is correct
           (setf (vr-details *parent-vr*)
                 (cons *current-vr* (vr-details *parent-vr*))
                 (vr-passed-p *parent-vr*)
                 (and (vr-passed-p *parent-vr*)
                      (every #'vr-passed-p
                        (vr-details *parent-vr*))))
         ;; no *parent-vr* means its most outer execution of
         ;; do-verify form, so pass current verification to
         ;; *vr-handler*.
         (funcall *vr-handler* *current-vr*)))))

;;;;
;;;; * effects
;;;;
;;;; specify and verify individual effects, like returning values
;;;; or printing messages.
;;;;
;;;; effect object holds a function in its -verify slot. it take
;;;; form of expectations and return a form that will generate a
;;;; function that verify actual result.
;;;;

(defparameter *effect-list* nil)

(defstruct effect
  name capture verify report)

(defmacro define-effect (name args &rest options)
  (multiple-value-bind (capture verify report slot)
      (values-list (mapcar (lambda (key)
                             (cdr (find key options :key #'car)))
                     '(:capture :verify :report :slot)))
    (let ((slot (when slot (car slot))))
      `(save
        (make-effect
         :name ',name
         :capture (,(if (arrow-p (car capture)) 'list 'lambda) ,@capture)
         :verify
         ,(when verify
            (lambda-bind (pass verify-form) verify
              `(lambda ,args
                 `(apply (lambda ,',args
                           (lambda (actual &optional (n 0))
                             (let ((,',slot (,',(slot-accessor 'behavior slot)
                                             actual)))
                               (do-verify ,',verify-form
                                 ,@',report))))
                         ,,pass)))))
        *effect-list*
        :key #'effect-name))))

(setf (get 'define-effect 'lisp-indent-hook) 1
      (get 'capture 'lisp-indent-hook) 1)

(define-effect result nil
  (:capture (form expectations)
   `(cdr (capture result
           (handler-case
               (cons nil (multiple-value-list ,form))
             (condition (c)
               (cons c nil)))))))

(define-effect returns (&rest values)
  (:slot result)
  (:verify `(list ,@values)
   (and (null (car result))
        (apply #'>= (mapcar #'length (list (cdr result) values)))
        (every #'meet values (cdr result))))
  (:report
   (format t "~&Failure #~S:~%" n)
   (apply #'format t "   Expected returns ~S~@{, ~S~}.~%"
          (mapcar (lambda (v)
                    (cond ((predicate-p v) (predicate-source v))
                          (t v)))
            values))
   (if #1=(car result)
       (format t "   Actually signaled ~S.~%" (condition-name #1#))
     (apply #'format t "   Actually returned ~S~@{, ~S~}.~%" (cdr result)))))

(define-effect signals (type &optional message)
  (:slot result)
  (:verify `(list (quote! ,type) ,message)
   (and (car result)
        (typep (car result) type)
        (or (null message)
            (string= (si:*condition-string (car result)) message))))
  (:report
   (format t "~&Failure #~S:~%" n)
   (format t "   Expected signal ~S.~%" type)
   (if #1=(car result)
       (format t "   Actually signaled ~S.~%" (condition-name #1#))
     (apply #'format t "   Actually returned ~S~@{, ~S~}.~%"
            (cdr result)))))


(define-effect outputs (type &optional message)
  (:capture (form expectations)
   (let ((streams '(*standard-output*))) ;TEMP: you need to pick streams up from expectations
     `(let (,@(mapcar (lambda (s)
                        `(,s (make-string-output-stream)))
                streams))
        (multiple-value-prog1 ,form
          (capture outputs
            (list ,@(mapcar (lambda (s)
                              `(cons ',s (get-output-stream-string ,s)))
                      streams))))))))


;;;;
;;;; * expect form --- inspect behavior and verify it
;;;;

(defstruct behavior
  source result outputs)

(defmacro inspect (source &optional expectations)
  ;; TEMP: NOTE: effects to be captured would depend on specified expectations,
  ;; and |result| should be the first one to prevent throwing error.
  ;; NOTE: think about inheriting capture
  (let ((effects '(result outputs))
        (form source)
        (actual (gensym)))
    (while effects
      (setq form (funcall (effect-capture (find (car effects) *effect-list*
                                            :key #'effect-name))
                          form expectations)
            effects (cdr effects)))
    `(let ((,actual (make-behavior :source ',source)))
       (macrolet ((capture (slot value)
                    `(setf (,(slot-accessor 'behavior slot) ,',actual) ,value)))
         ,form ,actual))))

(defmacro expect (form &rest expectations)
  (with-gensyms (actual results)
    (labels ((expand-verify (expectation)
               (apply (effect-verify (find (car expectation) *effect-list*
                                       :key #'effect-name))
                      (cdr expectation))))
      `(let ((,actual (inspect ,form ',expectations))
             (i 0)
             ,results)
         (do-verify (setf ,results
                          (mapcar (lambda (verifier)
                                    (funcall verifier ,actual (incf i)))
                            (list ,@(mapcar #'expand-verify
                                      expectations))))
           (format t "~&~%~S  ~{~:[X~;.~]~}~%" ',form ,results))))))

(setf (get 'expect 'lisp-indent-hook) 1)


;;;;
;;;; * specify --- grouping expectations for later verification
;;;;

(defstruct spec
  name doc thunk)

(defparameter *spec-list* nil)

(defmacro specify (name &body body)
  (multiple-value-bind (doc body)
      (find-doc body)
    `(save (make-spec
            :name ',name
            :doc ,doc
            :thunk (lambda ()
                     (do-verify (progn ,@body)
                       (format t "verifying ~S~%" ',name))))
           *spec-list* :key #'spec-name)))

(setf (get 'specify 'lisp-indent-hook) 1)

(defun verify-specs (specs &key (report t))
  (interactive
      (list (find (completing-read
                   "Spec: "
                   (mapcar (lambda (spec)
                             (string (spec-name spec)))
                     *spec-list*)
                   :must-match t)
              *spec-list* :key (lambda (spec)
                                 (string (spec-name spec)))
              :test #'string=)))
  (let ((*report-destination*
            (cond ((streamp report) report)
                  ((stringp report) (make-buffer-stream
                                     (get-temp-buffer report)))
                  ((eql report t) *standard-output*)))
        (all-passed t))
    (dolist (spec specs all-passed)
      (unless (funcall (spec-thunk spec))
        (setq all-passed nil)))))

;;;;
;;;; * spec-file
;;;;

(defparameter *spec-file-prefix* "./specs/")
(defparameter *spec-file-suffix* ".l")

(defun get-spec-filename (filename  &key
                                    (prefix *spec-file-prefix*)
                                    (suffix *spec-file-suffix*))
  (merge-pathnames
   (concat (directory-namestring filename) prefix
           (pathname-name filename) suffix)))

(defparameter *spec-file-specs-cache* nil
  "((filename utime-last-load specs...) ...)")

(defun load-spec-file (filename)
  (setq filename (merge-pathnames filename))
  (or (let ((buffer (get-file-buffer filename)))
        (when (and buffer (buffer-modified-p buffer))
          (let ((*spec-list* nil))
            (eval-buffer buffer)
            *spec-list*)))
      (let ((found (assoc filename *spec-file-specs-cache*)))
        (when (and found
                   (< (file-write-time filename) (second found)))
          (cddr found)))
      (let ((*spec-list* nil))
        (load-file filename)
        (cddr (save (list* filename (get-universal-time) *spec-list*)
                    *spec-file-specs-cache*)))))

(defun spec-file-p (filename)
  (find filename (mapcar #'get-spec-filename
                   (remove-if #'null
                     (mapcar #'get-buffer-file-name
                       (buffer-list))))
    :test #'path-equal))

(defparameter *spec-file-not-found* :ignore
  ":create    find-file
   :create+   find-file-other-window
   :ask       ask for create it
   :ask+      ask for create in other-window
   :ignore    do nothing
   :error     throw error")

(defun verify-current-buffer (&optional (buffer (selected-buffer)))
  (interactive)
  (when (and (member buffer-mode '(lisp-mode lisp-interaction-mode)))
    ;(eval-buffer buffer)
    (multiple-value-bind (src-file spec-file)
        (let ((name (get-buffer-file-name buffer)))
          (if (spec-file-p name)
              (values (find name (mapcar #'get-buffer-file-name
                                   (buffer-list))
                        :key (lambda (name)
                               (when name (get-spec-filename name)))
                        :test #'equal)
                      name)
            (values name (get-spec-filename name))))
      (if (file-exist-p spec-file)
          (verify-specs (load-spec-file spec-file)
                        :report (format nil "*VR: ~A*"
                                        (buffer-name
                                         (get-file-buffer src-file))))
        (case *spec-file-not-found*
          (:create (find-file spec-file))
          (:create+ (find-file-other-window spec-file))
          (:ask  (when (y-or-n-p "Create spec file? ~A" spec-file)
                   (find-file spec-file)))
          (:ask+ (when (y-or-n-p "Create spec file? ~A" spec-file)
                   (find-file-other-window spec-file)))
          (:error (error "spec file not found: ~A" spec-file))
          (t nil))))))


;;;; xl-expectations.l ends here