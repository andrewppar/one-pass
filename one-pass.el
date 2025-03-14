;;; one-pass.el --- 1password integration for emacs

;; Copyright (C) 2025 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created: 13 March 2025
;; Keywords: convenience, lisp, tools
;; Version: 0.1

;; Commentary:

;; This package provides a simple interface to 1password from Emacs.
;; It allows you to retrieve and manage your 1password items directly
;; from within Emacs.

;; Code:

(defconst one-pass--executable
  (executable-find "op")
  "Path to the 1password CLI executable.")

(defun one-pass--run (object command &rest args)
  "Run a 1password COMMAND on OBJECT with ARGS and return the output."
  (if one-pass--executable
      (save-window-excursion
	(let ((buffer (generate-new-buffer "*one-pass*"))
	      (arguments (append args (list "--format" "json")))
	      (exec one-pass--executable)
	      (result nil))
	  (switch-to-buffer buffer)
	  (apply #'call-process exec nil buffer nil object command arguments)
	  (setq result
		(json-parse-string
		 (buffer-substring-no-properties (point-min) (point-max))
		 :object-type 'plist :array-type 'list))
	  (kill-buffer buffer)
	  result))
    (warn "1password CLI not found. Please install it to use one-pass.el.")))

(defun one-pass/get-item (item-id)
  "Get the 1password item with ITEM-ID."
  (seq-reduce
   (lambda (result field)
     (let ((key (intern (format ":%s" (plist-get field :label)))))
       (plist-put result key (plist-get field :value))))
   (plist-get (one-pass--run "item" "get" item-id) :fields)
   '()))

(defun one-pass--list-items (categories tags)
  "List all 1password items."
  (let ((args '()))
    (when categories
      (push (string-join categories ",") args)
      (push "--categories" args))
    (when tags
      (push (string-join tags ",") args)
      (push "--tags" args))
    (apply #'one-pass--run "item" "list" args)))

(defun one-pass/list-items (&rest specs)
  "List all 1password items.
SPECS is a list of keyword args whose respected keys are:
:categories - a list of categories to filter by
:tags - a list of tags to filter by "
  (let ((categories (plist-get specs :categories))
	(tags (plist-get specs :tags))
	(args '()))

    (mapcar
     (lambda (item)
       (plist-get item :title))
     (one-pass--list-items categories tags))))

(defconst one-pass--buffer-name "*one-pass*"
  "Name of the one-pass buffer.")

(defun one-pass--plist-keys (plist)
  (let ((keys '())
	(key nil))
    (dolist (item plist)
      (if key
	  (setq key nil)
	(progn
	  (push item keys)
	  (setq key item))))
    keys))

;;;###autoload
(defun one-pass/view-item (item)
  (interactive (list (read-string "item: " )))
  (let ((buffer (get-buffer-create one-pass--buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (let* ((raw-results (one-pass/get-item item))
	     (keys (one-pass--plist-keys raw-results))
	     (max-key (seq-reduce
		       (lambda (acc key)
			 (max acc (length (symbol-name key))))
		       keys
		       0))
	     (rows '())
	     (title (format "1password item: %s" item)))
	(dolist (key keys)
	  (let ((padding (make-string (- max-key (length (symbol-name key))) ?\s))
		(field (substring (symbol-name key) 1))
		(value (plist-get raw-results key)))
	    (push (format "%s%s: %s" field padding value) rows)))
	(insert
	 (string-join
	  (cons title (cons (make-string (length title) ?\=) rows))
	  "\n"))
	(goto-char (point-min))
	(display-buffer buffer)))))

(defun one-pass--view-item-list-group (groups category)
  (append (list "" category (make-string (length category) ?\=))
	  (sort
	   (mapcar
	    (lambda (item) (plist-get item :title))
	    (alist-get category groups nil nil #'equal))
	   #'string<)))

;;;###autoload
(defun one-pass/view-item-list (categories tags)
  (interactive (list (read-string "categories: ")
		     (read-string "tags: ")))
  (let ((buffer (get-buffer-create one-pass--buffer-name))
	(category-list (unless (string= categories "") (split-string categories ",")))
	(tag-list (unless (string= tags "") (split-string tags ","))))
    (with-current-buffer buffer
      (erase-buffer)
      (let* ((groups (seq-group-by
		     (lambda (item) (plist-get item :category))
		     (one-pass--list-items category-list tag-list)))
	    (group-names (mapcar #'car groups))
	    (title (format "1password items"))
	    (rows nil))
	(if (<= (length groups) 1)
	    (setq rows (seq-drop (one-pass--view-item-list-group groups (car group-names)) 3))
	  (dolist (category group-names)
	    (dolist (row (reverse (one-pass--view-item-list-group groups category)))
	      (push row rows))))
	(insert
	 (string-join
	  (cons title (cons (make-string (length title) ?\=) rows))
	  "\n"))
	(goto-char (point-min))
	(display-buffer buffer)))))


(provide 'one-pass)
;; one-pass.el ends here
