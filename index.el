;;; Mode for editing an index (a list of terms) that keeps the list
;;; sorted and doesn't allow duplicate entries. (Mainly for use in
;;; Step 1 of my How to Write a Book process
;;; (https://gigamonkeys.com/how-to-write-a-book/)

(defun index-electric-newline ()
  (interactive)
  (beginning-of-line)
  (let ((entry (index-get-line)))
    (let ((kill-whole-line nil))
      (kill-line))
    (index-insert-entry entry)))

(defun index-insert-entry (entry)
  (save-excursion
    (let ((pos (index-find-position (downcase entry))))
      (goto-char pos)
      (when (not (string= (downcase entry) (downcase (index-get-line))))
        (insert entry)
        (newline))))
  (if nil
      (let ((after-comma (index-extract-after-comma entry)))
        (when after-comma
          (message "Adding %s" after-comma)
          (index-insert-entry after-comma)))))


(defun index-extract-after-comma (s)
  "Extract the text after the first comma in s."
  (let ((parts (split-string s ",")))
    (if (> (length parts) 1)
        (string-trim (nth 1 parts))
      nil)))

(defun index-tidy ()
  (interactive)
  (index-sort)
  (index-dedupe))

(defun index-dedupe ()
  (interactive)
  (save-excursion
    (index-goto-start)
    (while (< (point) (point-max))
      (let ((current (index-get-line)))
        (forward-line 1)
        (while (and (not (string= (index-get-line) "")) (string= current (index-get-line)))
          (let ((kill-whole-line t))
            (kill-line)))))))

(defun index-sort ()
  (interactive)
  (save-excursion
    (index-goto-start)
    (let ((sort-fold-case t))
      (sort-lines nil (point) (point-max)))))

(defun index-goto-start ()
  (goto-char (point-min))
  (forward-line 1)
  (while (string= (index-get-line) "")
    (forward-line 1)))



(defun index-find-position (entry)
  (save-excursion
    (index-goto-start)

    ;; Find first existing entry >= to proposed entry
    (while (and (not (string= "" (index-get-line))) (string< (downcase (index-get-line)) entry))
        (forward-line 1))

    (line-beginning-position)))

(defun index-get-line ()
  (buffer-substring-no-properties
   (line-beginning-position) (line-end-position)))

(define-minor-mode index-mode
  "Write an index of terms."
  nil
  :lighter " index"
  :global nil
  :keymap
  (list (cons (kbd "RET") 'index-electric-newline)))

(provide 'index)
