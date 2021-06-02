;;; Mode for editing an index (a list of terms) that keeps the list
;;; sorted and doesn't allow duplicate entries. (Mainly for use in
;;; Step 1 of my How to Write a Book process
;;; (https://gigamonkeys.com/how-to-write-a-book/)

(defun index-electric-newline ()
  (interactive)
  (let ((entry (index-get-line)))
    (beginning-of-line)
    (save-excursion
      (let ((kill-whole-line nil))
        (kill-line))
      (let ((pos (index-find-position (downcase entry))))
        (goto-char pos)
        (when (not (string= (downcase entry) (downcase (index-get-line))))
          (insert entry)
          (newline))))))

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
