(define-derived-mode longlines-mode
  text-mode "Longlines" "Mode for editing text with no hard line breaks."
  (visual-line-mode)
  (visual-fill-column-mode)
  (smart-quote-mode t)
  (set-buffer-file-coding-system 'utf-8 t t))

(defun longlines-clean-spaces ()
  (interactive)
  (let (start end)
    (save-excursion
      (backward-paragraph)
      (when (not (eql (point) (point-min)))
        (forward-line)
        (setf start (point))
        (forward-paragraph)
        (left-char)
        (setf end (point))
        (replace-regexp "\\s +" " " nil start end)
        (message "Tidied whitespace.")))))

(define-key longlines-mode-map "\M-q" 'longlines-clean-spaces)

(provide 'longlines)
