(define-derived-mode list-outline-mode
  outline-mode "List outline" "Mode for editing markup/markdown-style list structured outlines."
  (auto-fill-mode t)
  (smart-quote-mode t)
  (set (make-local-variable 'outline-regexp) "\\( *\\)- ")
  (set (make-local-variable 'outline-level) (lambda () (1+ (/ (- (match-end 1) (match-beginning 1)) 2))))
  (set-buffer-file-coding-system 'utf-8 t t))

(provide 'list-outline)
