;;; markup.el -- Major mode for editing Markup based text.
;;;
;;; Copyright (c) 2010-2017, Peter Seibel
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;     * Redistributions of source code must retain the above
;;;       copyright notice, this list of conditions and the following
;;;       disclaimer.
;;;
;;;     * Redistributions in binary form must reproduce the above
;;;       copyright notice, this list of conditions and the following
;;;       disclaimer in the documentation and/or other materials
;;;       provided with the distribution.
;;;
;;;     * Neither the name of Peter Seibel nor the names of its
;;;       contributors may be used to endorse or promote products
;;;       derived from this software without specific prior written
;;;       permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
;;; TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
;;; THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;

(defvar *markup-latest-buffer* nil)

(defun markup-set-latest-buffer ()
  (interactive)
  (setf *markup-latest-buffer* (current-buffer)))

(defun markup-tag (prefix tag)
  (interactive "p\nsTag: ")
  (let (start end)
    (if (or (= prefix 4) mark-active)
        (setq start (min (point) (mark))
              end (max (point) (mark)))
        (setq start (markup-find-previous-space)
              end (point-marker)))
    (goto-char end)
    (insert "}")
    (save-excursion
      (goto-char start)
      (insert (format "\\%s{" tag)))))

(defmacro markup-formatter (tag)
  `#'(lambda (prefix)
      (interactive "p")
      (markup-tag prefix ,tag)))

(defun markup-find-previous-space ()
  (save-excursion
    (backward-sexp 1)
    (point)))

(defun markup-in-verbatim ()
  (or
   (save-excursion
     (beginning-of-line)
     (and (looking-at paragraph-start)
          (looking-at "   ")))
   (save-excursion
     (backward-paragraph 1)
     (forward-line 1)
     (looking-at "   "))))

(defun markup-in-code ()
  (message "Checking for code")
  (let ((where (point))
        (start nil)
        (end nil))
    (save-excursion
      (when (search-backward "\\code{" nil t)
        (search-forward "{")
        (setq start (point))
        (backward-char 1)
        (setf end (if (ignore-errors (progn (forward-sexp 1) t)) (point) (1+ where)))))
    (and start (<= start where) (< where end))))

(defun markup-fix-longlines ()
  (interactive)
  (let ((nullstring (string ?\000)))
    (goto-char (point-min))
    (replace-regexp "\n *\n" nullstring)
    (goto-char (point-min))
    (replace-regexp "\n" " ")
    (goto-char (point-min))
    (replace-regexp nullstring "\n\n")))

(defun markup-add-link ()
  (interactive)
  (let ((link (markup-find-previous-link)))
    (cond
     (link
      (goto-char (point-max))
      (insert "[" link "] <")
      (insert (read-string "URL: "))
      (save-excursion (insert ">\n"))
      (markup-tidy-links))
     (t (message "No link found.")))))

(defun markup-mark-link (prefix)
  (interactive "p")
  (let (start end)
    (if (or (= prefix 4) mark-active)
        (setq start (min (point) (mark))
              end (max (point) (mark)))
        (setq start (markup-find-previous-space)
              end (point-marker)))
    (goto-char end)
    (insert "]")
    (save-excursion
      (goto-char start)
      (insert (format "[")))))

(defun markup-tidy-links ()
  (interactive)
  (let ((pat "^\\[[^]]+\\] +<[^>]+>"))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward-regexp pat nil t)
        (beginning-of-line)
        (let ((start (point)))
          (save-excursion
          (goto-char (point-max))
          (search-backward-regexp pat start t)
          (beginning-of-line)
          (next-line)
          (sort-lines nil start (point))
          (align-regexp start (point) "\\(\\s-*\\)<")))))))

(defun markup-find-previous-link ()
  (interactive)
  (let ((start nil)
        (end nil))
    (save-excursion
      (search-backward "[")
      (setf start (1+ (point)))
      (save-excursion
        (search-forward "]")
        (setf end (1- (point))))
      (if (search-forward "|" end t)
          (setf start (point))))
    (if (and start end)
        (buffer-substring-no-properties start end)
      nil)))

(defun markup-paste-code ()
  (interactive)
  (save-excursion
    (let ((start (point))
          (end nil))
      (yank)
      (setf end (point))
      (markup-indent-code start end))))

(defun markup-indent-code (start end)
  (interactive "r")
  (let ((indent (markup-measure-indentation start end)))
    (indent-code-rigidly start end (- 3 indent))))

(defun markup-send-code (start end)
  (interactive "r")
  (cond
   (*markup-latest-buffer*
    (let ((text (buffer-substring-no-properties start end)))
      (save-current-buffer
        (set-buffer *markup-latest-buffer*)
        (let ((start (point)))
          (insert text)
          (markup-indent-code start (point))))))
   (t (message "No latest markup buffer."))))

(defun markup-measure-indentation (start end)
  (interactive "r")
  (save-excursion
    (let ((indent nil))
      (goto-char start)
      (while (< (point) end)
        (let ((i (markup-measure-line-indentation)))
          (when i (setf indent (min i (or indent i))))
          (forward-line)))
      indent)))

(defun markup-measure-line-indentation ()
  (save-excursion
    (let (start end)
      (end-of-line)
      (setf end (point))
      (beginning-of-line)
      (setf start (point))
      (when (and (search-forward-regexp "^\\(\\s-*\\)" end t) (< (point) end))
        (- (match-end 1) (match-beginning 1))))))

(defun markup-autogenerate-html ()
  (interactive)
  (add-hook 'after-save-hook #'markup-generate-html nil t))

(defun markup-generate-html ()
  "Generate HTML from the current file assuming we are connected
to SLIME and the YAMP package has been loaded."
  (interactive)
  (cond
   ((not (fboundp 'slime-connected-p)))
   ((not (slime-connected-p))
    (message "SLIME not connected to Lisp. Can't generate html."))
   ((not (markup-yamp-exists))
    (message "SLIME connected but YAMP not loaded. Can't generate html."))
   (t
    (let ((out (markup-send-slime-request)))
      (if out
          (message "Generated HTML: %s" out)
        (message "Didn't generate HTML."))))))

(defun markup-yamp-exists ()
  (slime-eval '(cl:not (cl:not (cl:find-package "COM.GIGAMONKEYS.YAMP")))))

(defun markup-send-slime-request ()
  (slime-eval `(com.gigamonkeys.yamp::generate-html ,(buffer-file-name))))

(define-derived-mode markup-mode
  outline-mode "Markup" "Mode for editing Markup based text."
  (auto-fill-mode t)
  (smart-quote-mode t)
  (make-local-variable '*smart-quote-disabled-tests*)
  (push 'markup-in-verbatim *smart-quote-disabled-tests*)
  (push 'markup-in-code *smart-quote-disabled-tests*)
  (make-local-variable 'indent-region-function)
  (setq indent-region-function #'markup-indent-code)
  (add-hook 'after-save-hook #'markup-generate-html nil t)
  (add-hook 'after-save-hook #'markup-set-latest-buffer nil t)
  (set-buffer-file-coding-system 'utf-8 t t))

(define-key markup-mode-map "\C-cf" nil)
(define-key markup-mode-map "\C-cfi" (markup-formatter "i"))
(define-key markup-mode-map "\C-cfb" (markup-formatter "b"))
(define-key markup-mode-map "\C-cfn" (markup-formatter "note"))
(define-key markup-mode-map "\C-cfc" (markup-formatter "code"))
(define-key markup-mode-map "\C-cfm" (markup-formatter "math"))

(defun markup-section-break ()
  (interactive)
  (insert "§")
  (center-line))

(defun markup-mark-cut-section (start end name)
  (interactive "r\nMname: ")
  (goto-char end)
  (dotimes (i (1+ (comment-add nil)))
    (insert comment-start))
  (if (not (string-suffix-p " " comment-start)) (insert " "))
  (insert "--8<")
  (dotimes (i (- fill-column (+ (current-column) (length comment-end))))
    (insert "-"))
  (insert comment-end)
  (insert "\n")
  (goto-char start)
  (dotimes (i (1+ (comment-add nil)))
    (insert comment-start))
  (if (not (string-suffix-p " " comment-start)) (insert " "))
  (insert "--8<--- ")
  (insert name)
  (insert " ")
  (dotimes (i (- fill-column (+ (current-column) (length comment-end))))
    (insert "-"))
  (insert comment-end)
  (insert "\n"))

(provide 'markup)
