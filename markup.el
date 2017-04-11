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

(define-derived-mode markup-mode
  outline-mode "Markup" "Mode for editing Markup based text."
  (auto-fill-mode t)
  (smart-quote-mode t)
  (make-local-variable '*smart-quote-disabled-tests*)
  (push 'markup-in-verbatim *smart-quote-disabled-tests*)
  (push 'markup-in-code *smart-quote-disabled-tests*)
  (set-buffer-file-coding-system 'utf-8 t t))

(define-key markup-mode-map "\C-cf" nil)
(define-key markup-mode-map "\C-cfi" (markup-formatter "i"))
(define-key markup-mode-map "\C-cfb" (markup-formatter "b"))
(define-key markup-mode-map "\C-cfn" (markup-formatter "note"))
(define-key markup-mode-map "\C-cfc" (markup-formatter "code"))
(define-key markup-mode-map "\C-cfm" (markup-formatter "math"))


(provide 'markup)
