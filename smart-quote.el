;;; smart-quote.el -- Minor mode for insterting curly quotes and em-
;;; or en-dashes into otherwise plain text.
;;;
;;; Copyright (c) 2009-2011, Peter Seibel
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

(require 'cl)

;; Do two ASCII dashes get translated to an emdash (with no spaces) or,
;; as some typographers prefer, an endash with surrounding spaces.
(defvar *smart-quote-use-mdash* t)

(defvar *smart-quote-before-open-quotes* '(" " "\n" "(" "[" "{" "“" "‘" "—"))

(defvar *smart-quote-disabled-tests* ()
  "A list of functions to run before inserting smart characters.
  If any hook function returns true, insert the regular character.")

(defun smart-quote-disabled ()
  (smart-quote-check-hooks *smart-quote-disabled-tests*))

(defun smart-quote-check-hooks (hooks)
  ;; Run each hook, returning T if any hook returns T, NIL otherwise.
  (and hooks (or (funcall (car hooks)) (smart-quote-check-hooks (cdr hooks)))))

(defun smart-quote-need-open-quote-p ()
  (or (= (point) (point-min))
      (let ((char (char-before)))
        (member (char-to-string char) *smart-quote-before-open-quotes*))))

(defun smart-quote-insert-single-quote ()
  (interactive)
  (smart-quote-insert (if (smart-quote-need-open-quote-p) "‘" "’")))

(defun smart-quote-insert-double-quote ()
  (interactive)
  (smart-quote-insert (if (smart-quote-need-open-quote-p) "“" "”")))

(defvar smart-quote-names
  '(("”" "Right double quote" "\"")
    ("\"" "Straight double quote" "\"")
    ("“" "Left double quote" "\"")
    ("″" "Double prime" "\"")
    ("’" "Right single quote (apostrophe)" "'")
    ("'" "Straight single quote" "'")
    ("′" "Prime" "'")
    ("‘" "Left single quote" "'")
    ("–" "Endash" "--")
    ("-" "Hyphen" "-")
    ("—" "Emdash" "--")
    ("−" "Minus sign" "-")
    ("…" "Ellipses" "...")))

(defvar smart-quote-rotations
  '(("“" . "”")
    ("”" . "\"")
    ("\"" . "″")
    ("″" . "“")
    ("‘" . "’")
    ("’" . "'")
    ("'" . "′")
    ("′" . "‘")
    ("—" . "–")
    ("–" . "-")
    ("-" . "−")
    ("−" . "—")))

(defun smart-quote-insert (what)
  (let* ((entry (assoc what smart-quote-names))
         (name (cadr entry))
         (unsmart (caddr entry)))
    (cond
     ((smart-quote-disabled)
      (insert-and-inherit unsmart))
     (t
      (insert-and-inherit what)
      (message name)))))

(defun smart-quote-rotate-quote ()
  (interactive)
  (let* ((old (char-to-string (char-before)))
         (new (cdr (assoc old smart-quote-rotations))))
    (cond
     (new
      (delete-backward-char 1)
      (smart-quote-insert new))
     (t
      (message (concat "Don't know how to rotate " old))))))

(defun smart-quote-insert-dash ()
  (interactive)
  (let ((char (char-before)))
    (cond
     ((and char (char-equal char ?-))
      (delete-backward-char 1)
      (smart-quote-insert (if *smart-quote-use-mdash* "—" " – ")))
     (t
      (insert-and-inherit "-")))))

(defun smart-quote-insert-ellipsis ()
  (interactive)
  (let ((char1 (char-before (1- (point))))
        (char2 (char-before)))
    (cond
     ((and char1 char2 (char-equal char1 ?.) (char-equal char2 ?.))
      (delete-backward-char 2)
      (smart-quote-insert "…"))
     (t
      (insert-and-inherit ".")))))

(defun smart-quote-accent ()
  (interactive)
  (let ((char (char-before)))
    (delete-backward-char 1)
    (cond
     ((char-equal char ?\`) (insert ?\u0300))
     ((or (char-equal char ?\') (char-equal char (string-to-char "’"))) (insert ?\u0301))
     ((char-equal char ?^) (insert ?\u0302))
     ((char-equal char ?\~) (insert ?\u0303))
     ((char-equal char ?\:) (insert ?\u0308))
     (t (message "Don't know how to convert %s to accent." char)))))

(define-minor-mode smart-quote-mode
  "Insert the Unicode characters for curly quotes automatically."
  nil
  :lighter " sq"
  :global nil
  :keymap
  (list
   '("'" . smart-quote-insert-single-quote)
   '("\"" . smart-quote-insert-double-quote)
   '("-" . smart-quote-insert-dash)
   '("." . smart-quote-insert-ellipsis)
   (cons (kbd "C-'") 'smart-quote-rotate-quote)
   (cons (kbd "C-\"") 'smart-quote-rotate-quote)
   (cons (kbd "C-M-'") 'smart-quote-accent)
   ))

(provide 'smart-quote)
