;; Copyright (C) 2002-2004 Peter Seibel

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

(provide 'coding-diary)

(defvar *coding-diary:came-from-buffer* nil
  "The buffer we were in when we started writing a transcript comment.")

(defvar *coding-diary:transcript-buffer* nil
  "The buffer for the transcript file itself.")

(defvar *coding-diary:text-after-saved-comment* nil
  "Used by coding-diary:save-region-to-transcript when we want to write a comment first.")

(defvar coding-diary::coding-diary-mode-map nil
  "Local keymap for coding diary.")
(if coding-diary::coding-diary-mode-map
    nil
    (let ((map (make-keymap)))
      (define-key map [(control c) ?!] 'coding-diary:toggle-to-from-transcript)
      (define-key map [(control c) (control t)] 'coding-diary::insert-timestamp)
      (setq coding-diary::coding-diary-mode-map map)))

(define-minor-mode coding-diary-minor-mode
    "Toggle Coding Diary minor mode.

With no argument, this command toggles the mode. Non-null prefix
argument turns on the mode. Null prefix argument turns off the mode.
When Coding Diary mode is enabled, commands are provided to make it
easy to add timestamped entries to a coding diary."
  nil
  " CodingDiaryMinor"
  '(("\C-c." . coding-diary:new-transcript-comment)
    ("\C-c," . coding-diary:append-to-transcript)
    ("\C-c!" . coding-diary:toggle-to-from-transcript))
  
  (make-local-variable '*coding-diary:transcript-buffer*))

(defface coding-diary-date-face
  '((t (:inherit font-lock-string-face)))
  "Face used to highlight dates in date lines."
  :version "21.1"
  :group 'change-log)

(defvar coding-diary-font-lock-keywords
  '(("^2004-[-0-9: ]*" (0 'coding-diary-date-face))
    ("^\\sw+, [0-9][0-9] \\sw+ [0-9][0-9][0-9][0-9], [0-9]+:[0-9][0-9]:[0-9][0-9] \\sw+ (\\sw+)"
     (0 'coding-diary-date-face)))
  "Additional expressions to highlight in CodingDiary mode.")

(defun coding-diary::insert-timestamp ()
  (interactive)
  (insert (format-time-string "%A, %d %B %Y, %I:%M:%S %p (%Z)") "\n\n"))

(defun coding-diary:new-transcript-comment ()
  "Add a comment to the transcript. If the mark is active the contents
of the region will be copied into the transcript."
  (interactive)
  (coding-diary::insert-into-transcript t))

(defun coding-diary:append-to-transcript ()
  (interactive)
  (coding-diary::insert-into-transcript nil))

(defun coding-diary:append-to-comment ()
  (interactive)
  (coding-diary::insert-into-transcript nil))


(defun coding-diary::insert-into-transcript (timestamp-p)
  "Add a comment to the transcript. If the mark is active the contents
of the region will be copied into the transcript."
  (let ((region-text (if mark-active
                       (buffer-substring-no-properties (point) (mark)) nil)))
    (coding-diary:show-transcript)
    (goto-char (point-max))
    (delete-blank-lines)
    (let ((cursor nil))
      (when timestamp-p
        (insert "\n")
        (coding-diary::insert-timestamp)
        (setf cursor (point))
        (insert "\n"))
      (when region-text
        (insert "\n")
        (let ((start (point)))
          (insert region-text "\n")
          (indent-code-rigidly start (point) 2)))
      (when cursor (goto-char cursor)))))

(defun coding-diary::in-transcript-p ()
  (eql major-mode 'coding-diary::coding-diary-mode))

(defun coding-diary:toggle-to-from-transcript ()
  (interactive)
  (if (coding-diary::in-transcript-p)
    (switch-to-buffer *coding-diary:came-from-buffer*)
    (coding-diary:show-transcript)))

(defun coding-diary:show-transcript ()
  "Switch to the buffer containing the transcript file"
  (interactive)
  (unless (and *coding-diary:transcript-buffer* 
               (buffer-live-p *coding-diary:transcript-buffer*))
    (coding-diary::open-transcript))
  (let ((came-from (current-buffer)))
    (switch-to-buffer *coding-diary:transcript-buffer* t)
    (set (make-local-variable '*coding-diary:came-from-buffer*) came-from)))

(defun coding-diary::diary-file-name ()
  (expand-file-name "CODING-DIARY" (file-name-directory (buffer-file-name))))

(defun coding-diary::open-transcript ()
  (setq *coding-diary:transcript-buffer*
        (find-file-noselect (coding-diary::diary-file-name)))
  (save-excursion
    (set-buffer *coding-diary:transcript-buffer*)
    (coding-diary::coding-diary-mode)))

(define-derived-mode coding-diary::coding-diary-mode
    indented-text-mode
  "CodingDiary"
  "Major mode for editing a coding diary.
\\{coding-diary::coding-diary-mode-map}"
  (setq buffer-read-only nil)
  (set (make-local-variable 'font-lock-defaults)
       '(coding-diary-font-lock-keywords t nil nil backward-paragraph))
  (set (make-local-variable 'require-final-newline) t))
