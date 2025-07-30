;;; -*- lexical-binding: t -*-

;; TODO - write custom indent-line-function that knows about program elements
;; and doesn't indent or

(require 'nxml-mode)
(require 'smart-quote)

;; Incomplete list
(defvar *ptx-block-tags*
  '(p note program part chapter section subsection introduction conclusion sidebyside ul ol))

(defun ptx-next-or-eof (what)
  (save-excursion
    (or (search-forward what (point-max) t) (point-max))))

(defun ptx-in-open-tag ()
  (interactive)
  (let ((where (point)))
    (save-excursion
      (let ((open (search-backward "<" (point-min) t)))
        (when open
          (let ((close (ptx-next-or-eof ">")))
            (< open where close)))))))

(defun ptx-in-element (e)
  (let ((open-tag-pat (format "<%s\\( [^>]*?\\)?>" e))
        (close-tag (format "</%s>" e)))
    (lambda ()
      (interactive)
      (let ((where (point)))
        (save-excursion
          (let ((open (re-search-backward open-tag-pat nil t)))
            (when open
              (let ((close (ptx-next-or-eof close-tag)))
                (< open where close)))))))))

(defun ptx-auto-tag ()
  (interactive)
  (when (and (looking-at "[ \t\n]") (looking-back "<\\([[:alnum:]-]+\\)[^<]*>"))
    (let ((tag (match-string 1)))
      (save-excursion (insert (format "</%s>" tag)))
      (when (ptx-block-p tag)
        (newline-and-indent)
        (previous-line)
        (end-of-line)
        (newline-and-indent)))))

(defun ptx-block-p (tag)
  ;; incomplete list.
  (member (intern (downcase tag)) *ptx-block-tags*))

(defun ptx-add-tag (prefix tag)
  (interactive "p\nsTag: ")
  (let (start end)
    (if (or (= prefix 4) mark-active)
        (setq start (min (point) (mark))
              end (max (point) (mark)))
        (setq start (ptx-find-start-of-code)
              end (point-marker)))
    (cond
     ;; If adding an empty tag, position cursor between tags.
     ((= start end)
      (insert (format "<%s>" tag))
      (save-excursion
        (insert (format "</%s> " tag))))
     ;; If on one line, assume an inline tag
     ((< (count-lines start end) 2)
      (goto-char end)
      (insert (format "</%s>" tag))
      (save-excursion
        (goto-char start)
        (insert (format "<%s>" tag))))
     ;; Otherwise assume a block tag
     (t
      (goto-char end)
      (insert (format "</%s>\n" tag))
      (setq end (point))
      (save-excursion
        (goto-char start)
        (insert (format "<%s>\n" tag)))
      (indent-region start end)))))


(defun ptx-insert-xref ()
  (interactive)
  (let ((text (string-trim (substring-no-properties (current-kill 0 t)) "\"" "\"")))
    (insert (format "<xref ref=\"%s\" />" text))))

(defun ptx-expand-entity ()
  (interactive)
  (let ((char (buffer-substring-no-properties (1- (point)) (point))))
    (cond
     ((string-equal char ">")
      (backward-delete-char 1)
      (insert "&gt;"))
     ((string-equal char "<")
      (backward-delete-char 1)
      (insert "&lt;"))
     ((string-equal char "&")
      (backward-delete-char 1)
      (insert "&amp;")))))

(defun ptx-split-block-element ()
  "Split the current enclosing element into two of the same kind. Designed
for block elements like <p>."
  (interactive)
  (just-one-space -1)
  (nxml-split-element)
  (when (not (looking-at "[[:space:]\n]*</"))
    (fill-paragraph))
  (save-excursion
    (previous-line 2)
    (end-of-line)
    (insert "\n"))
  (when (looking-at "[[:space:]\n]*</")
    (indent-for-tab-command)
    (previous-line 1)
    (end-of-line)
    (insert "\n")
    (indent-for-tab-command)))

(defun ptx-join-block-element ()
  (interactive)
  (nxml-backward-up-element)
  (beginning-of-line)
  (insert "\n")
  (just-one-space -1))


(defun ptx-find-close-tag ()
  (save-excursion
    (nxml-up-element)
    (let ((tag-end (point)))
      (search-backward "</")
      (buffer-substring-no-properties (point) tag-end))))

(defun ptx-find-previous-space ()
  (save-excursion
    (search-backward " ")
    (1+ (point))))

(defun ptx-find-start-of-word ()
  (save-excursion
    (re-search-backward "[^[:alnum:]]")
    (1+ (point))))

(defun ptx-find-start-of-code ()
  (if (looking-back "[[:alnum:]]")
      (ptx-find-start-of-word)
    (ptx-find-previous-space)))

(defmacro ptx-formatter (tag)
  `#'(lambda (prefix)
      (interactive "p")
      (ptx-add-tag prefix ,tag)))

(defun ptx-code-visualization-url ()
  (interactive)
  (let ((default-directory (locate-dominating-file (buffer-file-name) "make-visualizer-link.py")))
    (insert
     (with-temp-buffer
       (let ((coding-system-for-read 'utf-8)
             (coding-system-for-write 'utf-8))
         (insert (current-kill 0))
         (call-process-region (point-min) (point-max) "uv" t t nil "run" "make-visualizer-link.py")
         (buffer-string))))))

(define-derived-mode ptx-mode
  nxml-mode "PreTeXt"
  "Major mode for editing PreTeXt files, based on `nxml-mode`."
  (smart-quote-mode)
  (make-local-variable '*smart-quote-disabled-tests*)
  (make-local-variable 'isearch-lax-whitespace)
  (make-local-variable 'isearch-regexp-lax-whitespace)
  (make-local-variable 'search-whitespace-regexp)

  (push 'ptx-in-open-tag *smart-quote-disabled-tests*)
  (push (ptx-in-element "c") *smart-quote-disabled-tests*)
  (push (ptx-in-element "code") *smart-quote-disabled-tests*)
  (push (ptx-in-element "cline") *smart-quote-disabled-tests*)
  (push (ptx-in-element "program") *smart-quote-disabled-tests*)

  ;; Make incremental search treat newlines as whitespace
  (setq isearch-lax-whitespace t
        isearch-regexp-lax-whitespace t
        search-whitespace-regexp "[ \t\r\n]+")

  ;; Make ispell skip XML markup to focus on the text.
  (setq-local ispell-skip-region-alist
              (append ispell-skip-region-alist
                      '(
                        ("<[^>]+" . ">")     ; Skip XML tags
                        ("<!--" . "-->")      ; Skip XML comments
                        ("<\\?xml" . "\\?>")  ; Skip XML declarations
                        )))

  (add-hook 'post-self-insert-hook 'ptx-auto-tag nil t)

  (set-buffer-file-coding-system 'utf-8 t t))

(define-key ptx-mode-map (kbd "C-c C-c") (ptx-formatter "c"))
(define-key ptx-mode-map (kbd "C-c C-e") 'ptx-expand-entity)
(define-key ptx-mode-map (kbd "C-c C-p") 'ptx-split-block-element)
(define-key ptx-mode-map (kbd "C-c C-t") 'ptx-add-tag)
(define-key ptx-mode-map (kbd "C-c C-v") 'ptx-code-visualization-url)
(define-key ptx-mode-map (kbd "C-c C-x") 'ptx-insert-xref)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ptx\\'" . ptx-mode))

(provide 'ptx)
