;;;; Some functions to make editing book easier.

(load (concat (file-name-directory (or load-file-name (buffer-file-name (current-buffer)))) "common-lisp-symbols"))
(load (concat (file-name-directory (or load-file-name (buffer-file-name (current-buffer)))) "coding-diary"))

;; elisp for editing English text (including text that happens to be
;; embedded in HTML, etc.)


(defun pbs:text-search (s)
  (interactive "sT-search: ")
  (re-search-forward
   (pbs::make-text-search-regexp s)
   (point-max)
   t))

(defun pbs::make-text-search-regexp (s)
  (mapconcat #'identity (split-string s) "[ \f\t\n\r\v]*"))


(defun pbs:add-latex-tag (point mark tag)
  (interactive "r\nsTag: ")
  (save-excursion
    (goto-char mark)
    (insert "}")
    (goto-char point)
    (insert "\\")
    (insert tag)
    (insert "{")))

(defun pbs:insert-tagged-text (tag text)
  (interactive "sTag: \nsText: \n")
  (insert (format "\\%s{%s}" tag text)))


(defun pbs:occur-headers (n)
  (interactive "p")
  (occur
   (cond
     ((= n 1) "^\\\\h1")
     ((= n 4) "^\\\\h"))))


(defun pbs:add-markup (prefix tag)
  (interactive "p\nsTag: ")
  (let (start end)
    (if (or (= prefix 4) mark-active)
        (setq start (min (point) (mark))
              end (max (point) (mark)))
        (setq start (pbs:find-previous-space)
              end (point-marker)))
    (let ((text (buffer-substring-no-properties start end)))
      (when (and (string-equal tag "code")
                 (string-match "^[-*:0-9a-zA-Z&]+-[-*:0-9a-zA-Z&]+$" text))
        (pbs:save-code-name text)))
    (goto-char end)
    (insert "}")
    (save-excursion
      (goto-char start)
      (insert (format "\\%s{" tag)))))

(defun pbs:markup-region (tag start end)
  (goto-char end)
  (insert "}")
  (save-excursion
    (goto-char start)
    (insert (format "\\%s{" tag))))

(defmacro pbs:make-formatter (tag)
  `#'(lambda (prefix)
      (interactive "p")
      (pbs:add-markup prefix ,tag)))

(defun pbs:find-previous-space ()
  (or (pbs:previous-format-directive)
      (save-excursion
        (backward-sexp 1)
        ;;(search-backward-regexp "\\s +")
        ;;(+ 1 (point))
        (point))))

(defun pbs::previous-word-start ()
  (save-excursion
    (backward-word 1)
    (point)))

(defun pbs::previous-word-end ()
  (save-excursion
    (backward-word 1)
    (forward-word 1)
    (point)))


(defun pbs:join-next-line ()
  (interactive)
  (end-of-line)
  (delete-blank-lines)
  (forward-line 1)
  (join-line))

(defun pbs:join-line ()
  (interactive)
  (end-of-line)
  (delete-blank-lines)
  (forward-line 1)
  (join-line))


(defun pbs:insert-evaluation ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (search-forward "(" (point-max) nil)
    (backward-char 1)
    (let ((start (point)))
      (forward-sexp)
      (let ((expression (format "%s" (buffer-substring-no-properties start (point)))))
        (when (looking-at "\\s *==>")
          (kill-line))
        (pbs:slime-eval-print-last-expression expression)))))

;;; BUSTED by changes in slime.
(defun pbs:slime-eval-print-last-expression (string)
  (slime-eval-async
   `(swank:interactive-eval (format "%s" ,string))
   (slime-current-package)
   (lexical-let ((buffer (current-buffer))
                 (pos (point)))
     (lambda (result)
       (with-current-buffer buffer
         (goto-char pos)
         (insert
          (format " ==> %s" result)))))))


(defun pbs:up-to-top-level-heading ()
  (while (ignore-errors (outline-up-heading 1) t)))

(defun pbs:browse-to-chapter (arg)
  (interactive "p")
  (save-excursion
    (let ((filename
         (save-excursion
           (pbs:up-to-top-level-heading)
           (re-search-forward (concat outline-regexp "\\(.*\\)") (point-max) t)
           (let ((heading (buffer-substring-no-properties (match-beginning 1) (match-end 1))))
             (fi:eval-in-lisp (format "(book::title2filename \"%s\")" heading)))))
        (name
         (save-excursion
           (ignore-errors (outline-up-heading 1))
           (re-search-forward (concat outline-regexp "\\(.*\\)") (point-max) t)
           (let ((heading (buffer-substring-no-properties (match-beginning 1) (match-end 1))))
             (fi:eval-in-lisp (format "(book::title2filename \"%s\")" heading))))))
    (fi:eval-in-lisp (format "(book::regenerate-chapter \"%s\")" filename))
    (browse-url
     (format
      (cond
       ((= arg 1) "file:///home/peter/www.gigamonkeys.com/book/%s.html#%s")
       ((= arg 4) "file:///home/peter/www.gigamonkeys.com/book/%s-headers.html#%s")
       ((= arg 16) "file:///home/peter/www.gigamonkeys.com/book/word-counts-2.html#%s"))
      filename name)))))

(defvar *tagging* nil)

(defun pbs:tag-common-lisp-symbols ()
  (interactive)
  (let ((previous (pbs:previous-symbol)))
    (when (and (plusp (length previous)) (not (or (pbs:in-outline-header) (pbs:in-code-block))))
      (let ((markup
             (cond
              ((gethash previous *common-lisp-symbols*) "cl")
              ((or (pbs:keywordp previous)
                   (pbs:special-var-p previous)
                   (pbs:format-directive-p previous)) "code")
              ((pbs:code-name-p previous) "code"))))
        (when markup
          (pbs:markup-region
           markup
           (- (point) (length previous)) (point)))))))

(defvar *pbs:code-names* nil)

(defun pbs:save-code-name (name)
  (interactive (list (read-from-minibuffer "Name: " (pbs:previous-symbol))))
  (unless (pbs:code-name-p name)
    (push name *pbs:code-names*)))

(defun pbs:remove-code-name (name)
  (interactive "sName: ")
  (setq *pbs:code-names* (delete name *pbs:code-names*)))

(defun pbs:code-name-p (str)
  (member str *pbs:code-names*))


(defun pbs:keywordp (str)
  (= ?: (aref str 0)))

(defun pbs:special-var-p (str)
  (and (= ?* (aref str 0))
       (= ?* (aref str (1- (length str))))
       (> (length str) 2)))

(defun pbs:using-slime-p ()
  (and (fboundp 'slime-connected-p) (slime-connected-p)))

(defun pbs:switch-to-lisp ()
  (interactive)
  (if (pbs:using-slime-p)
      (slime-switch-to-output-buffer)
    (fi:toggle-to-lisp)))

(defvar pbs:*to-lisp-buffer* nil)

(defun pbs:toggle-to-lisp ()
  (interactive)
  (if (pbs:using-slime-p)
      (cond
       ((eq (current-buffer) (slime-repl-buffer))
        (switch-to-buffer pbs:*to-lisp-buffer*))
       (t
        (setf pbs:*to-lisp-buffer* (current-buffer))
        (slime-repl)))
    (fi:toggle-to-lisp)))

(defun pbs:toggle-to-lisp (n)
  (interactive "p")
  (cond
   ((or (= n 4) (= n 16))
    (slime-repl))
   ((= n 64)
    (slime-list-connections))
   ((eq (current-buffer) (slime-repl-buffer))
    (switch-to-buffer (slime-recently-visited-buffer 'lisp-mode)))
   (t
    (slime-repl))))


(global-set-key [(control ?c) ?l] 'pbs:toggle-to-lisp)

(defun pbs:find-too-long-lines ()
  (interactive)
  (do ((length (- (position-of (end-of-line)) (position-of (beginning-of-line)))))
      ((or (> length 80) (plusp (forward-line 1))))))

(defmacro position-of (&rest body)
  `(save-excursion ,@body (point)))

(defun pbs:in-outline-header ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at outline-regexp)
      (goto-char (match-end 0))
      (or (looking-at " ") (= (point) (position-of (end-of-line)))))))

(defun pbs:in-code-block ()
  (save-excursion
    (beginning-of-line)
    (looking-at "    ")))

(defun pbs:previous-symbol ()
  (interactive)
  (save-excursion
    (let ((end (point)))
      (let ((beg (if (re-search-backward "[^-*:0-9a-zA-Z&~]" (point-min) t) (1+ (point)) (point-min))))
        (buffer-substring-no-properties beg end)))))

(defun pbs:format-directive-p (str)
  (string-match "^\\~[,0-9']*[:@]*[ASWC%&|~RDBOXFEG$RP(T</*[{}?;^]$" (upcase str)))

(defun pbs:previous-format-directive ()
  (interactive)
  (save-excursion
    (let* ((end (point))
           (beg (if (re-search-backward "\\~[,0-9']*[:@]*[]ASWC%&|~RDBOXFEG$RP(T</*[{}?;^]" (point-min) t) (point) (point-min)))
           (str (buffer-substring-no-properties beg end)))
      (and (pbs:format-directive-p str) (= end (+ beg (length str))) beg))))

(defun pbs:previous-thing ()
  (let ((previous (pbs:previous-symbol)))
    (or
     (and (plusp (length previous)) (gethash previous *common-lisp-symbols*) previous)
     (and (plusp (length previous)) (or (pbs:keywordp previous) (pbs:special-var-p previous)) previous)
     (pbs:previous-format-directive)
     (and (plusp (length previous)) (pbs:code-name-p previous) previous))))

(defvar *normal-syntax-table* (make-syntax-table))

(defun pbs:backward-word (&optional arg)
  (interactive "p")
  (with-syntax-table *normal-syntax-table*
    (backward-word arg)))

(defun pbs:forward-word (&optional arg)
  (interactive "p")
  (with-syntax-table *normal-syntax-table*
    (forward-word arg)))

(defun pbs:backward-kill-word (&optional arg)
  (interactive "p")
  (with-syntax-table *normal-syntax-table*
    (backward-kill-word arg)))

(defun pbs:kill-word (&optional arg)
  (interactive "p")
  (with-syntax-table *normal-syntax-table*
    (kill-word arg)))


(defun pbs:setup-syntax-table ()
  ;; Need to modify the syntax table to control when abbrev expansion
  ;; happens. Unfortunately this borks up some of the movement-by-word
  ;; commands. Should probably make a copy of the standard syntax
  ;; table and then make by word
  (modify-syntax-entry ?* "w")
  (modify-syntax-entry ?- "w")
  (modify-syntax-entry ?& "w")
  (modify-syntax-entry ?' "."))

(defun pbs:turn-on-auto-tagging ()
  (interactive)
  (abbrev-mode 1)
  (pbs:setup-syntax-table)
  (set (make-local-variable 'pre-abbrev-expand-hook)
       (adjoin #'pbs:tag-common-lisp-symbols pre-abbrev-expand-hook)))


(defvar pbs:*base-words-per-reinforcement* 100)
(defvar pbs:*random-deviation* 50)

(defvar pbs:*words-written* 0)
(defvar pbs:*next-reinforcement* 100)

(defun pbs:reinforce-words ()
  (interactive)
  (incf pbs:*words-written*)
  (when (= pbs:*words-written* pbs:*next-reinforcement*)
    (beep)
    (message "Good job! Another %d words written." pbs:*words-written*)
    (setf pbs:*words-written* 0)
    (setf pbs:*next-reinforcement*
          (+ pbs:*base-words-per-reinforcement*
             (- pbs:*random-deviation* (* 2 (random pbs:*random-deviation*)))))))


(define-derived-mode pbs:book-mode
  outline-mode "Book" "Mode for editining my book."
  (pbs:turn-on-auto-tagging))


(define-key pbs:book-mode-map [(control ?c) right] 'pbs:demote-outline-levels)
(define-key pbs:book-mode-map [(control ?c) left] 'pbs:promote-outline-levels)
(substitute-key-definition 'forward-word 'pbs:forward-word pbs:book-mode-map (current-global-map))
(substitute-key-definition 'backward-word 'pbs:backward-word pbs:book-mode-map (current-global-map))
(substitute-key-definition 'kill-word 'pbs:kill-word pbs:book-mode-map (current-global-map))
(substitute-key-definition 'backward-kill-word 'pbs:backward-kill-word pbs:book-mode-map (current-global-map))


(defun pbs:mw-lookup ()
  (interactive)
  (browse-url (format "http://www.m-w.com/cgi-bin/dictionary?book=Dictionary&va=%s" (pbs:previous-word))))

(defun pbs:previous-word (&optional n)
  (unless n (setf n 1))
  (save-excursion
    (let ((end (point)))
      (backward-word 1)
      (buffer-substring-no-properties (point) end))))


(defun pbs:google-lookup (words)
  (interactive "p")
  (browse-url (format "http://www.google.com/search?hl=en&ie=UTF-8&oe=UTF-8&q=%s&btnG=Google+Search" (pbs:previous-word))))





(defun pbs:find-doubled-words ()
  (interactive)
  (let ((seen 0)
        (deleted 0))
    (while (search-forward-regexp "\\b\\(\\w+\\)[ \t\n]+\\1\\b" (point-max) t)
      (incf seen)
      (let ((o (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put o 'face 'highlight)
        (when (y-or-n-p "Delete extra word? ")
          (incf deleted)
          (replace-match "\\1" t)
          (backward-word 1))
        (delete-overlay o)))
    (message (format "Done. Deleted %d from %d pairs." deleted seen))))


(defvar *do-not-count* 0)

(defun pbs:count-words (n)
  (interactive "p")
  (flet ((count (beg end label)
           (save-excursion
             (goto-char beg)
             (let ((count 0))
               (while (and (forward-word 1) (< (point) end))
                 (incf count))
               (decf count *do-not-count*)
               (message "%d words in %s." count label)
               count))))
    (save-excursion
      (cond
        ((= n 4)
         (save-excursion
           (outline-mark-subtree)
           (setf mark-active t)
           (let ((*do-not-count* 0))
             (count (region-beginning) (region-end) "section"))))
        ((= n 1)
         (if mark-active
             (let ((*do-not-count* 0))
               (count (region-beginning) (region-end) "region"))
             (count (point-min) (point-max) "buffer")))))))






(defun pbs:save-word-count (n)
  (interactive "p")
  (save-excursion
    (let ((file (buffer-file-name))
          (words (pbs:count-words n)))
      (when (and (= n 1) (not mark-active))
        (set-buffer (find-file-noselect "word-counts.txt"))
        (goto-char (point-max))
        (insert (format "%s %s - %d\n" (format-time-string "%Y-%m-%d %H:%M:%S") file words))
        (save-buffer)
        (message "%d words." words)))))

(defun pbs:save-start-of-day-count ()
  (interactive)
  (set (make-variable-buffer-local '*do-not-count*)
       (let ((*do-not-count* 0))
         (pbs:count-words 1))))

(defun pbs:count-words-by-character ()
  (interactive)
  (message "%d words in buffer." (/ (point-max) 5)))



(defun pbs:outline-change-levels (fn)
  (interactive)
  (save-excursion
    (let (start end)
      (unless mark-active
        (outline-mark-subtree)
        (setf mark-active t))
      (setf start (min (point) (mark)))
      (setf end (max (point) (mark)))

      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (while (search-forward-regexp outline-regexp (point-max) t)
          (beginning-of-line)
          (funcall fn)
          (end-of-line))))))

(defun pbs:promote-outline-levels ()
  (interactive)
  (pbs:outline-change-levels #'(lambda () (delete-char 1))))

(defun pbs:demote-outline-levels ()
  (interactive)
  (pbs:outline-change-levels #'(lambda () (insert "*"))))

(defun pbs:add-outline-keybindings ()
  (interactive)
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map outline-mode-map)

    (define-key map [(control ?c) right] 'pbs:demote-outline-levels)
    (define-key map [(control ?c) left] 'pbs:promote-outline-levels)

    (substitute-key-definition
     'forward-word 'pbs:forward-word map (current-global-map))

    (substitute-key-definition
     'backward-word 'pbs:backward-word map (current-global-map))

    (substitute-key-definition
     'kill-word 'pbs:kill-word map (current-global-map))

    (substitute-key-definition
     'backward-kill-word 'pbs:backward-kill-word map (current-global-map))

    (use-local-map map)))


(defun pbs:lisp-indent-region (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (lisp-indent-line)
      (beginning-of-line)
      (insert "    ")
      (while (zerop (forward-line 1))
        (lisp-indent-line)))))


(defun pbs:copy-code-to-book (start end)
  (interactive "r")
  (make-local-variable 'pbs:book-buffer)
  (unless (boundp 'pbs:book-buffer)
    (setf pbs:book-buffer (read-buffer "Book buffer: ")))
  (save-excursion
    (let ((text (buffer-substring-no-properties start end)))
      (switch-to-buffer pbs:book-buffer)
      (newline 2)
      (delete-blank-lines)
      (newline 1)
      (let ((start (point)))
        (insert text)
        (indent-code-rigidly start (point) 4))
      (newline 1)
      (delete-blank-lines))))


(defun pbs:sexp-on-one-line ()
  (interactive)
  (save-excursion
    (pbs:start-of-sexp)
    (let ((start (point)))
      (forward-sexp)
      (save-restriction
        (narrow-to-region start (point))
        (goto-char start)
        (while (zerop (forward-line 1))
          (join-line)
          (end-of-line))))))


(defun pbs:start-of-sexp ()
  (interactive)
  (unless (looking-at "\\s *\(")
    (backward-up-list 1)))


(defun pbs:proof-read ()
  (interactive)
  (search-forward-regexp "\\b\\(we\\|us\\|our\\(s\\(el\\(f\\|ves\\)\\)?\\)?\\|only\\|tk\\|above\\|below\\|last\\|which\\|that\\|\\(there[[:space:]\n]+\\(is\\|are\\)\\)\\)\\b"))

(defvar *just-pronouns* nil)

(defun pbs:proof-read ()
  (interactive)
  (with-syntax-table *normal-syntax-table*
    (search-forward-regexp (if *just-pronouns*
                             "\\b\\(we\\|us\\|our\\(s\\(el\\(f\\|ves\\)\\)?\\)?\\)\\b"
                             "\\b\\(we\\|us\\|our\\(s\\(el\\(f\\|ves\\)\\)?\\)?\\|only\\|tk\\|above\\|below\\|last\\|which\\|that\\|\\(there[[:space:]\n]+\\(is\\|are\\)\\)\\)\\b"))))


(defconst *contraction-regexp*
  "\\b\\(\\(\\(I\\|he\\|she\\|it\\|we\\|you\\|they\\)[[:space:]\n]+\\(am\\|are\\|had\\|has\\|have\\|is\\|will\\|would\\)\\)\\|\\(\\(are\\|can\\|could\\|do\\|does\\|had\\|has\\|have\\|is\\|should\\|was\\|were\\|will\\|would\\)[[:space:]\n]+not\\)\\|\\(\\(there\\|that\\)[[:space:]\n]+is\\)\\)\\b")


(defun pbs:contract ()
  "Finds potential contractions and lets you convert them."
  (interactive)
  (let ((overlay nil))
    (unwind-protect
        (with-syntax-table *normal-syntax-table*
          (while (search-forward-regexp *contraction-regexp* (point-max) t)
            (let ((text (match-string 0)))
              (setq overlay (make-overlay (match-beginning 0) (match-end 0)))
              (let ((words (split-string text  "[[:space:]\n]+")))
                (mom-highlight-and-center overlay)
                (let ((correction (apply 'pbs:contraction words)))
                  (if (y-or-n-p "Contract? ")
                    (progn
                      (goto-char (overlay-start overlay))
                      (insert correction)
                      (delete-region (+ (length correction) (overlay-start overlay)) (overlay-end overlay))
                      (delete-overlay overlay)
                      (fill-paragraph nil)))))
              (delete-overlay overlay)
              (setq overlay nil))))
      (if overlay (delete-overlay overlay)))))


(defun pbs:fix-quotes ()
  (interactive)
  (let ((overlay nil))
    (unwind-protect
        (with-syntax-table *normal-syntax-table*
          (while (search-forward-regexp "\"\\([^\"]*\\)\"" (point-max) t)
            (when (not (pbs:in-code-block))
              (setq overlay (make-overlay (match-beginning 0) (match-end 0)))
              (mom-highlight-and-center overlay)
              (let ((correction (prompt-for-quote-correction (match-string 1))))
                (if (not (zerop (length correction)))
                  (progn
                    (delete-overlay overlay)
                    (replace-match correction t)))
                (delete-overlay overlay)
                (setq overlay nil)))))
      (if overlay (delete-overlay overlay)))))

(defun prompt-for-quote-correction (text)
  (if (y-or-n-p "Change to italic? ")
    (format "\\\\i{%s}" text)))

(defun pbs:contraction (first second)
  (cond
    ((string= "am" second) (concat first "'m"))
    ((string= "are" second) (concat first "'re"))
    ((string= "had" second) (concat first "'d"))
    ((string= "has" second) (concat first "'s"))
    ((string= "have" second) (concat first "'ve"))
    ((string= "is" second) (concat first "'s"))
    ((string= "will" second) (concat first "'ll"))
    ((string= "would" second) (concat first "'d"))
    ((string= "not" second)
     (cond
       ((string= "can" first) "can't")
       ((string= "will" first) "won't")
       (t (concat first "n't"))))))


(defun mom-highlight-and-center (overlay)
  (overlay-put overlay 'face 'highlight)
  (recenter 4))






(global-set-key [(control ?c) ?p] 'pbs:proof-read)
;;(global-set-key [(control ?c) ?p] 'pbs:find-packages)
;(global-set-key [(control ?c) ?w] 'pbs:save-word-count)

(defun pbs:find-packages ()
  (interactive)
  (let ((case-fold-search nil))
    (search-forward-regexp "[\n ][A-Z]+\\([-\\.][A-Z]+\\)+")))


;; there is there are there is

;; we us our ours ourself ourselves only is are TK there is there  are

(provide 'writing)
