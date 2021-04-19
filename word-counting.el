;;; nanowrimo.el -- elisp for tracking wordcount while working on
;;; NaNoWriMo novel.
;;;
;;; Copyright (c) 2009, Peter Seibel
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

;;; To use this code add a modeline like "-*- mode: nanowrimo; -*-" to
;;; the top of a file.

(defmacro defparameter (name value &optional docstring)
  `(progn
     (defvar ,name nil ,docstring)
     (setq-default ,name ,value)))

;; Number of words needed to win!
(defparameter *nanowrimo-goal* 50000)

;; Total number of days available
(defparameter *max-days* 30)

;; First day
(defparameter *start-date* (encode-time 0 0 0 1 11 (nth 5 (decode-time))))

;; File containing intermediate targets. Maintained by hand.
(defparameter *targets-file* "targets.sexp")

(defparameter *words-per-page* 250)

(defun nanowrimo-day ()
  (1+ (- (time-to-days (current-time)) (time-to-days *start-date*))))

(defun nanowrimo-read-file-as-sexp (file)
  (with-current-buffer (find-file-noselect file)
    (if (zerop (buffer-size))
        nil
      (read (buffer-string)))))

(defun nanowrimo-save-sexp-to-file (file sexp)
  (with-current-buffer (find-file-noselect file)
    (erase-buffer)
    (print sexp (current-buffer))
    (save-buffer)))

(defun nanowrimo-word-count-data-file ()
  (format ".%s-word-counts.sexp" (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))

(defun nanowrimo-word-count-full-log ()
  (format ".%s-word-counts-log.sexp" (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))

(defun nanowrimo-save-word-count (words file)
  (let ((counts (nanowrimo-read-file-as-sexp file))
        (today (nanowrimo-day)))
    (if (assoc today counts)
        (setf (cdr (assoc today counts)) words)
      (setf counts (cons (cons today words) counts)))
    (nanowrimo-save-sexp-to-file file (sort counts #'(lambda (x y) (< (car x) (car y)))))))

(defun nanowrimo-log-word-count (words)
  (with-current-buffer (find-file-noselect (nanowrimo-word-count-full-log))
    (goto-char (point-max))
    (insert (format "%d\t%d" (round (* (float-time) 1000)) words))
    (save-buffer)))

(defun nanowrimo-words-today (count)
  (- count (nanowrimo-day-n-total (1- (nanowrimo-day)))))

(defun nanowrimo-targets (current-count)
  (let ((targets (list (cons *nanowrimo-goal* "Win!"))))
    (cl-flet ((add-targets (new-targets)
                           (dolist (target new-targets)
                             (destructuring-bind (target-count . description) target
                               (cond
                                ((assoc target-count targets)
                                 (let ((item (assoc target-count targets)))
                                   (unless (= target-count *nanowrimo-goal*)
                                  (setf (cdr item) (format "%s, %s" (cdr item) description)))))
                                (t (push target targets)))))))
      (add-targets (nanowrimo-average-targets *nanowrimo-goal* *max-days* (nanowrimo-day)))
      (add-targets (nanowrimo-daily-targets *nanowrimo-goal* (nanowrimo-day)))
      (add-targets (nanowrimo-round-targets (* 1000 (ceiling (+ current-count (* 5 1000)) 1000)) 1000))
      (add-targets (nanowrimo-competitive-targets))
      (add-targets (nanowrimo-round-today-targets current-count 1000 10000))
      (add-targets (nanowrimo-todays-pace-target (nanowrimo-day-n-total (1- (nanowrimo-day))) (nanowrimo-day)))
      (if (> (nanowrimo-day) 1)
          (add-targets (nanowrimo-maintain-average-target current-count)))
      (add-targets (nanowrimo-increase-average-targets 50 2000))
      ;;(add-targets (nanowrimo-round-numbers-to-go *nanowrimo-goal* (- *nanowrimo-goal* 500) current-count 500))
      )

    (sort targets #'(lambda (x y) (< (car x) (car y))))))

(defun nanowrimo-day-n-total (n)
  (let* ((in-file (nanowrimo-read-file-as-sexp (nanowrimo-word-count-data-file)))
         (latest (assoc-if #'(lambda (x) (<= x n)) (reverse in-file))))
    (if latest (cdr latest) 0)))

(defun nanowrimo-words-on-day (n)
  (- (nanowrimo-day-n-total n) (nanowrimo-day-n-total (1- n))))

(defun nanowrimo-full-count ()
  (save-excursion
    (goto-char (point-min))
    (forward-line 1) ;; To account for the mode line.
    (nanowrimo-raw-count (point) (point-max))))

(defun nanowrimo-count-region ()
  (interactive)
  (let ((count (nanowrimo-raw-count (region-beginning) (region-end))))
    (message "%d words; (~%d pages)" count (ceiling count *words-per-page*))))

(defun nanowrimo-raw-count (start end)
  "Count words using 'wc -w' since that's what the official NaNoWriMo count seems to use. This assumes you're on a system where 'wc' is available."
  (save-excursion
    (let ((buffer-modified-p (buffer-modified-p))
          (p (point)))
      (call-process-region start end "wc" nil t nil "-w")
      (let ((count (read-from-whole-string (buffer-substring-no-properties p (point)))))
        (delete-region p (point))
        (set-buffer-modified-p buffer-modified-p)
        count))))

(defun nanowrimo-word-count ()
  "Count words in either the region (if active) or the whole file minus the modeline."
  (interactive)
  (if (not (< 0 (nanowrimo-day) (1+ *max-days*)))
      (message "Hey! No fair writing outside November.")
    (if mark-active
        (nanowrimo-count-region)
      (let ((count (nanowrimo-full-count)))
        (if (zerop count)
            (message "No words yet. Better get cracking.")
          (nanowrimo-report-word-count count *nanowrimo-goal* *max-days*))))))

(defun nanowrimo-competitive-targets ()
  (let ((place 0))
    (mapcar #'(lambda (x) (cons (car x) (format "%s (#%d)" (cdr x) (incf place))))
            (nanowrimo-read-file-as-sexp *targets-file*))))

(defun nanowrimo-average-target (goal target-day today)
  (ceiling (* today (/ goal (float target-day)))))

(defun nanowrimo-average-targets (goal start-target-day today)
  (cond
   ((<= start-target-day today) nil)
   (t (cons (cons (nanowrimo-average-target goal start-target-day today) (format "get on pace to finish by 11/%d" start-target-day))
            (nanowrimo-average-targets goal (1- start-target-day) today)))))

(defun nanowrimo-round-targets (goal step)
  (cond
   ((not (plusp goal)) nil)
   (t (cons (cons goal (format "hit even %s" (nanowrimo-commify goal))) (nanowrimo-round-targets (- goal step) step)))))

(defun nanowrimo-daily-targets (goal day)
  (cond
   ((= day *max-days*) nil)
   (t
    (let ((per-day (/ goal (float *max-days*))))
      (cons
       (cons (ceiling (/ (* goal day) (float *max-days*)))
             (format "hit 11/%s %s words-per-day goal" day (nanowrimo-commify per-day)))
       (nanowrimo-daily-targets goal (1+ day)))))))

(defun nanowrimo-maintain-average-target (count)
  (let* ((day (nanowrimo-day))
         (yesterday-total (nanowrimo-day-n-total (1- day)))
         (words-today (- count yesterday-total))
         (average-as-of-yesterday (/ yesterday-total (1- day))))
    (if (< words-today average-as-of-yesterday)
        (list (cons (+ count (- average-as-of-yesterday words-today))
                    "maintain average"))
      nil)))

(defun nanowrimo-increase-average-targets (step average)
  (let ((day (nanowrimo-day)))
    (cond
     ((not (plusp average)) nil)
     (t
      (cons
         (cons (* average day) (format "to increase average to %s" (nanowrimo-commify average)))
         (nanowrimo-increase-average-targets step (- average step)))))))


(defun nanowrimo-round-today-targets (current-count step max-per-day)
  (let* ((words-today (nanowrimo-words-today current-count))
         (first-round (* step (ceiling words-today step))))
    (cl-labels ((helper (i)
                   (let* ((for-today (+ (- first-round words-today) (* i step)))
                          (target (+ current-count for-today)))
                     (cond
                      ((> for-today max-per-day) nil)
                      (t  (cons (cons target (format "hit even %s today" (nanowrimo-commify (+ words-today for-today))))
                                (helper (1+ i))))))))
      (helper 0))))

(defun nanowrimo-todays-pace-target (start-of-day today)
  (let ((to-go (- *nanowrimo-goal* start-of-day)))
    (cl-labels ((helper (i)
             (cond
              ((> i (- *max-days* today)) nil)
              (t
               (let ((to-finish-in-pace (ceiling (/ to-go (float (1+ i))))))
                 (cons
                  (cons (+ start-of-day to-finish-in-pace)
                        (format "hit new steady pace for 11/%d finish" (+ today i)))
                  (helper (1+ i))))))))
      (helper 1))))

(defun nanowrimo-round-numbers-to-go (goal x current-count step)
  (cond
   ((<= x current-count) nil)
   (t (cons
       (cons x (format "even %s to go" (nanowrimo-commify (- goal x))))
       (nanowrimo-round-numbers-to-go goal (- x step) current-count step)))))




(defun nanowrimo-report-word-count (count goal days)
  "Given count, goal, and the number of days in which we want to finish, compute a bunch of statistics and report them. If today's word count is over the average needed to finish in `days' will decrease the number of days and try again."
  (let* ((day (nanowrimo-day))
         (words-to-go (- goal count))
         (full-days-passed (1- (nanowrimo-day)))
         (days-left (- days day))
         (words-per-day (ceiling words-to-go days-left))
         (words-today (- count (nanowrimo-day-n-total full-days-passed)))
         (average-so-far (/ count (float day)))
         (days-until-win (ceiling words-to-go average-so-far))
         (spare-days (- days-left days-until-win))
         (projected-word-count (round (* average-so-far *max-days*)))
         (words-by-midnight (ceiling (* (/ goal (float days)) day)))
         (next-target (find-if (lambda (x) (> (car x) count)) (nanowrimo-targets count)))
         (words-left-today (- words-by-midnight count))
         (pages (ceiling count *words-per-page*)))
    (nanowrimo-save-word-count count (nanowrimo-word-count-data-file))
    (nanowrimo-log-word-count count)
    (let ((message (format "%s total (~%s pages)" (nanowrimo-commify count) (nanowrimo-commify pages))))
      (cl-flet ((say (x &rest args)
              (setf message (concat message " " (apply #'format x args)))))
      (when (> goal count)
        (say "%s to go." (nanowrimo-commify words-to-go)))
      (say "%s done today." (nanowrimo-commify words-today))
      (when (> words-left-today 0)
        (say "%s left for today." (nanowrimo-commify words-left-today)))
      (say "%s words per day after today." (nanowrimo-commify words-per-day))
      (when next-target
        (destructuring-bind (target . description) next-target
          (say "%s %s." (nanowrimo-commify (- target count)) description)))
      (say "Current average %s." (nanowrimo-commify average-so-far))
      (when (> goal count)
        (say "Projected win: %s." (nanowrimo-win-date days-until-win)))
      (message message)))))

(defun nanowrimo-win-date (days-until-win)
  (let ((s (format-time-string "%d %B %Y" (nanowrimo-win-time days-until-win))))
    (if (string= (substring s 0 1) "0") (substring s 1) s)))

(defun nanowrimo-win-time (days-until-win)
  (time-add (current-time) (seconds-to-time (* 60 60 24 days-until-win))))

(defun nanowrimo-show-targets ()
  (interactive)
  (let* ((count (nanowrimo-full-count))
         (targets (nanowrimo-targets count))
         (start-of-day (nanowrimo-day-n-total (1- (nanowrimo-day))))
         (buffer (get-buffer-create "*nanowrimo-targets*")))
    (with-current-buffer buffer
      (setf buffer-read-only nil)
      (erase-buffer)
      (dolist (target targets)
        (destructuring-bind (target . description) target
          (if (and (> target count) (or (> count *nanowrimo-goal*) (>= *nanowrimo-goal* target)))
              (insert (format "%7s (%s; %s today) to %s\n"
                              (nanowrimo-commify (- target count))
                              (nanowrimo-commify target)
                              (nanowrimo-commify (- target start-of-day))
                              description))))))

    (switch-to-buffer buffer)
    (setf buffer-read-only t)
    (goto-char (point-min))
    (local-set-key (kbd "q") 'bury-buffer)))

(defun nanowrimo-commify (n)
  (with-temp-buffer
    (insert (format "%s" (round n)))
    (while (> (- (point)
                 (line-beginning-position))
              (if (>= n 0) 3 4))
      (backward-char 3)
      (insert ",")
      (backward-char 1))
    (buffer-string)))

(defun nanowrimo-insert-section-break ()
  (interactive)
  (newline)
  (insert "§")
  (newline)
  (newline)
  (save-excursion
    (previous-line 2)
    (center-paragraph)))

(defvar nanowrimo-mode-syntax-table
  (let ((st (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?– "w" st)
    st)
  "Syntax table used while in `text-mode'.")

(define-derived-mode nanowrimo-mode
  outline-mode "NaNoWriMo" "Mode for working on NaNoWriMo novel."
  :syntax-table nanowrimo-mode-syntax-table
  (smart-quote-mode t)
  (set (make-local-variable 'outline-regexp) "*+ ")
  (set (make-local-variable 'outline-level)
       (lambda ()
         (if (looking-at "*+") (- (match-end 0) (match-beginning 0)))))
  (set (make-local-variable '*smart-quote-use-mdash*) nil)
  (add-hook 'after-save-hook 'nanowrimo-word-count nil t)
  (set-input-method 'ucs)
  (inactivate-input-method)
  (set-buffer-file-coding-system 'utf-8 t t))

(define-key nanowrimo-mode-map (kbd "C-c w") 'nanowrimo-word-count)
(define-key nanowrimo-mode-map (kbd "C-c t") 'nanowrimo-show-targets)
(define-key nanowrimo-mode-map (kbd "C-c s") 'nanowrimo-insert-section-break)


(provide 'nanowrimo)
