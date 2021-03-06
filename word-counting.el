;;; word-counting.el -- elisp for tracking wordcounts.
;;;
;;; Copyright (c) 2009-2021, Peter Seibel
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

;;; To use this code add a modeline like "-*- mode: wc; -*-" to
;;; the top of a file.

(defvar *wc-words-per-page* 350)

(defvar *wc-targets-file* "targets.sexp")


;; These are set as buffer local variables in the mode function. The
;; first two are loaded from a config file and the third from the word
;; count log if it exists.
(defvar *wc-total-goal*)
(defvar *wc-daily-goal*)
(defvar *wc-start-date*)


(defun wc-day ()
  (1+ (- (time-to-days (current-time)) (time-to-days *wc-start-date*))))

(defun wc-read-file-as-sexp (file)
  (with-current-buffer (find-file-noselect file)
    (if (zerop (buffer-size))
        nil
      (read (buffer-string)))))

(defun wc-save-sexp-to-file (file sexp)
  (with-current-buffer (find-file-noselect file)
    (erase-buffer)
    (print sexp (current-buffer))
    (save-buffer)))

(defun wc-data-file (name)
  (let ((basename (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    (format ".%s-%s" basename name)))

(defun wc-word-count-data-file ()
  (wc-data-file "word-counts.sexp"))

(defun wc-word-count-full-log ()
  (wc-data-file "word-counts.log"))

(defun wc-config-file ()
  (wc-data-file "config.sexp"))

(defun wc-save-word-count (words file)
  (let ((counts (wc-read-file-as-sexp file))
        (today (wc-day)))
    (if (assoc today counts)
        (setf (cdr (assoc today counts)) words)
      (setf counts (cons (cons today words) counts)))
    (wc-save-sexp-to-file file (sort counts #'(lambda (x y) (< (car x) (car y)))))))

(defun wc-log-word-count (words)
  (with-current-buffer (find-file-noselect (wc-word-count-full-log))
    (goto-char (point-max))
    (backward-word 1)
    (let ((last-logged (thing-at-point 'number)))
      (when (/= last-logged words)
        (goto-char (point-max))
        (insert (format "%d\t%d\n" (round (* (float-time) 1000)) words))
        (save-buffer)))))

(defun wc-start-date ()
  "Start date is the date of the first logged word count or today."
  (with-current-buffer (find-file-noselect (wc-word-count-full-log))
    (goto-char (point-min))
    (let ((ts (thing-at-point 'number t)))
      (if ts
          (apply 'encode-time (decode-time (/ ts 1000.0)))
        (current-time)))))

(defun wc-start-date ()
  "Start date is the date of the first logged word count or today."
  (with-current-buffer (find-file-noselect (wc-word-count-full-log))
    (goto-char (point-min))
    (let ((ts (thing-at-point 'number t)))
      (if ts
          (apply 'encode-time (decode-time (/ ts 1000.0)))
        (current-time)))))




(defun wc-words-today (count)
  (- count (wc-day-n-total (1- (wc-day)))))

(defun wc-targets (current-count)
  "Generate various targets that we can aim at."
  (let ((targets (list (cons *wc-total-goal* "Done!"))))
    (cl-flet ((add-targets (new-targets)
                           (dolist (target new-targets)
                             (destructuring-bind (target-count . description) target
                               (cond
                                ((assoc target-count targets)
                                 (let ((item (assoc target-count targets)))
                                   (unless (= target-count *wc-total-goal*)
                                  (setf (cdr item) (format "%s, %s" (cdr item) description)))))
                                (t (push target targets)))))))
      (add-targets (wc-round-targets *wc-total-goal* 1000))
      (add-targets (wc-read-file-as-sexp *wc-targets-file*))
      (add-targets (wc-round-today-targets current-count 1000 10000))
      (add-targets (wc-ptg-targets *wc-total-goal* '(25 50 75)))
      (when (> (wc-day) 1)
        (add-targets (wc-maintain-average-target current-count))
        (add-targets (wc-increase-average-targets 50 2000))))

    (sort targets #'(lambda (x y) (< (car x) (car y))))))

(defun wc-day-n-total (n)
  (let* ((in-file (wc-read-file-as-sexp (wc-word-count-data-file)))
         (latest (assoc-if #'(lambda (x) (<= x n)) (reverse in-file))))
    (if latest (cdr latest) 0)))

(defun wc-full-count ()
  (save-excursion
    (goto-char (point-min))
    (forward-line 1) ;; To account for the mode line.
    (wc-raw-count (point) (point-max))))

(defun wc-count-region ()
  (interactive)
  (let ((count (wc-raw-count (region-beginning) (region-end))))
    (message "%d words; (~%d pages)" count (ceiling count *wc-words-per-page*))))

(defun wc-raw-count (start end)
  "Count words using 'wc -w' since that's what the official Nanowrimo count seems to use. This assumes you're on a system where 'wc' is available."
  (save-excursion
    (let ((buffer-modified-p (buffer-modified-p))
          (p (point)))
      (call-process-region start end "wc" nil t nil "-w")
      (let* ((output (buffer-substring-no-properties p (point)))
             (count (car (read-from-string output))))
        (delete-region p (point))
        (set-buffer-modified-p buffer-modified-p)
        count))))

(defun wc-word-count ()
  "Count words in either the region (if active) or the whole file minus the modeline."
  (interactive)
  (if mark-active
      (wc-count-region)
    (let ((count (wc-full-count)))
      (if (zerop count)
          (message "No words yet. Better get cracking.")
        (wc-report-word-count count *wc-total-goal* *wc-daily-goal*)))))

(defun wc-round-targets (goal step)
  (cond
   ((not (plusp goal)) nil)
   (t (cons (cons goal (format "hit even %s" (wc-commify goal))) (wc-round-targets (- goal step) step)))))

(defun wc-maintain-average-target (count)
  (let* ((day (wc-day))
         (words-today (wc-words-today count))
         (average-as-of-yesterday (/ (- count words-today) (1- day))))
    (if (< words-today average-as-of-yesterday)
        (list (cons (+ count (- average-as-of-yesterday words-today))
                    "maintain average"))
      nil)))

(defun wc-increase-average-targets (step average)
  (let ((day (wc-day)))
    (cond
     ((not (plusp average)) nil)
     (t
      (cons
         (cons (* average day) (format "to increase average to %s" (wc-commify average)))
         (wc-increase-average-targets step (- average step)))))))


(defun wc-round-today-targets (current-count step max-per-day)
  (let* ((words-today (wc-words-today current-count))
         (first-round (* step (ceiling words-today step))))
    (cl-labels ((helper (i)
                   (let* ((for-today (+ (- first-round words-today) (* i step)))
                          (target (+ current-count for-today)))
                     (cond
                      ((> for-today max-per-day) nil)
                      (t  (cons (cons target (format "hit even %s today" (wc-commify (+ words-today for-today))))
                                (helper (1+ i))))))))
      (helper 0))))

(defun wc-ptg-targets (goal percentages)
  (mapcar #'(lambda (p) (cons (ceiling (* goal (/ p 100.0))) (format "%d%% done." p))) percentages))


(defun wc-report-word-count (count goal daily-goal)
  "Given count, goal, and the number of days in which we want to finish, compute a bunch of statistics and report them."
  (let* ((day (wc-day))
         (words-to-go (- goal count))
         (full-days-passed (1- (wc-day)))
         (words-today (- count (wc-day-n-total full-days-passed)))
         (previous-average (/ (wc-day-n-total full-days-passed) (float (1- day))))
         (average-so-far (/ count (float day)))
         (days-until-completion (ceiling words-to-go average-so-far))
         (next-target (find-if (lambda (x) (> (car x) count)) (wc-targets count)))
         (pages (ceiling count *wc-words-per-page*)))
    (wc-save-word-count count (wc-word-count-data-file))
    (wc-log-word-count count)
    (let ((message nil))
      (cl-flet ((say (x &rest args)
                     (let ((new (apply #'format x args)))
                       (setf message (if message (concat message " " new) new)))))
        (say "%s done today; %s." (wc-commify words-today)
             (cond
              ((= daily-goal words-today)
               "perfect")
              ((> daily-goal words-today)
               (format "%s to go" (wc-commify (- daily-goal words-today))))
              (t
               (format "%s extra" (wc-commify (- words-today daily-goal))))))
        (when next-target
          (destructuring-bind (target . description) next-target
            (say "%s %s." (wc-commify (- target count)) description)))
        (when (> (wc-day) 1)
          (say "Current average %s." (wc-commify average-so-far))
          (say "Previous average %s." (wc-commify previous-average)))
        (say "Total: %s (~%s pages) done; %s." (wc-commify count) (wc-commify pages)
             (cond
              ((zerop words-to-go)
               "on the nose")
              ((> words-to-go 0)
               (format "%s words (~%s pages) to go" (wc-commify words-to-go) (wc-commify (ceiling words-to-go *wc-words-per-page*))))
              (t
               (format "%s words (~%s pages) over" (wc-commify words-to-go) (wc-commify (ceiling (abs words-to-go) *wc-words-per-page*))))))
        (when (> goal count)
          (say "Projected completion: %s." (wc-completion-date days-until-completion)))
        (message message)))))

(defun wc-completion-date (days-until-completion)
  (let ((s (format-time-string "%B %e, %Y" (wc-completion-time days-until-completion))))
    (replace-regexp-in-string " +" " " s)))

(defun wc-completion-time (days-until-completion)
  (time-add (current-time) (seconds-to-time (* 60 60 24 days-until-completion))))

(defun wc-show-targets ()
  (interactive)
  (let* ((count (wc-full-count))
         (targets (wc-targets count))
         (start-of-day (wc-day-n-total (1- (wc-day))))
         (buffer (get-buffer-create "*wc-targets*"))
         (goal *wc-total-goal*))
    (with-current-buffer buffer
      (setf buffer-read-only nil)
      (erase-buffer)
      (dolist (target targets)
        (destructuring-bind (target . description) target
          (if (and (> target count) (or (> count goal) (>= goal target)))
              (insert (format "%7s (%s; %s today) to %s\n"
                              (wc-commify (- target count))
                              (wc-commify target)
                              (wc-commify (- target start-of-day))
                              description))))))

    (switch-to-buffer buffer)
    (setf buffer-read-only t)
    (goto-char (point-min))
    (local-set-key (kbd "q") 'bury-buffer)))

(defun wc-commify (n)
  (with-temp-buffer
    (insert (format "%s" (round n)))
    (while (> (- (point)
                 (line-beginning-position))
              (if (>= n 0) 3 4))
      (backward-char 3)
      (insert ",")
      (backward-char 1))
    (buffer-string)))


(defun wc-load-config ()
  (interactive)
  (let ((config (wc-read-file-as-sexp (wc-config-file))))
    (set (make-local-variable '*wc-total-goal*) (cdr (assoc 'total-goal config)))
    (set (make-local-variable '*wc-daily-goal*) (cdr (assoc 'daily-goal config)))
    (set (make-local-variable '*wc-start-date*) (wc-start-date))))

(define-minor-mode wc-mode
  "Mode for tracking word count progress."
  :lighter " wc"
  (wc-load-config)
  (add-hook 'after-save-hook 'wc-word-count nil t))

(provide 'word-counting)
