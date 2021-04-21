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

(defmacro defparameter (name value &optional docstring)
  `(progn
     (defvar ,name nil ,docstring)
     (setq-default ,name ,value)))

;; Number of words needed to win!
(defparameter *wc-goal* 50000)

;; Total number of days available
(defparameter *wc-max-days* 30)

;; First day
(defparameter *wc-start-date* (encode-time 0 0 0 1 11 (nth 5 (decode-time))))


;; File containing intermediate targets. Maintained by hand.
(defparameter *wc-targets-file* "targets.sexp")

(defparameter *wc-words-per-page* 250)

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

(defun wc-word-count-data-file ()
  (format ".%s-word-counts.sexp" (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))

(defun wc-word-count-full-log ()
  (format ".%s-word-counts-log.sexp" (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))

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
    (insert (format "%d\t%d" (round (* (float-time) 1000)) words))
    (save-buffer)))

(defun wc-words-today (count)
  (- count (wc-day-n-total (1- (wc-day)))))

(defun wc-targets (current-count)
  (let ((targets (list (cons *wc-goal* "Win!"))))
    (cl-flet ((add-targets (new-targets)
                           (dolist (target new-targets)
                             (destructuring-bind (target-count . description) target
                               (cond
                                ((assoc target-count targets)
                                 (let ((item (assoc target-count targets)))
                                   (unless (= target-count *wc-goal*)
                                  (setf (cdr item) (format "%s, %s" (cdr item) description)))))
                                (t (push target targets)))))))
      (add-targets (wc-average-targets *wc-goal* *wc-max-days* (wc-day)))
      (add-targets (wc-daily-targets *wc-goal* (wc-day)))
      (add-targets (wc-round-targets (* 1000 (ceiling (+ current-count (* 5 1000)) 1000)) 1000))
      (add-targets (wc-competitive-targets))
      (add-targets (wc-round-today-targets current-count 1000 10000))
      (add-targets (wc-todays-pace-target (wc-day-n-total (1- (wc-day))) (wc-day)))
      (if (> (wc-day) 1)
          (add-targets (wc-maintain-average-target current-count)))
      (add-targets (wc-increase-average-targets 50 2000))
      ;;(add-targets (wc-round-numbers-to-go *wc-goal* (- *wc-goal* 500) current-count 500))
      )

    (sort targets #'(lambda (x y) (< (car x) (car y))))))

(defun wc-day-n-total (n)
  (let* ((in-file (wc-read-file-as-sexp (wc-word-count-data-file)))
         (latest (assoc-if #'(lambda (x) (<= x n)) (reverse in-file))))
    (if latest (cdr latest) 0)))

(defun wc-words-on-day (n)
  (- (wc-day-n-total n) (wc-day-n-total (1- n))))

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
        (wc-report-word-count count *wc-goal* *wc-max-days*)
        ;(message "%s words." (wc-commify count))
        ))))

(defun wc-competitive-targets ()
  (let ((place 0))
    (mapcar #'(lambda (x) (cons (car x) (format "%s (#%d)" (cdr x) (incf place))))
            (wc-read-file-as-sexp *wc-targets-file*))))

(defun wc-average-target (goal target-day today)
  (ceiling (* today (/ goal (float target-day)))))

(defun wc-average-targets (goal start-target-day today)
  (cond
   ((<= start-target-day today) nil)
   (t (cons (cons (wc-average-target goal start-target-day today) (format "get on pace to finish by 11/%d" start-target-day))
            (wc-average-targets goal (1- start-target-day) today)))))

(defun wc-round-targets (goal step)
  (cond
   ((not (plusp goal)) nil)
   (t (cons (cons goal (format "hit even %s" (wc-commify goal))) (wc-round-targets (- goal step) step)))))

(defun wc-daily-targets (goal day)
  (cond
   ((= day *wc-max-days*) nil)
   (t
    (let ((per-day (/ goal (float *wc-max-days*))))
      (cons
       (cons (ceiling (/ (* goal day) (float *wc-max-days*)))
             (format "hit 11/%s %s words-per-day goal" day (wc-commify per-day)))
       (wc-daily-targets goal (1+ day)))))))

(defun wc-maintain-average-target (count)
  (let* ((day (wc-day))
         (yesterday-total (wc-day-n-total (1- day)))
         (words-today (- count yesterday-total))
         (average-as-of-yesterday (/ yesterday-total (1- day))))
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

(defun wc-todays-pace-target (start-of-day today)
  (let ((to-go (- *wc-goal* start-of-day)))
    (cl-labels ((helper (i)
             (cond
              ((> i (- *wc-max-days* today)) nil)
              (t
               (let ((to-finish-in-pace (ceiling (/ to-go (float (1+ i))))))
                 (cons
                  (cons (+ start-of-day to-finish-in-pace)
                        (format "hit new steady pace for 11/%d finish" (+ today i)))
                  (helper (1+ i))))))))
      (helper 1))))

(defun wc-round-numbers-to-go (goal x current-count step)
  (cond
   ((<= x current-count) nil)
   (t (cons
       (cons x (format "even %s to go" (wc-commify (- goal x))))
       (wc-round-numbers-to-go goal (- x step) current-count step)))))




(defun wc-report-word-count (count goal days)
  "Given count, goal, and the number of days in which we want to finish, compute a bunch of statistics and report them. If today's word count is over the average needed to finish in `days' will decrease the number of days and try again."
  (let* ((day (wc-day))
         (words-to-go (- goal count))
         (full-days-passed (1- (wc-day)))
         (days-left (- days day))
         (words-per-day (ceiling words-to-go days-left))
         (words-today (- count (wc-day-n-total full-days-passed)))
         (previous-average (/ (wc-day-n-total full-days-passed) (float (1- day))))
         (average-so-far (/ count (float day)))
         (days-until-win (ceiling words-to-go average-so-far))
         (spare-days (- days-left days-until-win))
         (projected-word-count (round (* average-so-far *wc-max-days*)))
         (words-by-midnight (ceiling (* (/ goal (float days)) day)))
         (next-target (find-if (lambda (x) (> (car x) count)) (wc-targets count)))
         (words-left-today (- words-by-midnight count))
         (pages (ceiling count *wc-words-per-page*)))
    (wc-save-word-count count (wc-word-count-data-file))
    (wc-log-word-count count)
    (let ((message (format "%s total (~%s pages)" (wc-commify count) (wc-commify pages))))
      (cl-flet ((say (x &rest args)
              (setf message (concat message " " (apply #'format x args)))))
      (when (> goal count)
        (say "%s to go." (wc-commify words-to-go)))
      (say "%s done today." (wc-commify words-today))
      (when (> words-left-today 0)
        (say "%s left for today." (wc-commify words-left-today)))
      (say "%s words per day after today." (wc-commify words-per-day))
      (when next-target
        (destructuring-bind (target . description) next-target
          (say "%s %s." (wc-commify (- target count)) description)))
      (say "Current average %s." (wc-commify average-so-far))
      (say "Previous average %s." (wc-commify previous-average))
      (when (> goal count)
        (say "Projected completion: %s." (wc-win-date days-until-win)))
      (message message)))))

(defun wc-win-date (days-until-win)
  (let ((s (format-time-string "%d %B %Y" (wc-win-time days-until-win))))
    (if (string= (substring s 0 1) "0") (substring s 1) s)))

(defun wc-win-time (days-until-win)
  (time-add (current-time) (seconds-to-time (* 60 60 24 days-until-win))))

(defun wc-show-targets ()
  (interactive)
  (let* ((count (wc-full-count))
         (targets (wc-targets count))
         (start-of-day (wc-day-n-total (1- (wc-day))))
         (buffer (get-buffer-create "*wc-targets*")))
    (with-current-buffer buffer
      (setf buffer-read-only nil)
      (erase-buffer)
      (dolist (target targets)
        (destructuring-bind (target . description) target
          (if (and (> target count) (or (> count *wc-goal*) (>= *wc-goal* target)))
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


(defvar wc-mode-syntax-table
  (let ((st (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?– "w" st)
    st)
  "Syntax table used while in `text-mode'.")


(defun wc-set-targets (goal days start)
  (interactive "nGoal: \nnDays: \nsStart date: ")
  (set (make-local-variable '*wc-goal*) goal)
  (set (make-local-variable '*wc-max-days*) days)
  (set (make-local-variable '*wc-start-date*) (apply 'encode-time `(0 0 0 ,@(reverse (mapcar #'string-to-number (split-string start "-")))))))


(define-minor-mode wc-mode
  "Mode for tracking word count progress."
  :lighter " wc"
  (add-hook 'after-save-hook 'wc-word-count nil t)
)


;(define-key wc-mode-map (kbd "C-c w") 'wc-word-count)
;(define-key wc-mode-map (kbd "C-c t") 'wc-show-targets)


(provide 'word-counting)
