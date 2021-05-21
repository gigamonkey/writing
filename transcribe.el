(defvar *mplayer-process* nil)
(defvar *backup-seconds* 2)
(defvar *auto-rewind* 2) ; amount we rewind on a pause
(defvar *saved-output* ())
(defvar *paused* nil)

(defvar *late-timestamp-adjustment* 1)

(defvar *default-timestamp-replacement-fn* 'transcribe:insert-timestamp)
(defvar *timestamp-replacement-fn* ())

(defvar *initial-adaptive-backup-seconds* 1)
(defvar *adaptive-backup-seconds* *initial-adaptive-backup-seconds*)
(defvar *last-backup-timestamp* nil)
(defvar *high-water-mark* 0)

(defvar *mplayer-program* "/usr/local/bin/mplayer")

(defvar *months* '("" "January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"))

(defvar *audio-file*)

(defvar *speaker-names* ())

(defun push-timestamp-replacement-fn (fn)
  (push fn *timestamp-replacement-fn*))

(defun pop-timestamp-replacement-fn ()
  (if *timestamp-replacement-fn*
      (pop *timestamp-replacement-fn*)
    *default-timestamp-replacement-fn*))

(defun transcribe:audio-file ()
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "^Audio:\s+\\(.*\\)$" nil t)
    (match-string-no-properties 1)))

(defun transcribe:check ()
  ;; Assume we are already in a file with transcription mode on and the meta data in the file.
  (interactive)
  (transcribe:play-audio (transcribe:audio-file)))


(defun transcribe (audio-file transcript-directory)
  "Launch mplayer to play the file in slave mode so we can control it."
  (interactive "fAudio file: \nDTranscript directory: ")
  (let ((text-file (concat (file-name-as-directory transcript-directory) (file-name-nondirectory (file-name-sans-extension audio-file)) ".txt")))
    (find-file text-file)
    (transcription-mode)
    (transcribe:play-audio)
    (when (zerop (buffer-size))
      (transcribe:insert-metadata))))

(defun transcribe:play-audio (audio-file)
  "Launch mplayer to play the audio file so we can control it."
  (setf *audio-file* audio-file)
  (setf *mplayer-process*
        (start-process
         "mplayer"
         "*mplayer*"
         *mplayer-program*
         "-quiet"
         "-slave"
         "-idle"
         (file-truename audio-file)))
  (set-process-filter *mplayer-process* 'transcribe:grok-output)
  (setf *paused* nil))


(defun transcribe:actually-insert-metadata (filename timestamp)
  (cond
   ((string-match "\\([[:digit:]]\\{4\\}\\)\\([[:digit:]]\\{2\\}\\)\\([[:digit:]]\\{2\\}\\)-\\(.*\\)-[[:digit:]]*" filename)
    (let ((year (match-string 1 filename))
          (month (match-string 2 filename))
          (day (match-string 3 filename))
          (subject (capitalize (subst-char-in-string (string-to-char "-") (string-to-char " ") (match-string 4 filename)))))
      (insert "-*- mode: transcription; coding: utf-8 -*-\n\n")
      (insert "Subject:     " subject "\n")
      (insert "Interviewer: Peter Seibel" "\n")
      (insert "Date:        " day " " (nth (string-to-number month) *months*) " " year "\n")
      (insert "Location:    " (read-string "Location: ") "\n")
      (insert "MP3 File:    " filename "\n")
      (insert "Total time:  " (transcribe:hh-mm-ss timestamp) "\n\n")
      (insert "[00:00:00] ")))))

(defun transcribe:insert-metadata ()
  (interactive)
  (transcribe:do "get_time_length"))

(defun transcribe:newline ()
  "Start a new paragraph, inserting a timestamp."
  (interactive)
  (newline 2)
  (transcribe:request-timestamp))

(defun transcribe:timestamp-paragraph ()
  (interactive)
  (push-timestamp-replacement-fn 'transcribe:insert-late-timestamp)
  (transcribe:request-timestamp))

(defun transcribe:insert-late-timestamp (timestamp)
  (backward-paragraph)
  (next-line)
  (insert (transcribe:timestamp-string (- timestamp *late-timestamp-adjustment*)))
  (insert " ")
  (when *speaker-names* (transcribe:do-insert-speaker))
  (fill-paragraph nil))

(defun transcribe:insert-speaker ()
  (interactive)
  (backward-paragraph)
  (next-line)
  (beginning-of-line)
  (search-forward-regexp "\[[0-9:]+\] " (point-max) t)
  (transcribe:do-insert-speaker)
  (fill-paragraph nil))

(defun transcribe:find-previous-speaker ()
  (interactive)
  (save-excursion
    (backward-paragraph 2)
    (next-line)
    (beginning-of-line)
    (search-forward-regexp "\[[0-9:]+\] \\([^:]+\\):" nil t)
    (match-string-no-properties 1)))

(defun transcribe:do-insert-speaker ()
  (let ((prev (transcribe:find-previous-speaker)))
    (cond
     ((string= prev (car *speaker-names*))
      (message "Previous %s same as %s. Swapping." prev (car *speaker-names*))
      (transcribe:swap-speakers))
     (t
      (message "Previous %s already different from as %s. Not swapping." prev (car *speaker-names*)))))
  (let ((speaker (car *speaker-names*)))
    (insert (format "%s: " speaker))
    (transcribe:swap-speakers)))

(defun transcribe:swap-speakers ()
  (interactive)
  (setf *speaker-names* (cons (cdr *speaker-names*) (car *speaker-names*)))
  (message "Speakers: %s" *speaker-names*))

(defun transcribe:find-speakers ()
  (interactive)
  (let (first second)
    (save-excursion
      (goto-char (point-min))
      (when (search-forward-regexp "^Subject:\s+\\(.*\\)\s*$" nil t)
        (setf first (match-string-no-properties 1))
      (when (search-forward-regexp "Interviewer:\s+\\(.*\\)\s*$" nil t)
        (setf second (match-string-no-properties 1)))))
    (cond
     ((and first second)
      (transcribe:set-speakers first second)
      (message "Speakers: %s" *speaker-names*))
     (t
      (message "No speakers found")))))


(defun transcribe:set-speakers (first second)
  (interactive "sFirst speaker: \nsSecond speaker: ")
  (setf *speaker-names* (cons first second)))

(defun transcribe:unset-speakers ()
  (interactive)
  (setf *speaker-names* nil))

(defun transcribe:adaptive-backup ()
  (interactive)
  (push-timestamp-replacement-fn  'transcribe:adaptive-backup/process-timestamp)
  (transcribe:request-timestamp))

(defun transcribe:adaptive-backup/process-timestamp (timestamp)
  (message "Got timestamp %s when *last-backup-timestamp* is %s" (transcribe:hh-mm-ss timestamp) (transcribe:hh-mm-ss (or *last-backup-timestamp* 0)))
  (cond
   ((and *last-backup-timestamp* (<= timestamp *last-backup-timestamp*))
    (incf *adaptive-backup-seconds*))
   (t
    (setf *last-backup-timestamp* timestamp)
    (setf *adaptive-backup-seconds* *initial-adaptive-backup-seconds*)))
  (transcribe:goto (- *last-backup-timestamp* *adaptive-backup-seconds*)))

(defun transcribe:set-high-water-mark ()
  (interactive)
  (push-timestamp-replacement-fn  'transcribe:set-high-water-mark/process-timestamp)
  (transcribe:request-timestamp))

(defun transcribe:clear-high-water-mark ()
  (interactive)
  (setf *high-water-mark* (transcribe:parse-hh-mm-ss (transcribe:find-preceeding-timestamp))))

(defun transcribe:set-high-water-mark/process-timestamp (timestamp)
  (message "Setting *high-water-mark* to %s" (transcribe:hh-mm-ss timestamp))
  (setf *high-water-mark* timestamp))

(defun transcribe:toggle-pause ()
  (interactive)
  (when (not *paused*) (transcribe:move (- *auto-rewind*)))
  (transcribe:do "pause")
  (setf *paused* (not *paused*)))

(defun transcribe:jump ()
  (interactive)
  (let ((seconds (transcribe:parse-hh-mm-ss (transcribe:find-preceeding-timestamp))))
    (transcribe:goto seconds)))

(defun transcribe:back-to-high-water-mark ()
  (interactive)
  (transcribe:goto *high-water-mark*))

(defun transcribe:backup-timestamp ()
  (interactive)
  (transcribe:move-timestamp -1))

(defun transcribe:advance-timestamp ()
  (interactive)
  (transcribe:move-timestamp 1))

(defun transcribe:fast-forward ()
  (interactive)
  (transcribe:move *backup-seconds*))

(defun transcribe:rewind ()
  (interactive)
  (transcribe:move (- *backup-seconds*)))

(defun transcribe:move (amount)
  (transcribe:do "seek %d" amount)
  (push-timestamp-replacement-fn
   (lambda (timestamp)
     (message (transcribe:timestamp-string timestamp))))
  (transcribe:request-timestamp))

(defun transcribe:goto (seconds)
  (interactive "nTime in seconds: ")
  (message (transcribe:timestamp-string seconds))
  (transcribe:do "seek %d 2" seconds))

(defun transcribe:request-timestamp ()
  (transcribe:do "get_time_pos"))

(defun transcribe:quit ()
  (interactive)
  (when *mplayer-process*
    (transcribe:do "quit")))

(defun transcribe:kill-buffer-function ()
  (when (eq major-mode 'transcription-mode)
    (transcribe:quit)))

(defun transcribe:do (fmt &rest args)
  (when (zerop (process-exit-status *mplayer-process*))
    (let ((command (apply 'format (format "pausing_keep %s\n" fmt) args)))
      (process-send-string *mplayer-process* command))))

(defun transcribe:grok-output (process output)
  (cond
   ((string-match "ANS_TIME_POSITION=\\(.*\\)" output)
    (let* ((pos (match-string 1 output))
           (timestamp (round (string-to-number pos))))
      (funcall (pop-timestamp-replacement-fn) timestamp)))
   ((string-match "ANS_LENGTH=\\(.*\\)" output)
    (let* ((pos (match-string 1 output))
           (timestamp (round (string-to-number pos))))
      (transcribe:actually-insert-metadata *audio-file* timestamp)))))


(defun transcribe:insert-timestamp (timestamp)
  (setf *high-water-mark* timestamp)
  (insert (transcribe:timestamp-string timestamp))
  (insert " ")
  (when *speaker-names* (transcribe:do-insert-speaker)))

(defun transcribe:timestamp-string (timestamp)
  (format "[%s]" (transcribe:hh-mm-ss timestamp)))

(defun transcribe:hh-mm-ss (total-seconds)
  (let* ((seconds (mod total-seconds 60))
         (total-minutes (floor total-seconds 60))
         (minutes (mod total-minutes 60))
         (hours (floor total-minutes 60)))
    (format "%02d:%02d:%02d" hours minutes seconds)))

(defun transcribe:parse-hh-mm-ss (string)
  (destructuring-bind (hh mm ss) (mapcar 'string-to-number (split-string string ":"))
    (+ ss (* mm 60) (* hh 60 60))))

(defun transcribe:find-preceeding-timestamp ()
  (save-excursion
    (backward-paragraph 1)
    (search-forward-regexp "\\[\\([0-9]*:[0-9][0-9]:[0-9][0-9]\\)\\]")
    (match-string-no-properties 1)))

(defun transcribe:move-timestamp (amount)
  (let* ((old (transcribe:parse-hh-mm-ss (transcribe:find-preceeding-timestamp)))
         (new (+ old amount)))
    (save-excursion
      (backward-paragraph 1)
      (re-search-forward "\\[\\([0-9]*:[0-9][0-9]:[0-9][0-9]\\)\\]")
      (replace-match (format "[%s]" (transcribe:hh-mm-ss new))))
    (setf *high-water-mark* new)
    (transcribe:jump)))

(defun transcribe:fix-space ()
  (interactive)
  (backward-word 1)
  (delete-backward-char 1)
  (forward-char 1)
  (insert " "))

(defun transcribe:start-transcribing ()
  (interactive)
  (push-timestamp-replacement-fn
   (lambda (timestamp)
     (let ((time (current-time)))
       (save-excursion
         (save-window-excursion
                (find-file (expand-file-name "~/coders-at-work/time-data/transcription-log.sexp"))
                (goto-char (point-max))
                (insert (format "(:start %d :tape-time %d)\n" (+ (ash (first time) 16) (second time)) timestamp)))))))
  (transcribe:request-timestamp))

(defun transcribe:end-transcribing ()
  (interactive)
  (push-timestamp-replacement-fn
   (lambda (timestamp)
     (let ((time (current-time)))
       (save-excursion
         (save-window-excursion
                (find-file (expand-file-name "~/coders-at-work/time-data/transcription-log.sexp"))
                (goto-char (point-max))
                (insert (format "(:end %d :tape-time %d)\n" (+ (ash (first time) 16) (second time)) timestamp)))))))
  (transcribe:request-timestamp))

(defun transcribe:remove-paragraph ()
  (interactive)
  (save-excursion
    (forward-paragraph 1)
    (backward-char 1)
    (let ((start (point)))
      (forward-paragraph 2)
      (search-forward "}")
      (kill-region start (point)))))

(defvar transcription-mode-syntax-table
  (let ((st (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    st)
  "Syntax table used while in `text-mode'.")


(define-derived-mode transcription-mode
  text-mode "Transcription" "Mode for transcribing audio."
  :syntax-table transcription-mode-syntax-table
  (auto-fill-mode t)
  (smart-quote-mode t)
  (set-buffer-file-coding-system 'utf-8 t t)
  (make-local-variable '*mplayer-process*)
  (make-local-variable '*backup-seconds*)
  (make-local-variable '*saved-output*)
  (make-local-variable '*paused*)
  (make-local-variable '*timestamp-replacement-fn*)
  (make-local-variable '*adaptive-backup-seconds*)
  (make-local-variable '*high-water-mark*)
  (make-local-variable '*last-backup-timestamp*)
  (make-local-variable '*speaker-names*)
  (transcribe:find-speakers))

(add-hook 'kill-buffer-hook 'transcribe:kill-buffer-function)

(define-key transcription-mode-map (kbd "RET") 'transcribe:newline)
(define-key transcription-mode-map (kbd "M-RET") 'transcribe:timestamp-paragraph)
(define-key transcription-mode-map (kbd "C-<return>") 'transcribe:insert-speaker)
(define-key transcription-mode-map (kbd "C-.") 'transcribe:insert-speaker)

(define-key transcription-mode-map (kbd "M-SPC") 'transcribe:toggle-pause)
(define-key transcription-mode-map (kbd "C-M-SPC") 'transcribe:adaptive-backup)
(define-key transcription-mode-map (kbd "S-C-M-SPC") 'transcribe:jump)
;;(define-key transcription-mode-map (kbd "C-<return>") 'transcribe:set-high-water-mark)
(define-key transcription-mode-map (kbd "C-M-<return>") 'transcribe:clear-high-water-mark)
(define-key transcription-mode-map (kbd "C-c SPC") 'transcribe:fix-space)

(define-key transcription-mode-map (kbd "C-<up>") 'transcribe:back-to-high-water-mark)
(define-key transcription-mode-map (kbd "C-<right>") 'transcribe:fast-forward)
(define-key transcription-mode-map (kbd "C-<left>") 'transcribe:rewind)
(define-key transcription-mode-map (kbd "C-c C-b") 'transcribe:backup-timestamp)
(define-key transcription-mode-map (kbd "C-c C-f") 'transcribe:advance-timestamp)

(provide 'transcribe)
