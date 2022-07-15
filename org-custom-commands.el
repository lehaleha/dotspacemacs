
;;
;; Primitive tests of dynblocks
;;

(defun org-dblock-write:mytest1 (params)
  (let ((start (or (plist-get params :start) "no-start"))
        (end (or (plist-get params :end) "no-end")))
    (insert (format "Start: %s, end: %s" start end)))
)


(defun org-dblock-write:mantest2 (params)
  (let ((fmt (or (plist-get params :format) "%d. %m. %Y")))
    (insert "Last block update at: "
            (format-time-string fmt))))

(defun org-dblock-write:mytest2 (params)
  "Inserts all L2 headers from current org file."
  (insert (mapconcat 'identity (org-map-entries (lambda () (org-element-property :title (org-element-at-point))) "LEVEL<4" 'file) "\n"))
  )

;;
;; Impl 1 - via org-map-entries
;;

;;
;; Entrypoint for dynblock.
;;
;; Notes
;; - First sketch, not used, left for ref/tests.
;; - Filter:
;;   - "timegoal" - works for tags.
;;   - "DAILY_TIME_GOAL>0" - works for properties
(defun org-dblock-write:all-time-goal-headers (params)
  "Inserts all headers from current org file which have DAILY_TIME_GOAL property."
  (insert (mapconcat 'identity (org-map-entries (lambda () (org-element-property :title (org-element-at-point))) "DAILY_TIME_GOAL>0" 'file) "\n"))
  )

;;
;; Entrypoint for dynblock.
;;
;; Print time goals
(defun org-dblock-write:print-timegoals (params)
  "Prints time goals and clocked-in time for all entries from
   current org file which have DAILY_TIME_GOAL property."

  ;; Start/end are expected to be there. Otherwise it would use week of our birthdays %)
  (let* ((tstart (org-matcher-time (or (plist-get params :start) "<1983-10-11 Mon>")))
        (tend (org-matcher-time (or (plist-get params :end) "1983-10-17 Sun")))
        (scope (or (plist-get params :scope) 'file))
        (days (+(- (time-to-days tend) (time-to-days tstart)) 1)))

    ;;(insert (format "Dbg: Days: %d\n\n" days))

    ;; Print out table.
    (insert (format "Time goals progress at [%s].\nPeriod of %d days: from [%s] to [%s].\n"
                    (format-time-string "%d-%m-%Y %a %H:%M")
                    days
                    (format-time-string "%d-%m-%Y %a" tstart)
                    (format-time-string "%d-%m-%Y %a" tend)))
    (insert "|-+-+-+-|\n")
    (insert "| Area/Task    | Target | Clocked-in | % complete | Status | \n")
    (insert "|-+-+-+-|\n")
    (insert (mapconcat 'identity
                       (org-map-entries
                        (lambda ()
                          (ll-get-timegoal-line days tstart tend)
                          )
                        "DAILY_TIME_GOAL>0" scope)
                       "\n"))
    (insert "\n|-+-+-+-|")
    (org-table-align))
)


(defun ll-get-title ()
  "Returns formatted title of the org-element at point"
  (format "- %s" (org-element-property :title (org-element-at-point)))
  )

;; TODO:
;; - Handle target period.
;; - optimize perf - call org-entry-properties and look up for all info
(defun ll-get-timegoal-line (days tstart tend)
  "Returns timegoal status for an org-element at point"
  (let* ((goal (* (org-duration-to-minutes (org-entry-get (point) "DAILY_TIME_GOAL")) days))
         (clocked (myorg-clock-sum-current-item tstart tend))
         (dailygoal (/ goal days))
         (statusStr (if (< clocked goal)
                      (format "%s (%s/day) left"
                        (org-duration-from-minutes (- goal clocked))
                        (org-duration-from-minutes (/ (- goal clocked) days)))
                       "Done")))
    (format
     ;; |name|goal|clocked-in (per-day)|delta (per-day)|%|
     "| %s | %s (%s/day) | %s (%s/day) | %s | %s |"
     (ll-get-title)
     (org-duration-from-minutes goal)
     (org-duration-from-minutes dailygoal)
     (org-duration-from-minutes clocked)
     (org-duration-from-minutes (/ clocked days))
     (format "%d%%" (*(/ clocked goal) 100))
     statusStr)
  )
)

;;
;; My variant of the org-clock-sum-current-item, which receives start/end dates.
;;
;; Notes
;; - It's almost full copy of the original, with only difference in call to org-clock-sum.
;;
(defun myorg-clock-sum-current-item (tstart tend)
  "Return time, clocked on current item within given interval."
  (save-excursion
    (save-restriction
      (if (and (featurep 'org-inlinetask)
	             (or (org-inlinetask-at-task-p)
		               (org-inlinetask-in-task-p)))
	        (narrow-to-region (save-excursion (org-inlinetask-goto-beginning) (point))
			                      (save-excursion (org-inlinetask-goto-end) (point)))
	      (org-narrow-to-subtree))
      (org-clock-sum tstart tend)
      org-clock-file-total-minutes)))
