
(defun org-dblock-write:mytest1 (params)
  (insert "Fooo")
  )


(defun org-dblock-write:mantest2 (params)
  (let ((fmt (or (plist-get params :format) "%d. %m. %Y")))
    (insert "Last block update at: "
            (format-time-string fmt))))

(defun org-dblock-write:mytest2 (params)
  "Inserts all L2 headers from current org file."
  (insert (mapconcat 'identity (org-map-entries (lambda () (org-element-property :title (org-element-at-point))) "LEVEL<4" 'file) "\n"))
  )

;; Filter:
;; - "timegoal" - works for tags.
;; - "TIMEGOAL>0" - works for properties
(defun org-dblock-write:all-time-goal-headers (params)
  "Inserts all headers from current org file which have TIMEGOAL property."
  (insert (mapconcat 'identity (org-map-entries (lambda () (org-element-property :title (org-element-at-point))) "TIMEGOAL>0" 'file) "\n"))
  )

;; Print time goals
;; TODO
;; - formatted
(defun org-dblock-write:print-timegoals (params)
  "Inserts all headers from current org file which have TIMEGOAL property."
  (insert "|-+-+-+-|\n")
  (insert "| Area/Task    | Goal   | Clocked-in |    % |\n")
  (insert "|-+-+-+-|\n")
  (insert (mapconcat 'identity
                     (org-map-entries
                      (lambda ()
                        (ll-get-timegoal-line)
                        )
                      "TIMEGOAL>0" 'file)
                     "\n"))
  (insert "\n|-+-+-+-|\n")
  (org-table-align)
  )

(defun ll-get-title ()
  "Returns formatted title of the org-element at point"
  (format "- %s" (org-element-property :title (org-element-at-point)))
  )

;; TODO:
;; - Handle target period. 
;; - optimize perf - call org-entry-properties and look up for all info
(defun ll-get-timegoal-line ()
  "Returns timegoal status for an org-element at point in form |<name>|<goal>|<actual>|"
  (let ((goal (org-duration-to-minutes (org-entry-get (point) "TIMEGOAL")))
        (clocked (org-clock-sum-current-item))

        )
    (format
     "| %s | %s | %s | %s|"
     (ll-get-title)
     (org-duration-from-minutes goal)
     (org-duration-from-minutes clocked)
     (format "%d%%" (*(/ clocked goal) 100))
     )
    )
)
