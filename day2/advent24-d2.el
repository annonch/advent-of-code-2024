(defun read-file-into-2d-array (file-path)
  "Read the file at FILE-PATH and return its contents as a 2D array.
Rows are separated by newlines, and columns by spaces."
  (with-temp-buffer
    (insert-file-contents file-path)
    (let ((lines (split-string (buffer-string) "\n" t))) ; Split by newline
      (mapcar (lambda (line)
                (split-string line " " t)) ; Split each line by space
              lines))))

(defun is-safe-? (report)
  "Determines if report is safe or not by ensuring the sequence
 is strictly monotonically increasing or decreasing by levels of
 one, two, or three."
  (setq safe t)
  (setq inc nil)
  (setq dec nil)
  (if (>= (length report) 1)
      (dotimes (i (- (length report) 1)) ;;loop over all numbers in report
	(setq diff
	      (-
	       (string-to-number (elt report i))
	       (string-to-number (elt report (+ i 1)))))
	(if (or
	     (equal diff 1)
	     (equal diff 2)
	     (equal diff 3))
	    (setq dec t)
	  (if (or
	       (equal diff -1)
	       (equal diff -2)
	       (equal diff -3))
	      (setq inc t)
	    (setq safe nil)))
	))
  (if (or
       (not safe)
       (and dec inc))
      nil t)
  )

(defun dampen (report)
  (message "Attempting dampening on %s" report)
  (let ((i 0)
	(safe nil))
    (while (and (< i (length report)) (not safe))
      (setq alt-report (vconcat (seq-subseq report 0 i)
				(seq-subseq report (+ i 1) (length report))))
      (setq safe (is-safe-? alt-report))
      (setq i (1+ i))
      (message "safe?: %s" safe))
    safe))

(message "%s" (dampen ["93" "94" "92" "93" "95" "96" "97"]))


;; (setq all-reports (read-file-into-2d-array "advent24-2-s.txt"))
(setq all-reports (read-file-into-2d-array "advent24-2.txt"))
(message "Report Data: %S" all-reports)
(setq safe-reports 0)
(setq unsafe-reports 0)

;; apply map over all-reports
(mapc (lambda (x)
	(if (is-safe-? x)
	    (setq safe-reports (+ safe-reports 1))
	  (if (dampen x)
	      (setq safe-reports (+ safe-reports 1))
	    (setq unsafe-reports (+ unsafe-reports 1)))))
      all-reports)

(message "Total number of reports: %s"
	 (length all-reports))
(message "Number of Safe Reports: %s"
	 safe-reports)
(message "Number of Un-Safe Reports: %s"
	 unsafe-reports)
