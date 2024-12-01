(defun read-2-column-file (file)
  "Read a file with two columns of integers into two separate arrays."
  (let ((col1 '())  ;; List for the first column
        (col2 '()))  ;; List for the second column
    ;; Open the file for reading
    (with-temp-buffer
      (insert-file-contents file)  ;; Read the contents of the file
      (goto-char (point-min))  ;; Start from the beginning of the buffer
      (while (not (eobp))  ;; Loop through the lines
        (let ((line (thing-at-point 'line t)))  ;; Read a line
          (when line  ;; If the line is not nil
            (let ((tokens (split-string line)))  ;; Split the line by whitespace
              (push (string-to-number (nth 0 tokens)) col1)  ;; First column value
              (push (string-to-number (nth 1 tokens)) col2))))  ;; Second column value
        (forward-line 1)))  ;; Move to the next line
    ;; Return the two columns as lists
    (list (nreverse col1) (nreverse col2))))

;;(setq ar1 [3 4 2 1 3 3]) ;; array 1
;;(setq ar2 [4 3 5 3 9 3]) ;; array 2


(setq file "advent24-1.txt")
;;(setq file "advent24-1-s.txt")

(setq ars (read-2-column-file file))

(setq ar1 (car ars))
(setq ar2 (car (cdr ars)))

(message "Original ar1: %S" ar1)
(message "Original ar2: %S" ar2)

(setq ar1 (sort ar1 #'<))
(setq ar2 (sort ar2 #'<))

(message "New ar1: %S" ar1)
(message "New ar2: %S" ar2)

(setq toffset 0)
(message "\narray length: %d\n" (length ar1))

(defun update-hash-table (hash-table key)
  "Set KEY in HASH-TABLE to 1 or increment its value if it already exists."
  (let ((current-value (gethash key hash-table 0)))
    (puthash key (+ 1 current-value) hash-table)))

(setq elf-hash-table (make-hash-table :test 'equal)) 
(dotimes (i (length ar1))  ;; Loop over indices 0 to (length-1)
  (setq a1 (elt ar1 i))
  (setq a2 (elt ar2 i))
  
  (message "a1 Element at index %d: %d" i a1)
  (message "a2 Element at index %d: %d" i a2)
  (setq diff  (abs (- a1 a2)))
  (message "Difference at index %d: %d" i diff)
  (update-hash-table elf-hash-table a2)
  (message "Table value of %d: %d" a2 (gethash a2 elf-hash-table))
  (setq toffset (+ toffset diff))
  )

(setq similarity-score 0)

(dotimes (i (length ar1))  ;; Loop over indices 0 to (length-1)
  (setq a1 (elt ar1 i))
  (if (gethash a1 elf-hash-table) (setq similarity-score (+ similarity-score (* (gethash a1 elf-hash-table) a1))) nil)
  (message "Incrementing Similarity Score is %d" similarity-score))

;;(setq similarity-score (sum-hash-table-values elf-hash-table))

(message "\n\nTotal diff: %d" toffset)
(message "Similarity Score: %d\n\n\n" similarity-score)
