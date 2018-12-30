(defparameter *cells* '((4 4) (4 5) (5 4) (6 4) (7 5)))

(defun print-map (cells)
  (loop for row from 1 to 30
    do (loop for column from 1 to 30
	 do (if (member (list row column) cells :test #'equal)
	     (princ " X ")
	     (princ "   ")))
    (princ #\newline)))

(defun find-all-neighbour-cells (cell)
  (list (list (+ (car cell) 1) (cadr cell))
	(list (+ (car cell) 1) (+ (cadr cell) 1))
	(list (+ (car cell) 1) (- (cadr cell) 1))
	(list (- (car cell) 1) (cadr cell))
	(list (- (car cell) 1) (+ (cadr cell) 1))
	(list (- (car cell) 1) (- (cadr cell) 1))
	(list (car cell) (- (cadr cell) 1))
	(list (car cell) (+ (cadr cell) 1))))

(defun find-alive-neighbours (cell cells)
  (let ((perturbations (find-all-neighbour-cells cell)))
    (intersection perturbations cells :test #'equal)))

(defun find-dead-neighbours (cell cells)
  (let ((perturbations (find-all-neighbour-cells cell)))
    (set-difference perturbations cells :test #'equal)))

(defun find-all-potential-cells (cells)
  (let ((potential-cells nil))
    (dolist (cell cells)
      (dolist (pot-cell (find-dead-neighbours cell cells))
	(push pot-cell potential-cells)))
    (remove-duplicates potential-cells :test #'equal)))

(defun find-cells-to-awaken (cells)
  (let ((potential-cells (find-all-potential-cells cells))
	(awaken-cells nil))
    (dolist (potential-cell potential-cells)
      (if (eq (list-length (find-alive-neighbours potential-cell cells)) 3)
	  (push potential-cell awaken-cells)))
    (remove-duplicates awaken-cells :test #'equal)))

(defun find-cells-to-kill (cells)
  (let ((cells-to-kill nil))
    (dolist (cell cells)
      (if (or (> (list-length (find-alive-neighbours cell cells)) 3)
	   (< (list-length (find-alive-neighbours cell cells)) 2))
	   (push cell cells-to-kill)))
    (remove-duplicates cells-to-kill :test #'equal)))

(defun next-iteration ()
  (let ((kill-cells (find-cells-to-kill *cells*))
	(awaken-cells (find-cells-to-awaken *cells*)))
    (dolist (cell awaken-cells)
      (push cell *cells*))
    (remove-duplicates *cells* :test #'equal)
    (defparameter *cells* (set-difference *cells* kill-cells))
    (print-map *cells*)))

(defun start-game ()
  (loop
     (next-iteration)
     (sleep 1)))
