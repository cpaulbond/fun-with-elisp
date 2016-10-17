(require 'cl-lib)

(cl-defstruct maze rows cols data)

(defmacro maze-pt (w r c)
  `(+ (* (mod ,r (maze-rows ,w)) (maze-cols ,w))
      (mod ,c (maze-cols ,w))))

(defmacro maze-ref (w r c)
  `(aref (maze-data ,w) (maze-pt ,w ,r ,c)))

(defun new-maze (rows cols)
  (setq rows (1+ rows)
        cols (1+ cols))
  (let ((m (make-maze :rows rows :cols cols :data (make-vector (* rows cols) nil))))
    
    (dotimes (r rows)
      (dotimes (c cols)
        (setf (maze-ref m r c) (copy-sequence '(wall ceiling)))))

    (dotimes (r rows)
      (maze-set m r (1- cols) 'visited))

    (dotimes (c cols)
      (maze-set m (1- rows) c 'visited))

    (maze-unset m 0 0 'ceiling) ;; Maze Entrance
    (maze-unset m (1- rows) (- cols 2) 'ceiling) ;; Maze Exit

    m))

(defun maze-is-set (maze r c v)
  (member v (maze-ref maze r c)))

(defun maze-set (maze r c v)
  (let ((cell (maze-ref maze r c)))
    (when (not (member v cell))
      (setf (maze-ref maze r c) (cons v cell)))))

(defun maze-unset (maze r c v)
  (setf (maze-ref maze r c) (delete v (maze-ref maze r c))))

(defun print-maze (maze)
  (princ (format "rows=%d,cols=%d\n" (1- (maze-rows maze)) (1- (maze-cols maze))))
  (dotimes (r (1- (maze-rows maze)))

    (dotimes (c (1- (maze-cols maze)))
      (princ (if (maze-is-set maze r c 'ceiling) "+---" "+   ")))
    (princ "+")
    (terpri)

    (dotimes (c (1- (maze-cols maze)))
      (princ (if (maze-is-set maze r c 'wall) "|   " "    ")))
    (princ "|")
    (terpri))

  (dotimes (c (1- (maze-cols maze)))
    (princ (if (maze-is-set maze (1- (maze-rows maze)) c 'ceiling) "+---" "+   ")))
  (princ "+")
  (terpri))

(defun dump-maze (maze)
  (princ "     ")
  (dotimes (c (maze-cols maze))
    (princ (format "%3d " c)))
  (terpri)

  (dotimes (r (maze-rows maze))

    (princ (format "%3d: " r))

    (dotimes (c (maze-cols maze))
      (princ (if (maze-is-set maze r c 'ceiling) "c" "."))
      (princ (if (maze-is-set maze r c 'wall) "w" "."))
      (princ (if (maze-is-set maze r c 'visited) "v" "."))
      (princ " "))

    (terpri)))

(defun shuffle (lst)
  (sort lst (lambda (a b) (= 1 (random 2)))))

(defun to-visit (maze row col)
  (let (empty)
    (dolist (p '((0 . +1) (0 . -1) (+1 . 0) (-1 . 0)))
      (let ((r (+ row (car p)))
            (c (+ col (cdr p))))
      (unless (maze-is-set maze r c 'visited)
        (push (cons r c) empty))))
    (shuffle empty)))

(defun make-passage (maze r1 c1 r2 c2)
  (if (= r1 r2)
      (if (< c1 c2)
          (maze-unset maze r2 c2 'wall) ; right
        (maze-unset maze r1 c1 'wall))  ; left
    (if (< r1 r2)
        (maze-unset maze r2 c2 'ceiling)   ; up
      (maze-unset maze r1 c1 'ceiling))))  ; down
      
(defun dig-maze-recursive (maze row col)
  (setq max-specpdl-size (* max-specpdl-size 2))
  (setq max-lisp-eval-depth (* max-lisp-eval-depth 2))
  
  (maze-set maze row col 'visited)
  (let ((p (to-visit maze row col)))
    (while p
      (let ((r (caar p))
            (c (cdar p)))
        (make-passage maze row col r c)
        (dig-maze-recursive maze r c))
      (setq p (to-visit maze row col)))))

(defun dig-maze-non-recursive (maze row col)
  (let (backup
        (run 0))
    (maze-set maze row col 'visited)
    (push (cons row col) backup)
    (while backup
      (setq run (1+ run))
      (when (> run (/ (+ row col) 3))
        (setq run 0)
        (setq backup (shuffle backup)))
      (setq row (caar backup)
            col (cdar backup))
      (let ((p (to-visit maze row col)))
        (if p
            (let ((r (caar p))
                  (c (cdar p)))
              (make-passage maze row col r c)
              (maze-set maze r c 'visited)
              (push (cons r c) backup))
          (pop backup)
          (setq backup (shuffle backup))
          (setq run 0))))))

(defun generate ()
  (let* ((rows (string-to-number (elt command-line-args-left 0)))
         (cols (string-to-number (elt command-line-args-left 1)))
         (m (new-maze rows cols)))
    ;;(dig-maze-recursive m (random rows) (random cols))
    (dig-maze-non-recursive m (random rows) (random cols))
    (print-maze m)))

(defun ceilings (line)
  (let (rtn
        (i 1))
    (while (< i (length line))
      (push (eq ?- (elt line i)) rtn)
      (setq i (+ i 4)))
    (nreverse rtn)))

(defun walls (line)
  (let (rtn
        (i 0))
    (while (< i (length line))
      (push (eq ?| (elt line i)) rtn)
      (setq i (+ i 4)))
    (nreverse rtn)))

(defun parse-maze (file-name)
  (let ((rtn)
        (lines (with-temp-buffer
                 (insert-file-contents-literally file-name)
                 (split-string (buffer-string) "\n" t))))
    (while lines
      (push (ceilings (pop lines)) rtn)
      (push (walls (pop lines)) rtn))
    (nreverse rtn)))

(defun read-maze (file-name)
  (let* ((raw (parse-maze file-name))
         (rows (- (length raw) 2))
         (cols (/ (length (car raw)) 2))
         (maze (new-maze rows cols)))
    (princ (format "rows=%d,cols=%d\n" rows cols))
    maze))
  

(defun solve ()
  (let ((maze (read-maze (elt command-line-args-left 0))))
    (princ maze)))

         
(provide 'maze)
