(defun dump-maze (maze)
  (princ "     ")
  (dotimes (c (maze-cols maze))
    (princ (format "%4d " c)))
  (terpri)

  (dotimes (r (maze-rows maze))

    (princ (format "%3d: " r))

    (dotimes (c (maze-cols maze))
      (princ (if (maze-is-set maze r c 'ceiling) "c" "."))
      (princ (if (maze-is-set maze r c 'wall) "w" "."))
      (princ (if (maze-is-set maze r c 'visited) "v" "."))
      (princ (if (maze-is-set maze r c 'solution) "s" "."))
      (princ " "))

    (terpri)))

(defun clear-maze-visited (m)
  (let ((rows (1- (maze-rows m)))
        (cols (1- (maze-cols m))))

    (dotimes (r rows)
      (dotimes (c cols)
        (maze-unset m r c 'visited)))))

(defun dig-maze-recursive (maze row col)
  (setq max-specpdl-size (* max-specpdl-size 2))
  (setq max-lisp-eval-depth (* max-lisp-eval-depth 2))
  
  (maze-set maze row col 'visited)
  (let ((p (shuffle (to-visit maze row col))))
    (while p
      (let ((r (caar p))
            (c (cdar p)))
        (make-passage maze row col r c)
        (dig-maze-recursive maze r c))
      (setq p (shuffle (to-visit maze row col))))))

