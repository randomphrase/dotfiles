;; Stolen from http://www.emacswiki.org/emacs/TransposeWindows
(defun transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(defun transpose-windows-dir (dir &optional arg)
  (interactive "P")
  (let* ((this-win (selected-window))
         (this-win-buffer (window-buffer this-win))
         (next-win (windmove-find-other-window dir arg this-win))
         (next-win-buffer (window-buffer next-win)))
    (cond ((null next-win)
           (error "No window %s from selected window" dir))
          ((and (window-minibuffer-p next-win)
                (not (minibuffer-window-active-p next-win)))
           (error "Minibuffer is inactive"))
          (t
           (set-window-buffer next-win this-win-buffer)
           (set-window-buffer this-win next-win-buffer)
           (select-window next-win)
           ))
    ))

(defun transpose-windows-left (&optional arg)
  "Transpose this window with the window to the left"
  (interactive "P")
  (transpose-windows-dir 'left arg)
  )

(defun transpose-windows-right (&optional arg)
  "Transpose this window with the window to the left"
  (interactive "P")
  (transpose-windows-dir 'right arg)
  )
