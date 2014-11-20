(eval-after-load "markdown-mode"
  ;; Remap M-arrow keys to C-M-arrow
  `(dolist (K '("M-<left>" "M-<right>" "M-<up>" "M-<down>"))
    (let ((from (kbd K))
          (to (kbd (concat "C-" K))))
      
      (define-key markdown-mode-map to (lookup-key markdown-mode-map from))
      (define-key markdown-mode-map from nil)
      )))
  
(provide 'init/markdown)
