;; CMake mode for CMake*.txt files
(defun my-cmake-hook ()
  (setq cmake-tab-width 4)
  (subword-mode 1)
  (local-set-key (kbd "C-h c") 'cmake-help-command)
  )

(add-hook 'cmake-mode-hook 'my-cmake-hook)

(provide 'init/cmake)
