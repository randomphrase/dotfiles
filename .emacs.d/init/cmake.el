;; CMake mode for CMake*.txt files
(require 'cmake-mode)
(setq cmake-tab-width 4)

(when (fboundp 'subword-mode)
  (add-hook 'cmake-mode-hook (lambda ()
                               (subword-mode 1))))

(provide 'init/cmake)
