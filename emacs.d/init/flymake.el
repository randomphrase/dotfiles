;; Use ede-compdb to configure flymake
(require 'flymake)
(setq flymake-allowed-file-name-masks
      (cons '("\\.[ch]\\(pp\\)?$" ede-compdb-flymake-init)
            flymake-allowed-file-name-masks))

;; This makes emacs on Mac sad...
(setq flymake-gui-warnings-enabled nil)

;; Enable flymake
(add-hook 'find-file-hook 'flymake-find-file-hook)

;; Use flymake-cursor to show error at cursor
(require 'flymake-cursor)

(provide 'init/flymake)
