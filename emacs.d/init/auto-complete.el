(require 'auto-complete)
(add-to-list 'ac-dictionary-directories (expand-file-name "ac-dict" user-emacs-directory))

(require 'auto-complete-config)
(ac-config-default)

(provide 'init/auto-complete)
