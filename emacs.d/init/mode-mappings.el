;; load makefile-mode for *.mak files and Doxyfile files
(add-to-list 'auto-mode-alist '("\\.mak$" . makefile-mode))
(add-to-list 'auto-mode-alist '("Doxyfile" . makefile-mode))

;; C++ Files
(add-to-list 'auto-mode-alist '("\\.[it]pp$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.[it]xx$" . c++-mode))

;; CMake files
(autoload 'cmake-mode "cmake-mode")
(add-to-list 'auto-mode-alist '("CMake[A-Za-z0-9-]*\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

;; XML files
(add-to-list 'auto-mode-alist '("\\.plist$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xml$"   . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsl$"   . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsd$"   . nxml-mode))
;(add-to-list 'auto-mode-alist '("\\.php[34]?$"   . nxml-mode))

;; Shell script
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

;; C# source
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;; Relax NG Compact
(autoload 'rnc-mode "rnc-mode")
(setq auto-mode-alist       
      (cons '("\\.rnc\\'" . rnc-mode) auto-mode-alist))

;; SSH config files
(autoload 'ssh-config-mode "ssh-config-mode" t)
(add-to-list 'auto-mode-alist '(".ssh/config\\'"  . ssh-config-mode))
(add-to-list 'auto-mode-alist '("sshd?_config\\'" . ssh-config-mode))
(add-hook 'ssh-config-mode-hook 'turn-on-font-lock)

;; Markdown files
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
;(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; CSS mode
;; (autoload 'css-mode "css-mode")
;; (setq auto-mode-alist       
;;       (cons '("\\.css\\'" . css-mode) auto-mode-alist))

;; Javascript mode for .js files
;; (autoload 'js2-mode "js2" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Transparently open .gpg encrypted files
(require 'epa-file)
(epa-file-enable)

(provide 'init/mode-mappings)
