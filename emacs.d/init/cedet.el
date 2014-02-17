;;
;; CEDET Stuff
;;
;; See also: https://gist.github.com/3930120

;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;; IMPORTANT: For Emacs >= 23.2, you must place this *before* any
;; CEDET component (including EIEIO) gets activated by another 
;; package (Gnus, auth-source, ...).

;;(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
;;(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)

;; Enable Semantic
(semantic-mode 1)

;; TODO: Needed?
(require 'semantic/ia)
(require 'semantic/lex-spp)

;; Ensure semantic can get info from gnu global
(require 'semantic/db-global)
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)

;; don't use semantic on remote files:
(add-to-list 'semantic-inhibit-functions 'remote-buffer-p)

(defun my-cedet-hook ()
  (local-set-key [(control c) (j)] 'semantic-ia-fast-jump)
  (local-set-key [(control c) (q)] 'semantic-ia-show-doc)
  (local-set-key [(control c) (s)] 'semantic-ia-show-summary)

  (local-set-key [(control c) (left)] 'senator-fold-tag)
  (local-set-key [(control c) (right)] 'senator-unfold-tag)

  ;; Use auto-complete with semantic
  (add-to-list 'ac-sources 'ac-source-semantic)
  )
(add-hook 'c-mode-common-hook 'my-cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'my-cedet-hook)

(defun my-cedet-c-hook ()
  (local-set-key [(control c) (=)] 'semantic-decoration-include-visit)
  (local-set-key [(control c) (p)] 'semantic-analyze-proto-impl-toggle)

  (local-set-key [(control c) (control r)] 'semantic-symref)
  )
(add-hook 'c-mode-common-hook 'my-cedet-c-hook)

(provide 'init/cedet)
