;; Alastair's emacs file
;;
;; Overall structure, much code, and several ideas copied shamelessly from https://github.com/magnars/.emacs.d

(add-to-list 'load-path user-emacs-directory)

;; Set up appearance early to avoid momentary display
(require 'init/appearance)

; Set up lisp, exec, info paths
(require 'init/paths)

;; Setup packages
(require 'init/package)

;; ;; Install extensions if they're missing
;; (defun init--install-packages ()
;;   (packages-install
;;    (cons 'exec-path-from-shell melpa)
;;    (cons 'magit melpa)
;;    (cons 'paredit melpa)
;;    (cons 'move-text melpa)
;;    (cons 'gist melpa)
;;    (cons 'htmlize melpa)
;;    (cons 'elisp-slime-nav melpa)
;;    ;(cons 'elnode marmalade)
;;    (cons 'slime-js marmalade)
;;    (cons 'git-commit-mode melpa)
;;    (cons 'gitconfig-mode melpa)
;;    (cons 'gitignore-mode melpa)
;;    (cons 'clojure-mode melpa)
;;    (cons 'clojure-test-mode melpa)
;;    (cons 'nrepl melpa)))

;; (condition-case nil
;;     (init--install-packages)
;;   (error
;;    (package-refresh-contents)
;;    (init--install-packages)))


;; Lets start with a smattering of sanity
(require 'init/sane-defaults)


;; Start session mode - TODO: redundant?
(require 'session)
(add-hook 'after-init-hook 'session-initialize)


; Favourite modes
(eval-after-load 'org '(require 'init/org))

(require 'init/tramp)
(require 'init/cedet)
(require 'init/c++)

;; Setup key bindings
(require 'init/key-bindings)

;; Map files to modes
(require 'init/mode-mappings)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;;
;; All custom variables live in here
;;
;; Put this last so that we can customise variables defined by modes loaded above
;;
(load custom-file t)
