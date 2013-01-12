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

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   (cons 'magit melpa)
   (cons 'session melpa)
   (cons 'cmake-mode marmalade)
   (cons 'gtags marmalade)
   ;; (cons 'exec-path-from-shell melpa)
   ;; (cons 'paredit melpa)
   ;; (cons 'move-text melpa)
   ;; (cons 'gist melpa)
   ;; (cons 'htmlize melpa)
   ;; (cons 'elisp-slime-nav melpa)
   ;; ;(cons 'elnode marmalade)
   ;; (cons 'slime-js marmalade)
   ;; (cons 'git-commit-mode melpa)
   ;; (cons 'gitconfig-mode melpa)
   ;; (cons 'gitignore-mode melpa)
   ;; (cons 'clojure-mode melpa)
   ;; (cons 'clojure-test-mode melpa)
   ;; (cons 'nrepl melpa)
   ))
   
(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))


;; Lets start with a smattering of sanity
(require 'init/sane-defaults)


;; Start session mode - TODO: redundant?
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;; require-soft macro stolen from http://www.emacswiki.org/emacs/LocateLibrary
(defmacro require-soft (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if `require' fails."
  `(require ,feature ,file 'noerror))

; Favourite modes
(eval-after-load 'org '(require 'init/org))

(require 'init/tramp)
(require 'init/cedet)
(require 'init/c++)

;; Setup key bindings
(require 'init/key-bindings)

;; Map files to modes
(require 'init/mode-mappings)

;; Load local stuff here
(let ((f (expand-file-name "local.el" user-emacs-directory)))
  (if (file-readable-p f)
      (load f)))

;;
;; All custom variables live in here
;;
;; Put this last so that we can customise variables defined by modes loaded above
;;
(load custom-file t)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))
