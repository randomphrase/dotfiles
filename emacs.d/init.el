;; Alastair's emacs file
;;
;; Overall structure, much code, and several ideas copied shamelessly from https://github.com/magnars/.emacs.d

(add-to-list 'load-path user-emacs-directory)

;; Set up appearance early to avoid momentary display
(require 'init/appearance)

; Set up lisp, exec, info paths
(require 'init/paths)

;; Needs to be set up early - replaces some built-in libraries
(load-file (expand-file-name "cedet/cedet-devel-load.el" extern-lisp-dir))

;; Do package management
(require 'cask)
(cask-initialize)
   
;; Start session mode - TODO: redundant?
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;; require-soft macro stolen from http://www.emacswiki.org/emacs/LocateLibrary
(defmacro require-soft (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if `require' fails."
  `(require ,feature ,file 'noerror))

;; Lets start with a smattering of sanity
(require 'init/sane-defaults)

; Favourite modes
(require 'init/semantic)
(require 'init/ede)
(require 'init/c++)
(require 'init/cmake)
(require 'init/flymake)
(require 'init/tramp)
(require 'init/smartparens)
(require 'init/company)
(require 'init/ibuffer)

(eval-after-load 'org '(require 'init/org))
(require 'init/markdown)

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
