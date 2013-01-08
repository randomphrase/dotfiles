;;
;; CEDET Stuff
;;


;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;; IMPORTANT: For Emacs >= 23.2, you must place this *before* any
;; CEDET component (including EIEIO) gets activated by another 
;; package (Gnus, auth-source, ...).
(load-file (expand-file-name "cedet/cedet-devel-load.el" extern-lisp-dir))

;; Add further minor-modes to be enabled by semantic-mode.
;; See doc-string of `semantic-default-submodes' for other things
;; you can use here.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t)


(require 'semantic/ia)
(require 'semantic/bovine/gcc)

;; (require 'semantic-ia)

;; MacPorts installs headers here, make sure semantic knows about them:
(let ((dir "/opt/local/include/"))
  (if (file-directory-p dir)
      (progn
        (semantic-reset-system-include 'c-mode)
        (semantic-add-system-include dir 'c-mode)
        (semantic-reset-system-include 'c++-mode)
        (semantic-add-system-include dir 'c++-mode)
        )))

;; Ensure semantic can get info from gnu global
(require 'semantic/db-global)
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)

;; (add-hook 'semantic-init-hooks (lambda ()
;;                                  (imenu-add-to-menubar "Tokens")))

;; Adds semantic analyze display for speedbar
(add-hook 'speedbar-load-hook (lambda () (require 'semantic-sb)))

;; don't use semantic on remote files:
(add-to-list 'semantic-inhibit-functions
             (lambda () (and buffer-file-name (file-remote-p buffer-file-name))))

(defun my-cedet-hook ()
  (local-set-key [(control c) (tab)] 'semantic-ia-complete-symbol-menu)
  (local-set-key [(control c) (\?)] 'semantic-ia-complete-symbol)
  ;;
  (local-set-key [(control c) (\>)] 'semantic-complete-analyze-inline)
  (local-set-key [(control c) (=)] 'semantic-decoration-include-visit)

  (local-set-key [(control c) (j)] 'semantic-ia-fast-jump)
  (local-set-key [(control c) (q)] 'semantic-ia-show-doc)
  (local-set-key [(control c) (s)] 'semantic-ia-show-summary)
  (local-set-key [(control c) (p)] 'semantic-analyze-proto-impl-toggle)
  )
;;(add-hook 'semantic-init-hooks 'my-cedet-hook)
(add-hook 'lisp-mode-hook 'my-cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'my-cedet-hook)
;; (add-hook 'erlang-mode-hook 'my-cedet-hook)

;; Don't parse these files...
;; (add-hook 'semantic--before-fetch-tags-hook
;;           (lambda () (if (string-match
;;                           "^c:/program files/boost/boost_\[0-9_\]+/boost/preprocessor/\\(repetition\\|seq\\)/"
;;                           (buffer-file-name))
;;                          nil
;;                        t)))


;;
;; EDE
;;

(global-ede-mode t)

(defun my-locate-pch-header (name dir)
  (cond
   ((string= "globalPch.hpp" name)
    (concat dir "pchNone/globalPch.hpp"))
   ((string-match "^\\([a-zA-Z]+\\)Pch.hpp$" name)
    (concat dir (match-string 1 name) "/pchNone/" name))
   ))

(defun my-locate-project (&optional dir)
  "Return a full file name to the project file stored in dir, or nil"
  (let ((cmakepath (expand-file-name "CMakeLists.txt" dir)))
    (if (and (not (file-remote-p cmakepath))
             (file-exists-p cmakepath))
        cmakepath
      nil)))

(defun my-load-project (dir)
  "Load a project of type `cpp-root' for the directory DIR.
     Return nil if there isn't one."
  (ede-cpp-root-project (file-name-nondirectory dir)
                        :locate-fcn 'my-locate-pch-header
                        :file (expand-file-name "CMakeLists.txt" dir)
                        :include-path '( "/" )
                        :system-include-path (list (expand-file-name "external" dir) )
;;;                       :spp-table '( ( "_MSC_VER" . "1400" ) )
                        ))

(add-to-list 'ede-project-class-files
     	     (ede-project-autoload "cpp-root"
                                   :name "Repo Projects"
                                   :file 'ede-cpp-root
                                   :proj-file 'my-locate-project
                                   :proj-root 'ede-cpp-root-project-root
                                   :load-type 'my-load-project
                                   :class-sym 'ede-cpp-root)
     	     t)


;; contrib stuff
(load-file (expand-file-name "cedet/contrib/cedet-contrib-load.el" extern-lisp-dir))

(require 'eassist (expand-file-name "cedet/contrib/eassist.el" extern-lisp-dir))
(add-to-list 'eassist-header-switches '("cxx" . ("hxx" "hpp" "h")))
(add-to-list 'eassist-header-switches '("hxx" . ("cxx" "cpp")))
(add-to-list 'eassist-header-switches '("ipp" . ("hxx" "hpp" "h")))
(add-to-list 'eassist-header-switches '("hpp" . ("cxx" "cpp" "ipp")))


(provide 'init/cedet)
