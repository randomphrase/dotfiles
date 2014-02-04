;;
;; CEDET Stuff
;;
;; See also: https://gist.github.com/3930120

;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;; IMPORTANT: For Emacs >= 23.2, you must place this *before* any
;; CEDET component (including EIEIO) gets activated by another 
;; package (Gnus, auth-source, ...).
(when (load-file (expand-file-name "cedet/cedet-devel-load.el" extern-lisp-dir))

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

  ;; contrib stuff
  (load-file (expand-file-name "cedet/contrib/cedet-contrib-load.el" extern-lisp-dir))

  (require 'eassist (expand-file-name "cedet/contrib/eassist.el" extern-lisp-dir))
  (add-to-list 'eassist-header-switches '("cxx" . ("hxx" "hpp" "h")))
  (add-to-list 'eassist-header-switches '("hxx" . ("cxx" "cpp")))
  (add-to-list 'eassist-header-switches '("ipp" . ("hxx" "hpp" "h")))
  (add-to-list 'eassist-header-switches '("hpp" . ("cxx" "cpp" "ipp")))
  
  ;; Ensure semantic can get info from gnu global
  (require 'semantic/db-global)
  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode)
  
  ;; don't use semantic on remote files:
  (add-to-list 'semantic-inhibit-functions 'remote-buffer-p)
  
  (defun my-cedet-hook ()
    (local-set-key [(meta return)] 'semantic-ia-complete-symbol-menu)
    ;;(local-set-key [(control c) (tab)] 'semantic-ia-complete-symbol-menu)
    (local-set-key [(control c) (\?)] 'semantic-ia-complete-symbol)
    ;;
    (local-set-key [(control c) (\>)] 'semantic-complete-analyze-inline)
    (local-set-key [(control c) (=)] 'semantic-decoration-include-visit)
    
    (local-set-key [(control c) (j)] 'semantic-ia-fast-jump)
    (local-set-key [(control c) (q)] 'semantic-ia-show-doc)
    (local-set-key [(control c) (s)] 'semantic-ia-show-summary)
    (local-set-key [(control c) (p)] 'semantic-analyze-proto-impl-toggle)
    (local-set-key (kbd "C-c <left>") 'semantic-tag-folding-fold-block)
    (local-set-key (kbd "C-c <right>") 'semantic-tag-folding-show-block)
    )
  (add-hook 'c-mode-common-hook 'my-cedet-hook)
  (add-hook 'lisp-mode-hook 'my-cedet-hook)
  (add-hook 'emacs-lisp-mode-hook 'my-cedet-hook)
  
  (defun my-cedet-c-hook ()
    (local-set-key [(meta o)] 'eassist-switch-h-cpp)
    (local-set-key "\C-ce" 'eassist-list-methods)
    (local-set-key "\C-c\C-r" 'semantic-symref)
    )
  (add-hook 'c-mode-common-hook 'my-cedet-c-hook)
)

(defvar my-project-build-directories
  '(("None" . "build")
    ("Debug" . "build.dbg")
    ("Release" . "build.rel")
    ("RelWithDebInfo" . "build.r+d")))


;;
;; EDE
;;
;;(when (fboundp 'global-ede-mode)

(global-ede-mode t)
(require 'ede-compdb)

(defun vc-project-root (dir)
  (require 'vc)
  (let* ((default-directory dir)
         (backend (vc-deduce-backend)))
    (and backend (vc-call-backend backend 'root default-directory))))

(defun my-load-cmake-project (dir)
  "Creates a project for the given directory sourced at dir"
  (let ((default-directory dir)
        (config-and-dir (car (cl-member-if (lambda (c) (file-readable-p (expand-file-name "compile_commands.json" (cdr c))))
                                           my-project-build-directories))))
    (unless config-and-dir
      (message "Couldn't determine build directory for project at %s" dir))
    (ede-add-project-to-global-list
     (ede-ninja-project 
      (file-name-nondirectory (directory-file-name dir))
      :file (expand-file-name "CMakeLists.txt" dir)
      :compdb-file (expand-file-name "compile_commands.json" (cdr config-and-dir))
      :configuration-default (car config-and-dir)
      :configuration-directories (mapcar #'cdr my-project-build-directories)
      :configurations (mapcar #'car my-project-build-directories)
      :build-command "cmake --build ."
      ))))

(ede-add-project-autoload
 (ede-project-autoload "CMake"
                       :file 'ede-compdb
                       :proj-file "CMakeLists.txt"
                       :proj-root 'vc-project-root
                       :load-type 'my-load-cmake-project
                       :class-sym 'ede-compdb-project))
 ;'unique)

;; Name the compilation buffer after the current project - allows more than one project to be
;; compiled simultaneously
(defun my-compilation-buffer-name-function (maj)
  (let ((projname (if ede-object-root-project (eieio-object-name-string ede-object-root-project) nil)))
    (concat "*" (downcase maj) (when projname ":") (when projname projname) "*")))

(setq compilation-buffer-name-function 'my-compilation-buffer-name-function)

(defun my-ede-hook ()
  (when (fboundp 'cmake-project-compile-buffer-file)
    (local-set-key [(ctrl f7)] 'cmake-project-compile-buffer-file))
  (when (fboundp 'cmake-project-build-custom-target)
    (local-set-key [f8] 'cmake-project-build-custom-target))
  (when (fboundp 'ede-compile-target)
    (local-set-key [(ctrl f8)] 'ede-compile-target))
  (when (fboundp 'cmake-project-compile-target-fast)
    (local-set-key [(ctrl shift f8)] 'cmake-project-compile-target-fast))
  )
(add-hook 'ede-minor-mode-hook 'my-ede-hook)

;;  )

(provide 'init/cedet)
