;;
;; CEDET Stuff
;;


;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;; IMPORTANT: For Emacs >= 23.2, you must place this *before* any
;; CEDET component (including EIEIO) gets activated by another 
;; package (Gnus, auth-source, ...).
(when (load-file (expand-file-name "cedet/cedet-devel-load.el" extern-lisp-dir))

  ;;(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
  ;;(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)
  (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
  (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  (add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
  (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)

  ;; Enable Semantic
  (semantic-mode 1)

  (require 'semantic/bovine/gcc)
  (require 'semantic/ia)
  (require 'semantic/decorate/include)
  (require 'semantic/lex-spp)

  ;; add some hard-to-find include directories
  (dolist (dir '("/usr/include/boost-1_49"
                 "/usr/local/xsd-3.2.0-x86_64-linux-gnu/libxsd"))
    (if (file-directory-p dir)
        (semantic-add-system-include dir 'c++-mode)))

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
  
  ;; (add-hook 'semantic-init-hooks (lambda ()
  ;;                                  (imenu-add-to-menubar "Tokens")))

  ;; Adds semantic analyze display for speedbar
  ;; (defun require-semantic-sb ()
  ;;   (require 'semantic-sb))
  ;; (add-hook 'speedbar-load-hook 'require-semantic-sb)
  
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

    (when (fboundp 'cmake-project-compile-buffer-file)
      (local-set-key [(ctrl f7)] 'cmake-project-compile-buffer-file))
    (when (fboundp 'cmake-project-build-custom-target)
      (local-set-key [f8] 'cmake-project-build-custom-target))
    (when (fboundp 'ede-compile-target)
      (local-set-key [(ctrl f8)] 'ede-compile-target))
    (when (fboundp 'cmake-project-compile-target-fast)
      (local-set-key [(ctrl shift f8)] 'cmake-project-compile-target-fast))
    )
  (add-hook 'lisp-mode-hook 'my-cedet-hook)
  (add-hook 'emacs-lisp-mode-hook 'my-cedet-hook)
  
  (defun my-cedet-c-hook ()
    (local-set-key [(meta o)] 'eassist-switch-h-cpp)
    (local-set-key "\C-ce" 'eassist-list-methods)
    (local-set-key "\C-c\C-r" 'semantic-symref)
    )
  (add-hook 'c-mode-common-hook 'my-cedet-c-hook)

  (semanticdb-enable-gnu-global-databases 'c-mode t)
  (semanticdb-enable-gnu-global-databases 'c++-mode t)

  (when (cedet-ectag-version-check t)
    (semantic-load-enable-primary-ectags-support))

  ;; Called by c-mode initialization hook
  (defun c++-setup-boost (boost-root)
    (when (file-accessible-directory-p boost-root)
      (let ((cfiles (recur-list-files boost-root "\\(config\\|user\\)\\.hpp")))
        (dolist (file cfiles)
          (add-to-list 'semantic-lex-c-preprocessor-symbol-file file)))))
)


;;
;; EDE
;;
(when (fboundp 'global-ede-mode)

  (global-ede-mode t)
  (require 'ede-cmake)

  (defun my-locate-header (name dir proj)
    "locate name in current build directory if required"
    (let ((build (ignore-errors (cmake-build-directory proj))))
      (when build
        (let* ((builddir (file-name-as-directory build))
               (path (concat builddir name)))
          (if (file-exists-p path) path)
    ))))

  (defvar my-project-build-directories
    '(("None" . "~/build")
      ("Debug" . "~/build.dbg.64")
      ("Release" . "~/build.rel.64")
      ("RelWithDebInfo" . "~/build.r+d.64")))

  (defun my-project-root-build-locator (config root-dir)
    "Locates a build directory for a project."
    (let ((build-base (assoc config my-project-build-directories)))
      (if build-base
          (concat (file-name-as-directory (cdr build-base))
                  (file-name-nondirectory (directory-file-name root-dir)))
        nil)))

  (defun my-load-project (dir)
    "Load a project of type `cpp-root' for the directory DIR.
     Return nil if there isn't one."
    (ede-cmake-cpp-project 
     (file-name-nondirectory (directory-file-name dir))
     :directory dir
     :locate-fcn 'my-locate-header
     :locate-build-directory 'my-project-root-build-locator
     :build-tool (cmake-ninja-build-tool "Make" :additional-parameters "-j24")
     :include-path '( "/" "/pchNone" "/external/orc/9" )
     :spp-table '( ("override" . "") )
     ))

  (ede-add-project-autoload
   (ede-project-autoload "CMake"
                         :file 'ede-cmake
                         :proj-file "CMakeLists.txt"
                         :proj-root 'ede-cmake-cpp-project-root
                         :load-type 'my-load-project
                         :class-sym 'ede-cmake-cpp-project)
   'unique)

  ;; Name the compilation buffer after the current project - allows more than one project to be
  ;; compiled simultaneously
  (defun my-compilation-buffer-name-function (maj)
    (let ((projname (if ede-object-root-project (aref ede-object-root-project object-name) nil)))
      (concat "*" (downcase maj) (when projname ":") (when projname projname) "*")))

  (setq compilation-buffer-name-function 'my-compilation-buffer-name-function)
  )

(provide 'init/cedet)
