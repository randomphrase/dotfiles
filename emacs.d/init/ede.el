;;
;; EDE
;;
;;(when (fboundp 'global-ede-mode)

(global-ede-mode t)
(require 'ede-compdb)

(defun vc-project-root (dir)
  "Return the root of project in DIR."
  (require 'vc)
  (let* ((default-directory dir)
         (backend (vc-deduce-backend)))
    (or (and backend (vc-call-backend backend 'root dir))
        dir)))

(defvar my-project-build-directories
  '(("None" . "build")
    ("Debug" . "build.dbg")
    ("Release" . "build.rel")
    ("RelWithDebInfo" . "build.r+d")))

(defun my-load-cmake-project (dir)
  "Creates a project for the given directory sourced at dir"
  (let* ((default-directory dir)
         (projname (file-name-nondirectory (directory-file-name dir)))
         (config-and-build-dirs
          (mapcar (lambda (c)
                    (cons (car c)
                          ;; Expand directory
                          (if (file-name-absolute-p (cdr c)) (expand-file-name projname (cdr c))
                            (expand-file-name (cdr c) dir))))
                  my-project-build-directories))
         (active-config-and-dir
          (car (cl-member-if (lambda (c)
                               (file-readable-p (expand-file-name "rules.ninja" (cdr c))))
                             config-and-build-dirs))))
    (unless active-config-and-dir
      (message "Couldn't determine build directory for project at %s" dir))
    (ede-add-project-to-global-list
     (ede-ninja-project 
      projname
      :file (expand-file-name "CMakeLists.txt" dir)
      :compdb-file (expand-file-name "rules.ninja" (cdr active-config-and-dir))
      :configuration-default (or (car active-config-and-dir) (car (car my-project-build-directories)))
      :configuration-directories (mapcar #'cdr config-and-build-dirs)
      :configurations (mapcar #'car config-and-build-dirs)
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

(defun ede-object-system-include-path ()
  "Returns the system include path for the current buffer"
  (when ede-object
    (ede-system-include-path ede-object)))

(defun my-ede-hook ()
  ;; These are a bit more convenient than default bindings
  (local-set-key [f8] 'ede-compile-selected)
  (local-set-key [(ctrl f8)] 'ede-compile-target)

  ;; set the project root for use with find-file-in-project
  (when ede-object-root-project
    (setq ffip-project-root (oref ede-object-root-project directory)))

  ;; enable include file completion
  (when (boundp 'achead:get-include-directories-function)
    (setq achead:get-include-directories-function 'ede-object-system-include-path))
  )
(add-hook 'ede-minor-mode-hook 'my-ede-hook)

(provide 'init/ede)
