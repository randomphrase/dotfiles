;=============================================================================
;
;  Program:   CMake - Cross-Platform Makefile Generator
;  Module:    $RCSfile: cmake-mode.el,v $
;
;  Copyright (c) 2000-$Date: 2006/09/23 20:32:34 $ Kitware, Inc., Insight Consortium.  All rights reserved.
;  See Copyright.txt or http://www.cmake.org/HTML/Copyright.html for details.
;
;     This software is distributed WITHOUT ANY WARRANTY; without even
;     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;     PURPOSE.  See the above copyright notices for more information.
;
;=============================================================================
;;; cmake-mode.el --- major-mode for editing CMake sources

;------------------------------------------------------------------------------

;;; Commentary:

;; Provides syntax highlighting and indentation for CMakeLists.txt and
;; *.cmake source files.
;;
;; Add this code to your .emacs file to use the mode:
;;
;;  (setq load-path (cons (expand-file-name "/dir/with/cmake-mode") load-path))
;;  (require 'cmake-mode)
;;  (setq auto-mode-alist
;;        (append '(("CMakeLists\\.txt\\'" . cmake-mode)
;;                  ("\\.cmake\\'" . cmake-mode))
;;                auto-mode-alist))

;------------------------------------------------------------------------------

;;; Code:

;;
;; Regular expressions used by line indentation function.
;;
(defconst cmake-regex-blank "^[ \t]*$")
(defconst cmake-regex-comment "#.*$")
(defconst cmake-regex-blank-comment "#^[ \t]*$")
(defconst cmake-regex-paren-left "(")
(defconst cmake-regex-paren-right ")")
(defconst cmake-regex-argument-quoted
  "\"\\([^\"\\\\]\\|\\\\\\(.\\|\n\\)\\)*\"")
(defconst cmake-regex-argument-unquoted
  "\\([^ \t\r\n()#\"\\\\]\\|\\\\.\\)\\([^ \t\r\n()#\\\\]\\|\\\\.\\)*")
(defconst cmake-regex-token (concat "\\(" cmake-regex-comment
                                    "\\|" cmake-regex-paren-left
                                    "\\|" cmake-regex-paren-right
                                    "\\|" cmake-regex-argument-unquoted
                                    "\\|" cmake-regex-argument-quoted
                                    "\\)"))
(defconst cmake-regex-indented (concat "^\\("
                                       cmake-regex-token
                                       "\\|" "[ \t\r\n]"
                                       "\\)*"))
(defconst cmake-regex-block-open
  "^\\(IF\\|MACRO\\|FOREACH\\|ELSE\\|ELSEIF\\|WHILE\\)$")
(defconst cmake-regex-block-close
  "^[ \t]*\\(ENDIF\\|ENDFOREACH\\|ENDMACRO\\|ELSE\\|ELSEIF\\|ENDWHILE\\)[ \t]*(")

;------------------------------------------------------------------------------

;;
;; Helper functions for line indentation function.
;;
(defun cmake-line-starts-inside-string ()
  "Determine whether the beginning of the current line is in a string."
  (if (save-excursion
        (beginning-of-line)
        (let ((parse-end (point)))
          (beginning-of-buffer)
          (nth 3 (parse-partial-sexp (point) parse-end))
          )
        )
      t
    nil
    )
  )

(defun cmake-find-last-indented-line ()
  "Move to the beginning of the last line that has meaningful indentation."
  (let ((point-start (point))
        region)
    (forward-line -1)
    (setq region (buffer-substring-no-properties (point) point-start))
    (while (and (not (bobp))
                (or (looking-at cmake-regex-blank)
                    (not (and (string-match cmake-regex-indented region)
                              (= (length region) (match-end 0))))))
      (forward-line -1)
      (setq region (buffer-substring-no-properties (point) point-start))
      )
    )
  )

;------------------------------------------------------------------------------

;;
;; Line indentation function.
;;
(defun cmake-indent ()
  "Indent current line as CMAKE code."
  (interactive)
  (beginning-of-line)
  (if (cmake-line-starts-inside-string)
      ()
    (if (bobp)
        (indent-line-to 0)
      (let ((point-start (point))
            token cur-indent)

        (save-excursion
          ; Search back for the last indented line.
          (cmake-find-last-indented-line)

          ; Start with the indentation on this line.
          (setq cur-indent (current-indentation))

          ; Search forward counting tokens that adjust indentation.
          (while (re-search-forward cmake-regex-token point-start t)
            (setq token (match-string 0))
            (if (string-match (concat "^" cmake-regex-paren-left "$") token)
                (setq cur-indent (+ cur-indent cmake-tab-width))
              )
            (if (string-match (concat "^" cmake-regex-paren-right "$") token)
                (setq cur-indent (- cur-indent cmake-tab-width))
              )
            (if (and
                 (string-match cmake-regex-block-open token)
                 (looking-at (concat "[ \t]*" cmake-regex-paren-left))
                 )
                (setq cur-indent (+ cur-indent cmake-tab-width))
              )
            )
          )

        ; If this is the end of a block, decrease indentation.
        (if (looking-at cmake-regex-block-close)
            (setq cur-indent (- cur-indent cmake-tab-width))
          )

        ; Indent this line by the amount selected.
        (if (< cur-indent 0)
            (indent-line-to 0)
          (indent-line-to cur-indent)
          )
        )
      )
    )
  )

;------------------------------------------------------------------------------

;;
;; Fill comment paragraph functions.
;;
(defconst cmake-fill-comment-prefix "# ")

(defun cmake-fill-comment-paragraph-justify ()
  "Fills the current comment paragraph with justified margins."
  (interactive)
  (cmake-fill-comment-paragraph 1)
  )

(defun cmake-fill-comment-paragraph (&optional justify)
  "Fills the current comment paragraph."
  (interactive "P")
  (let ((opos (point-marker))
        (begin nil)
        (end nil)
        (indent nil)
        )

    ; Check if we are inside a comment.
    (if (not (progn
               (back-to-indentation)
               (looking-at cmake-regex-comment)))
        (error "not inside a comment paragraph ..."))
    ; *** are right-side comments valid; how do we treat them here??? ***

    (message "filling comment paragraph ...")

    ;;
    ;; Find limits of paragraph.
    ;;
    ; Find end of paragraph.
    (save-excursion
      (while (and
              ; we are in a comment
              (progn
                (back-to-indentation)
                (and (looking-at cmake-regex-comment)
                     (not (looking-at cmake-regex-blank-comment))))
              ; and not at the end of the buffer
              (progn
                (end-of-line)
                (not (= (point) (point-max))))
              )
        (forward-line 1)
        )
      (if (progn
            (back-to-indentation)
            (not (and (looking-at cmake-regex-comment)
                      (not (looking-at cmake-regex-blank-comment)))))
          (forward-line -1))
      (end-of-line)
      (setq end (point-marker))
      )
    ; Find beginning of paragraph.
    (save-excursion
      (while (and
              ; we are in a comment
              (progn
                (back-to-indentation)
                (and (looking-at cmake-regex-comment)
                     (not (looking-at cmake-regex-blank-comment))))
              ; and not at the beginning of the buffer
              (progn
                (beginning-of-line)
                (not (= (point) (point-min))))
              )
        (forward-line -1)
        )
      (if (progn
            (back-to-indentation)
            (not (and (looking-at cmake-regex-comment)
                      (not (looking-at cmake-regex-blank-comment))))
            )
          (forward-line 1))
      (back-to-indentation)
      (setq begin (point-marker))
      (setq indent(current-column))
      )

    ;;
    ;; Delete leading whitespace and uncomment.
    ;;
    (save-excursion
      (goto-char begin)
      (beginning-of-line)
      (while (re-search-forward
              (concat "^[ \t]*\\("
                      cmake-fill-comment-prefix
                      "\\|#\\)[ \t]*"
                      )
              end t)
        (replace-match "")
        )
      )

    ;;
    ;; Fill paragraph
    ;;
    ; Calculate fill width minus indent minus prefix.
    (setq fill-column (- fill-column
                         indent
                         (length cmake-fill-comment-prefix)
                         ))
    ; Fill paragraph.
    (fill-region begin end justify)
    ; Restore fill width.
    (setq fill-column (+ fill-column
                         indent
                         (length cmake-fill-comment-prefix)
                         ))

    ;;
    ;; Re-comment and re-indent region.
    ;;
    (save-excursion
      (goto-char begin)
      (setq count (point-marker))
      (while (< count end)
        (beginning-of-line)
        (indent-to indent)
        (insert cmake-fill-comment-prefix)
        (forward-line 1)
        (setq count (point-marker))
        )
      )

    ;;
    ;; Delete the extra line that gets inserted somehow in XEmacs???
    ;;
    (if version-xemacs
      (save-excursion
        (goto-char end)
        (end-of-line)
        (delete-char 1)
        )
      )

    (message "filling comment paragraph ... done")
    (goto-char opos)
    )
  )

;------------------------------------------------------------------------------

;;
;; Keyword highlighting regex-to-face map.
;;
(defconst cmake-font-lock-keywords
  (list '("^[ \t]*\\(\\w+\\)[ \t]*(" 1 font-lock-function-name-face))
  "Highlighting expressions for CMAKE mode."
  )

;------------------------------------------------------------------------------

;;
;; Syntax table for this mode.  Initialize to nil so that it is
;; regenerated when the cmake-mode function is called.
;;
(defvar cmake-mode-syntax-table nil "Syntax table for cmake-mode.")
(setq cmake-mode-syntax-table nil)

;;
;; User hook entry point.
;;
(defvar cmake-mode-hook nil)

;;
;; Indentation increment.
;;
(defvar cmake-tab-width 2)

;------------------------------------------------------------------------------

;;
;; CMake mode startup function.
;;
(defun cmake-mode ()
  "Major mode for editing CMake listfiles."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'cmake-mode)
  (setq mode-name "CMAKE")

  ; Create the syntax table
  (setq cmake-mode-syntax-table (make-syntax-table))
  (set-syntax-table cmake-mode-syntax-table)
  (modify-syntax-entry ?_  "w" cmake-mode-syntax-table)
  (modify-syntax-entry ?\(  "()" cmake-mode-syntax-table)
  (modify-syntax-entry ?\)  ")(" cmake-mode-syntax-table)
  (modify-syntax-entry ?# "<" cmake-mode-syntax-table)
  (modify-syntax-entry ?\n ">" cmake-mode-syntax-table)

  ; Setup font-lock mode.
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(cmake-font-lock-keywords))

  ; Setup indentation function.
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'cmake-indent)

  ; Setup comment syntax.
  (make-local-variable 'comment-start)
  (setq comment-start "#")

  ; Some local overrides of functions
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'cmake-fill-comment-paragraph)
  
  ; Run user hooks.
  (run-hooks 'cmake-mode-hook))

; This file provides cmake-mode.
(provide 'cmake-mode)

;;; cmake-mode.el ends here
