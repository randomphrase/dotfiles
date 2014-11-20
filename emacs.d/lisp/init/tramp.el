;; TRAMP
(when (eq window-system 'w32)
  ;; Here is my favourite tramp method for windows...
  (require 'tramp)
  (add-to-list 'tramp-methods
               '("pscpx"
                 (tramp-login-program "plink")
                 (tramp-login-args
                  (("-load")
                   ("%h")
                   ("-t")
                   ("env 'TERM=dumb' 'PROMPT_COMMAND=' 'PS1=$ '")
                   ("/bin/sh")))
                 (tramp-remote-sh "/bin/sh")
                 (tramp-copy-program "pscp")
                 (tramp-copy-args
                  (("-P" "%p")
                   ("-scp")
                   ("-p" "%k")))
                 (tramp-copy-keep-date t))))
;; bjdev02 has an ancient /bin/sh which means it won't work with tramp. Fortunately the standard
;; bash seems to work, sheesh.
;(add-to-list 'tramp-default-method-alist '("bjdev02" "" "pscp"))

(defadvice vc-bzr-registered (around my-vc-bzr-registered-tramp activate)
  "Don't try to use BZR on files accessed via TRAMP."
  (if (and (fboundp 'tramp-tramp-file-p)
           (tramp-tramp-file-p (ad-get-arg 0)))
      nil
    ad-do-it))

(provide 'init/tramp)
