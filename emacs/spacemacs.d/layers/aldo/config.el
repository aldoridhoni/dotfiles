(push "\\*fish\\*\.\+" spacemacs-useful-buffers-regexp)

;; eshell
(setq tramp-default-method "ssh")
(add-hook 'eshell-mode-hook (lambda ()
  (eshell/alias "e" "find-file $1")
  (eshell/alias "ff" "find-file $1")
  (eshell/alias "ee" "find-file-other-window $1")
  ;; The 'ls' executable requires the Gnu version on the Mac
  (let ((ls (if (file-exists-p "/usr/local/bin/gls")
        "/usr/local/bin/gls"
        "/bin/ls")))
  (eshell/alias "ll" (concat ls " -AlohG --color=always")))))
