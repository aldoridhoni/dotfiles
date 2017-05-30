(message "(aldo) --> config.el")

(push "\\*fish\\*\.\+" spacemacs-useful-buffers-regexp)

;; eshell
(setq tramp-default-method "ssh")

;; Auto follow symlinks
(setq vc-follow-symlinks t)

;; Doc View
(setq doc-view-resolution 300)

;; (setf (nth 3 spacemacs-evil-cursors) '("emacs" "#3c6eb4"  box))

(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell/alias "e" "find-file $1")
            (eshell/alias "ff" "find-file $1")
            (eshell/alias "ee" "find-file-other-window $1")
            ;; The 'ls' executable requires the Gnu version on the Mac
            (let ((ls (if (file-exists-p "/usr/local/bin/gls")
                          "/usr/local/bin/gls"
                        "/bin/ls")))
              (eshell/alias "ll" (concat ls " -AlohG --color=always")))))

(add-hook 'window-setup-hook
          (lambda ()
            (set 'evil-emacs-state-cursor '("#3c6eb4" box))
            (aldo//scratch-buffer)
            (when (spacemacs/system-is-mac)
              (setq ns-use-srgb-colorspace nil)
              (load-theme 'leuven)
              (modify-frame-parameters nil '((fullscreen . fullboth))))))
