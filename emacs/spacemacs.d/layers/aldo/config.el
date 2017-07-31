(message "(aldo) --> config.el")

(defvar my-cursor-color 'red)

(defvar my-cursor-type 'bar)

(defvar terminal-theme 'monokai)

(add-hook
 'emacs-startup-hook
 (defun aldo/startup-hook ()
   (aldo//debug-message "emacs-startup-hook")
   (when (boundp 'spacemacs-buffer-name)
     (setq spacemacs-buffer-name "*scratch*"))
   (setq initial-buffer-choice (lambda () (get-buffer "*scratch*")))
   (setq tramp-default-method "ssh")

   ;; Follow links
   (setq vc-follow-symlinks t)

   (setq doc-view-resolution 300)
   (setq custom-theme-directory (file-name-as-directory (concat dotspacemacs-directory "themes")))
   (setq x-stretch-cursor t)
   (setq evil-emacs-state-cursor (list my-cursor-color my-cursor-type))
   (setq shell-file-name "/bin/sh")

   ;; Don't make this buffer hidden
   (push "\\*fish\\*\.\+" spacemacs-useful-buffers-regexp)
   (push "\\*ssh\\*\*" spacemacs-useful-buffers-regexp)
   ))

(add-hook
 'window-setup-hook
 (lambda ()
   "Run functions. Run only once at startup and very late after emacs-startup-hook."
   (aldo//debug-message "window-setup-hook !!!")
   (blink-cursor-mode 1)
   (global-vi-tilde-fringe-mode -1)
   ;; (aldo//scratch-buffer)
   (aldo//theme-mod)
   (if (display-graphic-p)
       (progn
         (set-fringe-style '(nil . 0))
         (aldo//set-fringe))
     (progn
       (load-theme terminal-theme t)))))

(add-hook
 'spacemacs-post-theme-change-hook
 (lambda ()
   (setq evil-emacs-state-cursor (list my-cursor-color my-cursor-type))
   (evil-emacs-state)
   (blink-cursor-mode 1)
   (aldo//debug-message "theme-change-hook")
   (aldo//theme-mod)
   ))

(add-hook
 'server-visit-hook
 (lambda ()
   (aldo//debug-message "server-visit-hook")
   ))

(add-hook
 'evil-emacs-state-entry-hook
 (lambda ()
   (aldo//debug-message "state-entry-hook")
   ))

(add-hook
 'find-file-hook
 (lambda ()
   (aldo//debug-message "find-file-hook")
   ))

(add-hook
 'change-major-mode-hook
 (lambda ()
   (aldo//debug-message "change-major-mode-hook")
   ))

(add-hook
 'markdown-mode-hook
 (lambda ()
   (setq delete-trailing-lines nil)))

(add-hook
 'term-exec-hook
 (lambda ()
   "Don't ask for confirmation on kill terminal buffer."
   (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

(add-hook
 'kill-emacs-query-functions
 (lambda () (y-or-n-p "Do you really want to exit Emacs? "))
 'append)

(add-hook
 'eshell-mode-hook
 (lambda ()
   (message "eshell-mode-hook")
   (eshell/alias "e" "find-file $1")
   (eshell/alias "ff" "find-file $1")
   (eshell/alias "ee" "find-file-other-window $1")
   ;; The 'ls' executable requires the Gnu version on the Mac
   (let ((ls (if (file-exists-p "/usr/local/bin/gls")
                 "/usr/local/bin/gls"
               "/bin/ls")))
     (eshell/alias "ll" (concat ls " -AlohG --color=always")))))

(add-to-list
 'after-make-frame-functions
 (lambda (new)
   (aldo//debug-message "after-make-frame-functions")
   (when (server-running-p)
     (if (not (display-graphic-p new))
         (progn
           (setq powerline-default-separator 'utf-8)
           (setq dotspacemacs-mode-line-unicode-symbols nil)
           (spaceline-compile)
           (load-theme terminal-theme t)
           (aldo//theme-mod new))
       (progn
         (setq powerline-default-separator 'slant)
         (setq dotspacemacs-mode-line-unicode-symbols 1)
         (spaceline-compile)
         (load-theme (car dotspacemacs-themes) t))))))

(add-to-list
 'delete-frame-functions
 (lambda (selected)
   (aldo//debug-message "delete-frame-functions")
   (when (server-running-p)
     (let ((other-frame (remove selected (frame-list))))
     (if (member t (mapcar 'display-graphic-p other-frame))
         (progn
           (setq powerline-default-separator 'slant)
           (setq dotspacemacs-mode-line-unicode-symbols 1)
           (spaceline-compile)
           (load-theme (car dotspacemacs-themes) t))
       (progn
         (setq powerline-default-separator 'utf-8)
         (setq dotspacemacs-mode-line-unicode-symbols nil)
         (spaceline-compile)
         (load-theme terminal-theme t)
         (aldo//theme-mod (car other-frame))
         ))))))

;; Kill term buffer when process exit
(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (kill-buffer))
