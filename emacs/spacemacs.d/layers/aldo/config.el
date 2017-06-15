(message "(aldo) --> config.el")

(defvar my-cursor-color 'red)

(defvar my-cursor-type 'bar)

(defvar terminal-theme 'monokai)

(add-hook
 'emacs-startup-hook
 (defun aldo/startup-hook ()
   (setq initial-buffer-choice (lambda () (get-buffer "*scratch*")))
   ))

(add-hook
 'window-setup-hook
 (lambda ()
   "Set variable and run functions. Run only once at startup and very late."
   (setq evil-emacs-state-cursor (list my-cursor-color my-cursor-type))
   (blink-cursor-mode 1)
   (global-vi-tilde-fringe-mode -1)
   (aldo//scratch-buffer)
   (push "\\*fish\\*\.\+" spacemacs-useful-buffers-regexp)
   (setq tramp-default-method "ssh")
   (setq vc-follow-symlinks t)
   (setq doc-view-resolution 300)
   (setq custom-theme-directory (file-name-as-directory (concat dotspacemacs-directory "themes")))
   (setq x-stretch-cursor t)
   (put 'dired-find-alternate-file 'disabled nil)

   (if (display-graphic-p)
       (progn
         (set-fringe-style '(nil . 0))
         (aldo//set-fringe)
         (aldo//theme-mod))
     (progn
       (load-theme terminal-theme t)))))

(add-hook
 'spacemacs-post-theme-change-hook
 (lambda ()
   (setq evil-emacs-state-cursor (list my-cursor-color my-cursor-type))
   (evil-emacs-state)
   (blink-cursor-mode 1)))

(add-hook
 'evil-emacs-state-entry-hook
 (lambda ()
   ;; (message "state-entry-hook")
   ))

(add-hook
 'find-file-hook
 (lambda ()
   ;; (message "find-file-hook")
   ))

(add-hook
 'change-major-mode-hook
 (lambda ()
   ;; (message "change-major-mode-hook")
   ))

(add-hook
 'markdown-mode-hook
 (lambda ()
   (setq delete-trailing-lines nil)))

(add-hook
 'eshell-mode-hook
 (lambda ()
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
 (lambda (_)
   ;; (message "after-make-frame-functions")
   (when (server-running-p)
     (if (not (display-graphic-p _))
         (progn
           (setq powerline-default-separator 'utf-8)
           (setq dotspacemacs-mode-line-unicode-symbols nil)
           (spaceline-compile)
           (load-theme 'monokai t))
       (progn
         (setq powerline-default-separator 'slant)
         (setq dotspacemacs-mode-line-unicode-symbols 1)
         (spaceline-compile)
         (load-theme (car dotspacemacs-themes) t))))))

(add-to-list
 'delete-frame-functions
 (lambda (_)
   ;; (message "delete-frame-functions")
   (when (server-running-p)
     (if (not (display-graphic-p _))
         (progn
           (setq powerline-default-separator 'slant)
           (setq dotspacemacs-mode-line-unicode-symbols 1)
           (spaceline-compile)
           (load-theme (car dotspacemacs-themes) t))
       (progn
         (setq powerline-default-separator 'utf-8)
         (setq dotspacemacs-mode-line-unicode-symbols nil)
         (spaceline-compile)
         (load-theme 'monokai t))))))
