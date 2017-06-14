(message "(aldo) --> config.el")

;; Theme modification
(setq theming-modifications
      '(;; Daylerees Earthsong
        (theme-light
         (highlight :background "#ffe792" :foreground "#ffffff")
         (region :background "#60A365" :foreground "#ffffff")
         (shadow :foreground "#3b3a32")
         (secondary-selection :background "#60A365")
         (font-lock-builtin-face :foreground "#95CC5E")
         (font-lock-comment-delimiter-face :inherit (font-lock-comment-face))
         (font-lock-comment-face :foreground "#d6cab9")
         (font-lock-constant-face :foreground "#DB784D")
         (font-lock-doc-face :foreground "#9a9082")
         (font-lock-function-name-face :foreground "#60A365")
         (font-lock-keyword-face :foreground "#95CC5E")
         (font-lock-negation-char-face nil)
         (font-lock-preprocessor-face :foreground "#95CC5E")
         (fonfedt-lock-regexp-grouping-backslash :inherit (bold))
         (font-lock-regexp-grouping-construct :inherit (bold))
         (font-lock-string-face :foreground "#F8BB39")
         (font-lock-type-face :inherit 'default)
         (font-lock-variable-name-face :foreground "#95CC5E")
         (font-lock-warning-face :background "#00a8c6" :foreground "#f8f8f0")
         )

        ;; Expresso light color
        (spacemacs-light
         (font-lock-builtin-face :foreground "#626FC9")
         (font-lock-comment-face :foreground "#7F7F7F")
         (font-lock-constant-face :foreground "#7653C1" :background "#F3F2FF")
         (font-lock-doc-string-face :foreground "#1A93AE" :background "#F4F9FE")
         (font-lock-function-name-face :foreground "#4E279A")
         (font-lock-keyword-face :foreground "#6700B9")
         (font-lock-preprocessor-face :foreground "#434343")
         (font-lock-reference-face :foreground "#4E279A" :background "#F3F2FF")
         (font-lock-string-face :foreground "#BC670F" :background "#FDFBF5")
         (font-lock-type-face :foreground "#699D36")
         (font-lock-variable-name-face :foreground "#7B8C4D")
         (font-lock-warning-face :foreground "#F93232")
         )

        ;; Apply to all themes
        (t
         (fringe :background nil)
         (spacemacs-emacs-face :background "#3c6eb4" :foreground "#ffffff" :inherit (quote mode-line))
         (mode-line-buffer-id :foreground "#e59728" :weight bold))))

;; https://stackoverflow.com/questions/26824328/hide-change-emacs-fringe-bent-arrows-due-to-word-wrapping
(define-fringe-bitmap 'left-curly-arrow
  ;; bracket
  [#b00000000
   #b00001000
   #b00001000
   #b00001110
   #b00000000
   #b00000000
   #b00000000
   #b00000000]

  ;; bullet
  ;; [#b00000000
  ;;  #b00000000
  ;;  #b00000000
  ;;  #b00000000
  ;;  #b00000000
  ;;  #b00000000
  ;;  #b00000000
  ;;  #b00011100
  ;;  #b00111110
  ;;  #b00111110
  ;;  #b00111110
  ;;  #b00011100
  ;;  #b00000000
  ;;  #b00000000
  ;;  #b00000000
  ;;  #b00000000
  ;;  #b00000000]
  )

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

(add-hook
 'window-setup-hook
 (lambda ()
   (set 'evil-emacs-state-cursor '("#3c6eb4" hollow))
   (global-vi-tilde-fringe-mode -1)
   (set-fringe-style '(nil . 0))
   (aldo//scratch-buffer)
   (push "\\*fish\\*\.\+" spacemacs-useful-buffers-regexp)
   (setq tramp-default-method "ssh")
   (setq vc-follow-symlinks t)
   (setq doc-view-resolution 300)
   (setq custom-theme-directory (file-name-as-directory (concat dotspacemacs-directory "themes")))
   (put 'dired-find-alternate-file 'disabled nil)

   (if (display-graphic-p)
     (progn
       (when (spacemacs/system-is-mac)
         (setq ns-use-srgb-colorspace nil)
         (load-theme 'leuven)
         (modify-frame-parameters nil '((fullscreen . fullboth)))))
     (progn
       (load-theme 'monokai t)))))

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
