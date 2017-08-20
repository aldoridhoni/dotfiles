(defun aldo/fish-term (&optional cmd)
  "Muti-term with fish.
CMD optional argument is command to run after starting shell.
Separate multi line command with \n."
  (interactive)
  (let ((multi-term-program "fish")
        (multi-term-buffer-name "fish* *"))
    (multi-term)
    (when cmd
      (term-send-raw-string cmd)
      (term-send-raw-string "\n"))))

(defun aldo/dired-layer ()
  "Open this layer path in dired buffer."
  (interactive)
  (dired
   (concat (car dotspacemacs-configuration-layer-path) "aldo")))

(defun aldo/helm-ff-layer ()
  (interactive)
  (helm-find-files-1
   (file-name-as-directory
    (concat (car dotspacemacs-configuration-layer-path) "aldo"))))

(defun aldo/toggle-powerline-separator ()
  "Toggle powerline separator between utf-8 and slant."
  (interactive)
  (if (get 'aldo/toggle-powerline-separator 'state)
      (progn
        (setq powerline-default-separator 'utf-8)
        (powerline-set-selected-window)
        (powerline-reset)
        (spaceline-compile)
        (put 'aldo/toggle-powerline-separator 'state nil))
    (progn
      (setq powerline-default-separator 'slant)
      (powerline-set-selected-window)
      (powerline-reset)
      (spaceline-compile)
      (put 'aldo/toggle-powerline-separator 'state t))))

(defun aldo/browse-org-export-twbs ()
  "Run Org export to Bootstrap HTML and open in browser."
  (interactive)
  (org-twbs-export-as-html)
  (browse-url-of-buffer))

(defvar in_presentation nil "Used in aldo/clear-ui")
(defvar-local hide-mode-line nil)

(defun aldo/toggle-clear-ui ()
  (interactive)
  (if in_presentation
      (setq in_presentation nil)
    (setq in_presentation t))
  (aldo/clear-ui))

(add-hook 'after-change-major-mode-hook 'aldo/clear-ui)

(defun aldo/clear-ui ()
  "Toggle UI, good for presentation."
  (interactive)
  (dolist (buf (buffer-list))
    (when (spacemacs/useful-buffer-p buf)
      (with-current-buffer buf
        (if in_presentation
            (progn
              (which-key-mode -1)
              (spacemacs/no-linum)
              (when mode-line-format
                (setq hide-mode-line mode-line-format
                      mode-line-format nil)))
          ;; else - show it all
          (which-key-mode)
          (aldo//linum-maybe-on)
          (when hide-mode-line
            (setq mode-line-format hide-mode-line
                  hide-mode-line nil)))
        (force-mode-line-update)
        (redraw-display)
        ))))

;; Non-interactive functions

(defun aldo//linum-maybe-on ()
  (when (spacemacs/enable-line-numbers-p)
    (linum-mode 1)))

(defun aldo//debug-message (msg)
  "Show message only when run with --debug-init"
  (when init-file-debug
    (message (propertize "(aldo) --> %s" 'face 'font-lock-constant-face) msg)))

(defun aldo//scratch-buffer ()
  "Insert recent file list to scratch buffer."
  (unless noninteractive
    (get-buffer "*scratch*")
    (with-current-buffer "*scratch*"
      (let ((spacemacs-buffer-name "*scratch*"))
        (insert "\n")
        (insert (format "Welcome back %s!" user-full-name))
        (insert "\n")
        (spacemacs-buffer/insert-page-break)
        (spacemacs-buffer//insert-file-list
         "Recent Files:"
         (spacemacs//subseq recentf-list 0 25))
        (spacemacs-buffer/insert-page-break)
        (page-break-lines-mode)
        (local-set-key (kbd "RET") 'widget-button-press)
        (local-set-key [down-mouse-1] 'widget-button-click)
        (local-set-key [mouse-2] 'mouse-set-point))
      )))

(defun aldo//set-fringe ()
  "Change fringe from arrow to L shaped.
  See https://stackoverflow.com/questions/26824328/hide-change-emacs-fringe-bent-arrows-due-to-word-wrapping"

  (define-fringe-bitmap 'left-curly-arrow
    ;; L bracket
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
    ))

(defun* aldo//theme-mod (&optional (frame (selected-frame)))
  "Theme modification using built in spacemacs theming layer variable \"theming-modifications\"."
  (aldo//debug-message (format "theme-mod : %s, on : %s" spacemacs--cur-theme frame))
  (setq theming-modifications
        '(;; Daylerees Earthsong
          (theme-name-light
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
           )))

  ;; Apply modification to all themes
  (add-to-list 'theming-modifications
               (list 't
                     '(fringe :background nil)
                     '(term :background nil :foreground nil)
                     (list 'spacemacs-emacs-face :background (if (display-graphic-p frame) "#3c6eb4" "#005faf") :foreground "#ffffff" :inherit 'mode-line)
                     '(mode-line-buffer-id :foreground "#e59728" :weight bold)))
  ;; Reset ansi-color
  (custom-theme-set-variables
   spacemacs--cur-theme
   '(ansi-term-color-vector
     [term term-color-black term-color-red term-color-green term-color-yellow term-color-blue term-color-magenta term-color-cyan term-color-white]))
  (spacemacs/update-theme)
  )
