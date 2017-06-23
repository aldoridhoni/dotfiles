(defun aldo/fish-term (&optional cmd)
  "Muti-term with fish.
CMD optional argument is command to run after starting shell.
Separate multi line command with \n."
  (interactive)
  (let ((multi-term-program "fish")
        (multi-term-buffer-name "fish* *"))
    (multi-term)
    (when cmd
      (progn
        (term-send-raw-string cmd)
        (term-send-raw-string "\n")))))

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

(defun aldo/clear-ui ()
  "Toggle UI, good for presentation."
  (interactive)
  (dolist (buf (buffer-list))
    (when (spacemacs/useful-buffer-p buf)
      (with-current-buffer buf
        (spacemacs/toggle-mode-line)
        (spacemacs/toggle-line-numbers)
        (spacemacs/toggle-which-key)
        ))))

;; Non-interactive functions

(defun aldo//debug-message (msg)
  "Show message only when run with --debug-init"
  (when init-file-debug
    (message "(aldo) --> %s" msg)))

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

(defun aldo//theme-mod (&optional frame)
  "Theme modification using built in spacemacs theming layer variable \"theming-modifications\"."
  (or frame (setq frame (selected-frame)))
  (aldo//debug-message (format "theme-mod : %s, on : %s" spacemacs--cur-theme frame))
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
           )))

  ;; Apply to all themes
  (add-to-list 'theming-modifications
               (list 't
                     '(fringe :background nil)
                     (list 'spacemacs-emacs-face :background (if (display-graphic-p frame) "#3c6eb4" "#005faf") :foreground "#ffffff" :inherit 'mode-line)
                     '(mode-line-buffer-id :foreground "#e59728" :weight bold)))
  (spacemacs/update-theme)
  )
