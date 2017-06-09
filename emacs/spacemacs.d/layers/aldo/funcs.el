(defun aldo/fish-term ()
  " Muti-term with fish "
  (interactive)
  (let ((multi-term-program "fish")
        (multi-term-buffer-name "fish* *"))
    (multi-term)))

(defun aldo/open-localhost ()
  (interactive)
  (ansi-term "fish" "fish-shell"))

(defun aldo/dired-layer ()
  " Open dired buffer in this layer path "
  (interactive)
  (dired
   (concat
    (car dotspacemacs-configuration-layer-path) "aldo")))

(defun aldo/toggle-powerline-separator ()
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
  (interactive)
  (org-twbs-export-as-html)
  (browse-url-of-buffer))

(defun aldo//scratch-buffer ()
  (let ((spacemacs-buffer-name "*scratch*"))
    (recentf-mode)
    (insert "\n\n")
    (insert (format "Welcome back %s!" user-full-name))
    (insert "\n\n")
    (spacemacs-buffer/insert-page-break)
    (spacemacs-buffer//insert-file-list
     "Recent Files:"
     (spacemacs//subseq recentf-list 0 25))
    (spacemacs-buffer/insert-page-break)
    (page-break-lines-mode)
    (local-set-key (kbd "RET") 'widget-button-press)
    (local-set-key [down-mouse-1] 'widget-button-click)
    (kill-buffer "*spacemacs*")))
