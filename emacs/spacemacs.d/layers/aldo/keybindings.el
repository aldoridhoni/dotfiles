;; These keys make xwidget behave like normal browser
;; (define-key xwidget-webkit-mode-map [mouse-4] 'xwidget-webkit-scroll-down)
;; (define-key xwidget-webkit-mode-map [mouse-5] 'xwidget-webkit-scroll-up)
;; (define-key xwidget-webkit-mode-map (kbd "<up>") 'xwidget-webkit-scroll-down)
;; (define-key xwidget-webkit-mode-map (kbd "<down>") 'xwidget-webkit-scroll-up)
;; (define-key xwidget-webkit-mode-map (kbd "M-w") 'xwidget-webkit-copy-selection-as-kill)
;; (define-key xwidget-webkit-mode-map (kbd "C-c") 'xwidget-webkit-copy-selection-as-kill)

;; Persp
(global-set-key (kbd "C-x <XF86Back>") 'persp-prev)
(global-set-key (kbd "C-x <XF86Forward>") 'persp-next)

(global-set-key (kbd "C-S-d") 'spacemacs/duplicate-line-or-region)
(global-set-key (kbd "C-S-v") 'clipboard-yank)
