;;;###autoload
(defun fish-term ()
  (interactive)
  (let ((multi-term-program "fish")
        (multi-term-buffer-name "fish* *"))
    (multi-term)))

;;;###autoload
(defun open-localhost ()
  (interactive)
  (ansi-term "fish" "fish-shell"))
