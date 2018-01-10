;; from https://www.emacswiki.org/emacs/GoodFonts#toc3 and https://gist.github.com/haxney/3055728

(defun font-is-mono-p (font-family)
  ;; with-selected-window
  (let ((wind (selected-window))
        m-width l-width)
   (with-current-buffer "asdf"
     (set-window-buffer (selected-window) (current-buffer))
     (text-scale-set 4)
     (insert (propertize "l l l l l" 'face `((:family ,font-family))))
     (goto-char (line-end-position))
     (setq l-width (car (posn-x-y (posn-at-point))))
     (newline)
     (forward-line)
     (insert (propertize "m m m m m" 'face `((:family ,font-family) italic)))
     (goto-char (line-end-position))
     (setq m-width (car (posn-x-y (posn-at-point))))
     (eq l-width m-width))))

(defun compare-monospace-fonts ()
  "Display a list of all monospace font faces."
  (interactive)
  (pop-to-buffer "*Monospace Fonts*")

  (erase-buffer)
  (dolist (font-family (font-family-list))
    (when (font-is-mono-p font-family)
      (let ((str font-family))
        (newline)
        (insert
         (propertize (concat "The quick brown fox jumps over the lazy dog 1 l; 0 O o ("
                             font-family ")\n") 'face `((:family ,font-family)))
         (propertize (concat "The quick brown fox jumps over the lazy dog 1 l; 0 O o ("
font-family ")\n") 'face `((:family ,font-family) italic)))))))
