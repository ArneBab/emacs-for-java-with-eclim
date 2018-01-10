(require 'eclimd)


;; local idle timer thanks to https://emacs.stackexchange.com/a/13275/614

(defun run-with-local-idle-timer (secs repeat function &rest args)
  "Like `run-with-idle-timer', but always runs in the `current-buffer'.

Cancels itself, if this buffer was killed."
  (let* (;; Chicken and egg problem.
         (fns (make-symbol "local-idle-timer"))
         (timer (apply 'run-with-idle-timer secs repeat fns args))
         (fn `(lambda (&rest args)
                (if (not (buffer-live-p ,(current-buffer)))
                    (cancel-timer ,timer)
                  (with-current-buffer ,(current-buffer)
                    (apply (function ,function) args))))))
    (fset fns fn)
    fn))


(defun my-java-mode-hook ()
  (eclim-mode t)
  (run-with-local-idle-timer 1 t 'eclim-problems-compilation-buffer)
  (message "to see compile warnings and errors, call M-x eclim-problems-compilation-buffer once eclimd is started"))

(add-hook 'java-mode-hook 'my-java-mode-hook)
;; use only java-mode, not jde-mode
(remove-alist 'auto-mode-alist "\\.java\\'")
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode))

;; compilation error messages,
;; see help-at-pt-display-when-idle and help-at-pt-timer-delay
(help-at-pt-set-timer)

;; regular auto-complete initialization
(require 'auto-complete-config)
(ac-config-default)

;; add the emacs-eclim source
(require 'ac-emacs-eclim)
(ac-emacs-eclim-config)

(provide 'java)
