
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu 0.3)
 '(ac-auto-start 1)
 '(ac-comphist-threshold 0.7)
 '(ac-menu-height 15)
 '(ac-quick-help-height 50)
 '(browse-url-browser-function (quote eww-browse-url))
 '(compilation-auto-jump-to-first-error nil)
 '(compilation-error-regexp-alist
   (quote
    (("\\[ERROR]\\ \\(/.*\\):\\[\\([0-9]*\\),\\([0-9]*\\)]" 1 2 3)
     absoft ada aix ant bash borland python-tracebacks-and-caml comma cucumber msft edg-1 edg-2 epc ftnchek iar ibm irix java jikes-file maven jikes-line gcc-include ruby-Test::Unit gnu lcc makepp mips-1 mips-2 msft omake oracle perl php rxp sparc-pascal-file sparc-pascal-line sparc-pascal-example sun sun-ada watcom 4bsd gcov-file gcov-header gcov-nomark gcov-called-line gcov-never-called perl--Pod::Checker perl--Test perl--Test2 perl--Test::Harness weblint guile-file guile-line
     ("^Testcase: \\(.*\\) took \\(.*\\) sec
.*\\(FAILED\\).*
.*
.*
.*
Test \\(.*\\) FAILED" nil nil nil 2 1
(3
 (quote compilation-error-face)))
     ("^Testcase: \\(.*\\) took \\(.*\\) sec
.*\\(FAILED\\).*
" nil nil nil 2 1
(3
 (quote compilation-error-face))))))
 '(compilation-scroll-output (quote first-error))
 '(default-frame-alist (quote ((width . 140) (height . 40))))
 '(doc-view-resolution 150)
 '(doc-view-scale-internally nil)
 '(dynamic-completion-mode t)
`(eclim-eclipse-dirs
`( ,(expand-file-name "~/eclipse/") "/Applications/eclipse" "/usr/lib/eclipse" "/usr/local/lib/eclipse" "/usr/share/eclipse" "/Applications/Eclipse.app/Contents/Eclipse/"))
 '(eclim-java-documentation-root "/home/babenhauserheide/Java/jdk-8u151-docs-all/docs/")
 '(eclim-print-debug-messages nil)
 '(eclim-problems-refresh-delay 1)
 '(eclim-problems-show-pos t)
 '(eclimd-autostart t)
 '(eclimd-autostart-with-default-workspace t)
 '(eclimd-default-workspace "~/eclipse-workspace")
 '(flyspell-default-dictionary "en")
 '(font-use-system-font t)
 '(glasses-face (quote bold))
 '(glasses-original-separator "")
 '(glasses-separate-capital-groups t)
 '(glasses-separate-parentheses-p nil)
 '(glasses-separator "")
 '(help-at-pt-timer-delay 0.1)
 '(highlight-indent-guides-character 9474)
 '(highlight-indent-guides-method (quote character))
 '(ido-buffer-disable-smart-matches nil)
 '(ido-cr+-auto-update-blacklist t)
 '(ido-cr+-function-whitelist nil)
 '(ido-cr+-max-items 300000)
 '(ido-cr+-replace-completely nil)
 '(ido-enable-dot-prefix t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-max-work-file-list 10)
 '(ido-mode (quote both) nil (ido))
 '(ido-ubiquitous-mode t)
 '(ido-use-filename-at-point (quote guess))
 '(ido-use-url-at-point t)
 '(inhibit-startup-screen t)
 '(org-agenda-files (quote ("~/Plan/plan.org")))
 '(org-agenda-start-with-clockreport-mode t)
 '(org-babel-load-languages (quote ((emacs-lisp . t) (gnuplot . t))))
'(org-capture-templates
(quote
 (("A" "Arlaubstag-Result" entry
   (file+headline "~/Plan/plan.org" "Arlaubstag")
   "** ❢ Arlaubstag %<%Y-%m-%d> %?" :clock-in t :clock-resume t)
  ("a" "Arlaubstags-Idee" entry
   (file+headline "~/Plan/plan.org" "Arlaubstag")
   "** %?" :clock-in t :clock-resume t)
  ("s" "Standup entry" entry
   (file+headline "~/Plan/plan.org" "StandUp Entwicklung")
   "*** ❢ sup: %<%Y-%m-%d>
*Done:*
- %? 

*In Progress:*
- 
" :clock-in t :clock-resume t)
  ("l" "Task for later" entry
   (file+headline "~/Plan/plan.org" "Tasks")
   "** ❢ %?" :clock-in t :clock-resume t)
  ("i" "Task to start immediately after filing" entry
   (file+headline "~/Plan/plan.org" "Tasks")
   "** ☯ %?" :jump-to-captured t :clock-in t :clock-keep t))))
 '(org-clock-in-resume t)
 '(org-clock-mode-line-total (quote today))
 '(org-clock-report-include-clocking-task t)
 '(org-default-notes-file "~/Plan/plan.org")
 '(org-directory "~/Plan")
 '(org-edit-src-content-indentation 0)
 '(org-pretty-entities nil)
 '(org-src-fontify-natively t)
 '(org-src-tab-acts-natively nil)
'(package-archives
(quote
 (("gnu" . "http://elpa.gnu.org/packages/")
  ("melpa" . "https://melpa.org/packages/"))))
'(package-selected-packages
(quote
 (goto-chg furl javadoc-lookup dummy-h-mode dumb-jump rfringe java-snippets ob-restclient restclient tabbar magit buttercup yasnippet-snippets yatemplate noflet browse-kill-ring ido-yes-or-no ido-hacks ido-sort-mtime ido-completing-read+ amx smex counsel counsel-pydoc swiper qml-mode fic-mode goto-last-change htmlize org-jira s yasnippet which-key gnuplot eslint-fix flycheck flycheck-demjsonlint flycheck-kotlin flycheck-package flycheck-pycheckers flycheck-yamllint highlight-indent-guides ac-emacs-eclim auto-complete kanban markdown-mode markdown-toc)))
 '(tabbar-background-color "gainsboro")
 '(tabbar-mode t nil (tabbar))
 '(tabbar-separator (quote (0.8)))
 '(tabbar-use-images nil)
 '(tool-bar-mode nil)
 '(type-break-demo-boring-stats t)
 '(type-break-good-rest-interval 300)
 '(type-break-interval 5400)
 '(type-break-keystroke-threshold (quote (nil . 10500)))
 '(type-break-mode t)
 '(type-break-mode-line-message-mode t)
 '(type-break-query-interval 300)
 '(type-break-query-mode t)
 '(type-break-warning-repeat 120)
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Liberation Mono" :foundry "1ASC" :slant normal :weight normal :height 107 :width normal))))
 '(tabbar-button-highlight ((t (:inherit tabbar-button))))
 '(tabbar-default ((t (:inherit variable-pitch :background "gray75" :foreground "gray50" :height 0.8))))
 '(tabbar-modified ((t (:inherit tabbar-unselected :foreground "green" :box (:line-width 1 :color "white" :style released-button)))))
 '(tabbar-selected ((t (:inherit tabbar-default :background "gainsboro" :foreground "dim gray" :box (:line-width 1 :color "white" :style pressed-button)))))
 '(tabbar-selected-modified ((t (:inherit tabbar-selected :foreground "red" :box (:line-width 1 :color "white" :style released-button)))))
 '(tabbar-unselected ((t (:inherit tabbar-default)))))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;; load lisplets: 
;   Small lisp snippets to activate or adapt features or add small stuff.
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'babcore2)

(require 'journal)
(let ((journal-dir "~/Plan/journal"))
  (if (file-directory-p journal-dir)
      (setq-default journal-dir journal-dir)))

;; adjust tool bar
; from oxygen-icons, the oxygen art team (that’s how they want to be called), under LGPL.
(when (find-image '((:type xpm :file "planner.xpm")))
  (tool-bar-add-item "planner" 'agenda-and-todo 'agenda-and-todo))
; from oxygen-icons, the oxygen art team (that’s how they want to be called), under LGPL.
(when (find-image '((:type xpm :file "okteta.xpm")))
  (tool-bar-add-item "okteta" 'deft 'journal))
; from oxygen-icons, the oxygen art team (that’s how they want to be called), under LGPL.
(when (find-image '((:type xpm :file "journal-new.xpm")))
  (tool-bar-add-item "journal-new" 'journal 'new-journal-entry))

; add goto last change reverse (see goto-chg in babcore)
(global-set-key [(control meta .)] 'goto-last-change-reverse)

;; no fsync on write (faster on USB-stick).
; Set `write-region-inhibit-fsync' to non-nil: avoid fsync on write. "Customize Save" in the !Emacs Manual (C-h r g Customize Save <RET>).
; On sane filesystems like (not 'vfat), this removes the short freezing on write.
(setq write-region-inhibit-fsync t)

; put the current w3m url into register a
(defun w3m-current-url-to-register-a ()
  (interactive)
  (set-register 97 w3m-current-url))

(defun org-insert-scaled-screenshot ()
  "Insert a scaled screenshot 
for inline display 
into your org-mode buffer."
  (interactive)
  (let ((filename 
         (concat "screenshot-" 
                 (substring 
                  (shell-command-to-string 
                   "date +%Y%m%d%H%M%S")
                  0 -1 )
                 ".png")))
    (let ((scaledname 
           (concat filename "-width300.png")))
      (shell-command 
       (concat "import -window root " 
               filename))
      (shell-command 
       (concat "convert -adaptive-resize 300 " 
               filename " " scaledname))
      (insert (concat "[[./" scaledname "]]")))))

;; Use conf-mode for editing the .hgrc
(add-to-list 'auto-mode-alist '("\\.?hgrc\\'"  . conf-mode))

; a simple way to get the current date
(defun date-today ()
  "print the current time and date in iso-format"
  (interactive)
  ; string format: empty line, date, ----, empty line.
  (insert (format-time-string "%Y-%m-%d")))

(defun date-now ()
  "print the current time and date in iso-format"
  (interactive)
  ; string format: empty line, date, ----, empty line.
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun strikethrough (start end)
  (interactive "r") 
  (goto-char (min start end))
  (while (< (point) (+ (max start end) (abs (- start end))))
	(forward-char)
	(insert "̶")))

;;; Open files as root - quickly
(defcustom find-file-root-prefix "/sudo:root@localhost:"
"Tramp root prefix to use.")

(defun find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable by
user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat find-file-root-prefix file)))
    (find-file file)))
;; or some other keybinding...
;; (global-set-key (kbd "C-x F") 'djcb-find-file-as-root)

(defun find-current-as-root ()
  "Reopen current file as root"
  (interactive)
  (set-visited-file-name (concat find-file-root-prefix (buffer-file-name)))
  (setq buffer-read-only nil))

(defun byte-compile-emacs-d ()
  "Byte-compile every *.el file in ~/.emacs.d"
  (interactive)
  (let ((init-el (expand-file-name "~/.emacs.d/init.el"))
        (files (split-string (shell-command-to-string (concat "find " (expand-file-name "~") "/.emacs.d -iname '*.el'")))))
    (loop for file in files do
          (if (and (file-exists-p file)
                   (not (equalp init-el file)))
              (byte-compile-file file)))))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "show shell colors in the compilation buffer"
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


;; Add yasnippets
(yas-global-mode 1)

;; regular auto-complete initialization
(require 'auto-complete-config)
(ac-config-default)

;; add the emacs-eclim source
(require 'ac-emacs-eclim)
(ac-emacs-eclim-config)

;;; fix ac-internal eclim setup: start completing from the first character (especially from a .)
;; redefine the basic eclim source to fire at a period:
(ac-define-source eclim
                  '((candidates . eclim--completion-candidates)
                    (action . ac-emacs-eclim-action)
                    (prefix . c-dot)
		    (requires . 0) ;; default is 1
                    (document . eclim--completion-documentation)
                    (cache)
                    (selection-face . ac-emacs-eclim-selection-face)
                    (candidate-face . ac-emacs-eclim-candidate-face)
                    (symbol . ".")))

;; start the eclim-shipped completion at the first char.
(ac-define-source emacs-eclim
                  '((candidates . eclim--completion-candidates)
                    (action . ac-emacs-eclim-action)
                    (prefix . eclim-completion-start)
		    ;; (requires . 0) ;; default is 1
                    (document . eclim-java-show-documentation-for-completion) ; eclim--completion-documentation)
                    (cache)
                    (selection-face . ac-emacs-eclim-selection-face)
                    (candidate-face . ac-emacs-eclim-candidate-face)
                    (symbol . "e")))

;; Activate eclim in all Java buffers
(defun my-java-mode-hook ()
  (eclim-mode t)
  (glasses-mode t)
  (add-to-list 'ac-sources 'ac-source-eclim))
(add-hook 'java-mode-hook 'my-java-mode-hook)

;; Use C-c C-c to open the problem correction dialog.
(define-key eclim-mode-map (kbd "C-c C-c") 'eclim-problems-correct)
;; Fix: 'eclim-java-method-signature-at-point does not exist. Use the shortcut to show the documentation.
(define-key eclim-mode-map (kbd "C-c C-e s") 'eclim-java-show-documentation-for-current-element)
;; Fake jump to test via dumb jump
(defun dumb-jump-to-test (&optional prompt)
  "Add Test to IDENTIFIER and jump to the result"
  (interactive "P")
  (let ((identifier (or (and (not prompt) (eclim--java-current-class-name))
			(read-from-minibuffer "Identifier without test suffix :"))))
    (dumb-jump-go nil nil (concat identifier "Test"))))
(define-key eclim-mode-map (kbd "C-c C-e f t") 'dumb-jump-to-test)


(defun eclim--completion-documentation (symbol)
  "Look up the documentation string for the given SYMBOL.
SYMBOL is looked up in `eclim--completion-condidates'."
  (let ((doc (assoc-default 'info (cl-find symbol eclim--completion-candidates :test #'string= :key #'eclim--completion-candidate-menu-item))))
    (when doc
      (eclim--render-doc doc))))

;; Show documentation for completions. TODO: Currently only works on classes, not on methods.
(defun render-html-to-text (&optional readable)
  (let ((dom (libxml-parse-html-region (point-min) (point-max))))
    (when readable
      (eww-score-readability dom)
      (setq dom (eww-highest-readability dom)))
    (erase-buffer)
    (shr-insert-document dom)))

(defun eclim-java-show-documentation-for-completion (symbol)
  "Displays the doc comments for the element at the pointers position."
  (let* ((package (substring symbol (+ 3 (string-match " - " symbol))))
	 (class (replace-regexp-in-string
		 "()" ""
		 (substring symbol 0 (or (string-match " : " symbol) (string-match " - " symbol)))))
	 (packagesymbol (concat package "." class))
	 (file (apply #'concat (reverse (gethash packagesymbol jdl/index))))
	 ;; (anchor (concat "<a name=\"" class "\""))
	 ;; (anchormark "---ANCHOR---")
	 (doc (if file
		  (ignore-errors
		    (concat (with-current-buffer
			     (url-retrieve-synchronously file)
			     ;; find anchors for method, if they exist, and set marks
			     ;; (while (search-forward anchor nil t)
			     ;;   (replace-match (concat anchormark anchor) nil t)) 
			     ;; parse the HTML
			     (render-html-to-text t)
			     ;; replace nonbreaking spaces
			     (goto-char 0)
			     (while (search-forward " " nil t)
			       (replace-match " " nil t)) 
			     ;; (goto-char 0)
			     ;; (search-forward package nil t)
			     ;; (delete-region 1 (min (point) (point-max)))
			     ;; (goto-char 0)
			     ;; (search-forward anchormark nil t)
			     ;; (delete-region 1 (min (point) (point-max)))
			     (string-trim (buffer-substring (point-min) (point-max))))))
		symbol)))
    doc))
(require 'javadoc-lookup)
;; (javadoc-add-roots "/home/babenhauserheide/Java/jdk-8u151-docs-all/docs/" "~/ci-status/target/site/apidocs/")
;; adjust adding artifacts to report all errors together at the end
(defun javadoc-add-artifacts (&rest artifacts)
  "Add Maven repository artifacts to the javadoc-lookup index.
An artifact is specified by a sequence of three strings:
 [groupId artifactId version]."
  (let ((errors '()))
    (dolist (artifact artifacts)
      (if (maven-fetch artifact)
	  (javadoc-add-roots (maven-fetch-unpack artifact))
	(setq errors
	      (cons (format "Failed to fetch %s" artifact) errors))))
    (if (not (null errors))
	(error (string-join errors "\n")))))
(defun javadoc-add-all-maven-artifacts ()
  "add all maven artifacts in the current java project"
  (interactive)
  (let ((dir (eclim--project-dir)))
    (with-temp-buffer
      (insert (shell-command-to-string (concat "cd " dir "; mvn dependency:resolve | grep '.INFO.    ' | sed 's/.INFO.    //'")))
      ;; format is group:artifact:jar:version:phase
      ;; we need group artifact version
      (goto-char 0)
      (while (not (equal (point) (point-max)))
	(search-forward ":")
	(replace-match " ")
	(search-forward ":")
	(replace-match " ")
	;; delete the jar
	(set-mark (point))
	(search-forward ":")
	(delete-region (mark) (point))
	(search-forward ":")
	(backward-char)
	(kill-line)
	(forward-line))
      (apply 'javadoc-add-artifacts
	     (map 'list (lambda (x) (split-string x " " t))
		  (split-string (buffer-string) "\n" t))))))
;; open javadoc in eww with a new window
(defcustom javadoc-lookup-in-other-window t
  "Use a new window to lookup code.")
(defun javadoc-lookup (name)
  "Lookup based on class name."
  (interactive (list (jdl/completing-read)))
  (let ((file (apply #'concat (reverse (gethash name jdl/index)))))
    (when javadoc-lookup-in-other-window (other-window 1))
    (when file (browse-url file))))

(defun eww-browse-url-other-window (url)
    (eww-browse-url url t))


;; Highlight TODO, FIXME, ... in any programming mode
(require 'fic-mode)
(add-hook 'prog-mode-hook 'fic-mode)

;; Highlight intentation (delayed until the default face is available)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;; test ivy (is unnerving)
;; (require 'ivy)
;; (require 'swiper)
;; (setq ivy-use-virtual-buffers t)
;; (global-set-key "\C-s" 'counsel-grep-or-swiper)
;; (global-set-key (kbd "C-c C-r") 'ivy-resume)
;; (global-set-key (kbd "C-x C-f") 'counsel-find-file) ;; I prefer horizontal ido
;; (global-set-key (kbd "M-x") 'counsel-M-x) ;; stick with execute-extended-command, add smex

;; add smex
(defun smex-update-after-load (unused)
  (when (boundp 'smex-cache)
    (smex-update)))
(add-hook 'after-load-functions 'smex-update-after-load)

;; browse kill ring
(browse-kill-ring-default-keybindings) ;; works will with ivy search
;; ido kill ring
(require 'kill-ring-ido)

;; ido-preview
(require 'ido-preview)
(add-hook 'ido-setup-hook
  (lambda()
    (define-key ido-completion-map (kbd "C-M-p") (lookup-key ido-completion-map (kbd "C-p")))
    (define-key ido-completion-map (kbd "C-M-n") (lookup-key ido-completion-map (kbd "C-n"))) ; currently, this makes nothing. Maybe they'll make C-n key lately.
    (define-key ido-completion-map (kbd "C-p") 'ido-preview-backward)
    (define-key ido-completion-map (kbd "C-n") 'ido-preview-forward)))


(defun yas-ido-expand ()
  "Lets you select (and expand) a yasnippet key"
  (interactive)
    (let ((original-point (point)))
      (while (and
              (not (= (point) (point-min) ))
              (not
               (string-match "[[:space:]\n]" (char-to-string (char-before)))))
        (backward-word 1))
    (let* ((init-word (point))
           (word (buffer-substring init-word original-point))
           (list (yas-active-keys)))
      (goto-char original-point)
      (let ((key (remove-if-not
                  (lambda (s) (string-match (concat "^" word) s)) list)))
        (if (= (length key) 1)
            (setq key (pop key))
          (setq key (ido-completing-read "key: " list nil nil word)))
        (delete-char (- init-word original-point))
        (insert key)
        (yas-expand)))))
(global-set-key (kbd "C-<tab>") 'yas-ido-expand)

;; Disy-specific enhancements
(require 'disy)

;; global keymap setup
(require 'keyboard-shortcuts)


;;;;;;;;;;;;;
;;; Fixes ;;;
;;;;;;;;;;;;;

;; Ignore ido save errors
(defun ido-kill-emacs-hook ()
  (ignore-errors (ido-save-history)))

;;; Autoconf mode
;; Mark all AC_* and AS_* functions as builtin.
(add-hook 'autoconf-mode-hook 
          (lambda () 
            (add-to-list 'autoconf-font-lock-keywords '("\\(\\(AC\\|AS\\|AM\\)_.+?\\)\\((\\|\n\\)" (1 font-lock-builtin-face)))))

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((extended-command-history . 300)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 5000)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 2000)
                (shell-command-history    . 50)
                register-alist)))

;; restore only 5 buffers at once and the rest lazily
(setq desktop-restore-eager 2)

;; nicer org clocktable layout
(defun my-org-clocktable-indent-string (level)
  (if (= level 1)
      ""
    (let ((str "└"))
      (while (> level 2)
        (setq level (1- level)
              str (concat str "──")))
      (concat str "─> "))))

(advice-add 'org-clocktable-indent-string :override #'my-org-clocktable-indent-string)

;; continuously update agenda view, from http://thomasf.github.io/solarized-css/test/org-hacks.html
(defun kiwon/org-agenda-redo-in-other-window ()
  "Call org-agenda-redo function even in the non-agenda buffer."
  (interactive)
  (let ((agenda-window (get-buffer-window org-agenda-buffer-name t)))
    (when agenda-window
      (with-selected-window agenda-window (org-agenda-redo)))))
(add-hook 'org-clock-in-hook 'kiwon/org-agenda-redo-in-other-window)
(add-hook 'org-clock-out-hook 'kiwon/org-agenda-redo-in-other-window)
(add-hook 'org-after-todo-state-change-hook 'kiwon/org-agenda-redo-in-other-window)
