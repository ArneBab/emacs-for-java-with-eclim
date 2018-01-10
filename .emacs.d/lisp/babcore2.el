;;; babcore2.el --- Core Emacs configuration. This should be the minimum in every emacs config.

;; Copyright (C) 2017 Arne Babenhauserheide

;; Author: Arne Babenhauserheide (and various others in Emacswiki and elsewhere).
;; Maintainer: Arne Babenhauserheide
;; Created 29. August 2017
;; Version: 0.0.1
;; Version Keywords: core configuration
;; Package-Requires: ((goto-last-change) (key-chord))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Quick Start / installation:
;; 1. Download this file and put it next to other files Emacs includes
;; 2. Add this to you .emacs file and restart emacs:
;; 
;;      (require 'babcore2)
;;
;; Use Case: Use a common core configuration so you can avoid the
;;   tedious act of gathering all the basic stuff over the years and
;;   can instead concentrate on the really cool new stuff Emacs offers
;;   you.
;;

;;; Change Log:

;; 2017-08-29 - Initial release

;;; Code:

; go to the last change
(require 'goto-last-change)
(global-set-key [(control .)] 'goto-last-change)
; M-. can conflict with etags tag search. But C-. can get overwritten
; by flyspell-auto-correct-word. And goto-last-change needs a really
; fast key.
(global-set-key [(meta .)] 'goto-last-change)
; ensure that even in worst case some goto-last-change is available
(global-set-key [(control meta .)] 'goto-last-change)

; use key chords invoke commands
(require 'key-chord)
(key-chord-mode 1)
; buffer actions
(key-chord-define-global "vb"     'eval-buffer)
; frame actions
(key-chord-define-global "xo"     'other-window);
(key-chord-define-global "x1"     'delete-other-windows)
(key-chord-define-global "x0"     'delete-window)
(defun kill-this-buffer-if-not-modified ()
  (interactive)
  ; taken from menu-bar.el
  (if (menu-bar-non-minibuffer-window-p)
      (kill-buffer-if-not-modified (current-buffer))
    (abort-recursive-edit)))
; file actions
(key-chord-define-global "bf"     'ido-switch-buffer)
(key-chord-define-global "bk"     'kill-this-buffer-if-not-modified)
(key-chord-define-global "cf"     'ido-find-file)
(key-chord-define-global "vc"     'vc-next-action)

(setq visible-bell t)

(defun show-frame (&optional frame)
  "Show the current Emacs frame or the FRAME given as argument.

And make sure that it really shows up!"
  (raise-frame)
  ; yes, you have to call this twice. Don’t ask me why…
  ; select-frame-set-input-focus calls x-focus-frame and does a bit of
  ; additional magic.
  (select-frame-set-input-focus (selected-frame))
  (select-frame-set-input-focus (selected-frame)))

;; let emacs blink when something interesting happens.
;; in KDE this marks the active Emacs icon in the tray.
(defun x-urgency-hint (frame arg &optional source)
  "Set the x-urgency hint for the frame to arg: 

- If arg is nil, unset the urgency.
- If arg is any other value, set the urgency.

If you unset the urgency, you still have to visit the frame to make the urgency setting disappear (at least in KDE)."
    (let* ((wm-hints (append (x-window-property 
                "WM_HINTS" frame "WM_HINTS" source nil t) nil))
     (flags (car wm-hints)))
    (setcar wm-hints
        (if arg
        (logior flags #x100)
          (logand flags (lognot #x100))))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))

(defun x-urgent (&optional arg)
  "Mark the current emacs frame as requiring urgent attention. 

With a prefix argument which does not equal a boolean value of nil, remove the urgency flag (which might or might not change display, depending on the window manager)."
  (interactive "P")
  (let (frame (selected-frame))
  (x-urgency-hint frame (not arg))))

; Default KDE keybindings to make emacs nicer integrated into KDE. 

; can treat C-m as its own mapping.
; (define-key input-decode-map "\C-m" [?\C-1])

(defun revert-buffer-preserve-modes ()
  (interactive)
  (revert-buffer t nil t))

; C-m shows/hides the menu bar - thanks to http://stackoverflow.com/questions/2298811/how-to-turn-off-alternative-enter-with-ctrlm-in-linux
; f5 reloads
(defconst kde-default-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map [f5] 'revert-buffer-preserve-modes)
    (define-key map [?\C-1] 'menu-bar-mode)
    (define-key map [?\C-+] 'text-scale-increase)
    (define-key map [?\C--] 'text-scale-decrease) ; shadows 'negative-argument which is also available via M-- and C-M--, though.
    (define-key map [C-kp-add] 'text-scale-increase)
    (define-key map [C-kp-subtract] 'text-scale-decrease)
    map)
  "Keymap for `kde-default-keys-minor-mode'.")

;; Minor mode for keypad control
(define-minor-mode kde-default-keys-minor-mode
  "Adds some default KDE keybindings"
  :global t
  :init-value t
  :lighter ""
  :keymap 'kde-default-keys-minor-mode-map
  )

;; Set the frame title as by http://www.emacswiki.org/emacs/FrameTitle
(setq frame-title-format (list "%b ☺ " (user-login-name) "@" (system-name) "%[ - GNU %F " emacs-version)
      icon-title-format (list "%b ☻ " (user-login-name) "@" (system-name) " - GNU %F " emacs-version))

(provide 'babcore2)
;;; babcore2.el ends here
