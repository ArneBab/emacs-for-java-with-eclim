;;; keyboard-shortcuts.el --- Central place for custom key setup

;; Copyright (C) 2017 Arne Babenhauserheide for Disy

;; Author: Arne Babenhauserheide (and various others in Emacswiki and elsewhere).
;; Maintainer: Arne Babenhauserheide
;; Created 29. August 2017
;; Version: 0.0.1
;; Version Keywords: core configuration
;; Package-Requires:

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
;;
;; If you add shortcuts in external tools (i.e. calling emacsclient
;; for desktop-global shortcuts), add a note how to reproduce
;; this. Always also add the shortcut within Emacs to ensure that the
;; shortcuts keep working in a terminal.

;; Quick Start / installation:
;; 1. Download this file and put it next to other files Emacs includes
;; 2. Add this to you .emacs file and restart emacs:
;;
;;      (require 'keyboard-shortcuts)
;;

;;; Change Log:

;; 2017-12-18 - started

;;; Code:

;; F12: C-f12 is taken by KDE plasma, M-C-f12 is switch terminal!

;; KDE: show custom agenda with kanban via f12:
(with-eval-after-load 'org
  (setq org-agenda-custom-commands
        '(("o" "Agenda and TODOs"
           ((agenda nil ((org-agenda-compact-blocks nil)(org-agenda-block-separator ?-)(org-agenda-overriding-header "")))
            (todo "" ((org-agenda-block-separator ?-)))
	    (tags "KANBAN" ((org-agenda-block-separator ?-)(org-agenda-compact-blocks nil)(org-agenda-overriding-header ""))))))))

(defun my/org-agenda-show-kanban ()
  (interactive)
  (save-excursion
    (search-forward ":KANBAN:")
    (org-agenda-goto)
    (org-narrow-to-subtree)
    (show-all)
    (fit-window-to-buffer)
    (widen)
    (recenter-top-bottom 0)
    (other-window -1)))

(defun agenda-and-todo ()
  (interactive)
  (org-agenda nil "o")
  (my/org-agenda-show-kanban))
;;      systemsettings shortcuts: map f12 to
;;        emacsclient -e '(progn (show-frame)(agenda-and-todo))'
(global-set-key (kbd "<f12>") 'agenda-and-todo)

;; KDE: record new issue with M-f12 (alt f12):
;;      systemsettings shortcuts: map alt f12 to
;;        emacsclient -e '(progn (show-frame)(org-capture))'
(global-set-key (kbd "M-<f12>") 'org-capture)

;; clock into the current task via S-f12 (shift f12). rationale: shift
;; is used to shift from one task to another without clocking out.
(global-set-key (kbd "S-<f12>") 'org-clock-in)

;; KDE: global clock out via M-S-f12 (alt-shift f12):
;;      systemsettings shortcuts: map f12 to
;;        emacsclient -e '(progn (show-frame)(org-clock-out))'
(global-set-key (kbd "M-S-<f12>") 'org-clock-out)

;; KDE: update all agenda kanban tables with C-S-f12 (ctrl-shift f12)
(defun kanban-update-all ()
  (interactive)
  (cl-loop for i in org-agenda-files do
	   (with-current-buffer (find-file-noselect i)
	     (save-excursion ;; avoid changing the position in the agenda files
	       (beginning-of-buffer)
	       (while (search-forward "='(kanban-" nil t)
		 (org-ctrl-c-ctrl-c))))))
;;      systemsettings shortcuts: map control shift f12 to
;;        emacsclient -e '(progn (show-frame)(kanban-update-all))'
(global-set-key (kbd "C-S-<f12>") 'kanban-update-all)

;; Use the menu key for amx, because amx can be unreliable at times, so it should not shadow M-x
(global-set-key (kbd "<menu>") 'amx)

(provide 'keyboard-shortcuts)
;;; keyboard-shortcuts.el ends here
