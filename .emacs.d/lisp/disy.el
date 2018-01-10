;;; disy.el --- Emacs configuration for work at Disy

;; Copyright (C) 2017 Arne Babenhauserheide for Disy

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
;;      (require 'disy)
;;

;;; Change Log:

;; 2017-12-11 - started

;;; Code:

(defconst my-c-lineup-maximum-indent 30)

(defun my-c-lineup-arglist (langelem)
  (let ((ret (c-lineup-arglist langelem)))
    (if (< (elt ret 0) my-c-lineup-maximum-indent)
	ret
      (save-excursion
	(goto-char (cdr langelem))
	(vector (+ (current-column) 8))))))

(require 'google-c-style)

(defun my-indent-setup ()
  (google-set-c-style)
  (setcdr (assoc 'arglist-cont-nonempty c-offsets-alist)
	  '(c-lineup-gcc-asm-reg my-c-lineup-arglist)))

(add-hook 'java-mode-hook 'my-indent-setup)

;; TODO: finalize this, it's still coarse
(c-add-style "disy-java"
	     '("java"
	       (c-basic-offset . 2)	; Guessed value
	       (c-offsets-alist
		(annotation-top-cont . 0)   ; Guessed value
		(arglist-cont . 0)	; Guessed value
		(arglist-intro . +)	; Guessed value
		(block-close . 0)	; Guessed value
		(class-close . 0)	; Guessed value
		(defun-block-intro . +)	; Guessed value
		(inclass . +)		; Guessed value
		(inline-close . 0)	; Guessed value
		(statement . 0)		; Guessed value
		(statement-block-intro . +) ; Guessed value
		(statement-cont . +)	; Guessed value
		(topmost-intro . +)	; Guessed value
		(access-label . -)
		(annotation-var-cont . +)
		(arglist-close . c-lineup-close-paren)
		(arglist-cont-nonempty . c-lineup-arglist)
		(block-open . 0)
		(brace-entry-open . 0)
		(brace-list-close . 0)
		(brace-list-entry . 0)
		(brace-list-intro . +)
		(brace-list-open . 0)
		(c . c-lineup-C-comments)
		(case-label . 0)
		(catch-clause . 0)
		(class-open . 0)
		(comment-intro . c-lineup-comment)
		(composition-close . 0)
		(composition-open . 0)
		(cpp-define-intro c-lineup-cpp-define +)
		(cpp-macro . -1000)
		(cpp-macro-cont . +)
		(defun-close . 0)
		(defun-open . 0)
		(do-while-closure . 0)
		(else-clause . 0)
		(extern-lang-close . 0)
		(extern-lang-open . 0)
		(friend . 0)
		(func-decl-cont . +)
		(incomposition . +)
		(inexpr-class . +)
		(inexpr-statement . +)
		(inextern-lang . +)
		(inher-cont . c-lineup-multi-inher)
		(inher-intro . +)
		(inlambda . c-lineup-inexpr-block)
		(inline-open . +)
		(inmodule . +)
		(innamespace . +)
		(knr-argdecl . 0)
		(knr-argdecl-intro . +)
		(label . 2)
		(lambda-intro-cont . +)
		(member-init-cont . c-lineup-multi-inher)
		(member-init-intro . +)
		(module-close . 0)
		(module-open . 0)
		(namespace-close . 0)
		(namespace-open . 0)
		(objc-method-args-cont . c-lineup-ObjC-method-args)
		(objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
		(objc-method-intro .
				   [0])
		(statement-case-intro . +)
		(statement-case-open . 0)
		(stream-op . c-lineup-streamop)
		(string . -1000)
		(substatement . +)
		(substatement-label . 2)
		(substatement-open . +)
		(template-args-cont c-lineup-template-args +)
		(topmost-intro-cont . c-lineup-topmost-intro-cont))))

(provide 'disy)
;;; disy.el ends here
