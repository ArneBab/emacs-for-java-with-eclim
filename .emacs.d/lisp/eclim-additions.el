;;; eclim-java-additions.el --- additions to the Eclipse interface.  -*- lexical-binding: t -*-

;; Copyright (C) 2018  Arne Babenhauserheide for Disy Interactive
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Contributors
;;
;; - 
;;
;;
;;; Commentary:
;;
;;; Conventions
;;
;; Conventions used in this file: Name internal variables and functions
;; "eclim--<descriptive-name>", and name eclim command invocations
;; "eclim/command-name", like eclim/project-list.

;;* Eclim Java

(require 'eclim-java)

;; (define-key eclim-mode-map (kbd "C-c C-e f t")   'eclim-java-find-junit-test)

;; (defun eclim--java-junit-find-test (project file offset encoding)
;;   (concat eclim-executable
;;           " -command java_junit_find_test -p " project
;;           " -f " file
;;           " -o " (number-to-string offset)
;;           " -e " encoding))

;; (defun eclim-java-find-junit-test (project file offset encoding)
;;   "Find the current JUnit tests for current project or
;; current class or current method.

;; This method hooks onto the running Eclipse process and is thus
;; much faster than running mvn test -Dtest=TestClass#method."
;;   (interactive (list (eclim-project-name)
;;                      (eclim--project-current-file)
;;                      (eclim--byte-offset)
;;                      (eclim--current-encoding)))
;;   (if (not (string= major-mode "java-mode"))
;;       (message "Running JUnit tests only makes sense for Java buffers.")
;;     (eclim/with-results hits ("java_junit_find_test" "-p" project "-f" file "-o" (number-to-string offset))
;;       (eclim--find-display-results (cdr i) hits t))))

(provide 'eclim-additions)
