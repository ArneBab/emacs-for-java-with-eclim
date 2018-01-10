;;; flycheck-demjsonlint.el --- Flychecker for json-mode using jsonlint from demjson

;; Copyright (C) 2016 Zenkie Zhu

;; Author: Zenkie Zhu <451218651@qq.com>
;; Keywords: convenience, tools
;; Homepage: https://github.com/z4139jq/flycheck-demjsonlint
;; Version: 0.1
;; Package-Requires: ((flycheck "30"))

;;; License:

;; Licensed under the MIT license, see:
;; https://github.com/z4139jq/flycheck-demjsonlint/blob/master/LICENSE

;;; Commentary:

;; Flycheck checker for json-mode using jsonlint from demjson which support lint rules.

;; # Setup
;;   0. Uninstall zaach/jsonlint if necessary to avoid conflicts: npm -g uninstall jsonlint
;;   1. Install demjson: pip install demjson or easy_install demjson
;;   2. (require 'flycheck-demjsonlint) in your init file

;; # Lint Rules(.demjsonrc)
;;   * Built-in support: .eslintrc, package.json
;;   * Precedence: ~/.demjsonrc over pkg-dir/.demjsonrc
;;   * Format: one record per line, `filename=jsonlint-cmd-options`, e.g. `.eslintrc=-S`
;;   * Implementation: demjsonlint is just a wrapper of jsonlint from demjson, jsonlint-cmd-options is passed to jsonlint as is


;;; Code:

(require 'flycheck)

(flycheck-define-checker json-demjsonlint
  "A JSON syntax and style checker using jsonlint from demjson.

See URL `https://github.com/dmeranda/demjson'."
  :command ("demjsonlint" source)
  :error-patterns
  ((warning line-start
            (file-name)
            ":" line
            ":" column ": Warning: "
            (message) line-end)
   (error line-start
          (file-name)
          ":" line
          ":" column ": Error: "
          (message) line-end))
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors errors))
  :modes json-mode)

(add-to-list 'flycheck-checkers 'json-demjsonlint)
(setq flycheck-json-demjsonlint-executable
      (expand-file-name "demjsonlint"
                        (file-name-directory (or load-file-name (buffer-file-name)))))

(provide 'flycheck-demjsonlint)
;;; flycheck-demjsonlint.el ends here
