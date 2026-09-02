;;; hel-macros.el --- Macros for defining commands and advices -*- lexical-binding: t -*-
;;
;; Copyright © 2025-2026 Yuriy Artemyev
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.13.0
;; Homepage: https://github.com/helheim-emacs/hel
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;; Hel is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Hel is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Macros used to define Hel commands and advices.
;;
;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'map)
(require 'dash)
(require 'hel-vars)
(require 'hel-lib)
(require 'pcre2el)

;;;; Define advices

(cl-defmacro hel-define-advice (symbol (how lambda-list &optional (name 'hel))
                                       &rest body)
  "Wrapper around `define-advice' that automatically add/remove advice
when `hel-mode' is toggled on or off."
  (declare (indent 2)
           (doc-string 3)
           (debug (sexp sexp def-body)))
  (let ((advice (intern (format "%s@%s" symbol name))))
    `(prog1 (defun ,advice ,lambda-list ,@body)
       (cl-pushnew '(,symbol ,how ,advice) hel--advices :test #'equal)
       (when hel-mode
         (advice-add ',symbol ,how #',advice)))))

(defmacro hel-advice-add (symbol how function)
  "Wrapper around `advice-add' that automatically add/remove advice
when `hel-mode' is toggled on or off"
  (declare (indent defun)
           (debug (symbolp keywordp symbolp)))
  `(progn
     (cl-pushnew (list ,symbol ,how ,function) hel--advices :test #'equal)
     (when hel-mode
       (advice-add ,symbol ,how ,function))))

;;;; Define command

(defmacro hel-define-command (command args &rest body)
  "Define Hel COMMAND.
Wrapper around `defun' macro, that additionally takes following keyword
parameters:

`:multiple-cursors'
  - t    Command will be executed for all cursors;
  - nil  Command will be executed only for main cursor.

`:merge-selections'
  Any Emacs Lisp FORM, that will be evaluated after COMMAND execution
  and if it evaluates to non-nil — overlapping selections (regions)
  will be merged into single selection.

\(fn COMMAND (ARGS...) [DOC] [[KEY VALUE]...] BODY...)"
  (declare (indent defun)
           (doc-string 3)
           (debug ( &define name
                    [&optional lambda-list]
                    [&optional stringp]
                    [&rest keywordp sexp]
                    [&optional ("interactive" [&rest form])]
                    def-body)))
  (-let* ((doc (pcase (car-safe body)
                 ((and `(format . ,_) doc-form)
                  (eval doc-form t))
                 ((and (pred stringp) doc)
                  doc)))
          ((kwargs . body) (hel-split-keyword-args (if doc (cdr body) body)))
          (properties (->> kwargs
                           (map-apply (lambda (key value)
                                        (pcase key
                                          (:multiple-cursors
                                           `(put ',command 'multiple-cursors ,value))
                                          (:merge-selections
                                           `(put ',command 'merge-selections
                                                 ,(if (symbolp value)
                                                      `',value
                                                    `(lambda () ,value))))))))))
    ;; macro expansion
    `(progn
       (defun ,command (,@args)
         ,@(if doc `(,doc))
         ,@body)
       ,@properties)))

;; Since Emacs 31, the autoload scraper expands a macro that carries
;; this property, so a plain `;;;###autoload' before a `hel-define-command'
;; form yields an `autoload' call. Emacs 29 and 30 know only a fixed set of
;; definers,and copy the whole form into the loaddefs file, where it runs
;; before Hel is loaded. To stay portable, write the cookie out in full:
;;
;;     ;;;###autoload (autoload 'my-command "my-file" nil t)
;;     (hel-define-command my-command () ...)
;;
(function-put 'hel-define-command 'autoload-macro 'expand)

;;; .
(provide 'hel-macros)
;;; hel-macros.el ends here
