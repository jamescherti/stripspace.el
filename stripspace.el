;;; stripspace.el --- Ensures that Emacs removes trailing whitespace before saving a buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2025 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 0.9.9
;; URL: https://github.com/jamescherti/stripspace.el
;; Keywords: convenience
;; Package-Requires: ((emacs "24.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Ensures that Emacs removes trailing whitespace before saving a buffer.

;;; Code:

(defgroup stripspace nil
  "Ensures that Emacs removes trailing whitespace before saving a buffer"
  :group 'stripspace
  :prefix "stripspace-"
  :link '(url-link
          :tag "Github"
          "https://github.com/jamescherti/stripspace.el"))

(defcustom stripspace-verbose nil
  "Enable displaying messages (e.g., when files are compiled).
When set to non-nil, this option will cause messages to be shown during the
compilation process, providing feedback on the compilation status."
  :type 'boolean
  :group 'stripspace)

(defcustom stripspace-debug nil
  "Non-nil to display debug messages in the *stripspace:debug* buffer.
This displays a lot of messages."
  :type 'boolean
  :group 'stripspace)

(defun stripspace--message (&rest args)
  "Display a message with the same ARGS arguments as `message'."
  (apply #'message (concat "[stripspace] " (car args)) (cdr args)))

(defmacro stripspace--verbose-message (&rest args)
  "Display a verbose message with the same ARGS arguments as `message'."
  `(progn
     (when stripspace-debug
       (stripspace--debug-message ,(car args) ,@(cdr args)))
     (when stripspace-verbose
       (stripspace--message
        (concat "[stripspace] " ,(car args)) ,@(cdr args)))))

(defmacro stripspace--debug-message (&rest args)
  "Display a debug message with the same ARGS arguments as `message'.
The messages are displayed in the *stripspace* buffer."
  `(when stripspace-debug
     (stripspace--insert-message "*stripspace:debug*"
                                                    ,(car args) ,@(cdr args))))

;;;###autoload
(define-minor-mode stripspace-mode
  "Toggle `stripspace-mode'."
  :global t
  :lighter " stripspace"
  :group 'stripspace
  (if stripspace-mode
      t
    t))

(provide 'stripspace)
;;; stripspace.el ends here
