;;; stripspace.el --- Auto remove trailing whitespace before saving a buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2025 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 0.9.9
;; URL: https://github.com/jamescherti/stripspace.el
;; Keywords: convenience
;; Package-Requires: ((emacs "24.3"))
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

;;; Defgroup and defcustom

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

(defcustom stripspace-restore-column nil
  "Restore the column after deleting the trailing whitespace."
  :type 'boolean
  :group 'stripspace)

;;; Internal variables

(defvar-local stripspace--column nil)

;;; Internal functions

(defun stripspace--message (&rest args)
  "Display a message with the same ARGS arguments as `message'."
  (apply #'message (concat "[stripspace] " (car args)) (cdr args)))

(defmacro stripspace--verbose-message (&rest args)
  "Display a verbose message with the same ARGS arguments as `message'."
  `(progn
     (when stripspace-verbose
       (stripspace--message
        (concat "[stripspace] " ,(car args)) ,@(cdr args)))))

(defun stripspace--save-column ()
  "Save the current cursor column position and remove trailing whitespace.
This function is triggered by `before-save-hook'. It stores the current column
in a buffer-local variable and deletes any trailing whitespace."
  (setq stripspace--column (current-column))
  (delete-trailing-whitespace))

(defun stripspace--move-to-saved-column ()
  "Restore the cursor to the previously saved column after saving.
This function is triggered by `after-save-hook'. It attempts to move the cursor
back to its original column while ensuring the buffer remains unmodified.
Restoring trailing whitespace is only done to maintain cursor position without
marking the buffer as changed."
  (when stripspace-restore-column
    (unwind-protect
        (progn
          (when stripspace--column
            ;; Restore the column position, adding spaces if necessary
            (move-to-column stripspace--column t))
          ;; Prevent marking the buffer as modified
          (set-buffer-modified-p nil))
      (setq stripspace--column nil))))

;;; Modes

;;;###autoload
(define-minor-mode stripspace-local-mode
  "Toggle `stripspace-local-mode'.
This mode ensures that trailing whitespace is removed before saving a buffer."
  :global t
  :lighter " StripSpc"
  :group 'stripspace
  (if stripspace-local-mode
      (progn
        ;; Mode enabled
        (add-hook 'before-save-hook #'stripspace--save-column -99 t)
        (add-hook 'after-save-hook #'stripspace--move-to-saved-column 99 t))
    ;; Mode disabled
    (remove-hook 'before-save-hook #'stripspace--save-column t)
    (remove-hook 'after-save-hook #'stripspace--move-to-saved-column t)))

(provide 'stripspace)
;;; stripspace.el ends here
