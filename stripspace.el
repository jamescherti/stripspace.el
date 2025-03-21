;;; stripspace.el --- Auto remove trailing whitespace and restore column -*- lexical-binding: t; -*-

;; Copyright (C) 2025 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.0.0
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
;; The `stripspace.el' Emacs package offers `stripspace-local-mode', which
;; ensures that trailing whitespace is removed before saving a buffer.
;;
;; Additionally, The stripspace package offers an optional feature controlled by
;; the `stripspace-restore-column' variable (disabled by default), which, when
;; enabled, preserves the cursor's column position even after stripping spaces.
;; This is useful when extra spaces are added and the file is saved. While
;; stripspace removes trailing whitespace from both the saved file and the
;; currently edited buffer, it ensures that the spaces before the cursor on the
;; current line remain unchanged. This maintains a consistent editing experience
;; and prevents the cursor from shifting due to the removal of spaces from the
;; current line, in addition to other lines.

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
  "Restore the cursor's column after deleting the trailing whitespace."
  :type 'boolean
  :group 'stripspace)

(defvar stripspace-before-save-hook-depth -99
  "The `before-save-hook' hook depth for the function that saves the column.
Using a negative depth close to -100 (e.g., -99) ensures that the function that
saves the column is executed early in the `before-save-hook', before other
functions, allowing column information to be saved before other modifications
take place.")

(defvar stripspace-after-save-hook-depth 99
  "The `after-save-hook' hook depth for the function that restores the column.
Using a positive depth close to 100 (e.g., 99) ensures that the function that
restores the column is executed late in the `after-save-hook', after other
functions, allowing the column restoration to occur last, once all other
post-save processing has been completed.")

;;; Internal variables

(defvar-local stripspace--column nil
  "Internal variable used to store the column position before saving.")

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
  :lighter " StripSPC"
  :group 'stripspace
  (if stripspace-local-mode
      (progn
        ;; Mode enabled
        (add-hook 'before-save-hook #'stripspace--save-column
                  stripspace-before-save-hook-depth t)
        (add-hook 'after-save-hook #'stripspace--move-to-saved-column
                  stripspace-after-save-hook-depth t))
    ;; Mode disabled
    (remove-hook 'before-save-hook #'stripspace--save-column t)
    (remove-hook 'after-save-hook #'stripspace--move-to-saved-column t)))

(provide 'stripspace)
;;; stripspace.el ends here
