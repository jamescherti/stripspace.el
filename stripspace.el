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

;;; Code:

;;; Customizations

(defgroup stripspace nil
  "Ensures that Emacs removes trailing whitespace before saving a buffer"
  :group 'stripspace
  :prefix "stripspace-"
  :link '(url-link
          :tag "Github"
          "https://github.com/jamescherti/stripspace.el"))

(defcustom stripspace-verbose nil
  "Enable displaying verbose messages.
When non-nil, display in the minibuffer whether trailing whitespaces have been
removed and, if not, the reason for their retention (e.g., the buffer was not
clean)."
  :type 'boolean
  :group 'stripspace)

(defcustom stripspace-restore-column nil
  "Restore the cursor's column after deleting the trailing whitespace."
  :type 'boolean
  :group 'stripspace)

(defcustom stripspace-only-if-initially-clean nil
  "Non-nil to only delete whitespace when the buffer is clean initially.
The initial cleanliness check is performed when `stripspace-local-mode' is
enabled.
Change it to nil to always delete whitespace."
  :type 'boolean
  :group 'stripspace)

(defcustom stripspace-clean-function #'delete-trailing-whitespace
  "Function used to remove trailing whitespace from the current buffer.
This function is invoked to eliminate any extraneous spaces or tabs at the end
of lines.
Alternative functions include:
- `delete-trailing-whitespace' (default)
- `whitespace-cleanup'."
  :type 'function
  :group 'stripspace)

;;; Variables

(defvar stripspace-before-save-hook-depth -99
  "Depth for the hook that removes trailing whitespace in `before-save-hook'.
A negative depth close to -100 (e.g., -99) ensures that this function runs early
in `before-save-hook', allowing other modifications to occur first.

Additionally, `before-save-hook' saves the current column position, which is
later restored in `after-save-hook' when `stripspace-restore-column' is non-nil.

Running this function early in `before-save-hook' ensures that the column
information is saved before all other modifications have been made.

For example, the Reformatter package, which reformats buffers, runs during
`before-save-hook'. Running stripspace beforehand ensures that the column is
saved before reformatting is applied.")

(defvar stripspace-after-save-hook-depth 99
  "Depth for the hook that restores the cursor column in `after-save-hook'.
A positive depth close to 100 (e.g., 99) ensures that this function runs late,
allowing column restoration to occur after all other post-save processing.

For example, the Apheleia package, which reformats buffers, runs during
`after-save-hook'. Running stripspace after it ensures that the column is
restored after reformatting has been completed.")

;;; Internal variables

(defvar-local stripspace--clean :undefined
  "Indicates whether the buffer contains no trailing whitespace.
This variable is used to track the state of trailing whitespace in the buffer.")

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
        (concat ,(car args)) ,@(cdr args)))))

(defun stripspace-clean ()
  "Delete trailing whitespace in the current buffer."
  (interactive)
  (funcall stripspace-clean-function)
  (setq stripspace--clean t))

(defun stripspace--clean-p ()
  "Return non-nil if the whitespace has already been deleted."
  (let ((contents (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-buffer
      (insert contents)
      (set-buffer-modified-p nil)
      (stripspace-clean)
      (not (buffer-modified-p)))))

(defun stripspace--delete-trailing-whitespace-maybe ()
  "Delete trailing whitespace, maybe."
  (let ((delete-trailing-whitespace t))
    (when (and stripspace-only-if-initially-clean
               (not (eq stripspace--clean t)))
      (setq delete-trailing-whitespace nil))
    (when delete-trailing-whitespace
      (stripspace-clean))))

(defun stripspace--before-save-hook ()
  "Save the current cursor column position and remove trailing whitespace.
This function is triggered by `before-save-hook'. It stores the current column
in a buffer-local variable and deletes any trailing whitespace."
  (setq stripspace--column (current-column))
  (stripspace--delete-trailing-whitespace-maybe))

(defun stripspace--after-save-hook ()
  "Restore the cursor to the previously saved column after saving.
This function is triggered by `after-save-hook'. It attempts to move the cursor
back to its original column while ensuring the buffer remains unmodified."
  (when stripspace-restore-column
    (unwind-protect
        (progn
          (when stripspace--column
            ;; Restore the column position, adding spaces if necessary
            (move-to-column stripspace--column t))
          ;; Prevent marking the buffer as modified
          (set-buffer-modified-p nil))
      (setq stripspace--column nil)))
  (stripspace--verbose-message
   "%s"
   (cond
    ((eq stripspace--clean :undefined)
     (format "Run: %s" stripspace-clean-function))
    (stripspace--clean
     (format "Run (Reason: The buffer is clean): %s" stripspace-clean-function))
    (t
     (format "Ignored (Reason: The buffer is not clean)")))))

;;; Modes

;;;###autoload
(define-minor-mode stripspace-local-mode
  "Toggle `stripspace-local-mode'.
This mode ensures that trailing whitespace is removed before saving a buffer."
  :global nil
  :lighter " StripSPC"
  :group 'stripspace
  (if stripspace-local-mode
      (progn
        (when stripspace-only-if-initially-clean
          (when (eq stripspace--clean :undefined)
            (setq stripspace--clean (stripspace--clean-p))
            (stripspace--verbose-message "This buffer is%s clean: %s"
                                         (if stripspace--clean "" " NOT")
                                         (buffer-name))))

        ;; Mode enabled
        (add-hook 'before-save-hook #'stripspace--before-save-hook
                  stripspace-before-save-hook-depth t)
        (add-hook 'after-save-hook #'stripspace--after-save-hook
                  stripspace-after-save-hook-depth t))
    ;; Mode disabled
    (remove-hook 'before-save-hook #'stripspace--before-save-hook t)
    (remove-hook 'after-save-hook #'stripspace--after-save-hook t)))

(provide 'stripspace)
;;; stripspace.el ends here
