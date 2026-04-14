;;; stripspace.el --- Auto remove trailing whitespace and restore column -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; Version: 1.0.4
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
  "Ensures that Emacs removes trailing whitespace before saving a buffer."
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

(defcustom stripspace-restore-column t
  "Restore the cursor's column after deleting the trailing whitespace."
  :type 'boolean
  :group 'stripspace)

(defcustom stripspace-only-if-initially-clean nil
  "If non-nil, perform cleanup only when the buffer is clean initially.
This check is performed when `stripspace-local-mode' is enabled. Setting this
variable to nil will always trigger whitespace deletion, regardless of the
buffer's initial state."
  :type 'boolean
  :group 'stripspace)

(defcustom stripspace-cleanup-buffer-function 'delete-trailing-whitespace
  "Function used to remove trailing whitespace from the current buffer.
Examples of functions that can be used as `stripspace-cleanup-buffer-function':
- `delete-trailing-whitespace'.
- `whitespace-cleanup'."
  :type '(choice (const
                  :tag "delete-trailing-whitespace" delete-trailing-whitespace)
                 (const
                  :tag "whitespace-cleanup" whitespace-cleanup)
                 function)
  :group 'stripspace)

(defcustom stripspace-clean-buffer-p-function nil
  "Function used to determine if the buffer is considered clean.
If this is set to nil, stripspace will use an internal function to check the
buffer's cleanliness.
This function takes two arguments (beg and end), which specify the beginning
and end of the region."
  :type '(choice function (const nil))
  :group 'stripspace)

(defcustom stripspace-normalize-indentation nil
  "When non-nil, convert buffer tabs and spaces according to `indent-tabs-mode'."
  :type 'boolean
  :group 'stripspace)

(defcustom stripspace-normalize-indentation-function
  'stripspace--normalize-indentation
  "Function to convert buffer tabs and spaces according to `indent-tabs-mode'.

This function is invoked only when the variable
`stripspace-normalize-indentation' is non-nil."
  :type '(choice
          (const
           :tag "Use `stripspace--normalize-indentation'"
           stripspace--normalize-indentation)
          function)
  :group 'stripspace)

(defcustom stripspace-global-mode-exclude-modes
  '(view-mode special-mode minibuffer-mode comint-mode term-mode eshell-mode
              diff-mode org-agenda-mode message-mode markdown-mode)
  "Major modes for which `stripspace-global-mode' is not activated.
If the current buffer's major mode is derived from any mode in this list,
`stripspace-global-mode' will not enable `stripspace-local-mode' for that
buffer."
  :type '(repeat symbol)
  :group 'stripspace)

(defcustom stripspace-global-mode-exclude-special-buffers t
  "If non-nil, exclude special buffers from `stripspace-global-mode'.

Special buffers are non file-visiting buffers whose names begin with an asterisk
or a space, such as *Messages* or *Help*. When this option is enabled,
`stripspace-global-mode' will not be activated in such buffers."
  :type 'boolean
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
  (declare (indent 0) (debug t))
  `(progn
     (when stripspace-verbose
       (stripspace--message
        (concat ,(car args)) ,@(cdr args)))))

(defun stripspace--mode-cleanup-maybe ()
  "Delete trailing whitespace, maybe."
  (when (and (not buffer-read-only)
             (or (not stripspace-only-if-initially-clean)
                 (eq stripspace--clean t)))
    (stripspace-cleanup-buffer)))

(defun stripspace--mode-before-save-hook ()
  "Save the current cursor column position and remove trailing whitespace.
This function is triggered by `before-save-hook'. It stores the current column
in a buffer-local variable and deletes any trailing whitespace."
  (when (bound-and-true-p stripspace-local-mode)
    (setq stripspace--column (current-column))
    (condition-case err
        (let ((inhibit-interaction t))
          (ignore inhibit-interaction)
          (stripspace--mode-cleanup-maybe))
      (inhibited-interaction
       (stripspace--verbose-message
         "Cleanup aborted: user interaction was requested but inhibited (%s)"
         (error-message-string err))))))

(defun stripspace--mode-after-save-hook ()
  "Restore the cursor to the previously saved column after saving.
This function is triggered by `after-save-hook'. It attempts to move the cursor
back to its original column while ensuring the buffer remains unmodified."
  (when (bound-and-true-p stripspace-local-mode)
    ;; Restore column
    (when stripspace-restore-column
      (unwind-protect
          (progn
            (when stripspace--column
              ;; Restore the column position, adding spaces if necessary
              (move-to-column stripspace--column t)

              ;; Prevent marking the buffer as modified after restoring the
              ;; column
              (set-buffer-modified-p nil)))
        (setq stripspace--column nil)))

    ;; Display a message
    (stripspace--verbose-message
      "%s"
      (cond
       ((eq stripspace--clean :undefined)
        (format "Run: %s" stripspace-cleanup-buffer-function))
       (stripspace--clean
        (format "Run (Reason: The buffer is clean): %s"
                stripspace-cleanup-buffer-function))
       (t
        (format "Ignored (Reason: The buffer is not clean)"))))))

;;; Functions

(defun stripspace-clean-p (&optional beg end)
  "Return non-nil if the whitespace has already been deleted.
The BEG and END arguments represent the beginning and end of the region."
  (save-excursion
    (unless beg
      (setq beg (point-min)))

    (unless end
      (setq end (point-max)))

    (condition-case err
        (let ((inhibit-interaction t))
          (ignore inhibit-interaction)
          (cond
           (stripspace-clean-buffer-p-function
            (funcall stripspace-clean-buffer-p-function beg end))

           (t
            (let* ((contents (buffer-substring-no-properties beg end))
                   (orig-indent-tabs-mode indent-tabs-mode)
                   (orig-tab-width tab-width)
                   (orig-cleanup-func stripspace-cleanup-buffer-function)
                   (orig-norm-indent stripspace-normalize-indentation)
                   (orig-norm-indent-func stripspace-normalize-indentation-function)
                   (orig-delete-trailing-lines (bound-and-true-p delete-trailing-lines))
                   (orig-syntax-table (syntax-table))
                   (orig-whitespace-style (bound-and-true-p whitespace-style))
                   (orig-whitespace-action (bound-and-true-p whitespace-action)))
              (with-temp-buffer
                ;; Apply the captured variables to the temporary buffer
                (setq-local indent-tabs-mode orig-indent-tabs-mode)
                (setq-local tab-width orig-tab-width)
                (setq-local stripspace-cleanup-buffer-function orig-cleanup-func)
                (setq-local stripspace-normalize-indentation orig-norm-indent)
                (setq-local stripspace-normalize-indentation-function orig-norm-indent-func)
                (setq-local delete-trailing-lines orig-delete-trailing-lines)
                (set-syntax-table orig-syntax-table)
                (when orig-whitespace-style
                  (setq-local whitespace-style orig-whitespace-style))
                (when orig-whitespace-action
                  (setq-local whitespace-action orig-whitespace-action))

                (insert contents)
                (set-buffer-modified-p nil)
                (let (stripspace--clean)
                  (stripspace-cleanup-buffer))
                (not (buffer-modified-p)))))))
      (inhibited-interaction
       (stripspace--verbose-message
         (concat "Cleanliness check aborted (`stripspace-clean-p'): user "
                 "interaction was requested but inhibited (%s)")
         (error-message-string err))
       nil))))

;;; Autoloaded functions

(defun stripspace--normalize-indentation ()
  "Convert buffer tabs and spaces according to `indent-tabs-mode'.

This is disabled by default and can be enabled by setting
`stripspace-normalize-indentation' to t.

If `indent-tabs-mode' is non-nil, consecutive spaces at the beginning of lines
are converted into tabs where possible.

If `indent-tabs-mode' is nil, all tabs are replaced with the appropriate number
of spaces.

This operates on the entire buffer, processing each line individually from
beginning to end, ensuring consistent indentation and alignment according to the
current tab width settings."
  (let ((normalize-indentation-fun (if indent-tabs-mode #'tabify #'untabify)))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (funcall normalize-indentation-fun (point)
                 (progn (skip-chars-forward " \t") (point)))
        (forward-line 1)))))

(defun stripspace--cleanup-and-normalize-buffer ()
  "Delete trailing whitespace in the current buffer."
  (when buffer-read-only
    (user-error "[stripspace] Buffer is read-only"))
  (save-excursion
    (funcall stripspace-cleanup-buffer-function)
    (when stripspace-normalize-indentation
      (funcall stripspace-normalize-indentation-function))))

;;;###autoload
(defun stripspace-cleanup-buffer ()
  "Delete trailing whitespace in the current buffer."
  (interactive)
  (stripspace--cleanup-and-normalize-buffer)
  (setq stripspace--clean (if (buffer-narrowed-p)
                              (save-restriction
                                (widen)
                                (stripspace-clean-p))
                            t)))

;;;###autoload
(defalias 'stripspace-cleanup #'stripspace-cleanup-buffer)
(make-obsolete 'stripspace-cleanup 'stripspace-cleanup-buffer "1.0.5")

;;;###autoload
(defun stripspace-cleanup-region (beg end)
  "Delete trailing whitespace in the region between BEG and END."
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (let (stripspace--clean)
      (stripspace--cleanup-and-normalize-buffer))))

;;; Internal functions

(defun stripspace--special-buffer-p ()
  "Return non-nil if the current buffer is a special buffer."
  (let ((buffer-name (buffer-name)))
    (and buffer-name
         (or (string-prefix-p " " buffer-name)
             (string-prefix-p "*" buffer-name)
             (derived-mode-p 'special-mode))
         (not (buffer-file-name (buffer-base-buffer))))))

(defun stripspace--global-mode-maybe-enable ()
  "Enable `stripspace-local-mode' unless in ignored modes or minibuffer."
  (unless (or (minibufferp)
              (and stripspace-global-mode-exclude-special-buffers
                   (stripspace--special-buffer-p))
              (apply #'derived-mode-p stripspace-global-mode-exclude-modes)
              (bound-and-true-p stripspace-local-mode))
    (stripspace-local-mode 1)))

;;; Modes

;;;###autoload
(define-minor-mode stripspace-local-mode
  "Minor mode that removes trailing whitespace before a buffer is saved."
  :global nil
  :lighter " StripSPC"
  :group 'stripspace
  (if stripspace-local-mode
      (progn
        (if stripspace-only-if-initially-clean
            (progn
              (when (eq stripspace--clean :undefined)
                (stripspace--verbose-message
                  "Checking if the buffer is clean: %s (major-mode: %s)"
                  (buffer-name) major-mode)
                (setq stripspace--clean (stripspace-clean-p)))

              (stripspace--verbose-message
                "MODE ENABLED. This buffer is%s clean: %s (major-mode: %s)"
                (if stripspace--clean "" " NOT") (buffer-name) major-mode))
          (stripspace--verbose-message "MODE ENABLED: %s (major-mode: %s)"
                                       (buffer-name)
                                       major-mode))

        ;; Mode enabled
        (add-hook 'before-save-hook #'stripspace--mode-before-save-hook
                  stripspace-before-save-hook-depth t)
        (add-hook 'after-save-hook #'stripspace--mode-after-save-hook
                  stripspace-after-save-hook-depth t))
    ;; Mode disabled
    (remove-hook 'before-save-hook #'stripspace--mode-before-save-hook t)
    (remove-hook 'after-save-hook #'stripspace--mode-after-save-hook t)))

;;;###autoload
(define-globalized-minor-mode stripspace-global-mode
  stripspace-local-mode
  stripspace--global-mode-maybe-enable)

(provide 'stripspace)
;;; stripspace.el ends here
