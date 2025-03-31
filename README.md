# stripspace.el - Ensure Emacs Automatically removes trailing whitespace before saving a buffer, with an option to preserve the cursor column
![Build Status](https://github.com/jamescherti/stripspace.el/actions/workflows/ci.yml/badge.svg)
![License](https://img.shields.io/github/license/jamescherti/stripspace.el)
![](https://raw.githubusercontent.com/jamescherti/stripspace.el/main/.images/made-for-gnu-emacs.svg)

## Introduction

The **stripspace** Emacs package provides `stripspace-local-mode`, which automatically removes trailing whitespace and blank lines at the end of the buffer when saving.

(**Trailing whitespace** refers to any spaces or tabs that appear at the end of a line, beyond the last non-whitespace character. These characters serve no purpose in the content of the file and can cause issues with version control, formatting, or code consistency. Removing trailing whitespace helps maintain clean, readable files.)

It also includes an optional feature (`stripspace-only-if-initially-clean`, disabled by default), which, when enabled, ensures that trailing whitespace is removed only if the buffer was initially clean. This prevents unintended modifications to buffers that already contain changes, making it useful for preserving intentional whitespace or avoiding unnecessary edits in files managed by version control.

## Features

Here are the features of `(stripspace-local-mode)`:
- Before saving buffer: Automatically removes all trailing whitespace.
- After saving buffer: Restores the cursor's column position on the current line, including any spaces before the cursor. This ensures a consistent editing experience and prevents unintended cursor movement when saving a buffer and removing trailing whitespace. This behavior can be controller by the `stripspace-restore-column` variable (default: `t`).
- Even if the buffer is narrowed, *stripspace* removes trailing whitespace from the entire buffer. This behavior, controlled by the `stripspace-ignore-restrictions` variable (default: `t`).
- An optional feature `stripspace-only-if-initially-clean` (default: `nil`), which, when set to non-nil, instructs stripspace to only delete whitespace when the buffer is clean initially. The check for a clean buffer is optimized using a single regex search for trailing whitespace and another for blank lines.
- The `stripspace-verbose` variable, when non-nil, shows in the minibuffer whether trailing whitespaces have been removed or, if not, provides the reason for their retention.
- The functions for deleting whitespace are customizable, allowing the user to specify a custom function for removing trailing whitespace from the current buffer.
- The `stripspace-clean-function` variable allows specifying a function for removing trailing whitespace from the current buffer. This function is called to eliminate any extraneous spaces or tabs at the end of lines. (For example, this can be set to a built-in function such as `delete-trailing-whitespace` (default) or `whitespace-cleanup`.)

## Installation

### Install with straight (Emacs version < 30)

To install *stripspace* with `straight.el`:

1. It if hasn't already been done, [add the straight.el bootstrap code](https://github.com/radian-software/straight.el?tab=readme-ov-file#getting-started) to your init file.
2. Add the following code to the Emacs init file:
```emacs-lisp
(use-package stripspace
  :ensure t
  :straight (stripspace
             :type git
             :host github
             :repo "jamescherti/stripspace.el")
  :commands stripspace-local-mode
  ;; Enable for prog-mode and text-mode
  :hook ((prog-mode . stripspace-local-mode)
         (text-mode . stripspace-local-mode)
         (conf-mode . stripspace-local-mode))
  :custom
  ;; The `stripspace-only-if-initially-clean' option:
  ;; - nil to always delete trailing whitespace.
  ;; - Non-nil to only delete whitespace when the buffer is clean initially.
  ;; (The initial cleanliness check is performed when `stripspace-local-mode'
  ;; is enabled.)
  (stripspace-only-if-initially-clean nil)

  ;; Enabling `stripspace-restore-column' preserves the cursor's column position
  ;; even after stripping spaces. This is useful in scenarios where you add
  ;; extra spaces and then save the file. Although the spaces are removed in the
  ;; saved file, the cursor remains in the same position, ensuring a consistent
  ;; editing experience without affecting cursor placement.
  (stripspace-restore-column t))
```

### Installing with use-package and :vc (Built-in feature in Emacs version >= 30)

To install *stripspace* with `use-package` and `:vc` (Emacs >= 30):

``` emacs-lisp
(use-package stripspace
  :ensure t
  :vc (:url "https://github.com/jamescherti/stripspace.el"
       :rev :newest)
  :commands stripspace-local-mode
  ;; Enable for prog-mode and text-mode
  :hook ((prog-mode . stripspace-local-mode)
         (text-mode . stripspace-local-mode)
         (conf-mode . stripspace-local-mode))
  :custom
  ;; The `stripspace-only-if-initially-clean' option:
  ;; - nil to always delete trailing whitespace.
  ;; - Non-nil to only delete whitespace when the buffer is clean initially.
  ;; (The initial cleanliness check is performed when `stripspace-local-mode'
  ;; is enabled.)
  (stripspace-only-if-initially-clean t)

  ;; Enabling `stripspace-restore-column' preserves the cursor's column position
  ;; even after stripping spaces. This is useful in scenarios where you add
  ;; extra spaces and then save the file. Although the spaces are removed in the
  ;; saved file, the cursor remains in the same position, ensuring a consistent
  ;; editing experience without affecting cursor placement.
  (stripspace-restore-column t))
```

## Frequently asked question

### How to mark a buffer as clean if it is unclean?

When the `stripspace-only-if-initially-clean` variable is non-nil, *stripspace* deletes trailing whitespace only if the buffer is initially clean.

If the buffer is not clean, it remains marked as such, preventing trailing whitespace from being removed before saving.

To manually mark a buffer as clean, call the `(stripspace-clean)` function, which forces the deletion of trailing whitespace and updates the buffer's state.

### What are the differences between stripspace and ws-butler?

The *ws-butler* tracks modified lines and removes trailing whitespace only from those lines. However, it is slightly more complex, as it employs custom functions to track buffer changes, triggered by the following hooks: `after-change-functions`, `before-save-hook`, `after-save-hook`, `before-revert-hook`, `after-revert-hook`, and `edit-server-done-hook`.

In contrast, the *stripspace* package is lightweight. It operates solely on the `before-save-hook` to remove whitespace from the entire buffer using built-in Emacs functions, and on the `after-save-hook` to restore the cursor.

Optionally, when `stripspace-local-mode` is enabled, it can check if the buffer is already clean (with no whitespace) to determine whether trailing whitespace should be deleted automatically.

### What are the differences between stripspace and trimspace?

The trimspace package only removes trailing whitespace before saving a file.

The stripspace package, however, provides additional features: it can restore the cursor column, optionally check if the buffer is clean before trimming, and customization of the whitespace removal function, etc. See the [features](https://github.com/jamescherti/stripspace.el?tab=readme-ov-file#features) section of the README.md for details.

## Author and License

The *stripspace* Emacs package has been written by [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (C) 2025 James Cherti

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.

## Links

- [stripspace.el @GitHub](https://github.com/jamescherti/stripspace.el)

Other Emacs packages by the same author:
- [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d): This repository hosts a minimal Emacs configuration designed to serve as a foundation for your vanilla Emacs setup and provide a solid base for an enhanced Emacs experience.
- [compile-angel.el](https://github.com/jamescherti/compile-angel.el): **Speed up Emacs!** This package guarantees that all .el files are both byte-compiled and native-compiled, which significantly speeds up Emacs.
- [outline-indent.el](https://github.com/jamescherti/outline-indent.el): An Emacs package that provides a minor mode that enables code folding and outlining based on indentation levels for various indentation-based text files, such as YAML, Python, and other indented text files.
- [vim-tab-bar.el](https://github.com/jamescherti/vim-tab-bar.el): Make the Emacs tab-bar Look Like Vim’s Tab Bar.
- [easysession.el](https://github.com/jamescherti/easysession.el): Easysession is lightweight Emacs session manager that can persist and restore file editing buffers, indirect buffers/clones, Dired buffers, the tab-bar, and the Emacs frames (with or without the Emacs frames size, width, and height).
- [elispcomp](https://github.com/jamescherti/elispcomp): A command line tool that allows compiling Elisp code directly from the terminal or from a shell script. It facilitates the generation of optimized .elc (byte-compiled) and .eln (native-compiled) files.
- [tomorrow-night-deepblue-theme.el](https://github.com/jamescherti/tomorrow-night-deepblue-theme.el): The Tomorrow Night Deepblue Emacs theme is a beautiful deep blue variant of the Tomorrow Night theme, which is renowned for its elegant color palette that is pleasing to the eyes. It features a deep blue background color that creates a calming atmosphere. The theme is also a great choice for those who miss the blue themes that were trendy a few years ago.
- [Ultyas](https://github.com/jamescherti/ultyas/): A command-line tool designed to simplify the process of converting code snippets from UltiSnips to YASnippet format.
- [dir-config.el](https://github.com/jamescherti/dir-config.el): Automatically find and evaluate .dir-config.el Elisp files to configure directory-specific settings.
- [flymake-bashate.el](https://github.com/jamescherti/flymake-bashate.el): A package that provides a Flymake backend for the bashate Bash script style checker.
- [flymake-ansible-lint.el](https://github.com/jamescherti/flymake-ansible-lint.el): An Emacs package that offers a Flymake backend for ansible-lint.
- [inhibit-mouse.el](https://github.com/jamescherti/inhibit-mouse.el): A package that disables mouse input in Emacs, offering a simpler and faster alternative to the disable-mouse package.
- [quick-sdcv.el](https://github.com/jamescherti/quick-sdcv.el): This package enables Emacs to function as an offline dictionary by using the sdcv command-line tool directly within Emacs.
- [enhanced-evil-paredit.el](https://github.com/jamescherti/enhanced-evil-paredit.el): An Emacs package that prevents parenthesis imbalance when using *evil-mode* with *paredit*. It intercepts *evil-mode* commands such as delete, change, and paste, blocking their execution if they would break the parenthetical structure.
