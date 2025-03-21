# stripspace.el - Ensure Emacs Automatically removes trailing whitespace before saving a buffer, with an option to preserve the cursor column
![Build Status](https://github.com/jamescherti/stripspace.el/actions/workflows/ci.yml/badge.svg)
![License](https://img.shields.io/github/license/jamescherti/stripspace.el)
![](https://raw.githubusercontent.com/jamescherti/stripspace.el/main/.images/made-for-gnu-emacs.svg)

## Introduction

The **stripspace** Emacs package offers `stripspace-local-mode`, which ensures that trailing whitespace is removed when saving a buffer.

**Trailing whitespace** refers to any spaces or tabs that appear at the end of a line, beyond the last non-whitespace character. These characters serve no purpose in the content of the file and can cause issues with version control, formatting, or code consistency. Removing trailing whitespace helps maintain clean, readable files.

## Features

- Automatically removes trailing whitespace before saving buffers.
- An optional feature `stripspace-only-if-initially-clean` (disabled by default), which, when set to non-nil, instructs stripspace to only delete whitespace when the buffer is clean initially.
- The `stripspace-verbose` variable, when non-nil, shows in the minibuffer whether trailing whitespaces have been removed or, if not, provides the reason for their retention.
- The `(stripspace-clean)` function forces the deletion of trailing whitespace in the current buffer. When the `stripspace-only-if-initially-clean` variable is non-nil, this function also marks the buffer as clean, ensuring that `stripspace-local-mode` will remove trailing whitespace the next time the buffer is saved.
- An optional feature controlled by the `stripspace-restore-column` variable (disabled by default), which, when set to non-nil, preserves the cursor's column position even after stripping spaces. This is useful when extra spaces are added and the file is saved:
  - Saved file: Removes all trailing whitespace.
  - Currently edited file (buffer): Removes all trailing whitespace but **preserves the cursor's column position on the current line, including any spaces before the cursor**.
  This ensures a consistent editing experience and prevents unintended cursor movement when saving a buffer and removing trailing whitespace.
- The `stripspace-clean-function` customization allows the user to specify a function for removing trailing whitespace from the current buffer. This function is called to eliminate any extraneous spaces or tabs at the end of lines. Alternative functions include `delete-trailing-whitespace` (default) or `whitespace-cleanup`.

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
         (text-mode . stripspace-local-mode))
  :custom
  ;; Non-nil to only delete whitespace when the buffer is clean initially.
  ;; The initial cleanliness check is performed when `stripspace-local-mode'
  ;; is enabled.
  ;;
  ;; Change it to nil to always delete whitespace.
  (stripspace-only-if-initially-clean t)

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
         (text-mode . stripspace-local-mode))
  :custom
  ;; Non-nil to only delete whitespace when the buffer is clean initially.
  ;; The initial cleanliness check is performed when `stripspace-local-mode'
  ;; is enabled.
  ;;
  ;; Change it to nil to always delete whitespace.
  (stripspace-only-if-initially-clean t)

  ;; Enabling `stripspace-restore-column' preserves the cursor's column position
  ;; even after stripping spaces. This is useful in scenarios where you add
  ;; extra spaces and then save the file. Although the spaces are removed in the
  ;; saved file, the cursor remains in the same position, ensuring a consistent
  ;; editing experience without affecting cursor placement.
  (stripspace-restore-column t))
```

## Author and License

The *stripspace* Emacs package has been written by [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (C) 2025 James Cherti

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.

## Links

- [stripspace.el @GitHub](https://github.com/jamescherti/stripspace.el)
