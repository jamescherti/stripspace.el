# stripspace.el - Ensure Emacs Automatically removes trailing whitespace before saving a buffer, with an option to preserve the cursor column
![License](https://img.shields.io/github/license/jamescherti/stripspace.el)
![](https://raw.githubusercontent.com/jamescherti/stripspace.el/main/.images/made-for-gnu-emacs.svg)

The `stripspace.el` Emacs package offers `stripspace-local-mode`, which ensures that trailing whitespace is removed before saving a buffer.

Additionally, it provides an optional feature controlled by the `stripspace-restore-column` variable (disabled by default), which, when enabled, preserves the cursor's column position even after stripping spaces. This is useful in scenarios where you add extra spaces and then save the file. Although the spaces are removed in the saved file, the cursor remains in the same position, maintaining a consistent editing experience.

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
