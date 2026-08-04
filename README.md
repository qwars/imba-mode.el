# imba-mode.el

An [Emacs](https://www.gnu.org/software/emacs/) major mode for
[Imba](http://imba.io) — a programming language for the web that compiles to
performant JavaScript.

![Screen](https://raw.githubusercontent.com/qwars/imba-mode.el/c3a9b54cc4d313f517e85687ec53c4b1f7bfda8d/screen.png)

## Features

- **Syntax highlighting** for Imba 1.x: keywords, tag/class declarations,
  properties, instance variables (`@var`), block arguments (`$0`–`$9`), tag
  lifecycle methods (`build`, `setup`, `mount`, `tick`, `render`, …), and more.
- **Comments** — single-line `# …` comments and multi-line `### … ###` blocks
  are highlighted with distinct faces so they are easy to tell apart.
- **Completion at point** (works with `completion-at-point` and is picked up
  automatically by `company-mode` via `company-capf`):
  - Imba keywords and predeclared globals;
  - HTML tag names plus tags declared in the current buffer, right after `<`;
  - instance variables after `@`;
  - an optional user dictionary (compatible with old `auto-complete-mode`
    dictionaries).
- **Region block comments** with `C-c :` — wraps a region (or the current line)
  in `### … ###` markers, preserving the indentation of the first line.
- **Outline navigation** over `tag` / `def` / `class` / `export` structure via
  `outline-minor-mode`.
- **Whitespace visualization** — tabs and trailing spaces via `whitespace-mode`.

Imba is indentation-based and tab-indented; the mode respects that and keeps
`TAB` inserting a literal tab character.

## Requirements

- GNU Emacs 28.2 or newer
- Optional: [company](https://company-mode.github.io/) for in-buffer completion
  popups (the mode works without it via `M-/`)

## Installation

### use-package

```elisp
(unless (file-exists-p "~/.emacs.d/modules/imba-mode.el")
  (shell-command "git clone git@github.com:qwars/imba-mode.el.git ~/.emacs.d/modules/imba-mode.el"))

(use-package imba-mode
  :load-path "~/.emacs.d/modules/"
  :init
  (require 'imba-mode)
  :config
  (defun imba-mode-hook-setup ()
    ;; completion: company picks up the capf from imba-mode via company-capf
    (company-mode 1)
    (setq-local company-idle-delay 0.3)
    ;; let the tag list after < pop up after the first letter
    (setq-local company-minimum-prefix-length 1)
    ;; visualize tabs and trailing whitespace
    (setq-local whitespace-style '(face trailing tabs tab-mark))
    (setq-local whitespace-display-mappings '((tab-mark 9 [8594 9] [92 9])))
    (whitespace-mode t)
    ;; file structure (tag/def/class as outline headings)
    (outline-minor-mode 1)
    (setq-local outline-regexp
                "[[:space:]]*\\(tag...\\|def...\\|\\bdo....\\|class.\\|export\\|#.....\\)"))
  :hook
  (imba-mode . imba-mode-hook-setup))
```

### Manual

```elisp
(add-to-list 'load-path "/path/to/imba-mode/")
(require 'imba-mode)
```

Files ending in `.imba` are opened in `imba-mode` automatically.

## Key bindings

| Key       | Command                                    |
|-----------|--------------------------------------------|
| `TAB`     | Insert a tab character                     |
| `C-TAB`   | Rigid-indent region right to a tab stop    |
| `C-M-TAB` | Rigid-indent region left to a tab stop     |
| `M-;`     | Comment / uncomment line (`comment-dwim`)  |
| `M-/`     | Completion at point                        |
| `C-c :`   | Toggle `### … ###` block comment           |

## Completion

`imba-mode` registers a `completion-at-point` function with three contexts:

1. **Tag names after `<`** — HTML tag names, `self`, and any tag declared in
   the current buffer with `tag Name` (or `export tag Name`).
2. **Instance variables after `@`** — collected from the current buffer.
3. **Identifiers** — Imba keywords, predeclared globals, and the optional
   dictionary.

### Dictionary

Set `imba-dictionary-file` to a file with one word per line (the same format
used by `auto-complete-mode` dictionaries). Words are merged into identifier
completion. Set it to `nil` to disable.

```elisp
(setq imba-dictionary-file "~/.emacs.d/modules/dict/imba-mode")
```

## Comments

- `# …` starts a single-line comment.
- `###` on its own line opens/closes a block comment. Use `C-c :` to wrap or
  unwrap a region.

## License

Copyright © 2018 Free Software Foundation, Inc.

Licensed under the GNU General Public License, version 2 or (at your option)
any later version. See [COPYING](https://www.gnu.org/licenses/gpl-2.0.html).
