# imba-mode.el
Emacs major mode for imba [http://imba.io](http://imba.io) [GitHub Imba](https://github.com/somebee/imba) 

## Installation

In your .emacs

    (add-to-list 'load-path "/place/where/you/put/it/")
    (require 'imba-mode)

## imba-mode-hook

```lisp
    (add-hook 'imba-mode-hook 
          (quote
           (lambda nil
             (auto-complete-mode 1)
             (setq ac-modes (append ac-modes '(imba-mode)))
             (ac-config-default)
             (setq-local whitespace-style
                         (quote
                          (face trailing tabs tab-mark)))
             (setq-local whitespace-display-mappings (quote ((tab-mark 9 [8594 9] [92 9]))))
             (whitespace-mode t)
             (set-face-foreground 'whitespace-tab "lightgray")
             (set-face-background 'whitespace-tab nil)
             (set-face-foreground 'whitespace-trailing "HotPink")
             (set-face-background 'whitespace-trailing "lightPink")
	         (setq-local electric-pair-skip-whitespace-chars (quote (9 10)))
             (setq-local auto-indent-untabify-on-visit-file 'tabify)
             (setq-local auto-indent-backward-delete-char-behavior nil)
             (setq-local auto-indent-untabify-on-save-file 'tabify)
             (setq-local auto-indent-newline-function 'newline-and-indent)
             (setq-local adaptive-fill-regexp "[\t]*")
             (outline-minor-mode 1)
             (setq-local outline-regexp "[[:space:]]*\\(tag...\\|def...\\|\\bdo....\\|class.\\|export\\)")
             )))
```

## auto-complete-mode dict imba-mode

ac-dictionary-directories - add "/place/where/you/put/it/dict"


## Screenshot

![Screen](https://raw.githubusercontent.com/qwars/imba-mode.el/c3a9b54cc4d313f517e85687ec53c4b1f7bfda8d/screen.png)
