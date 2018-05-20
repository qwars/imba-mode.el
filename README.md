# imba-mode.el
Emacs major mode for imba [http://imba.io](http://imba.io) [GitHub Imba](https://github.com/somebee/imba) 

## Installation

In your .emacs

    (add-to-list 'load-path "/place/where/you/put/it/")
    (require 'imba-mode)

## imba-mode-hook

    (eval-after-load "auto-complete-mode"
        '(add-to-list 'ac-modes 'imba-mode))
    (add-hook 'imba-mode-hook 
          (quote
           (lambda nil
 	             (auto-complete-mode 1)
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
                 (setq-local adaptive-fill-regexp "[\t]*"))))


## Screenshot

![Screen](https://raw.githubusercontent.com/qwars/imba-mode.el/c3a9b54cc4d313f517e85687ec53c4b1f7bfda8d/screen.png)

