;;; imba-mode.el --- Major mode for editing Imba files

;; Copyright (C) 2018 Free Software Foundation, Inc.
;;
;; Author: Alexandr Selunin <aka.qwars@gmail.com>
;; Maintainer: Alexandr Selunin <aka.qwars@gmail.com>
;; Created: 08 Mar 2018
;; Version: 0.01
;; Keywords

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'imba-mode)

;;; Code:


;; User definable variables


;;;###autoload
(defgroup imba nil
  "Support for the Imba serialization format"
  :group 'languages
  :prefix "imba-")

(defcustom imba-mode-hook nil
  "*Hook run by `imba-mode'."
  :type 'hook
  :group 'imba)

(defcustom imba-indent-offset 4
  "*Amount of offset per level of indentation."
  :type 'integer
  :safe 'natnump
  :group 'imba)


(defface imba-function-name-face
  '((t :foreground "Blue1" :weight bold))
  "Face for function parameters."
  :group 'imba )

(defface imba-function-state-name-face
  '((t :foreground "dark cyan" :weight bold))
  "Face for function state."
  :group 'imba )

(defface imba-keyword-face
  '((t :foreground "DarkGreen" :weight bold))
  "Face for function state."
  :group 'imba )

(defface imba-string-variable-name-face
  '((t :foreground "DarkRed" :weight bold))
  "Face for interpolation string."
  :group 'imba )

(setq imba-highlights
      '(("#\\s+.+$" . (0 font-lock-comment-face))
        ("\\b\\(export\\|import\\|require\\|Imba\\|route\\([\.-][A-Za-z_]+\\)*\\)\\b" . (1 'imba-keyword-face))        
        ("\\b\\([A-Za-z_][A-Za-z_0-9-]*\\)[\.]new" . (1 font-lock-type-face))
        ("[@][A-Za-z_][A-Za-z_0-9-]*" . (0 font-lock-variable-name-face))
        ("\\bdef[ \t]+\\(render\\|build\\|mount\\|setup\\|unmount\\|tikc\\|initialize\\b\\)" . (1 'imba-function-state-name-face))
        ("\\bexport[ \t]+\\(default\\)" . (1 'imba-function-state-name-face))
        ("\\b\\(tag\\|def\\|class\\)[ \t]+\\([A-Za-z_][A-Za-z_0-9-]+\\)" . (2 'imba-function-name-face))
        ("\\btag[ \t]+[A-Za-z_][A-Za-z_0-9-]*[ \t]+\\([<][ \t]+[A-Za-z_][A-Za-z_0-9-]*\\)" . (1 font-lock-type-face))
        ("[<]\\(self\\)" . (1 font-lock-constant-face))
        ("[<]\\([A-Za-z_]+\\)" . (1 'imba-string-variable-name-face))
        ("[<]\\([A-Za-z_][A-Za-z_0-9]*\\)" . (1 'imba-string-variable-name-face))
        ("[\'\"].+?[\'\"]" . (0 font-lock-string-face))
        ("\\([\.][A-Za-z_][A-Za-z_0-9-]*\\)" . (1 font-lock-preprocessor-face))
        ("\\([:][A-Za-z_][A-Za-z_0-9-]*\\)" . (1 font-lock-constant-face))
        ("\\([A-Za-z_][A-Za-z_0-9-]*[:\.][ \t]*\\)" . (1 font-lock-constant-face))
        ("\\([A-Za-z_][A-Za-z_0-9-]*\\)[:\.\(\[]" . (1 font-lock-variable-name-face))
        ("\\b\\(return\\|def\\|tag\\|var\\|let\\|const\\|prop\\|new\\|and\\|not\\|if\\|unless\\|else\\|when\\|for\\|until\\|while\\|do\\|map\\|class\\|setTimeout\\|setInterval\\|clearTimeout\\|clearInterval\\|parseInt\\|parseFloat\\|__dirname\\|process\\|console\\|document\\|window\\|this\\|then\\|in\\|self\\|delete\\)\\b" . (1 font-lock-function-name-face))
        ("\\b\\(attr\\|super\\|typeof\\|break\\|in\\|of\\|continue\\|as\\|from\\|render\\|unschedule\\|schedule\\|build\\|setup\\|mount\\|unmount\\|tikc\\|render\\)\\b" . (1 font-lock-keyword-face))
        ("\\b\\(null\\|true\\|false\\|undefined\\|await\\|async\\)\\b" . (1 font-lock-constant-face))        
        ("\\b\\(var\\|let\\|const\\)[ \t]+\\([A-Za-z_][A-Za-z_0-9]*\\)" . (2 font-lock-variable-name-face))        
        ))

(defvar imba-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    (modify-syntax-entry ?# "<" syntax-table)
    (modify-syntax-entry ?\n ">" syntax-table)
    syntax-table)
  "Syntax table in use in `imba-mode' buffers.")

(defun insert-tab-char ()
  "Insert a tab char. (ASCII 9, \t)"
  (interactive)
  (insert "\t"))

(define-derived-mode imba-mode nil "Imba"
  "Simple mode to edit Imba.

\\{imba-mode-map}"
  (setq-local tab-always-indent 'complete)
  (setq-local indent-tabs-mode t)
  (setq-local comment-multi-line t)
  (setq-local tab-width imba-indent-offset)
  (local-set-key (kbd "<tab>") 'insert-tab-char)
  (local-set-key (kbd "<C-tab>") 'indent-rigidly-right-to-tab-stop)
  (local-set-key (kbd "<C-M-tab>") 'indent-rigidly-left-to-tab-stop)
  (setq font-lock-defaults '(imba-highlights))
  (set-syntax-table imba-mode-syntax-table)
  (set (make-local-variable 'comment-start) "# "))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.imba" . imba-mode))

(provide 'imba-mode)
