;;; imba-mode.el --- Major mode for editing Imba files
;; Copyright (C) 2018 Free Software Foundation, Inc.
;;
;; Author: Alexandr Selunin <aka.qwars@gmail.com>
;; Maintainer: Alexandr Selunin <aka.qwars@gmail.com>
;; Created: 08 Mar 2018
;; Version: 0.05
;; Keywords: languages
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'imba-mode)

;;; Code:

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

(defcustom imba-dictionary-file
  "~/.emacs.d/modules/dict/imba-mode"
  "Файл словаря дополнения Imba (одно слово на строку).
Nil отключает словарь."
  :type '(choice (const :tag "Отключен" nil) file)
  :group 'imba)

(defface imba-function-name-face
  '((t :foreground "Blue1" :weight bold))
  "Face for function parameters."
  :group 'imba)

(defface imba-function-state-name-face
  '((t :foreground "dark cyan" :weight bold))
  "Face for function state."
  :group 'imba)

(defface imba-keyword-face
  '((t :foreground "DarkGreen" :weight bold))
  "Face for keywords."
  :group 'imba)

(defface imba-string-variable-name-face
  '((t :foreground "DarkRed" :weight bold))
  "Face for interpolation string."
  :group 'imba)

(defvar imba-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; # намеренно не объявляется комментарием в syntax table:
    ;; однострочные комментарии красятся правилом в imba-highlights,
    ;; а ###-блоки — через imba-syntax-propertize
    (modify-syntax-entry ?' "\"" st)
    (modify-syntax-entry ?\" "\"" st)
    st)
  "Syntax table for `imba-mode'.")

(defvar imba-highlights
  '(
    ;; Однострочные комментарии: # с начала строки (допускается отступ)
    ;; или после пробельного символа; не матчит ###-маркеры и строки
    ("\\(?:^\\|\\s-\\)\\(#[^\n]*\\)" . (1 font-lock-comment-face))
    ;; Ключевые слова и конструкции
    ("\\b\\(export\\|import\\|require\\|Imba\\|route\\([\.-][A-Za-z_]+\\)*\\)\\b" . (1 'imba-keyword-face))
    ("\\b\\([A-Za-z_][A-Za-z_0-9-]*\\)[\\.]new\\b" . (1 font-lock-type-face))
    ("[@][A-Za-z_][A-Za-z_0-9-]*" . (0 font-lock-variable-name-face))
    ("\\(\\$[0-9]+\\)" . (1 font-lock-variable-name-face))
    ("\\bdef[ \t]+\\(render\\|build\\|mount\\|setup\\|unmount\\|tick\\|initialize\\)\\b" . (1 'imba-function-state-name-face))
    ("\\bexport[ \t]+\\(default\\)" . (1 'imba-function-state-name-face))
    ("\\b\\(tag\\|def\\|class\\)[ \t]+\\([A-Za-z_][A-Za-z_0-9-]+\\)" . (2 'imba-function-name-face))
    ("\\btag[ \t]+[A-Za-z_][A-Za-z_0-9-]*[ \t]+\\([<][ \t]+[A-Za-z_][A-Za-z_0-9-]*\\)" . (1 font-lock-type-face))
    ("[<]\\(self\\)" . (1 font-lock-constant-face))
    ("[<]\\([A-Za-z_][A-Za-z_0-9]*\\)" . (1 'imba-string-variable-name-face))
    ("\\([\.][A-Za-z_][A-Za-z_0-9-]*\\)" . (1 font-lock-preprocessor-face))
    ("\\([:][A-Za-z_][A-Za-z_0-9-]*\\)" . (1 font-lock-constant-face))
    ("\\([A-Za-z_][A-Za-z_0-9-]*[:\.][ \t]*\\)" . (1 font-lock-constant-face))
    ("\\([A-Za-z_][A-Za-z_0-9-]*\\)[:\.\(\[]" . (1 font-lock-variable-name-face))
    ("\\b\\(return\\|def\\|tag\\|var\\|let\\|const\\|prop\\|new\\|or\\|and\\|not\\|if\\|unless\\|elif\\|else\\|when\\|for\\|until\\|while\\|do\\|map\\|class\\|setTimeout\\|setInterval\\|clearTimeout\\|clearInterval\\|parseInt\\|parseFloat\\|__dirname\\|process\\|console\\|document\\|window\\|this\\|then\\|in\\|self\\|delete\\)\\b" . (1 font-lock-function-name-face))
    ("\\b\\(attr\\|super\\|typeof\\|break\\|in\\|of\\|continue\\|as\\|from\\|render\\|unschedule\\|schedule\\|build\\|setup\\|mount\\|unmount\\|tick\\)\\b" . (1 font-lock-keyword-face))
    ("\\b\\(switch\\|by\\|own\\|extern\\)\\b" . (1 font-lock-keyword-face))
    ("\\b\\(yes\\|no\\)\\b" . (1 font-lock-constant-face))
    ("\\b\\(null\\|true\\|false\\|undefined\\|await\\|async\\)\\b" . (1 font-lock-constant-face))
    ("\\b\\(var\\|let\\|const\\)[ \t]+\\([A-Za-z_][A-Za-z_0-9]*\\)" . (2 font-lock-variable-name-face))
    ))

;; ---------- Автодополнение ----------

(defconst imba-keywords
  '("alt" "and" "as" "async" "attr" "await" "break" "build" "by"
    "class" "const" "continue" "createElement" "css" "ctrl" "data" "def"
    "delete" "do" "dom" "elif" "else" "export" "extern" "false"
    "flag" "flags" "for" "from" "if" "import" "in" "initialize"
    "let" "map" "meta" "mount" "new" "no" "not" "null"
    "of" "or" "own" "prop" "render" "require" "return" "schedule"
    "self" "setup" "shift" "super" "switch" "tag" "then" "this"
    "tick" "trigger" "true" "undefined" "unflag" "unless" "unmount"
    "unschedule" "until" "var" "when" "while" "yes")
  "Imba language keywords for completion.")

(defconst imba-globals
  '(;; встроенные объекты JS
    "Array" "Boolean" "Date" "Error" "Function" "JSON" "Map" "Math"
    "Number" "Object" "Promise" "RegExp" "Set" "String" "Symbol"
    ;; браузерные / web API (frontend)
    "CustomEvent" "FormData" "HTMLElement" "MutationObserver"
    "URLSearchParams" "XMLHttpRequest" "alert" "cancelAnimationFrame"
    "clearInterval" "clearTimeout" "confirm" "console" "document"
    "fetch" "history" "localStorage" "location" "navigator" "prompt"
    "requestAnimationFrame" "sessionStorage" "setInterval"
    "setTimeout" "window"
    ;; Imba / node
    "Imba" "__dirname" "parseFloat" "parseInt" "process" "scheduler")
  "Predeclared globals for completion.")

(defconst imba-html-tags
  '("a" "abbr" "address" "area" "article" "aside" "audio"
    "b" "base" "bdi" "bdo" "blockquote" "body" "br" "button"
    "canvas" "caption" "cite" "code" "col" "colgroup"
    "data" "datalist" "dd" "del" "details" "dfn" "dialog" "div" "dl" "dt"
    "em" "embed"
    "fieldset" "figcaption" "figure" "footer" "form"
    "h1" "h2" "h3" "h4" "h5" "h6" "head" "header" "hgroup" "hr" "html"
    "i" "iframe" "img" "input" "ins"
    "kbd"
    "label" "legend" "li" "link"
    "main" "map" "mark" "menu" "meta" "meter"
    "nav" "noscript"
    "object" "ol" "optgroup" "option" "output"
    "p" "param" "picture" "pre" "progress"
    "q"
    "rp" "rt" "ruby"
    "s" "samp" "script" "section" "select" "slot" "small" "source"
    "span" "strong" "style" "sub" "summary" "sup" "svg"
    "table" "tbody" "td" "template" "textarea" "tfoot" "th" "thead"
    "time" "title" "tr" "track"
    "u" "ul"
    "var" "video"
    "wbr")
  "HTML tag names for completion after <.")

(defun imba-dictionary ()
  "Слова из `imba-dictionary-file'."
  (when (and imba-dictionary-file
             (file-readable-p imba-dictionary-file))
    (with-temp-buffer
      (insert-file-contents imba-dictionary-file)
      (split-string (buffer-string) "[\r\n]+" t "[ \t]+"))))

(defun imba-buffer-tags ()
  "Collect tag names declared in the current buffer via `tag Name'."
  (let (tags)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^\\(?:export[ \t]+\\)?tag[ \t]+\\([A-Za-z_][A-Za-z_0-9-]*\\)"
              nil t)
        (push (match-string-no-properties 1) tags)))
    (delete-dups tags)))

(defun imba-instance-variables ()
  "Collect @instance variables from the current buffer."
  (let (vars)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[@][A-Za-z_][A-Za-z_0-9-]*" nil t)
        (push (match-string-no-properties 0) vars)))
    (delete-dups vars)))

(defvar imba--code-candidates nil
  "Кэш кандидатов для идентификаторов.")

(defun imba-code-candidates ()
  "Ключевые слова + глобали + словарь (кэш на сессию).
После правки словаря перезапустите Emacs."
  (or imba--code-candidates
      (setq imba--code-candidates
            (delete-dups
             (append imba-keywords imba-globals (imba-dictionary))))))

(defun imba-completion-at-point ()
  "Completion at point for Imba.
Contexts: tag name after <, @instance variables, keywords and globals."
  (unless (nth 8 (syntax-ppss))
    (let* ((end (point))
           (beg (save-excursion
                  (skip-chars-backward "A-Za-z_0-9-")
                  (point)))
           (prev (char-before beg)))
      (cond
       ((eq prev ?<)
        (list beg end
              (append '("self") imba-html-tags (imba-buffer-tags))
              :exclusive 'no))
       ((eq prev ?@)
        (list (1- beg) end
              (imba-instance-variables)
              :exclusive 'no))
       (t
        (list beg end
              (imba-code-candidates)
              :exclusive 'no))))))

;; ---------- Комментарии ----------

(defun imba-syntax-propertize (start end)
  "Set syntax properties for Imba block comments (###...###) between START and END.
Markers may have any whitespace indentation prefix."
  (goto-char start)
  (let ((open-p nil))
    ;; Учитываем ВСЕ маркеры ### до start, чтобы корректно
    ;; определить, открыт ли сейчас блочный комментарий
    (save-excursion
      (goto-char start)
      (while (re-search-backward "^[ \t]*###" nil t)
        (setq open-p (not open-p))))
    (while (re-search-forward "^[ \t]*###" end t)
      (if open-p
          (progn
            (put-text-property (match-beginning 0) (match-end 0)
                               'syntax-table (string-to-syntax "> c"))
            (setq open-p nil))
        (put-text-property (match-beginning 0) (match-end 0)
                           'syntax-table (string-to-syntax "< c"))
        (setq open-p t)))))

(defun imba-font-lock-syntactic-face (state)
  "Face for syntactic fontification: strings and ### block comments.
Block comments get `font-lock-doc-face' to be visually distinct
from single-line # comments."
  (if (nth 3 state)
      font-lock-string-face
    font-lock-doc-face))

(defun insert-tab-char ()
  "Insert a tab char. (ASCII 9, \t)"
  (interactive)
  (insert "\t"))

(define-derived-mode imba-mode nil "Imba"
  "Simple mode to edit Imba.
\\{imba-mode-map}"
  (setq-local tab-always-indent 'complete)
  (setq-local indent-tabs-mode t)
  (setq-local tab-width imba-indent-offset)
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-multi-line t)
  (setq-local syntax-propertize-function #'imba-syntax-propertize)
  (setq-local font-lock-syntactic-face-function #'imba-font-lock-syntactic-face)
  (add-hook 'completion-at-point-functions #'imba-completion-at-point nil t)
  (setq-local minor-mode-overriding-map-alist
              `((company-mode . ,(let ((map (make-sparse-keymap)))
                                   (define-key map (kbd "TAB") #'insert-tab-char)
                                   (define-key map (kbd "<tab>") #'insert-tab-char)
                                   map))))
  (local-set-key (kbd "<tab>") 'insert-tab-char)
  (local-set-key (kbd "<C-tab>") 'indent-rigidly-right-to-tab-stop)
  (local-set-key (kbd "<C-M-tab>") 'indent-rigidly-left-to-tab-stop)
  (local-set-key (kbd "M-;") 'comment-dwim)
  (local-set-key (kbd "M-/") 'completion-at-point)
  (define-key imba-mode-map (kbd "C-c :") 'imba-toggle-region-comment)
  (setq font-lock-defaults '(imba-highlights))
  )

(defun imba-toggle-region-comment (beg end)
  "Toggle Imba block comment (###) around region or current line.
Markers are placed with the same indentation as the first line
of the region, preserving Imba's indentation-based structure."
  (interactive "r")
  (save-excursion
    (let* ((use-region (use-region-p))
           (start (if use-region beg (line-beginning-position)))
           (finish (if use-region end (line-end-position)))
           ;; префикс пробельных символов первой строки региона
           (indent
            (save-excursion
              (goto-char start)
              (beginning-of-line)
              (buffer-substring-no-properties
               (point)
               (progn (back-to-indentation) (point)))))
           (marker (concat indent "###"))
           (marker-regexp (concat "^" (regexp-quote marker) "[ \t]*$"))
           (prev-line
            (save-excursion
              (goto-char start)
              (forward-line -1)
              (when (looking-at marker-regexp)
                (point))))
           (next-line
            (save-excursion
              (goto-char finish)
              (unless (bolp) (forward-line 1))
              (when (looking-at marker-regexp)
                (point)))))
      (if (and prev-line next-line)
          ;; Remove existing comment markers
          (progn
            (goto-char next-line)
            (delete-region (point) (progn (forward-line 1) (point)))
            (goto-char prev-line)
            (delete-region (point) (progn (forward-line 1) (point))))
        ;; Add comment markers
        (goto-char finish)
        (unless (bolp) (forward-line 1))
        (insert marker "\n")
        (goto-char start)
        (beginning-of-line)
        (insert marker "\n")))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.imba" . imba-mode))

(provide 'imba-mode)
