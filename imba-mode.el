;;; imba-mode.el --- Major mode for editing Imba files

;; Copyright (c) 2018 Free Software Foundation, Inc.
;;
;; Author: Alexandr Selunin <aka.qwars@gmail.com>
;; Maintainer: Alexandr Selunin <aka.qwars@gmail.com>
;; Created: 08 Mar 2018
;; Version: 0.01
;; Keywords: languages, imba

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Major mode for editing Imba files.

;; Put this file into your load-path and the following into your ~/.emacs:
;; (require 'imba-mode)

;;; Code:

(defgroup imba nil
  "Support for the Imba serialization format."
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
  "Face for function names."
  :group 'imba)

(defface imba-function-state-name-face
  '((t :foreground "dark cyan" :weight bold))
  "Face for function state names."
  :group 'imba)

(defface imba-keyword-face
  '((t :foreground "DarkGreen" :weight bold))
  "Face for keywords."
  :group 'imba)

(defface imba-string-variable-name-face
  '((t :foreground "DarkRed" :weight bold))
  "Face for string variables."
  :group 'imba)

(defface imba-lock-comment-face
  '((t :foreground "DarkGray"))
  "Face for string variables."
  :group 'imba)

;; Функция для поиска многострочных комментариев с учетом уровня отступа
(defun imba-font-lock-multiline-comments (limit)
  "Search for multiline comments up to LIMIT."
  (let ((found nil))
    (while (and (not found) (re-search-forward "\\(^[ \t]*?###\\)" limit t))
      (let ((indentation (match-string 1))
            (beg (match-beginning 0)))
        ;; (message "Found start marker at %d with indentation '%s'" beg indentation)
        (if (re-search-forward indentation limit t)
            (progn
              ;; (message "Found end marker at %d" (point))
              (set-match-data (list beg (point)))
              (setq found t))
          ;; (message "End marker not found, resetting position")
          (goto-char (1+ beg)))))
    found))

(defvar imba-highlights
  '(
    (imba-font-lock-multiline-comments . 'imba-lock-comment-face)
    ("#[ \t]+.*$" . 'imba-lock-comment-face)
    ("\"[^\"]*\"" . font-lock-string-face)
    ("'[^']*'" . font-lock-string-face)
    ("\\(#[a-zA-Z][a-zA-Z-_0-9]+\\)\\b" . font-lock-keyword-face)
    ("\\b\\(export\\|import\\|require\\|Imba\\|route\\([.-][A-Za-z_]+\\)*\\)\\b" . 'imba-keyword-face)
    ("\\b\\([A-Za-z_][A-Za-z_0-9-]*\\)[\\.]new\\b" . font-lock-type-face)
    ("[@][A-Za-z_][A-Za-z_0-9-]*" . font-lock-variable-name-face)
    ("\\bdef[ \t]+\\(render\\|build\\|mount\\|setup\\|unmount\\|tikc\\|initialize\\)\\b" . 'imba-function-state-name-face)
    ("\\bexport[ \t]+\\(default\\)" . 'imba-function-state-name-face)
    ("\\b\\(tag\\|def\\|class\\)[ \t]+\\([A-Za-z_][A-Za-z_0-9-]+\\)" . 'imba-function-name-face)
    ("\\btag[ \t]+[A-Za-z_][A-Za-z_0-9-]*[ \t]+\\([<][ \t]+[A-Za-z_][A-Za-z_0-9-]*\\)" . font-lock-type-face)
    ("[<]\\(self\\)" . font-lock-constant-face)
    ("[<]\\([A-Za-z_][A-Za-z_0-9]*\\)" . 'imba-string-variable-name-face)
    ("\\([.][A-Za-z_][A-Za-z_0-9-]*\\)" . font-lock-preprocessor-face)
    ("\\([:][A-Za-z_][A-Za-z_0-9-]*\\)" . font-lock-constant-face)
    ("\\([A-Za-z_][A-Za-z_0-9-]*[:\.][ \t]*\\)" . font-lock-constant-face)
    ("\\([A-Za-z_][A-Za-z_0-9-]*\\)[:\.\(\[]" . font-lock-variable-name-face)
    ("\\b\\(return\\|def\\|tag\\|var\\|let\\|const\\|prop\\|new\\|or\\|and\\|not\\|if\\|unless\\|elif\\|else\\|when\\|for\\|until\\|while\\|do\\|map\\|class\\|setTimeout\\|setInterval\\|clearTimeout\\|clearInterval\\|parseInt\\|parseFloat\\|__dirname\\|process\\|console\\|document\\|window\\|this\\|then\\|in\\|self\\|delete\\)\\b" . font-lock-function-name-face)
    ("\\b\\(attr\\|super\\|typeof\\|break\\|in\\|of\\|continue\\|as\\|from\\|render\\|unschedule\\|schedule\\|build\\|setup\\|mount\\|unmount\\|tikc\\)\\b" . font-lock-keyword-face)
    ("\\b\\(null\\|true\\|false\\|undefined\\|await\\|async\\)\\b" . font-lock-constant-face)
    ("\\b\\(var\\|let\\|const\\)[ \t]+\\([A-Za-z_][A-Za-z_0-9]*\\)" . font-lock-variable-name-face)
    ))

(defun insert-tab-char ()
   "Insert a tab char. (ASCII 9, \t)."
   (interactive)
   (insert "\t"))

(defun imba-toggle-region-comment ()
  "Toggle comments for a region in Imba mode using '###' markers."
  (interactive)
  (let* ((region-start (region-beginning))
         (region-end (region-end))
         (min-indent (save-excursion
                       (goto-char region-start)
                       (let ((min-indent most-positive-fixnum))
                         (while (< (point) region-end)
                           (let ((line-indent (current-indentation)))
                             (when (< line-indent min-indent)
                               (setq min-indent line-indent)))
                           (forward-line 1))
                         min-indent))))
    (save-excursion
      (goto-char region-end)
      (end-of-line)
      (insert "\n" (make-string min-indent ?\s) "###")
      (goto-char region-start)
      (beginning-of-line)
      (insert (make-string min-indent ?\s) "###\n"))))

(defun imba-mode-refresh-font-lock ()
  "Refresh font lock in the current buffer."
  (font-lock-flush)
  (font-lock-ensure))


(define-derived-mode imba-mode prog-mode "Imba"
  "Major mode for editing Imba files."
  (setq-local tab-always-indent 'complete)
  (setq-local indent-tabs-mode t)
  (setq-local tab-width imba-indent-offset)
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-multi-line t)
  (font-lock-mode 1)
  (setq font-lock-defaults '(imba-highlights))
  (local-set-key (kbd "<tab>") 'insert-tab-char)
  (local-set-key (kbd "<C-tab>") 'indent-rigidly-right-to-tab-stop)
  (local-set-key (kbd "<C-M-tab>") 'indent-rigidly-left-to-tab-stop)
  (local-set-key (kbd "M-;") 'comment-dwim)
  (local-set-key (kbd "C-c ;") 'imba-toggle-region-comment)
  (add-hook 'after-change-functions
             (lambda (beg end len)
               (imba-mode-refresh-font-lock))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.imba" . imba-mode))

(provide 'imba-mode)

;;; imba-mode.el ends here
