# imba-mode.el

Мажорный режим [Emacs](https://www.gnu.org/software/emacs/) для языка
[Imba](http://imba.io) — языка программирования для веба, который компилируется
в производительный JavaScript.

**English**: [README.md](README.md)

![Screen](https://raw.githubusercontent.com/qwars/imba-mode.el/c3a9b54cc4d313f517e85687ec53c4b1f7bfda8d/screen.png)

## Возможности

- **Подсветка синтаксиса** Imba 1.x: ключевые слова, объявления тегов/классов,
  свойства, переменные экземпляра (`@var`), аргументы блоков (`$0`–`$9`),
  методы жизненного цикла тега (`build`, `setup`, `mount`, `tick`, `render`, …)
  и другое.
- **Комментарии** — однострочные `# …` и многострочные блоки `### … ###`
  подсвечиваются разными face, чтобы их было легко различать.
- **Автодополнение в точке** (работает через `completion-at-point` и
  автоматически подхватывается `company-mode` через `company-capf`):
  - ключевые слова Imba и предопределённые глобальные переменные;
  - имена HTML-тегов и объявленных в буфере тегов сразу после `<`;
  - переменные экземпляра после `@`;
  - опциональный словарь пользователя (совместим со словарями старого
    `auto-complete-mode`).
- **Блочные комментарии для региона** по `C-c :` — оборачивает регион (или
  текущую строку) в маркеры `### … ###`, сохраняя отступ первой строки.
- **Навигация по структуре** через `outline-minor-mode` по `tag` / `def` /
  `class` / `export`.
- **Визуализация пробелов** — табуляции и хвостовые пробелы через
  `whitespace-mode`.

Imba использует блочную структуру на отступах и табуляцию; режим это учитывает
и оставляет за `TAB` вставку символа табуляции.

## Требования

- GNU Emacs 28.2 или новее
- Опционально: [company](https://company-mode.github.io/) для всплывающего
  автодополнения в буфере (без него режим работает через `M-/`)

## Установка

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
    ;; автодополнение: company подхватывает capf из imba-mode через company-capf
    (company-mode 1)
    (setq-local company-idle-delay 0.3)
    ;; список тегов после < всплывает уже после первой буквы
    (setq-local company-minimum-prefix-length 1)
    ;; визуализация табуляций и хвостовых пробелов
    (setq-local whitespace-style '(face trailing tabs tab-mark))
    (setq-local whitespace-display-mappings '((tab-mark 9 [8594 9] [92 9])))
    (whitespace-mode t)
    ;; структура файла (tag/def/class как заголовки outline)
    (outline-minor-mode 1)
    (setq-local outline-regexp
                "[[:space:]]*\\(tag...\\|def...\\|\\bdo....\\|class.\\|export\\|#.....\\)"))
  :hook
  (imba-mode . imba-mode-hook-setup))
```

### Вручную

```elisp
(add-to-list 'load-path "/path/to/imba-mode/")
(require 'imba-mode)
```

Файлы с расширением `.imba` открываются в `imba-mode` автоматически.

## Сочетания клавиш

| Клавиша   | Команда                                      |
|-----------|----------------------------------------------|
| `TAB`     | Вставить символ табуляции                    |
| `C-TAB`   | Жёсткий сдвиг региона вправо до таб-стопа    |
| `C-M-TAB` | Жёсткий сдвиг региона влево до таб-стопа     |
| `M-;`     | Закомментировать/раскомментировать строку    |
| `M-/`     | Автодополнение в точке                       |
| `C-c :`   | Обернуть/снять блочный комментарий `### … ###` |

## Автодополнение

`imba-mode` регистрирует функцию `completion-at-point` с тремя контекстами:

1. **Имена тегов после `<`** — HTML-теги, `self`, а также объявленные в
   текущем буфере теги через `tag Name` (или `export tag Name`).
2. **Переменные экземпляра после `@`** — собираются из текущего буфера.
3. **Идентификаторы** — ключевые слова Imba, предопределённые глобали и
   опциональный словарь.

### Словарь

Установите `imba-dictionary-file` на файл со списком слов (по одному на
строку) — тот же формат, что у словарей `auto-complete-mode`. Слова
добавляются к дополнению идентификаторов. Установите в `nil`, чтобы отключить.

```elisp
(setq imba-dictionary-file "~/.emacs.d/modules/dict/imba-mode")
```

## Комментарии

- `# …` начинает однострочный комментарий.
- `###` на отдельной строке открывает/закрывает блочный комментарий.
  Используйте `C-c :`, чтобы обернуть или снять такой блок.

## Лицензия

Copyright © 2018 Free Software Foundation, Inc.

Под лицензией GNU General Public License версии 2 или (по вашему выбору) любой
более поздней версии. См. [COPYING](https://www.gnu.org/licenses/gpl-2.0.html).
