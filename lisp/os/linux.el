;;; linux.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jamie Cui
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; setup new const variables, make sure to doom sync after change
(setq! jamie-use-remote-path 'nil) ; non-nil to use remote path

;; NOTE A Control-modified alphabetical character is always considered
;; case-insensitive: Emacs always treats C-A as C-a, C-B as C-b, and so forth.
;; The reason for this is historical.
(global-set-key (kbd "M-f") #'+default/search-buffer) ; set
(global-set-key (kbd "M-s") #'save-buffer)
(global-set-key (kbd "M-z") #'nil) ;; use evil to undo and redo!

;; NOTE I did not change the key bindings for the following in the powertoys,
;; so this will stick with the meta key short cut
(global-set-key (kbd "M-/") #'comment-line)
(global-set-key (kbd "M-=") #'doom/increase-font-size)
(global-set-key (kbd "M--") #'doom/decrease-font-size)

;; ----------------------------------------------------------------------------
;; Use Rime on Linux
;; ----------------------------------------------------------------------------
(setq fcitx-remote-command "fcitx5-remote")
(setq pyim-pinyin-fuzzy-alist nil) ; no fuzzing in chinese input

(after! ace-pinyin
  (setq ace-pinyin-simplified-chinese-only-p t))

(use-package! rime
  :config
  (setq! default-input-method "rime"
         rime-show-candidate 'popup)

  (global-set-key (kbd "C-SPC") #'toggle-input-method))
