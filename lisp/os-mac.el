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
;; setup default font

;; (setq doom-font (font-spec :family "0xProto Nerd Font Mono" :size 16 :weight 'medium))

;; (defun init-cjk-fonts()
;;   (dolist (charset '(kana han cjk-misc bopomofo))
;;     (set-fontset-font (frame-parameter nil 'font)
;;                       charset (font-spec :family "AR PL KaitiM GB" :size 18))))
;; (add-hook 'doom-init-ui-hook 'init-cjk-fonts)

;; setup new const variables, make sure to doom sync after change
(defconst jamie-org-remote-path "~/Library/Mobile Documents/com~apple~CloudDocs/org-remote/")
(defconst jamie-org-local-path "~/org-local/")
(defconst jamie-use-remote-path 't) ; non-nil to use remote path

;; presuites:
;; 1. brew install --cask squirrel
;; 2. see: https://github.com/DogLooksGood/emacs-rime/blob/master/INSTALLATION.org#macos-1
(use-package! rime
  :config
  (setq! default-input-method "rime"
         rime-show-candidate 'postframe
         rime-librime-root "~/.config/emacs/librime/dist"
         rime-emacs-module-header-root "/opt/homebrew/Cellar/emacs-plus@29/29.4/include/")
  (global-set-key (kbd "C-SPC") #'toggle-input-method))


