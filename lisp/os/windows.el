;;; windows.el -*- lexical-binding: t; -*-
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

;; HACK the following bindings are designed specifically for wsl, I've modified
;; windows keybing to the follwoing using the powerful powertoy keyboard manager
;; 1. M-c => C-c
;; 2. M-f => C-f
;; 3. M-q => M-f4
;; 4. M-s => C-s
;; 5. M-v => C-v
;; 6. M-z => C-z
;; 7. M-Z => C-Z
;;
;; NOTE above keybindings will also affect emacs, so we need to modify keybinding
;;
;; I would like the following:
;; 1. C-f: search in every mode
;; 2. C-s: save buffer

;; (setq doom-font (font-spec :family "0xProto Nerd Font Mono" :size 16 :weight 'medium))

;; (defun init-cjk-fonts()
;;   (dolist (charset '(kana han cjk-misc bopomofo))
;;     (set-fontset-font (frame-parameter nil 'font)
;;                       charset (font-spec :family "AR PL KaitiM GB" :size 18))))
;; (add-hook 'doom-init-ui-hook 'init-cjk-fonts)

;; setup new const variables, make sure to doom sync after change
(setq! jamie-use-remote-path 'nil) ; non-nil to use remote path

(global-set-key (kbd "M-f") #'+default/search-buffer) ; set
(global-set-key (kbd "M-s") #'save-buffer)
(global-set-key (kbd "M-/") #'comment-line)
(global-set-key (kbd "M-c") #'evil-yank)
(global-set-key (kbd "M-v") #'evil-paste-before)
(global-set-key (kbd "M-SPC") #'toggle-input-method)

;; ----------------------------------------------------------------------------
;; Hack from: https://gist.github.com/minorugh/1770a6aa93df5fe55f70b4d72091ff76
;; Emacs on WSL open links in Windows web browser
;; https://adam.kruszewski.name/2017/09/emacs-in-wsl-and-opening-links/
;; ----------------------------------------------------------------------------
(when (getenv "WSLENV")
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program  cmd-exe
            browse-url-generic-args     cmd-args
            browse-url-browser-function 'browse-url-generic
            search-web-default-browser 'browse-url-generic))))

;; ----------------------------------------------------------------------------
;; Only use rime on windows!
;; ----------------------------------------------------------------------------
(setq fcitx-remote-command "fcitx5-remote")
(setq pyim-pinyin-fuzzy-alist nil) ; no fuzzing in chinese input

(after! ace-pinyin
  (setq ace-pinyin-simplified-chinese-only-p t))

(use-package! rime
  :config
  (setq! default-input-method "rime"
         rime-show-candidate 'popup))

(after! org-download
  (setq org-download-screenshot-method
        "powershell.exe -Command \"(Get-Clipboard -Format image).Save('$(wslpath -w %s)')\"")
  )

(after! treemacs
  (define-key evil-treemacs-state-map (kbd "w")   #'nil))


(defun cp-current-file-to-windows()
  "Copy the current file to windows"
  (interactive)
  (let ((dest-path (concat "~/Desktop/tmp/" (format-time-string "%Y-%m-%d") "/")))
    (when buffer-file-name
      (make-directory dest-path 'parents)
      (message (concat "cp -r " buffer-file-name " " dest-path))
      (shell-command (concat "cp -r " buffer-file-name " " dest-path))
      ))
  )
