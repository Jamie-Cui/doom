;;; config.el -*- lexical-binding: t; -*-
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

;; ----------------------------------------------------------------------------
;; Generic Setup
;; ----------------------------------------------------------------------------
(setq user-full-name "Jamie Cui"
      user-mail-address "jamie.cui@outlook.com")

;; setup to use emacs-china mirros
;; see: https://elpamirror.emacs-china.org/
;; (setq package-archives '(("gnu"   . "http://1.15.88.122/gnu/")
;;                          ("melpa" . "http://1.15.88.122/melpa/")))

(defcustom jamie-use-remote-path 'nil
  "Whether to use the remote path. Set this var to non-nil to use remote path")
(defconst jamie-org-remote-path
  "~/Library/Mobile Documents/com~apple~CloudDocs/org-remote/")
(defconst jamie-org-local-path "~/org-local/")

;; setup theme
(setq doom-theme 'nil)
;; (setq doom-theme 'modus-vivendi)

;; setup default font
(setq doom-font
      (font-spec :family "0xProto Nerd Font Mono" :weight 'medium))

(if (display-graphic-p)
    ;; NOTE set fonts in graphic mode
    (progn
      (defun init-cjk-fonts()
        (dolist (charset '(kana han cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font)
                            charset (font-spec :family "AR PL KaitiM GB"))))
      (add-hook 'doom-init-ui-hook 'init-cjk-fonts)
      )
  ;; NOTE hide modeline in terminal mode
  (global-hide-mode-line-mode)
  )

;; Don't ask, just quit
(setq confirm-kill-emacs nil)

;; HACK: Query vc status for remote files
(setq ibuffer-vc-skip-if-remote nil)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(undecorated . t)) ;; no title bar

;; ----------------------------------------------------------------------------
;; Configuration: proxy
;; ----------------------------------------------------------------------------
(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
        ("http" . "127.0.0.1:8001")
        ("https" . "127.0.0.1:8001")))

;; ----------------------------------------------------------------------------
;; Editor Setup
;; ----------------------------------------------------------------------------

;; Paste and kill selected origin: https://emacs.stackexchange.com/a/15054
(fset 'evil-visual-update-x-selection 'ignore)

;; Fix chinese wrap
(setq word-wrap-by-category t)

;; Make Evil behaves more like vim
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))

;; ----------------------------------------------------------------------------
;; Configuration: undo
;; ----------------------------------------------------------------------------
(after! undo-tree
  (setq undo-tree-auto-save-history nil))

;; do-not truncate lines by default
(set-default 'truncate-lines nil)

;; disable vim "u" undo, I prefer use of "C-z"
;; (define-key evil-normal-state-map "u" nil)

;; use relative line numbers
(setq display-line-numbers-type 't)
(setq display-line-numbers-grow-only 't)
(setq display-line-numbers-width-start 't)


;; ----------------------------------------------------------------------------
;; Config thirdparty dependencies
;; ----------------------------------------------------------------------------

(use-package! keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; enable evil on pdf-view-mode
(evil-set-initial-state 'pdf-view-mode 'normal)

(after! corfu
  (evil-define-key 'insert corfu-mode-map (kbd "C-SPC") #'nil)
  (evil-define-key 'normal corfu-mode-map (kbd "C-SPC") #'nil)
  (evil-define-key 'visual corfu-mode-map (kbd "C-SPC") #'nil))

(after! dirvish
  (setq! dirvish-default-layout '(0 0.26 0.74)) ; same as dirvish-side
  (setq! dirvish-hide-details 't)
  (setq! dirvish-use-mode-line 'global)
  (setq! dirvish-use-header-line 'global)
  (add-hook 'dirvish-find-entry-hook
            (lambda (&rest _) (setq-local truncate-lines t)))
  )

(setq-default fill-column 80)

(use-package! edwina
  :ensure t
  :config
  (setq display-buffer-base-action '(display-buffer-below-selected))
  (edwina-setup-dwm-keys)
  (edwina-mode 1)
  (map!
   :leader
   :desc "edwina-zoom"               "w RET" #'edwina-zoom
   :desc "edwina-arrange"            "w r" #'edwina-arrange
   :desc "edwina-clone-window"       "w c" #'edwina-clone-window)
  )

;; ----------------------------------------------------------------------------
;; Load all my tweaks (instantly)
;; ----------------------------------------------------------------------------
(when (featurep :system 'macos)
  (load (concat doom-user-dir "lisp/os/mac.el")))
(when (featurep :system 'linux)
  ;; HACK wsl is recognized as linux
  (load (concat doom-user-dir "lisp/os/windows.el")))
(when (featurep :system 'windows)
  (load (concat doom-user-dir "lisp/os/windows.el")))

(load (concat doom-user-dir "lisp/jamie-define.el"))
(load (concat doom-user-dir "lisp/mode/org.el"))
(load (concat doom-user-dir "lisp/mode/latex.el"))
(load (concat doom-user-dir "lisp/dev.el"))
(load (concat doom-user-dir "lisp/package/gptel.el"))
;; (load (concat doom-user-dir "lisp/" "eaf.el"))

(add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
