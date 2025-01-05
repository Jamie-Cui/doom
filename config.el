;;; config.el -*- lexical-binding: t; -*-

;; ----------------------------------------------------------------------------
;; Generic Setup
;; ----------------------------------------------------------------------------
(setq user-full-name "Jamie Cui"
      user-mail-address "jamie.cui@outlook.com")

;; setup to use emacs-china mirros
;; see: https://elpamirror.emacs-china.org/
;; (setq package-archives '(("gnu"   . "http://1.15.88.122/gnu/")
;;                          ("melpa" . "http://1.15.88.122/melpa/")))

;; setup theme
(setq doom-theme 'nil)
;; (setq doom-theme 'modus-operandi)

(if :system 'macos (load (concat doom-user-dir "lisp/" "os-macos.el")))
(if :system 'linux (load (concat doom-user-dir "lisp/" "os-windiws.el")))
(if :system 'windows (load (concat doom-user-dir "lisp/" "os-windiws.el")))

;; Don't ask, just quit
(setq confirm-kill-emacs nil)

;; HACK: Query vc status for remote files
(setq ibuffer-vc-skip-if-remote nil)

(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; Maximized screen on doom start
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
;; Load all my tweaks (instantly)
;; ----------------------------------------------------------------------------
(load (concat doom-user-dir "lisp/" "org.el"))
(load (concat doom-user-dir "lisp/" "dev.el"))
(load (concat doom-user-dir "lisp/" "latex.el"))

;; NOTE :system 'windows does not recognize wsl
(load (concat doom-user-dir "lisp/" "os-windows.el"))

;; ----------------------------------------------------------------------------
;; Config thirdparty dependencies
;; ----------------------------------------------------------------------------

;; (use-package! holo-layer
;;   :load-path (lambda()(concat doom-user-dir "thirdparty/holo-layer/"))
;;   :config
;;   (require 'holo-layer)
;;   (holo-layer-enable)
;;   (setq!
;;    holo-layer-enable-cursor-animation 't
;;    holo-layer-cursor-animation-interval 10
;;    holo-layer-cursor-alpha 100
;;    holo-layer-python-command "~/miniconda3/bin/python"))

;; (use-package! disable-mouse
;;   :load-path (lambda()(concat doom-user-dir "thirdparty/disable-mouse/"))
;;   :config
;;   (require 'disable-mouse)
;;   (global-disable-mouse-mode)
;;   (mapc #'disable-mouse-in-keymap
;;         (list evil-motion-state-map
;;               evil-normal-state-map
;;               evil-visual-state-map
;;               evil-insert-state-map)))

(use-package! keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; in treemacs-evil.el
;; (define-key evil-treemacs-state-map (kbd "w")   #'treemacs-set-width)
(after! treemacs
  (define-key evil-treemacs-state-map (kbd "w")   #'nil))

;; enable evil on pdf-view-mode
(evil-set-initial-state 'pdf-view-mode 'normal)

(after! corfu
  (evil-define-key 'insert corfu-mode-map (kbd "C-SPC") #'nil)
  (evil-define-key 'normal corfu-mode-map (kbd "C-SPC") #'nil)
  (evil-define-key 'visual corfu-mode-map (kbd "C-SPC") #'nil))
