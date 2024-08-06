;;; config.el -*- lexical-binding: t; -*-

;; You may want to install the following open-source apps:
;; * terminal:          https://alacritty.org/                  $HOME/.alacritty.toml, see configs in https://alacritty.org/config-alacritty.html
;; * pdf:               https://skim-app.sourceforge.io/
;; * bib:               https://www.zotero.org/

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
(setq doom-theme 'modus-vivendi)

;; setup default font
;; (setq doom-font (font-spec :family "Monaco" :size 14 :weight 'medium))

;; setup new const variables, make sure to doom sync after change
(defconst jamie-org-remote-path "~/Library/Mobile Documents/com~apple~CloudDocs/org-remote/")
(defconst jamie-org-local-path "~/org-local/")
(defconst jamie-use-remote-path nil) ; non-nil to use remote path

;; Don't ask, just quit
(setq confirm-kill-emacs nil)

;; HACK: Query vc status for remote files
;; (setq ibuffer-vc-skip-if-remote nil)

;; HACK: Enable the vc gutter in remote files (e.g. open through TRAMP)
;; (setq +vc-gutter-in-remote-files t)

;; ----------------------------------------------------------------------------
;; Configuration: windows
;; ----------------------------------------------------------------------------
(setq scroll-step            1
      scroll-conservatively  10000
      next-screen-context-lines 5
      ;; move by logical lines rather than visual lines (better for macros)
      line-move-visual nil)

(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; Maximized screen on doom start
;; (add-to-list 'default-frame-alist '(undecorated . t)) ;; no title bar
;; (add-to-list 'default-frame-alist '(tool-bar-lines . 0))
;; (add-to-list 'default-frame-alist '(menu-bar-lines . 0))
;; (add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
;; (add-to-list 'default-frame-alist '(horizontal-scroll-bars . nil))

(map! :leader
      :desc "Switch to next frame" ;; Switch between different emacs frames
      "F" #'+evil/next-frame)

;; ----------------------------------------------------------------------------
;; Configuration: proxy
;; ----------------------------------------------------------------------------
(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
        ("http" . "127.0.0.1:8001")
        ("https" . "127.0.0.1:8001")))

;; ----------------------------------------------------------------------------
;; Load my own configurations
;; ----------------------------------------------------------------------------
(load (concat doom-user-dir "tweaks/editor.el"))
(load (concat doom-user-dir "tweaks/org.el"))
(load (concat doom-user-dir "tweaks/cppdev.el"))
(load (concat doom-user-dir "tweaks/latex.el"))
(load (concat doom-user-dir "tweaks/leetcode.el"))
(load (concat doom-user-dir "tweaks/define.el"))
