;;; editor.el -*- lexical-binding: t; -*-

;; setup interier shell (built-in with emacs) type
;; REVIEW not sure if this variable is used by tramp or not
(setq explicit-shell-file-name (executable-find "zsh")) ; emacs-c-code variable

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
;; Font Setup
;; ----------------------------------------------------------------------------

;; see: https://www.reddit.com/r/DoomEmacs/comments/vr5mtn/comment/ieuu0wi/?utm_source=share&utm_medium=web2x&context=3
;; The doom process for changing default font size, I stayed up late last night to learn this nugget:
;; 1. In Doom eMacs, press: SPC h F
;; 2. type "default" then Enter
;; 3. Press tab once to select "Customize this face" then Enter
;; 4. type "/pt" then Enter,
;; 5. Press Tab then Enter and enter "200" for for a 20 font size
;; 6. You can use the mouse finally and click "Apply and Save"

;; (setq doom-font (font-spec :size 14))
;; Plan A: 中文苹方, 英文Roboto Mono
;; (setq doom-font (font-spec :family "Monaco" :size 14)
;;       doom-serif-font doom-font
;;       doom-symbol-font (font-spec :family "Hei")
;;       doom-variable-pitch-font (font-spec :family "Hei" :weight 'extra-bold))

;; ;; 如果不把这玩意设置为 nil, 会默认去用 fontset-default 来展示, 配置无效
;; (setq use-default-font-for-symbols nil)

;; ;; Doom 的字体加载顺序问题, 如果不设定这个 hook, 配置会被覆盖失效
;; (add-hook! 'after-setting-font-hook
;;   (set-fontset-font t 'latin (font-spec :family "Monaco"))
;;   (set-fontset-font t 'symbol (font-spec :family "Monaco"))
;;   (set-fontset-font t 'mathematical (font-spec :family "Monaco"))
;;   (set-fontset-font t 'emoji (font-spec :family "Monaco")))

;; ----------------------------------------------------------------------------
;; Configuration: undo
;; ----------------------------------------------------------------------------
(after! undo-tree
  (setq undo-tree-auto-save-history nil))

;; theme disable line-highlight foreground face
;; (set-face-attribute 'highlight nil :foreground 'nil)

;; do-not truncate lines by default
(set-default 'truncate-lines nil)

;; use relative line numbering
;; (setq display-line-numbers-type 'relative)
