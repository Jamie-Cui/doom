;;; editor.el -*- lexical-binding: t; -*-

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

;; do not set line numer mode for those modes
;; (after! treemacs
;;   (dolist (mode '(org-mode-hook
;;                   term-mode-hook
;;                   vterm-mode-hook
;;                   shell-mode-hook
;;                   comint-mode-hook
;;                   message-mode-hook
;;                   treemacs-mode-hook
;;                   eshell-mode-hook))
;;     (add-hook mode (lambda() (display-line-numbers-mode 0)))))

;; ----------------------------------------------------------------------------
;; Completion
;; ----------------------------------------------------------------------------
;; Remember that! Corfu use M-n M-p to navigate between corfu candidates!

;; ----------------------------------------------------------------------------
;; Configuration: undo
;; ----------------------------------------------------------------------------
(after! undo-tree
  (setq undo-tree-auto-save-history nil))

;; do-not truncate lines by default
(set-default 'truncate-lines nil)

;; disable vim "u" undo, I prefer use of "C-z"
(define-key evil-normal-state-map "u" nil)

;; use relative line numbers
(setq display-line-numbers-type 'relative)
(setq display-line-numbers-grow-only 't)
(setq display-line-numbers-width-start 't)
