;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; load some definition functions from my own elisp file
(load (concat doom-user-dir "define.el"))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Jamie Cui"
      user-mail-address "jamie.cui@outlook.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;     doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
;; (setq doom-font (font-spec :family "Fira Code" :size 14 :slant 'normal :weight 'normal))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-monokai-ristretto)
(setq doom-theme 'leuven)

;; Maximized screen on doom start
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "/Users/shanzhu.cjm/Library/Mobile Documents/com~apple~CloudDocs/org")
(setq org-roam-directory "/Users/shanzhu.cjm/Library/Mobile Documents/com~apple~CloudDocs/roam")

(after! org
  ;; Setup org-latex-preview, load cryptocode, and scale the generated math imgs
  (add-to-list 'org-latex-packages-alist '("n,advantage, operators, sets, adversary, landau, probability, notions, logic, ff, mm, primitives, events, complexity, oracles, asymptotics, keys" "cryptocode" t))
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 0.9))

  )
  ;; Disable auto-fill in org mode
  (remove-hook 'org-mode-hook #'auto-fill-mode)


;; Return in org now follows link (globally)
(setq org-return-follows-link 't)

;; Make Evil behave more like vim
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))

;; Config undo with +tree
(after! undo-tree
  (setq undo-tree-auto-save-history nil))

;; Setup proxies for emacs
;; (setq url-proxy-services
;; '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
;; ("http" . "127.0.0.1:8001")
;; ("https" . "127.0.0.1:8001")))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;

;; Setup default tramp setting, from https://www.emacswiki.org/emacs/TrampMode
(setq tramp-default-method "sshx") ;; use sshx (since it supports zsh) instead of default scp

;; Config Tramp
(after! tramp
  (when (require 'lsp-mode nil t)
    (setq lsp-enable-snippet nil
          lsp-log-io nil
          lsp-enable-symbol-highlighting nil)
    (lsp-register-client
     (make-lsp-client
      :new-connection
      (lsp-tramp-connection-over-ssh-port-forwarding "clangd --header-insertion=never")
      :major-modes '(c-mode c++-mode)
      :remote? t
      :server-id 'clangd-remote))))


;; For cpplint
;; see: https://github.com/kkholst/.doom.d/blob/main/config.org
(after! flycheck
  (require 'flycheck-google-cpplint)
  (setq flycheck-c/c++-googlelint-executable "cpplint"
        flycheck-cppcheck-standards '("c++11"))
  (flycheck-add-next-checker 'c/c++-cppcheck '(warning . c/c++-googlelint))
  (add-hook! 'lsp-after-initialize-hook
    (run-hooks (intern (format "%s-lsp-hook" major-mode))))
  (defun my-c++-linter-setup ()
    (flycheck-add-next-checker 'lsp 'c/c++-googlelint))
  (add-hook 'c++-mode-lsp-hook #'my-c++-linter-setup))


