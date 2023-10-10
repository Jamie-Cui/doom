;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; load some definition functions from my own elisp file
(load (concat doom-user-dir "define.el"))

(setq user-full-name "Jamie Cui"
      user-mail-address "jamie.cui@outlook.com")

(setq doom-theme 'modus-vivendi)

;; Maximized screen on doom start
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; -----------------------------------------
;; Configuration: org mode / genearl typeing
;; -----------------------------------------
(setq org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/Sync/org")
(setq org-roam-directory "~/Library/Mobile Documents/com~apple~CloudDocs/Sync/org/roam")

(setq deft-directory "~/Library/Mobile Documents/com~apple~CloudDocs/Sync/org/deft")

;; Setup org-latex-preview, load cryptocode, and scale the generated math imgs
(after! org
  (add-to-list 'org-latex-packages-alist '("lambda, advantage, operators, sets, adversary, landau, probability, notions, logic, ff, mm, primitives, events, complexity, oracles, asymptotics, keys" "cryptocode" t))
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 0.95))
  (setq org-startup-with-latex-preview t)
  (setq org-startup-folded 'content)
  (setq org-startup-with-inline-images t))

;; for cite in org
(after! citar
  (add-to-list 'citar-notes-paths "~/Library/Mobile Documents/com~apple~CloudDocs/Sync/papers")
  (add-to-list 'citar-bibliography "~/Library/Mobile Documents/com~apple~CloudDocs/Sync/zotero_all.bib"))

;; Setup org-download directory
(after! org-download
  (setq org-download-method 'directory)
  (setq org-download-image-dir "img")
  (setq org-download-image-org-width 500)
  (setq org-download-link-format "[[file:%s]]\n"
        org-download-abbreviate-filename-function #'file-relative-name)
  (setq org-download-link-format-function #'org-download-link-format-function-default))

;; fix chinese wrap
(setq word-wrap-by-category t)

;; Return in org now follows link (globally)
(setq org-return-follows-link t)

;; Make Evil behaves more like vim
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))

;; -------------------------
;; Configuration: completion
;; -------------------------
;; Only complete when I ask!
;; https://www.reddit.com/r/DoomEmacs/comments/wdxah3/how_to_stop_word_autocomplete/
(after! company
  (setq company-idle-delay nil))

;; -------------------------
;; Configuration: undo
;; -------------------------
(after! undo-tree
  (setq undo-tree-auto-save-history nil))

;; --------------------
;; Configuration: proxy
;; --------------------
(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
        ("http" . "127.0.0.1:8001")
        ("https" . "127.0.0.1:8001")))

;; ----------------------------------
;; Configuration: code format on save
;; ----------------------------------
;; always format using the local formatter (especially when using lsp over tramp)
(setq +format-with-lsp nil) ;; do not format with lsp
(setq apheleia-remote-algorithm 'local)
(setf (alist-get 'clang-format apheleia-formatters)
      '("clang-format" "--style=file:/Users/shanzhu.cjm/Desktop/jdt/config/clang-format-sty" "-"))  ;; absolute path

;; -------------------------------------
;; Configuration: tramp: lsp, projectile
;; -------------------------------------
;; it's wired that vertico uses this to list all files
(setq projectile-git-fd-args "--color=never -H -0 -E .git -tf --strip-cwd-prefix")
(setq projectile-fd-executable "fdfind") ;; ubuntu's command is fdfind

;; Config Tramp
(after! tramp
  ;; Setup default tramp setting, from https://www.emacswiki.org/emacs/TrampMode
  (setq tramp-default-method "sshx") ;; use sshx (since it supports zsh) instead of default scp

  ;; Setup lsp over tramp
  (lsp-register-client
   (make-lsp-client
    :new-connection
    (lsp-tramp-connection-over-ssh-port-forwarding "clangd --header-insertion=never")
    :major-modes '(c-mode c++-mode)
    :remote? t
    :server-id 'clangd-remote)))

;; Use zsh over vterm tramp
(after! vterm
  (setq vterm-tramp-shells '(("sshx" "/bin/zsh"))))


;; -------------------------------------
;; Configuration: flycheck: cpplint
;; -------------------------------------
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


;; ---------------------------
;; Configuration: doom: others
;; ---------------------------

;; Don't ask, just quit
;; (setq confirm-kill-emacs nil)
