;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; load some definition functions from my own elisp file
(load (concat doom-user-dir "define.el"))

(setq user-full-name "Jamie Cui"
      user-mail-address "jamie.cui@outlook.com")

(setq doom-theme 'doom-dark+)

;; Manipulate windows
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; Maximized screen on doom start
(add-to-list 'default-frame-alist '(undecorated . t)) ;; no title bar

;; Don't ask, just quit
;; (setq confirm-kill-emacs nil)

;; -----------------------
;; Configuration: org mode
;; -----------------------
(setq org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/Sync/org")
(setq org-roam-directory "~/Library/Mobile Documents/com~apple~CloudDocs/Sync/org/roam")
(setq deft-directory "~/Library/Mobile Documents/com~apple~CloudDocs/Sync/org/deft")

;; Setup org-latex-preview, load cryptocode, and scale the generated math imgs
(after! org
  (add-to-list 'org-latex-packages-alist '("lambda, advantage, operators, sets, adversary, landau, probability, notions, logic, ff, mm, primitives, events, complexity, oracles, asymptotics, keys" "cryptocode" t))
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 0.95))
  (setq org-startup-with-latex-preview t)
  (setq org-startup-folded 'content)
  (setq org-startup-with-inline-images t)
  ;; Return in org now follows link (globally)
  (setq org-return-follows-link t))

;; Setup org-download directory
(after! org-download
  (setq org-download-method 'directory)
  (setq org-download-image-dir "img")
  (setq org-download-image-org-width 500)
  (setq org-download-link-format "[[file:%s]]\n"
        org-download-abbreviate-filename-function #'file-relative-name)
  (setq org-download-link-format-function #'org-download-link-format-function-default))

;; -----------------------
;; Configuration: Citation
;; -----------------------
(after! citar
  (add-to-list 'citar-notes-paths "~/Library/Mobile Documents/com~apple~CloudDocs/Sync/papers")
  (add-to-list 'citar-bibliography "~/Library/Mobile Documents/com~apple~CloudDocs/Sync/zotero_all.bib"))


;; ------------------------------
;; Configuration: genearl typeing
;; ------------------------------

;; Paste and kill selected origin: https://emacs.stackexchange.com/a/15054
(fset 'evil-visual-update-x-selection 'ignore)

;; Fix chinese wrap
(setq word-wrap-by-category t)

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
;; (setq url-proxy-services
;;       '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
;;         ("http" . "127.0.0.1:8001")
;;         ("https" . "127.0.0.1:8001")))

;; -------------------------------------
;; Configuration: lsp c++ format on save
;; -------------------------------------
;; a work-around from: https://github.com/doomemacs/doomemacs/issues/7490
(setq-hook! 'c++-mode-hook
  apheleia-inhibit t
  +format-with nil)
(add-hook 'c++-mode-hook
          (lambda()
            (add-hook 'before-save-hook #'+format/buffer nil t)))

;; -------------------------------------
;; Configuration: tramp, lsp, projectile
;; -------------------------------------
;; it's wired that vertico uses this to list all files
(setq projectile-git-fd-args "--color=never -H -0 -E .git -tf --strip-cwd-prefix")
;; (setq projectile-fd-executable (cl-find-if #'executable-find (list "fdfind" "fd")))
(setq projectile-fd-executable "fdfind") ;; since I'm using ubuntu machines

;; Config Tramp
(after! tramp
  ;; Setup default tramp setting, from https://www.emacswiki.org/emacs/TrampMode
  (setq tramp-default-method "sshx")) ;; use sshx (since it supports zsh) instead of default scp

;; Cionfigure lsp over tramp
(after! (:and lsp-mode tramp)
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

;; -------------------------------
;; My Package [latex-preview-pane]
;; -------------------------------
(require 'latex-preview-pane)

;; -------------------
;; My Package [zotero]
;; -------------------
;; load zotero packages
(require 'zotero)
(require 'zotero-browser)


;; ------------------
;; My Package [bazel]
;; ------------------
(require 'bazel) ;; load bazel package

;; format on save
(add-hook 'bazel-mode-hook
          (lambda()
            (add-hook 'before-save-hook #'bazel-buildifier nil t)))

;; ------------------------------------
;; My Package [flycheck-google-cpplint]
;; ------------------------------------
(require 'flycheck-google-cpplint) ;; try to load this package

;; see: https://github.com/kkholst/.doom.d/blob/main/config.org
(after! flycheck
  (setq flycheck-c/c++-googlelint-executable "cpplint"
        flycheck-cppcheck-standards '("c++17"))
  (flycheck-add-next-checker 'c/c++-cppcheck '(warning . c/c++-googlelint))
  (add-hook! 'lsp-after-initialize-hook
    (run-hooks (intern (format "%s-lsp-hook" major-mode))))
  (defun my-c++-linter-setup ()
    (flycheck-add-next-checker 'lsp 'c/c++-googlelint))
  (add-hook 'c++-mode-lsp-hook #'my-c++-linter-setup))

;; ------------------
;; Tweak key bindings
;; ------------------

;; Open elfeed
;; (map! :leader
;;       :desc "Open elfeed"
;;       "o e" #'elfeed)

;; Toggle time in modeline
(setq display-time-format "%Y-%m-%d-%a %H:%M")
(map! :leader
      :desc "Toggle time in modeline "
      "t t" #'display-time-mode)
