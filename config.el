;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; -------------------------------------------------------
;; For new devices, you only needs to modify the following
;; -------------------------------------------------------
(setq user-full-name "Jamie Cui"
      user-mail-address "jamie.cui@outlook.com")

;; setup theme
(setq doom-theme 'leuven)

;; setup my own paths
(defconst my-sync-root "~/Library/Mobile Documents/com~apple~CloudDocs/Sync/")
(defconst my-beorg-root "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/")

;; Manipulate windows
(add-to-list 'initial-frame-alist '(fullscreen . maximized)) ;; Maximized screen on doom start
(add-to-list 'default-frame-alist '(undecorated . t)) ;; no title bar

;; load some definition functions from my own elisp file
(load (concat doom-user-dir "define.el"))

;; HACK: load ht package
(add-to-list 'load-path (concat doom-local-dir "straight/repos/ht.el"))

;; Don't ask, just quit
;; (setq confirm-kill-emacs nil)

;; -----------------------
;; Configuration: org mode
;; -----------------------
(setq org-directory (concat my-beorg-root "org"))
(setq org-roam-directory (concat my-sync-root "roam"))
(setq deft-directory (concat my-sync-root "deft"))

;; Setup org-latex-preview, load cryptocode, and scale the generated math imgs
(after! org
  (add-to-list 'org-latex-packages-alist '("lambda, advantage, operators, sets, adversary, landau, probability, notions, logic, ff, mm, primitives, events, complexity, oracles, asymptotics, keys" "cryptocode" t))
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 0.95))
  (setq org-startup-with-latex-preview t) ;; startup with latex review
  (setq org-startup-folded 'content)
  (setq org-startup-with-inline-images t)
  (setq org-return-follows-link t) ;; return in org now follows link (globally)
  (require 'org-download) ;; drag-and-drop for images
  )

;; Setup org-download directory
(after! org-download
  (setq org-download-method 'directory)
  (setq-default org-download-image-dir "img") ;; see: https://www.emacswiki.org/emacs/BufferLocalVariable
  (setq org-download-image-org-width 500)
  (setq org-download-link-format "[[file:%s]]\n"
        org-download-abbreviate-filename-function #'file-relative-name)
  (setq org-download-link-format-function #'org-download-link-format-function-default))

;; setup org-agenda key binding
(after! org-agenda
  (define-key org-agenda-mode-map "j" 'evil-next-line)
  (define-key org-agenda-mode-map "k" 'evil-previous-line)
  (keymap-set org-agenda-mode-map "RET" 'org-agenda-show-and-scroll-up)
  (keymap-set org-agenda-mode-map "SPC" nil))

;; configure org-roam-uo
(after! org-roam
  (add-to-list 'load-path (concat doom-local-dir "straight/repos/org-roam-ui")) ;; manually load package
  (add-to-list 'load-path (concat doom-local-dir "straight/repos/emacs-web-server")) ;; manually load package
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

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
;; see: https://github.com/radian-software/apheleia/discussions/120
;; (add-to-list 'apheleia-formatters '(apheleia-lsp . apheleia-lsp-formatter))
;; (setf (alist-get 'elixir-mode apheleia-mode-alist)
;;       '(apheleia-lsp))
;; (setf (alist-get 'python-mode apheleia-mode-alist)
;;       '(apheleia-lsp))

;; a work-around from: https://github.com/doomemacs/doomemacs/issues/7490
(setq-hook! 'c++-mode-hook
  apheleia-inhibit t
  +format-with nil) ;; do not format with apheleia
(add-hook 'c++-mode-hook
          (lambda()
            (add-hook 'before-save-hook #'eglot-format-buffer)))

;; -------------------------------------
;; Configuration: tramp, lsp, projectile
;; -------------------------------------
;; it's wired that vertico uses this to list all files
(setq projectile-git-fd-args "--color=never -H -0 -E .git -tf --strip-cwd-prefix")
(setq projectile-fd-executable "fd") ;; on ubuntu, "ln -s /bin/fdfind /bin/fd"

;; Config Tramp
(after! tramp
  ;; Setup default tramp setting, from https://www.emacswiki.org/emacs/TrampMode
  (setq tramp-default-method "sshx") ;; use sshx (since it supports zsh) instead of default scp
  (customize-set-variable 'tramp-encoding-shell "/bin/zsh")
  (customize-set-variable 'tramp-default-remote-shell "/bin/zsh")
  )
;; Configure lsp over tramp
;; (after! (:and lsp-mode tramp)
;;   ;; Setup lsp over tramp
;;   (lsp-register-client
;;    (make-lsp-client
;;     :new-connection
;;     (lsp-tramp-connection-over-ssh-port-forwarding "clangd --header-insertion=never")
;;     :major-modes '(c-mode c++-mode)
;;     :remote? t
;;     :server-id 'clangd-remote)))

;; Use zsh over vterm tramp
(after! vterm
  (setq vterm-tramp-shells '(("sshx" "/bin/zsh"))))

;; -------------------------------
;; My Package [latex-preview-pane]
;; -------------------------------
(require 'latex-preview-pane)

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

;; ------------
;; Elfeed Setup
;; ------------
(after! elfeed
  (setq elfeed-feeds
        '("https://eprint.iacr.org/rss/rss.xml"
          "https://export.arxiv.org/rss/cs.CR"
          "https://blog.cryptographyengineering.com/feed"
          "https://decoded.avast.io/feed"
          "https://aws.amazon.com/blogs/security/feed"
          "https://newsletter.blockthreat.io/feed"
          "https://www.kb.cert.org/vulfeed/")))

;; elfeed local key bindings
(map! :after elfeed
      :map elfeed-search-mode-map
      :localleader

      :desc "Update feeds"
      "m" #'elfeed-update)

;; -------------------------
;; Tweak Global key bindings
;; -------------------------
(map! :leader
      :desc "Open elfeed" ;; Open elfeed
      "o e" #'elfeed)

(map! :leader
      :desc "Bazel run" ;; Bazel run target
      "c b" #'bazel-run)


(define-minor-mode my-override-mode
  "Overrides all major and minor mode keys" t)

(defvar my-override-map (make-sparse-keymap "my-override-map")
  "Override all major and minor mode keys")

(add-to-list 'emulation-mode-map-alists
             `((my-override-mode . ,my-override-map)))

(define-key my-override-map (kbd "<left>")
            (lambda ()
              (interactive)
              (message "Use Vim keys: h for Left")))

(define-key my-override-map (kbd "<right>")
            (lambda ()
              (interactive)
              (message "Use Vim keys: l for Right")))

(define-key my-override-map (kbd "<up>")
            (lambda ()
              (interactive)
              (message "Use Vim keys: k for Up")))

(define-key my-override-map (kbd "<down>")
            (lambda ()
              (interactive)
              (message "Use Vim keys: j for Down")))

(evil-make-intercept-map my-override-map)
