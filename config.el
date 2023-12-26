;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

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
(setq doom-theme 'wombat)

;; theme disable line-highlight foreground face
(set-face-foreground 'highlight nil)

;; setup my own paths
(defconst my-sync-root "~/Library/Mobile Documents/com~apple~CloudDocs/Sync/")
(defconst my-beorg-root "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/")

;; load some definition functions from my own elisp file
(load (concat doom-user-dir "define.el"))

;; HACK: load ht package
(add-to-list 'load-path (concat doom-local-dir "straight/repos/ht.el"))

;; setup interier shell (built-in with emacs) type
;; REVIEW not sure if this variable is used by tramp or not
(setq shell-file-name (executable-find "zsh")) ; emacs-c-code variable

;; Manipulate windows
(add-to-list 'initial-frame-alist '(fullscreen . maximized)) ;; Maximized screen on doom start
(add-to-list 'initial-frame-alist '(undecorated . t)) ;; no title bar

;; Paste and kill selected origin: https://emacs.stackexchange.com/a/15054
(fset 'evil-visual-update-x-selection 'ignore)

;; Fix chinese wrap
(setq word-wrap-by-category t)

;; Make Evil behaves more like vim
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))

;; Enable word count
(setq doom-modeline-enable-word-count t)

;; Don't ask, just quit
(setq confirm-kill-emacs nil)


;; ----------------------------------------------------------------------------
;; Configuration: org mode and citations
;; ----------------------------------------------------------------------------
(setq org-directory (concat my-beorg-root "org"))
(setq org-roam-directory (concat my-sync-root "roam"))
(setq deft-directory (concat my-sync-root "deft"))

(after! citar
  (add-to-list 'citar-notes-paths (concat my-sync-root "papers"))
  (add-to-list 'citar-bibliography (concat my-sync-root "zotero_all.bib")))

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

;; ----------------------------------------------------------------------------
;; Configuration: completion
;; ----------------------------------------------------------------------------
;; Only complete when I ask!
;; https://www.reddit.com/r/DoomEmacs/comments/wdxah3/how_to_stop_word_autocomplete/
(after! company
  (setq company-idle-delay nil))

;; ----------------------------------------------------------------------------
;; Configuration: undo
;; ----------------------------------------------------------------------------
(after! undo-tree
  (setq undo-tree-auto-save-history nil))

;; ----------------------------------------------------------------------------
;; Configuration: proxy
;; ----------------------------------------------------------------------------
;; (setq url-proxy-services
;;       '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
;;         ("http" . "127.0.0.1:8001")
;;         ("https" . "127.0.0.1:8001")))

;; ----------------------------------------------------------------------------
;; Configuration: lsp c++ format on save
;; ----------------------------------------------------------------------------
;; there are many workarounds:
;; 1. https://github.com/radian-software/apheleia/discussions/120
;; 2. https://github.com/doomemacs/doomemacs/issues/7490
;; but we want a workaround that works both on local projects and tramp projects
;; so the best way is to force c++/c mode to use eglot-format-buffer other than apheleia

;; setup local varaiables on c++-mode-hook
(setq-hook! 'c++-mode-hook
  apheleia-inhibit t
  +format-with nil) ;; do not format with apheleia

;; setup local varaiables on c-mode-hook
(setq-hook! 'c-mode-hook
  apheleia-inhibit t
  +format-with nil) ;; do not format with apheleia

(add-hook 'c++-mode-hook
          (lambda()
            (add-hook 'before-save-hook #'eglot-format-buffer)))

(add-hook 'c-mode-hook
          (lambda()
            (add-hook 'before-save-hook #'eglot-format-buffer)))

;; ----------------------------------------------------------------------------
;; Configuration: tramp, lsp, projectile, vterm
;; ----------------------------------------------------------------------------
;; it's wired that vertico uses this to list all files
(setq projectile-git-fd-args "--color=never -H -0 -E .git -tf --strip-cwd-prefix")
;; on ubuntu, you need to "ln -s /bin/fdfind /bin/fd"
(setq projectile-fd-executable "fd")

;; Config Tramp
(after! tramp
  ;; Setup default tramp setting, from https://www.emacswiki.org/emacs/TrampMode
  (setq tramp-default-method "sshx") ; use sshx (since it supports zsh and fish) instead of default scp
  (setq tramp-default-remote-shell "/bin/zsh") ; do-not-use executable-find
  (customize-set-variable 'tramp-encoding-shell "/bin/zsh") ; do-not-use executable-find
  )

;; Use zsh over vterm tramp
(after! vterm
  (setq vterm-shell (executable-find "zsh"))
  (setq vterm-tramp-shells '("sshx" "/bin/zsh")))

;; ----------------------------------------------------------------------------
;; My Package [latex-preview-pane]
;; ----------------------------------------------------------------------------
(require 'latex-preview-pane)

;; ----------------------------------------------------------------------------
;; My Package [bazel]
;; ----------------------------------------------------------------------------
(require 'bazel) ;; load bazel package

;; format on save
(add-hook 'bazel-mode-hook
          (lambda()
            (add-hook 'before-save-hook #'bazel-buildifier nil t)))

;; ----------------------------------------------------------------------------
;; My Package [flycheck-google-cpplint]
;; ----------------------------------------------------------------------------

;; see: https://github.com/kkholst/.doom.d/blob/main/config.org
(after! flycheck
  (require 'flycheck-google-cpplint) ;; try to load this package

  (setq flycheck-c/c++-googlelint-executable "cpplint"
        flycheck-cppcheck-standards '("c++17")))

;; ----------------------------------------------------------------------------
;; Elfeed Setup
;; ----------------------------------------------------------------------------
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

;; ----------------------------------------------------------------------------
;; Tweak Global key bindings
;; ----------------------------------------------------------------------------
(map! :leader
      :desc "Open elfeed" ;; Open elfeed
      "o e" #'elfeed)

(map! :leader
      :desc "Bazel actions" ;; Bazel run target
      "c b" #'bazel-run)
