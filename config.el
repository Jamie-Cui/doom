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
(setq doom-theme 'modus-vivendi)
;; (setq doom-theme 'wombat)

;; theme disable line-highlight foreground face
;; (set-face-attribute 'highlight nil :foreground 'nil)

;; do-not truncate lines by default
(set-default 'truncate-lines nil)

;; setup my own paths
(defconst my-sync-root "~/Library/Mobile Documents/com~apple~CloudDocs/Sync/")
(defconst my-beorg-root "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/")
(defconst my-home-root "~/")

;; load some definition functions from my own elisp file
(load (concat doom-user-dir "tweaks/define.el"))

;; configure to use external apps to open pdf, see https://emacs.stackexchange.com/questions/3105/how-to-use-an-external-program-as-the-default-way-to-open-pdfs-from-emacs
;; (load (concat doom-user-dir "openwith.el")) ;; I'm not using this

;; HACK: load ht package
(add-to-list 'load-path (concat doom-local-dir "straight/repos/ht.el"))

;; setup interier shell (built-in with emacs) type
;; REVIEW not sure if this variable is used by tramp or not
(setq explicit-shell-file-name (executable-find "zsh")) ; emacs-c-code variable

;; Manipulate windows
(setq scroll-step            1
      scroll-conservatively  10000
      next-screen-context-lines 5
      ;; move by logical lines rather than visual lines (better for macros)
      line-move-visual nil)
(add-to-list 'initial-frame-alist '(fullscreen . maximized)) ;; Maximized screen on doom start
(add-to-list 'initial-frame-alist '(undecorated . t)) ;; no title bar
(add-to-list 'initial-frame-alist '(tool-bar-lines . 0))
(add-to-list 'initial-frame-alist '(menu-bar-lines . 0))
(add-to-list 'initial-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'initial-frame-alist '(horizontal-scroll-bars . nil))

;; Don't ask, just quit
(setq confirm-kill-emacs nil)

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

;; Plan A: 中文苹方, 英文Roboto Mono
(setq doom-font (font-spec :family "Monaco" :size 14)
      doom-serif-font doom-font
      doom-symbol-font (font-spec :family "Hei")
      doom-variable-pitch-font (font-spec :family "Hei" :weight 'extra-bold))

;; 如果不把这玩意设置为 nil, 会默认去用 fontset-default 来展示, 配置无效
(setq use-default-font-for-symbols nil)

;; Doom 的字体加载顺序问题, 如果不设定这个 hook, 配置会被覆盖失效
(add-hook! 'after-setting-font-hook
  (set-fontset-font t 'latin (font-spec :family "Monaco"))
  (set-fontset-font t 'symbol (font-spec :family "Monaco"))
  (set-fontset-font t 'mathematical (font-spec :family "Monaco"))
  (set-fontset-font t 'emoji (font-spec :family "Monaco")))


;; ----------------------------------------------------------------------------
;; Configuration: org mode and citations
;; ----------------------------------------------------------------------------
(setq org-roam-directory (concat my-sync-root "roam"))
(setq org-directory (concat my-home-root "org")) ; local
(setq deft-directory (concat my-home-root "deft")) ; local

;; Re-configure deft-mode keybindings
(after! deft
  ;; start with evil normal mode
  (set-evil-initial-state! 'deft-mode 'normal)
  (map! :map deft-mode-map
        :localleader
        "RET" #'deft-new-file
        "a"   #'deft-archive-file
        "c"   #'deft-filter-clear
        "d"   #'deft-delete-file
        "f"   #'deft-find-file
        "g"   #'deft-refresh
        "l"   #'deft-filter
        "n"   #'deft-new-file
        "r"   #'deft-rename-file
        "s"   #'deft-toggle-sort-method
        "t"   #'deft-toggle-incremental-search))

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

;; use local mode on most cases
(setq apheleia-remote-algorithm 'remote)

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

;; disable eglot inlay
(setq eglot-ignored-server-capabilities '(:inlayHintProvider))

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
  (connection-local-update-profile-variables 'tramp-connection-local-default-shell-profile
                                             '((shell-file-name . "/bin/zsh")
                                               (shell-command-switch . "-c")))
  )

;; Use zsh over vterm tramp
(after! (:and vterm tramp)
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

;; bind *.BUILD file extension with bazel-mode
(add-to-list 'auto-mode-alist '("\\.BUILD\\'" . bazel-mode))

;; format on save
(add-hook 'bazel-mode-hook
          (lambda()
            (add-hook 'before-save-hook #'bazel-buildifier nil t)))

;; ----------------------------------------------------------------------------
;; My Package [flycheck-google-cpplint]
;; ----------------------------------------------------------------------------

;; see: https://github.com/kkholst/.doom.d/blob/main/config.org
(after! flycheck-eglot
  ;; We need to tweak a little bit to make cpplint and eglot to work together.
  ;; see: https://melpa.org/#/flycheck-eglot
  ;; 
  ;; By default, the Flycheck-Eglot considers the Eglot to be the only provider
  ;; of syntax checks.  Other Flycheck checkers are ignored.
  ;; There is a variable `flycheck-eglot-exclusive' that controls this.
  ;; You can override it system wide or for some major modes.
  (require 'flycheck-google-cpplint) ; try to load this package
  (setq! flycheck-eglot-exclusive nil)
  (flycheck-add-next-checker 'eglot-check
                             '(warning . c/c++-googlelint))
  (setq! flycheck-c/c++-googlelint-executable "cpplint" 
         flycheck-cppcheck-standards '("c++17"))
  )


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
      :localleader
      :map elfeed-search-mode-map
      :desc "Update feeds"
      "m" #'elfeed-update)

;; ----------------------------------------------------------------------------
;; Tweak Global key bindings
;; ----------------------------------------------------------------------------

(map! :leader
      :desc "Open elfeed" ;; Open elfeed
      "o e" #'elfeed)

(map! :localleader
      :map (c++-mode-map c-mode-map)
      :desc "Switch *.cpp/*.h" ;; find the header or source file corresponding to this file
      "m" #'ff-find-other-file)


(map! :localleader
      :map (c++-mode-map c-mode-map bazel-mode-map java-mode-map)
      :desc "Bazel build" ;; Bazel build target
      "b" #'bazel-build)

(map! :localleader
      :map (c++-mode-map c-mode-map bazel-mode-map java-mode-map)
      :desc "Bazel run" ;; Bazel run target
      "r" #'bazel-run)

(map! :localleader
      :map (c++-mode-map c-mode-map bazel-mode-map java-mode-map)
      :desc "Bazel test" ;; Bazel test target
      "t" #'bazel-test)
