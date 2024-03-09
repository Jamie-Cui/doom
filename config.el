;;; config.el -*- lexical-binding: t; -*-

;; HACK: load ht package
(add-to-list 'load-path (concat doom-local-dir "straight/repos/ht.el"))

;; You may want to install the following open-source apps:
;; * terminal:          https://alacritty.org/                  $HOME/.alacritty.toml, see configs in https://alacritty.org/config-alacritty.html
;; * pdf:               https://skim-app.sourceforge.io/
;; * bib:               https://www.zotero.org/
;; * ohz:               ~sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"~

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
;; (setq doom-theme 'doom-one) ; default is always good
;; (setq doom-theme 'wombat) ; I like wombat for now
;; (setq doom-theme 'doom-zenburn) ; code looks better in this theme
;; (setq doom-theme 'doom-acario-light) ; org looks better in this theme
(setq doom-theme 'modus-vivendi) ; code looks better in this theme

;; setup default font
(setq doom-font (font-spec :family "Monaco" :size 14 :weight 'medium))

;; setup my own paths
(defconst org-remote-path "~/Library/Mobile Documents/com~apple~CloudDocs/org-remote/")
(defconst org-local-path "~/org-local/")
(setq use-remote-path nil) ; non-nil to use remote path

;; Don't ask, just quit
(setq confirm-kill-emacs nil)

;; ----------------------------------------------------------------------------
;; Configuration: proxy
;; ----------------------------------------------------------------------------
;; (setq url-proxy-services
;;       '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
;;         ("http" . "127.0.0.1:8001")
;;         ("https" . "127.0.0.1:8001")))

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
          "https://www.kb.aert.org/vulfeed/"))
  (set-evil-initial-state! 'elfeed-search-mode 'normal))

;; elfeed local key bindings
(map! :after elfeed
      :localleader
      :map elfeed-search-mode-map
      :desc "Update feeds"
      "m" #'elfeed-update)

(map! :leader
      :desc "Open elfeed" ;; Open elfeed
      "o e" #'elfeed)

;; ----------------------------------------------------------------------------
;; Load my own tweaks
;; ----------------------------------------------------------------------------
(load (concat doom-user-dir "tweaks/window.el"))
(load (concat doom-user-dir "tweaks/editor.el"))
(load (concat doom-user-dir "tweaks/org.el"))
(load (concat doom-user-dir "tweaks/cppdev.el"))
(load (concat doom-user-dir "tweaks/latex.el"))

