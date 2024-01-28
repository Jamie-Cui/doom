;;; org.el -*- lexical-binding: t; -*-

;; ---------------------------------------------------------------------------- 
;; Configuration: org mode and citations
;; ----------------------------------------------------------------------------

(if use-sync-path
    (progn
      (setq org-roam-directory (concat org-remote-path "roam"))
      (setq org-directory (concat org-remote-path "org"))
      (setq deft-directory (concat org-remote-path "deft"))
      )
  (progn
    (setq org-roam-directory (concat org-local-path "roam"))
    (setq org-directory (concat org-local-path "org"))
    (setq deft-directory (concat org-local-path "deft"))
    )
  )

;; Re-configure deft-mode keybindings
(after! deft
  ;; start with evil normal mode
  (set-evil-initial-state! 'deft-mode 'normal))

(after! citar
  (add-to-list 'citar-notes-paths (concat org-remote-path "papers"))
  (add-to-list 'citar-bibliography (concat org-remote-path "zotero_all.bib")))

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
;; My Package [latex-preview-pane]
;; ----------------------------------------------------------------------------
(require 'latex-preview-pane)

