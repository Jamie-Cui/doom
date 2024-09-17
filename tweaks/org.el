;; Copyright (C) 2024 Jamie Cui
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'org-download) ;; drag-and-drop for images
(require 'xenops) ;; better org latex math
(require 'org-roam-ui) ;; ui for roam
(require 'simple-httpd) ;; org-roam-ui dependency

;; ---------------------------------------------------------------------------- 
;; Configuration: org mode and citations
;; ----------------------------------------------------------------------------
(if jamie-use-remote-path
    (progn
      (setq deft-directory (concat jamie-org-remote-path "deft"))
      (setq org-directory (concat jamie-org-remote-path "org")))
  (progn
    (setq deft-directory (concat jamie-org-local-path "deft"))
    (setq org-directory (concat jamie-org-local-path "org"))))

;; always use the remote path for org-roam
(setq org-roam-directory (concat jamie-org-remote-path "roam"))

;; ----------------------------------------------------------------------------
;; Configuration: note taking
;; ----------------------------------------------------------------------------

;; for macos only: add texbin to the system path
(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin/"))
(setq exec-path (append exec-path '("/Library/TeX/texbin/")))

;; Re-configure deft-mode keybindings
(after! deft
  ;; start with evil normal mode
  (set-evil-initial-state! 'deft-mode 'normal)
  (setq! deft-strip-summary-regexp ".*$")
  (setq! deft-current-sort-method 'title)
  )

(after! citar
  ;; put paper notes in roam folder
  (add-to-list 'citar-notes-paths (concat jamie-org-remote-path "roam"))
  (add-to-list 'citar-bibliography (concat jamie-org-remote-path "zotero_all.bib")))

;; Setup org-latex-preview, load cryptocode, and scale the generated math imgs
(after! org
  (add-to-list 'org-latex-packages-alist '("lambda, advantage, operators, sets, adversary, landau, probability, notions, logic, ff, mm, primitives, events, complexity, oracles, asymptotics, keys" "cryptocode" t))
  (setq org-startup-folded 'content)
  (setq org-startup-with-inline-images t)
  (setq org-startup-numerated t) ; startup with org-num-mode
  (setq org-num-max-level 2)  ; add numering for all titles

  ;; ----------------------------
  ;; use xenops to preview, it's simply better
  ;; ----------------------------
  (add-hook 'org-mode-hook #'xenops-mode)
  (setq xenops-math-image-current-scale-factor 1.2)
  (setq xenops-math-image-margin 0))

;; configure org-roam-ui
(after! org-roam-ui
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; Setup org-download directory
(after! org-download
  (setq-default org-download-image-dir "img") ; see: https://www.emacswiki.org/emacs/BufferLocalVariable
  (setq-default org-download-heading-lvl nil) ; no headings
  (setq org-download-method 'directory)
  (setq org-download-image-org-width 500)
  (setq org-download-link-format "[[file:%s]]\n"
        org-download-abbreviate-filename-function #'file-relative-name)
  (setq org-download-link-format-function #'org-download-link-format-function-default))

;; ----------------------------------------------------------------------------
;; Configuration: org agenda and calendar
;; ----------------------------------------------------------------------------
(after! org-agenda
  (evil-set-initial-state 'org-agenda-mode 'normal)
  (setq org-agenda-span 'week)
  (setq org-agenda-start-on-weekday 1)
  (setq org-deadline-warning-days 365)
  (setq org-agenda-start-day "+0d"))

(setq calendar-week-start-day 1) ; start with monday
