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

(use-package! xenops)
(use-package! org-download)
(use-package! simple-httpd)
(use-package! org-roam-ui)

;; ---------------------------------------------------------------------------- 
;; Configuration: org mode and citations
;; ----------------------------------------------------------------------------

(defun +org/get-org-directory ()
  "Get the org-directory"
  (if jamie-use-remote-path (concat jamie-org-remote-path "org")
    (concat jamie-org-local-path "org")))

(defun +org/get-deft-directory ()
  "Get the deft-directory"
  (if jamie-use-remote-path (concat jamie-org-remote-path "deft")
    (concat jamie-org-local-path "deft")))

(defun +org/get-roam-directory ()
  "Get the org-roam-directory"
  (if jamie-use-remote-path (concat jamie-org-remote-path "roam")
    (concat jamie-org-local-path "roam")))

(defun +org/get-zotero-path ()
  "Get the org-roam-directory"
  (if jamie-use-remote-path (concat jamie-org-remote-path "zotero_all.bib")
    (concat jamie-org-local-path "zotero_all.bib")))

;; NOTE same as org directory
;; (defun +org/get-gtd-directory ()
;;   "Get the org-gtd-directory"
;;   (if jamie-use-remote-path (concat jamie-org-remote-path "org")
;;     (concat jamie-org-local-path "org")))

(setq deft-directory (+org/get-deft-directory))
(setq org-directory (+org/get-org-directory))
(setq org-roam-directory (+org/get-roam-directory))
(setq org-agenda-files (directory-files-recursively (+org/get-org-directory) "\\.org$"))
(setq org-attach-id-dir (concat (+org/get-org-directory) ".attach"))

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
  (setq! deft-current-sort-method 'mtime))

(after! citar
  ;; put paper notes in roam folder
  (add-to-list 'citar-notes-paths org-roam-directory)
  (add-to-list 'citar-bibliography (+org/get-zotero-path)))

;; Setup org-latex-preview, load cryptocode, and scale the generated math imgs
(after! org
  (add-to-list 'org-latex-packages-alist '("lambda, advantage, operators, sets, adversary, landau, probability, notions, logic, ff, mm, primitives, events, complexity, oracles, asymptotics, keys" "cryptocode" t))

  ;; ----------------------------
  ;; use xenops to preview, it's simply better
  ;; ----------------------------
  (add-hook 'org-mode-hook #'xenops-mode)
  (setq xenops-math-image-current-scale-factor 1.2)
  (setq xenops-math-image-margin 0))

;; ----------------------------
;; Add Plantuml
;; ----------------------------
(after! (:and plantuml-mode org)
  (setq! plantuml-default-exec-mode 'executable)
  (setq! org-plantuml-jar-path (concat doom-user-dir "bin/plantuml.jar"))
  (setq! plantuml-jar-path (concat doom-user-dir "bin/plantuml.jar"))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages
   'org-babel-load-languages
   ;; this line activates plantuml
   '((plantuml . t))))

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

;; HACK error from xenops with org>9.7
;; https://github.com/syl20bnr/spacemacs/issues/16577
;; https://github.com/dandavison/xenops/pull/74/files
;; https://github.com/dandavison/xenops/issues/73
(after! xenops
  (defun fn/xenops-src-parse-at-point ()
    (-if-let* ((element (xenops-parse-element-at-point 'src))
               (org-babel-info
                (xenops-src-do-in-org-mode
                 (org-babel-get-src-block-info 'light (org-element-context)))))
        (xenops-util-plist-update
         element
         :type 'src
         :language (nth 0 org-babel-info)
         :org-babel-info org-babel-info)))

  (advice-add 'xenops-src-parse-at-point :override 'fn/xenops-src-parse-at-point))

;; ----------------------------------------------------------------------------
;; Configuration: org agenda, journal
;; ----------------------------------------------------------------------------

(setq! org-agenda-files (list
                         (expand-file-name
                          (concat (+org/get-org-directory) "/todo.org"))))

(after! org
  (setq org-startup-folded 'content)
  (setq org-startup-with-inline-images t)
  (setq org-startup-numerated t) ; startup with org-num-mode
  (setq org-num-max-level 2)  ; add numering for all titles
  (setq org-log-done 't))

(after! org-agenda
  ;; Archive in the current file, under the top-level headline
  ;; \"* Archived Tasks\".
  (setq! org-archive-location "::* Archived Tasks")
  (evil-set-initial-state 'org-agenda-mode 'normal)
  (setq org-agenda-span 'month)
  (setq org-agenda-start-on-weekday 1)
  (setq org-deadline-warning-days 365)
  (setq org-agenda-start-day "+0d")
  (setq org-agenda-skip-additional-timestamps-same-entry 't)
  (setq org-agenda-custom-commands
        '(("n" "Agenda and all TODOs"
           ((agenda "")
            (alltodo "")))
          ("d" "Yesteday Review"
           ((agenda "" ((org-agenda-span 'day)))
            (tags "TODO=\"DONE\"&CLOSED>=\"<-1d>\""
                  ((org-agenda-overriding-header "Review Finished Task: Yesteday and Today")))
            ))
          ("w" "Weekly Review"
           ((agenda "" ((org-agenda-span 'week)))
            (tags "TODO=\"DONE\"&CLOSED>=\"<-1w>\""
                  ((org-agenda-overriding-header "Review Finished Task: This Week")))
            ))
          ("m" "Monthly Review"
           ((agenda "" ((org-agenda-span 'month)))
            (tags "TODO=\"DONE\"&CLOSED>=\"<-1m>\""
                  ((org-agenda-overriding-header "Review Finished Task: This Month")))
            ))
          ("y" "Year Review"
           ((agenda "" ((org-agenda-span 'week)))
            (tags "TODO=\"DONE\""
                  ((org-agenda-overriding-header "Review Finished Task: This Year")))
            ))
          ))
  (setq! org-capture-templates
         '(("t" "Todo" entry
            (file+headline +org-capture-todo-file "Todo")
            "* [ ] %?\n%i\n%a" :prepend t)
           ("p" "Project" entry
            (file+olp +org-capture-todo-file "Project" "Unsorted")
            "* [ ] %?\n%i\n%a" :prepend t)
           ;; ("n" "Personal notes" entry
           ;;  (file+headline +org-capture-notes-file "Inbox")
           ;;  "* %u %?\n%i\n%a" :prepend t)
           ;; ("j" "Journal" entry
           ;;  (file+olp+datetree +org-capture-journal-file)
           ;;  "* %U %?\n%i\n%a" :prepend t)
           ;; ("p" "Templates for projects")
           ;; ("pt" "Project-local todo" entry
           ;;  (file+headline +org-capture-project-todo-file "Inbox")
           ;;  "* TODO %?\n%i\n%a" :prepend t)
           ;; ("pn" "Project-local notes" entry
           ;;  (file+headline +org-capture-project-notes-file "Inbox")
           ;;  "* %U %?\n%i\n%a" :prepend t)
           ;; ("pc" "Project-local changelog" entry
           ;;  (file+headline +org-capture-project-changelog-file "Unreleased")
           ;;  "* %U %?\n%i\n%a" :prepend t)
           ;; ("o" "Centralized templates for projects")
           ;; ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
           ;; ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
           ;; ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t)
           )
         )
  (evil-define-key 'normal org-agenda-mode-map (kbd "q") #'org-agenda-quit)
  (evil-define-key 'normal org-agenda-mode-map (kbd "Q") #'org-agenda-Quit)
  )

(after! calendar
  ;; start with monday
  (setq calendar-week-start-day 1))


;; ----------------------------------------------------------------------------
;; Configuration: org journal
;; ----------------------------------------------------------------------------

(after! org-journal
  (setq! org-journal-follow-mode 't)
  (setq! org-journal-file-type 'monthly)
  (setq! org-journal-carryover-items "TODO=\"IDEA\""))

(after! org
  (map! :leader
        :desc "Org Yank Stored Link"       "n y" #'org-store-link
        :desc "Org Export to Clipboard"    "n e" #'+org/export-to-clipboard
        :desc "Org Export to Clipboard"    "n E" #'+org/export-to-clipboard-as-rich-text
        :desc "Org Paste Stored Link"      "n p" #'org-insert-link)
  )
