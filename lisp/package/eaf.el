;;; eaf.el -*- lexical-binding: t; -*-
;;
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

(use-package! eaf
  :load-path (lambda()(concat doom-user-dir "thirdparty/eaf"))
  :init
  :custom
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser) ;; Make EAF Browser my default browser
  :config
  (defalias 'browse-web #'eaf-open-browser)

  ;; (require 'eaf-file-manager)
  ;; (require 'eaf-music-player)
  ;; (require 'eaf-image-viewer)
  ;; (require 'eaf-camera)
  ;; (require 'eaf-demo)
  ;; (require 'eaf-airshare)
  ;; (require 'eaf-terminal)
  ;; (require 'eaf-markdown-previewer)
  ;; (require 'eaf-video-player)
  ;; (require 'eaf-vue-demo)
  ;; (require 'eaf-file-sender)
  ;; (require 'eaf-pdf-viewer)
  ;; (require 'eaf-mindmap)
  ;; (require 'eaf-netease-cloud-music)
  ;; (require 'eaf-jupyter)
  ;; (require 'eaf-org-previewer)
  ;; (require 'eaf-system-monitor)
  ;; (require 'eaf-rss-reader)
  ;; (require 'eaf-file-browser)
  (require 'eaf-browser)
  ;; (require 'eaf-org)
  ;; (require 'eaf-mail)
  ;; (require 'eaf-git)
  (when (display-graphic-p)
    (require 'eaf-all-the-icons))

  (require 'eaf-evil)
  (define-key key-translation-map (kbd "SPC")
              (lambda (prompt)
                (if (derived-mode-p 'eaf-mode)
                    (pcase eaf--buffer-app-name
                      ("browser" (if  (string= (eaf-call-sync "call_function" eaf--buffer-id "is_focus") "True")
                                     (kbd "SPC")
                                   (kbd eaf-evil-leader-key)))
                      ("pdf-viewer" (kbd eaf-evil-leader-key))
                      ("image-viewer" (kbd eaf-evil-leader-key))
                      (_  (kbd "SPC")))
                  (kbd "SPC")))))
