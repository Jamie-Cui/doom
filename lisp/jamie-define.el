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

(defvar jamie-use-remote-path 't
  "Whether to use the remote path. Set this var to non-nil to use remote path")
(defconst jamie-org-remote-path
  "~/Library/Mobile Documents/com~apple~CloudDocs/org-remote/")
(defconst jamie-org-local-path "~/org-local/")

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
