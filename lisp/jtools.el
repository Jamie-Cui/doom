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

(require 'doom-lib)

(defvar jtools-bin-dowload-alist
  '(
    ("buildifier" . "https://github.com/bazelbuild/buildtools/releases/download/v6.3.2/buildifier-darwin-arm64")
    ("clangd.zip" . "https://github.com/clangd/clangd/releases/download/18.1.3/clangd-mac-18.1.3.zip")
    ("plantuml.jar" . "https://github.com/plantuml/plantuml/releases/download/v1.2024.7/plantuml-1.2024.7.jar")
    )
  "Alist of (KEY . FUNCTION) pairs"
  )

(defun jtools-download-bin-async ()
  "Download all dependencies into the bin directory, a.k.a. $HOME/.config/doom"
  (interactive)
  (let
      ((jtools-bin-dir (expand-file-name (concat (file-name-directory (symbol-name 'jtools-download-bin-async)) "../bin/"))))
    (print! (start (concat "Detected bin-dir at: " jtools-bin-dir)))
    (print-group!
      (dolist (p jtools-bin-dowload-alist)
        (print! (start (concat "Downloading " (car p) " from "  (cdr p) " ... (async) ")))
        (async-shell-command (concat "wget -c " (cdr p) " -O " jtools-bin-dir (car p)))
        ))))

;; (url-copy-file "https://github.com/bazelbuild/buildtools/releases/download/v6.4.0/buildifier-darwin-amd64" (concat private-doom-bin-dir "buildifier") t)

;; (message "Downloading clangd ... ")
;; (url-copy-file "https://github.com/clangd/clangd/releases/download/17.0.3/clangd-mac-17.0.3.zip" (concat private-doom-bin-dir "clangd.zip") t)

;; (shell-command "cd ../bin && unzip -o -q clangd.zip && mv clangd_17.0.3/bin/clangd . && rm clangd.zip && rm -rf clangd_17.0.3")
;; (shell-command "cd ../bin && chmod +x buildifier")

;; (with-temp-buffer
;;   (insert-file-contents "~/.zshrc") ; read zshrc
;;   (goto-char (point-max)) ; go-to the end of current buffer
;;   (insert (concat "export PATH=$PATH:" private-doom-bin-dir)) ; append contents to current buffer
;;   (write-region nil nil "~/.zshrc")))

