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

;; enable evil on pdf-view-mode
(evil-set-initial-state 'pdf-view-mode 'normal)

(after! tex
  ;; use remote bib
  ;; (setq reftex-default-bibliography (concat org-remote-path "zotero_all.bib"))

  ;; add key bindings
  (map! :localleader
        :map LaTeX-mode-map
        :desc ""                          "a" #'nil
        :desc "Run all"                   "r" #'TeX-command-run-all
        :desc "Run Tex-Clean"             "d" #'TeX-clean
        :desc "Run TeX-master-file-ask"   "M" #'TeX-master-file-ask
        :desc "Set tex engine"            "R" #'TeX-engine-set)

  ;; add custom engine
  ;; (add-to-list
  ;;  'TeX-engine-alist
  ;;  '(default-with-synctex "Default with shell escape"
  ;;    "pdftex --synctex=1"
  ;;    "pdflatex --synctex=1"
  ;;    ConTeXt-engine))

  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

  ;; disable offset
  (setq +latex-indent-item-continuation-offset 'nil)

  ;; set default tex-engine to xetex
  ;; (setq TeX-engine 'xetex)

  ;; set default viewers
  (setq-default +latex-viewers '(pdf-tools))

  ;; use cdlatex's snippets
  (map! :after cdlatex
        :map cdlatex-mode-map
        :i "TAB" #'cdlatex-tab))
