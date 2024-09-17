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
(setq +latex-viewers '(skim))

(after! tex
  ;; use remote bib
  ;; (setq reftex-default-bibliography (concat org-remote-path "zotero_all.bib"))

  ;; do not format with apheleia
  (setq-hook! 'LaTeX-mode-hook
    apheleia-inhibit t
    +format-with nil) ;; do not format with apheleia

  (map! :after latex
        :localleader
        :map LaTeX-mode-map
        :desc "Run Tex-Clean"             "c" #'TeX-clean
        :desc "Run TeX-master-file-ask"   "m" #'TeX-master-file-ask
        :desc "Set tex engine"            "e" #'TeX-engine-set)

  ;; Set to `auto' for continuation lines to be offset by `LaTeX-indent-line':
  ;;   \\item lines aligned
  ;;     like this, assuming `LaTeX-indent-line' == 2
  (setq +latex-indent-item-continuation-offset 'auto)

  ;; set default tex-engine to xetex
  (setq TeX-engine 'xetex)
  )

;; (after! tex-mode
;;   (set-company-backend! 'company-yasnippet))

;; enable yasnippet in tex-mode
(add-hook! 'tex-mode
  (lambda ()
    (set (make-local-variable 'company-backends)
         '((company-dabbrev-code company-yasnippet)))))

;; enable evil on pdf-view-mode
(evil-set-initial-state 'pdf-view-mode 'normal)


