;;; latex.el -*- lexical-binding: t; -*-
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

;; Set default viewers. +latex-viewers are used by modules/lang/latex/+views.el
;; immediately after latex module inited.
;;

(after! tex
  ;; Disable offset
  (setq +latex-indent-item-continuation-offset 'nil)

  ;; Set default tex-engine to xetex
  ;; (setq TeX-engine 'xetex)

  ;; Use adaptive-wrap for LaTex-mode
  (after! adaptive-wrap
    (remove-hook 'LaTeX-mode-hook 'adaptive-wrap-prefix-mode))

  ;; Use cdlatex's snippets
  (map! :after cdlatex
        :map cdlatex-mode-map
        :i "TAB" #'cdlatex-tab)

  ;; Setup format on save for Tex-mode
  (defun +latex/format-buffer ()
    (interactive)
    (apply
     #'call-process-region nil nil
     "latexindent -wd -s -l $HOME/.config/doom/templates/latexindent.yaml"))

  (defun +latex/save-format-buffer-hook-for-this-buffer ()
    (add-hook 'before-save-hook
              (lambda () (progn (+latex/format-buffer) nil)) nil t))

  (add-hook 'LaTeX-mode-hook
            (lambda () (+latex/save-format-buffer-hook-for-this-buffer)))
  )

;; Use citar for citations when converting org to latex
(after! citar
  (add-to-list 'org-cite-export-processors
               '(latex-citar (processor . citar-export-latex))))

(after! (:and ox-latex org)
  ;; HACK you need to manually set the following for each org file
  ;; #+latex: \maketitle
  (setq! org-latex-title-command 'nil)
  (setq! org-export-in-background 'nil)

  (add-to-list 'org-latex-classes
               '("acmart"
                 ;; HACK see: https://tex.stackexchange.com/a/587033
                 " \\documentclass[]{acmart}\\let\\Bbbk\\relax"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  )

(setq-default +latex-viewers '(pdf-tools))
