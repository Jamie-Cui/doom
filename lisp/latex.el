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

;; set default viewers
;; NOTE +latex-viewers are used by modules/lang/latex/+views.el immediately after
;; latex module inited.
;; (setq-default +latex-viewers '(pdf-tools))

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

  ;; tex-mode           ; latexindent is broken
  ;; latex-mode
  ;; LaTeX-mode
  
  ;; (add-hook 'LaTeX-mode-hook 'TeX-source-correlare-mode)
  ;; (add-hook 'LaTeX-mode-hook  (lambda ()
  ;;                               (setq-local +format-with 'latexindent)))
  ;; (after! LaTeX-mode
  ;;   (set-formatter! 'latexindent '("latexindent -l") :modes '(LaTex-mode)))

  ;; disable offset
  (setq +latex-indent-item-continuation-offset 'nil)

  ;; set default tex-engine to xetex
  ;; (setq TeX-engine 'xetex)

  (after! adaptive-wrap
    (remove-hook 'LaTeX-mode-hook 'adaptive-wrap-prefix-mode))


  ;; use cdlatex's snippets
  (map! :after cdlatex
        :map cdlatex-mode-map
        :i "TAB" #'cdlatex-tab))


(defun +latex/format-buffer ()
  (interactive)
  (let ((this-buffer (current-buffer))
        (my-command "latexindent")
        (temp-buffer (generate-new-buffer " *latexindent-message*")))
    (apply
     #'call-process-region
     nil ; start
     nil ; end
     my-command ; program
     nil ; delete
     nil ; destination
     nil ; display
     `
     ("-wd -s -l"
      "$HOME/.config/doom/templates/latexindent.yaml"
      ,(buffer-file-name (this-buffer))) ; arguments
     )))


(defun  +latex/save-format-buffer-hook-for-this-buffer ()
  (add-hook 'before-save-hook
            (lambda ()
              (progn
                (+latex/format-buffer)
                ;; Continue to save.
                nil))
            nil
            ;; Buffer local hook.
            t))


;; Example for elisp, could be any mode though.
(add-hook 'LaTeX-mode-hook
          (lambda () (+latex/save-format-buffer-hook-for-this-buffer)))
