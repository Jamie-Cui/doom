;;; editor.el -*- lexical-binding: t; -*-

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


