;;; editor.el -*- lexical-binding: t; -*-

;; enable evil on pdf-view-mode
(evil-set-initial-state 'pdf-view-mode 'normal)
(setq +latex-viewers '(skim))

(after! tex
  (map! :localleader
        :map (LaTeX-mode-map)
        :desc "Run TeX-master-file-ask" ;; ask for the master file of latex project
        "t" #'TeX-master-file-ask)

  ;; use remote bib for default
  (setq reftex-default-bibliography (concat org-remote-path "zotero_all.bib"))

  ;; do not format with apheleia
  (setq-hook! 'LaTeX-mode-hook
    apheleia-inhibit t
    +format-with nil) ;; do not format with apheleia


  ;; Set to `auto' for continuation lines to be offset by `LaTeX-indent-line':
  ;;   \\item lines aligned
  ;;     like this, assuming `LaTeX-indent-line' == 2
  (setq +latex-indent-item-continuation-offset 'auto)
  )


;; enable evil on pdf-view-mode
(evil-set-initial-state 'pdf-view-mode 'normal)
