;;; cppdev.el -*- lexical-binding: t; -*-

;; ----------------------------------------------------------------------------
;; Configuration: lsp c++ format on save
;; ----------------------------------------------------------------------------
;; there are many workarounds:
;; 1. https://github.com/radian-software/apheleia/discussions/120
;; 2. https://github.com/doomemacs/doomemacs/issues/7490
;; but we want a workaround that works both on local projects and tramp projects
;; so the best way is to force c++/c mode to use eglot-format-buffer other than apheleia

;; use local mode on most cases
(setq apheleia-remote-algorithm 'remote)

;; setup local varaiables on c++-mode-hook
(setq-hook! 'c++-mode-hook
  apheleia-inhibit t
  +format-with nil) ;; do not format with apheleia

;; setup local varaiables on c-mode-hook
(setq-hook! 'c-mode-hook
  apheleia-inhibit t
  +format-with nil) ;; do not format with apheleia

(add-hook 'c++-mode-hook
          (lambda()
            (add-hook 'before-save-hook #'eglot-format-buffer)))

(add-hook 'c-mode-hook
          (lambda()
            (add-hook 'before-save-hook #'eglot-format-buffer)))

;; disable eglot inlay
(setq eglot-ignored-server-capabilities '(:inlayHintProvider))

;; ----------------------------------------------------------------------------
;; Configuration: tramp, lsp, projectile, vterm
;; ----------------------------------------------------------------------------
;; it's wired that vertico uses this to list all files
(setq projectile-git-fd-args "--color=never -H -0 -E .git -tf --strip-cwd-prefix")
;; on ubuntu, you need to "ln -s /bin/fdfind /bin/fd"
(setq projectile-fd-executable "fd")

;; Config Tramp
(after! tramp
  ;; Setup default tramp setting, from https://www.emacswiki.org/emacs/TrampMode
  (setq tramp-default-method "sshx") ; use sshx (since it supports zsh and fish) instead of default scp
  (setq tramp-default-remote-shell "/bin/zsh") ; do-not-use executable-find
  (customize-set-variable 'tramp-encoding-shell "/bin/zsh") ; do-not-use executable-find
  (connection-local-update-profile-variables 'tramp-connection-local-default-shell-profile
                                             '((shell-file-name . "/bin/zsh")
                                               (shell-command-switch . "-c")))
  )

;; Use zsh over vterm tramp
(after! (:and vterm tramp)
  (setq vterm-shell (executable-find "zsh"))
  (setq vterm-tramp-shells '("sshx" "/bin/zsh")))

;; ----------------------------------------------------------------------------
;; My Package [bazel]
;; ----------------------------------------------------------------------------
(require 'bazel) ;; load bazel package

;; bind *.BUILD file extension with bazel-mode
(add-to-list 'auto-mode-alist '("\\.BUILD\\'" . bazel-mode))

;; format on save
(add-hook 'bazel-mode-hook
          (lambda()
            (add-hook 'before-save-hook #'bazel-buildifier nil t)))

;; ----------------------------------------------------------------------------
;; My Package [flycheck-google-cpplint]
;; ----------------------------------------------------------------------------

;; see: https://github.com/kkholst/.doom.d/blob/main/config.org
(after! flycheck-eglot
  ;; We need to tweak a little bit to make cpplint and eglot to work together.
  ;; see: https://melpa.org/#/flycheck-eglot
  ;; 
  ;; By default, the Flycheck-Eglot considers the Eglot to be theonly provider
  ;; of syntax checks.  Other Flycheck checkers are ignored.
  ;; There is a variable `flycheck-eglot-exclusive' that controls this.
  ;; You can override it system wide or for some major modes.
  (require 'flycheck-google-cpplint) ; try to load this package
  (setq! flycheck-eglot-exclusive nil)
  (flycheck-add-next-checker 'eglot-check
                             '(warning . c/c++-googlelint))
  (setq! flycheck-c/c++-googlelint-executable "cpplint" 
         flycheck-cppcheck-standards '("c++17"))
  )

(map! :localleader
      :map (c++-mode-map c-mode-map)
      :desc "Switch *.cpp/*.h" ;; find the header or source file corresponding to this file
      "m" #'ff-find-other-file)


(map! :localleader
      :map (c++-mode-map c-mode-map bazel-mode-map java-mode-map)
      :desc "Bazel build" ;; Bazel build target
      "b" #'bazel-build)

(map! :localleader
      :map (c++-mode-map c-mode-map bazel-mode-map java-mode-map)
      :desc "Bazel run" ;; Bazel run target
      "r" #'bazel-run)

(map! :localleader
      :map (c++-mode-map c-mode-map bazel-mode-map java-mode-map)
      :desc "Bazel test" ;; Bazel test target
      "t" #'bazel-test)

;; ----------------------------------------------------------------------------
;; Configuration: completion
;; ----------------------------------------------------------------------------
;; Only complete when I ask!
;; https://www.reddit.com/r/DoomEmacs/comments/wdxah3/how_to_stop_word_autocomplete/
(after! company
  (setq company-idle-delay nil))
