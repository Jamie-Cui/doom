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
  (setq tramp-default-method "sshx") ; use sshx (since it supportszsh and fish) instead of default scp
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

(use-package! bazel
  :config
  (add-to-list 'auto-mode-alist '("\\.BUILD\\'" . bazel-mode))
  (add-hook 'bazel-mode-hook
            (lambda()
              (add-hook 'before-save-hook #'bazel-buildifier nil t)))
  (setq-hook! 'bazel-mode-hook
    apheleia-inhibit t
    +format-with nil            ; do not format with apheleia
    +format-with-lsp nil)       ; do not format with lsp
  
  (defun bazel-refresh-compile-commands()
    "Refresh bazel project's compile_commmands.json"
    (interactive)
    (let ((workspace-file-additional-content
           "\
\n\
load(\"@bazel_tools//tools/build_defs/repo:git.bzl\", \"git_repository\")\n\
\n\
git_repository(\n\
        name = \"hedron_compile_commands\",\n\
        commit = \"388cc00156cbf53570c416d39875b15f03c0b47f\",\n\
        remote = \"https://github.com/hedronvision/bazel-compile-commands-extractor.git\",\n\
)\n\
\n\
load(\"@hedron_compile_commands//:workspace_setup.bzl\", \"hedron_compile_commands_setup\")\n\
\n\
hedron_compile_commands_setup()\n"
           )
          (build-file-additional-content-1 "load(\"@hedron_compile_commands//:refresh_compile_commands.bzl\", \"refresh_compile_commands\")\n")
          (build-file-additional-content-2 "\n\
refresh_compile_commands(\n\
    name = \"refresh_compile_commands\",\n\
    exclude_external_sources = True,\n\
    exclude_headers = \"external\",\n\
)\n")
          (build-file-name (concat (bazel--workspace-root buffer-file-name) "BUILD.bazel"))
          (build-file-name-bak (concat (bazel--workspace-root buffer-file-name) "BUILD.bazel.bak"))
          (workspace-file-name (concat (bazel--workspace-root buffer-file-name) "WORKSPACE"))
          (workspace-file-name-bak (concat (bazel--workspace-root buffer-file-name) "WORKSPACE.bak"))
          (default-directory  (bazel--workspace-root buffer-file-name))
          )
      (message "%s" workspace-file-additional-content)
      (message "%s" workspace-file-name) ; project workspace name
      (message "%s" workspace-file-name-bak) ; project workspace name

      ;; setup workspace file
      (with-temp-buffer
        (tramp-handle-insert-file-contents workspace-file-name) ; read workspace file
        (tramp-handle-write-region nil nil workspace-file-name-bak) ; write workspace file to backup
        (goto-char (point-max)) ; go-to the end of current buffer
        (insert workspace-file-additional-content) ; append contents to current buffer
        (tramp-handle-write-region nil nil workspace-file-name) ; write modified contents to workspace
        )
      
      (with-temp-buffer
        (tramp-handle-insert-file-contents build-file-name) ; read build file
        (tramp-handle-write-region nil nil build-file-name-bak) ; write workspace file to backup
        (insert build-file-additional-content-1) ; append contents to current buffer
        (goto-char (point-max)) ; go-to the end of current buffer
        (insert build-file-additional-content-2) ; append contents to current buffer
        (tramp-handle-write-region nil nil build-file-name) ; write modified contents to workspace
        )

      ;; run refresh_compile_commands
      (message "bazel run -s :refresh_compile_commands")
      (tramp-handle-shell-command "bazel run -s :refresh_compile_commands")
      (message "Refresh done. Start cleaning...")
      
      ;; copy-back workspace file
      (with-temp-buffer
        (tramp-handle-insert-file-contents workspace-file-name-bak) ; read workspace file
        (tramp-handle-write-region nil nil workspace-file-name) ; write back workspace
        )
      (tramp-sh-handle-delete-file workspace-file-name-bak) ; delete backup file
      (with-temp-buffer
        (tramp-handle-insert-file-contents build-file-name-bak) ; read workspace file
        (tramp-handle-write-region nil nil build-file-name) ; write back workspace
        )
      (tramp-sh-handle-delete-file build-file-name-bak) ; delete backup file
      )
    (message "Finished")
    ))

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

;; ----------------------------------------------------------------------------
;; Configuration: lsp
;; ----------------------------------------------------------------------------
;; ;; HACK from
;; ;; https://github.com/emacs-lsp/lsp-mode/issues/2709#issuecomment-1475039310
;; (defun lsp-tramp-connection-over-ssh-port-forwarding (command)
;;   "Like lsp-tcp-connection, but uses SSH portforwarding."
;;   (list
;;    :connect (lambda (filter sentinel name environment-fn _workspace)
;;               (let* ((host "localhost")
;;                      (lsp-port (lsp--find-available-port host (cl-incf lsp--tcp-port)))
;;                      (command (with-parsed-tramp-file-name buffer-file-name nil
;;                                 (message "[tcp/ssh hack] running LSP %s on %s / %s" command host localname)
;;                                 (let* ((unix-socket (format "/tmp/lsp-ssh-portforward-%s.sock" lsp-port))
;;                                        (command (list
;;                                                  "ssh"
;;                                                  ;; "-vvv"
;;                                                  "-L" (format "%s:%s" lsp-port unix-socket)
;;                                                  host
;;                                                  "socat"
;;                                                  (format "unix-listen:%s" unix-socket)
;;                                                  (format "system:'\"cd %s && %s\"'" (file-name-directory localname) command)
;;                                                  )))
;;                                   (message "using local command %s" command)
;;                                   command)))
;;                      (final-command (if (consp command) command (list command)))
;;                      (_ (unless (executable-find (cl-first final-command))
;;                           (user-error (format "Couldn't find executable %s" (cl-first final-command)))))
;;                      (process-environment
;;                       (lsp--compute-process-environment environment-fn))
;;                      (proc (make-process :name name :connection-type 'pipe :coding 'no-conversion
;;                                          :command final-command :sentinel sentinel :stderr (format "*%s::stderr*" name) :noquery t))
;;                      (tcp-proc (progn
;;                                  (sleep-for 1) ; prevent a connection before SSH has run socat. Ugh.
;;                                  (lsp--open-network-stream host lsp-port (concat name "::tcp")))))
;;                 (set-process-query-on-exit-flag proc nil)
;;                 (set-process-query-on-exit-flag tcp-proc nil)
;;                 (set-process-filter tcp-proc filter)
;;                 (cons tcp-proc proc)))
;;    :test? (lambda () t)))


;; ;; HACK from
;; ;; https://github.com/radian-software/apheleia/discussions/120
;; (defun apheleia-lsp-formatter-buffer (buffer scratch)
;;   (with-current-buffer buffer
;;     (if (lsp-feature? "textDocument/formatting")
;;         (let ((edits (lsp-request
;;                       "textDocument/formatting"
;;                       (lsp--make-document-formatting-params))))
;;           (unless (seq-empty-p edits)
;;             (with-current-buffer scratch
;;               (lsp--apply-text-edits edits 'format)))))))
;; (cl-defun apheleia-lsp-formatter
;;     (&key buffer scratch formatter callback &allow-other-keys)
;;   (apheleia-lsp-formatter-buffer buffer scratch)
;;   (funcall callback))
