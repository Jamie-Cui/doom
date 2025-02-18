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
                                               (shell-command-switch . "-c"))))
;; Use zsh over vterm tramp
(after! (:and vterm tramp)
  (setq vterm-shell (executable-find "zsh"))
  (setq vterm-tramp-shells '("sshx" "/bin/zsh")))
;; setup interier shell (built-in with emacs) type
;; REVIEW not sure if this variable is used by tramp or not
(setq explicit-shell-file-name (executable-find "zsh")) ; emacs-c-code variable


;; ----------------------------------------------------------------------------
;; Configuration: lsp c++ format on save
;; ----------------------------------------------------------------------------
;; there are many workarounds:
;; 1. https://github.com/radian-software/apheleia/discussions/120
;; 2. https://github.com/doomemacs/doomemacs/issues/7490
;; but we want a workaround that works both on local projects and tramp projects
;; so the best way is to force c++/c mode to use eglot-format-buffer other than apheleia

(after! apheleia-formatters
  ;; use local mode on most cases
  (setq apheleia-remote-algorithm 'local))

;; when we have eglot managed         
(after! eglot
  ;; disable eglot inlay
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider)))

;; ----------------------------------------------------------------------------
;; My Package [bazel]
;; ----------------------------------------------------------------------------
(use-package! bazel
  :config
  (add-to-list 'auto-mode-alist '("\\.BUILD\\'" . bazel-mode))
  (setq! bazel-buildifier-command (concat doom-private-dir "bin/buildifier"))
  (setq! bazel-buildifier-before-save 't)
  (map! :localleader
        :map bazel-mode-map
        :desc "Bazel build"       "b" #'bazel-build
        :desc "Bazel run"         "r" #'bazel-run
        :desc "Bazel test"        "t" #'bazel-test
        :desc "Bazel comile current file"        "m" #'bazel-compile-current-file))


;; ----------------------------------------------------------------------------
;; My Package [flycheck-google-cpplint]
;; ----------------------------------------------------------------------------

(after! flycheck-eglot
  (use-package! flycheck-google-cpplint
    :config
    ;; see: https://github.com/kkholst/.doom.d/blob/main/config.org
    ;; We need to tweak a little bit to make cpplint and eglot to work together.
    ;; see: https://melpa.org/#/flycheck-eglot
    ;;
    ;; By default, the Flycheck-Eglot considers the Eglot to be theonly provider
    ;; of syntax checks.  Other Flycheck checkers are ignored.
    ;; There is a variable `flycheck-eglot-exclusive' that controls this.
    ;; You can override it system wide or for some major modes.

    (setq! flycheck-eglot-exclusive nil)
    (flycheck-add-next-checker 'eglot-check
                               '(warning . c/c++-googlelint))
    (customize-variable
     '(flycheck-c/c++-googlelint-executable "cpplint")
     '(flycheck-cppcheck-standards "c++17")
     '(flycheck-googlelint-linelength  80)
     '(flycheck-googlelint-filter  "-whitespace,-whitespace/braces")
     ))
  )

;; ----------------------------------------------------------------------------
;; Magit
;; ----------------------------------------------------------------------------
;; use vim key bindings in magit-status-mode
;; (evil-set-initial-state 'magit-status-mode 'normal)
;; (evil-set-initial-state 'magit-diff-mode 'normal)
;; (evil-set-initial-state 'magit-revision-mode 'normal)

;; see: https://github.com/pythonic-emacs/anaconda-mode#faq
(setq anaconda-mode-localhost-address "localhost")


;; defining a new function to refresh compile commands
(defun +bazel/refresh-compile-commands()
  "Refresh bazel project's compile_commmands.json"
  (interactive)
  (let (
        ;; defining the useful variables
        (workspace-file-additional-content
         "\
\n\
load(\"@bazel_tools//tools/build_defs/repo:git.bzl\", \"git_repository\")\n\
\n\
git_repository(\n\
        name = \"hedron_compile_commands\",\n\
        commit = \"4f28899228fb3ad0126897876f147ca15026151e\",\n\
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
        (exit-cmd "mv WORKSPACE.bak WORKSPACE; mv BUILD.bazel.bak BUILD.bazel")
        (exit-rm-build-cmd "mv WORKSPACE.bak WORKSPACE; mv BUILD.bazel.bak BUILD.bazel")
        (bazel-run-cmd "bazel run -s :refresh_compile_commands;"))
    (unless (bazel--workspace-root buffer-file-name) (error "Invalid bazel workspace, please check your current buffer!"))

    (if (file-remote-p default-directory)
        (progn
          ;; setup workspace file
          (with-temp-buffer
            (message (concat "Workspace file found at: " workspace-file-name))
            (tramp-handle-insert-file-contents workspace-file-name) ; read workspace file
            (tramp-handle-write-region nil nil workspace-file-name-bak) ; write workspace file to backup
            (goto-char (point-max)) ; go-to the end of current buffer
            (insert workspace-file-additional-content) ; append contents to current buffer
            (tramp-handle-write-region nil nil workspace-file-name))

          ;; setup build file
          (with-temp-buffer
            (if (tramp-handle-file-exists-p build-file-name)
                (progn
                  (message (concat "Build file found at" build-file-name))
                  (tramp-handle-insert-file-contents build-file-name) ; read build file
                  (tramp-handle-write-region nil nil build-file-name-bak) ; write workspace file to backup
                  (insert build-file-additional-content-1) ; append contents to current buffer
                  (goto-char (point-max)) ; go-to the end of current buffer
                  (insert build-file-additional-content-2) ; append contents to current buffer
                  (tramp-handle-write-region nil nil build-file-name))
              (progn
                (message "Cannot find build file, continue gracefully without build file")
                (insert build-file-additional-content-1) ; append contents to current buffer
                (goto-char (point-max)) ; go-to the end of current buffer
                (insert build-file-additional-content-2) ; append contents to current buffer
                (tramp-handle-write-region nil nil build-file-name))))

          ;; run refresh_compile_commands
          (message "Running command (*over tramp*): \"bazel run -s :refresh_compile_commands &\"")
          (if (tramp-handle-file-exists-p build-file-name)
              (let ((exec-cmd (concat bazel-run-cmd exit-cmd "&")))
                (tramp-handle-shell-command exec-cmd))
            (let ((exec-cmd (concat bazel-run-cmd exit-rm-build-cmd "&")))
              (tramp-handle-shell-command exec-cmd))))
      (progn
        ;; setup workspace file
        (with-temp-buffer
          (message (concat "Workspace file found at: " workspace-file-name))
          (insert-file-contents workspace-file-name) ; read workspace file
          (write-region nil nil workspace-file-name-bak) ; write workspace file to backup
          (goto-char (point-max)) ; go-to the end of current buffer
          (insert workspace-file-additional-content) ; append contents to current buffer
          (write-region nil nil workspace-file-name))

        ;; setup build file
        (with-temp-buffer
          (if (file-exists-p build-file-name)
              (progn
                (message (concat "Build file found at" build-file-name))
                (insert-file-contents build-file-name) ; read build file
                (write-region nil nil build-file-name-bak) ; write workspace file to backup
                (insert build-file-additional-content-1) ; append contents to current buffer
                (goto-char (point-max)) ; go-to the end of current buffer
                (insert build-file-additional-content-2) ; append contents to current buffer
                (write-region nil nil build-file-name))
            (progn
              (message "Cannot find build file, continue gracefully without build file")
              (insert build-file-additional-content-1) ; append contents to current buffer
              (goto-char (point-max)) ; go-to the end of current buffer
              (insert build-file-additional-content-2) ; append contents to current buffer
              (write-region nil nil build-file-name))))

        ;; run refresh_compile_commands
        (message "Running command (*locally*): \"bazel run -s :refresh_compile_commands &\"")
        (if (file-exists-p build-file-name)
            (let ((exec-cmd (concat bazel-run-cmd exit-cmd "&")))
              (shell-command exec-cmd))
          (let ((exec-cmd (concat bazel-run-cmd exit-rm-build-cmd "&")))
            (shell-command exec-cmd)))))

    (message "Finished")))
