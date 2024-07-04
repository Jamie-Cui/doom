;;; $DOOMDIR/define.el -*- lexical-binding: t; -*-

(require 'bazel) ; load bazel package

;; defining a new function to refresh compile commands
(defun bazel-refresh-compile-commands()
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
