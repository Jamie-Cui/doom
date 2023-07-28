;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Jamie Cui"
      user-mail-address "jamie.cui@outlook.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;     doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
;; (setq doom-font (font-spec :family "Fira Code" :size 14 :slant 'normal :weight 'normal))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-monokai-ristretto)
(setq doom-theme 'doom-peacock)

;; maximized screen on doom start
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "/Users/shanzhu.cjm/Desktop/org")

(remove-hook 'org-mode-hook #'auto-fill-mode) ;; disable auto-fill in org mode
(setq org-return-follows-link 't)
(setq +org-capture-journal-file "journal.org.gpg") ;; encrypt with gpg
(setq +org-capture-notes-file "notes.org.gpg") ;; encrypt with gpg
(setq +org-capture-todo-file "notes.org.gpg") ;; encrypt with gpg
(after! epa-file
  (epa-file-enable)
)


;; config undo with +tree
(after! undo-tree
  (setq undo-tree-auto-save-history nil))


;; Setup proxies for emacs
;; (setq url-proxy-services
;; '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
;; ("http" . "127.0.0.1:8001")
;; ("https" . "127.0.0.1:8001")))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;

;; Setup default tramp setting, from https://www.emacswiki.org/emacs/TrampMode
(setq tramp-default-method "sshx") ;; use sshx (since it supports zsh) instead of default scp


;; hacks from
;; https://github.com/emacs-lsp/lsp-mode/issues/2709#issuecomment-1475039310
(defun lsp-tramp-connection-over-ssh-port-forwarding (command)
  "Like lsp-tcp-connection, but uses SSH portforwarding."
  (list
   :connect (lambda (filter sentinel name environment-fn _workspace)
              (let* ((host "localhost")
                     (lsp-port (lsp--find-available-port host (cl-incf lsp--tcp-port)))
                     (command (with-parsed-tramp-file-name buffer-file-name nil
                                (message "[tcp/ssh hack] running LSP %s on %s / %s" command host localname)
                                (let* ((unix-socket (format "/tmp/lsp-ssh-portforward-%s.sock" lsp-port))
                                       (command (list
                                                 "ssh"
                                                 ;; "-vvv"
                                                 "-L" (format "%s:%s" lsp-port unix-socket)
                                                 host
                                                 "socat"
                                                 (format "unix-listen:%s" unix-socket)
                                                 (format "system:'\"cd %s && %s\"'" (file-name-directory localname) command)
                                                 )))
                                  (message "using local command %s" command)
                                  command)))
                     (final-command (if (consp command) command (list command)))
                     (_ (unless (executable-find (cl-first final-command))
                          (user-error (format "Couldn't find executable %s" (cl-first final-command)))))
                     (process-environment
                      (lsp--compute-process-environment environment-fn))
                     (proc (make-process :name name :connection-type 'pipe :coding 'no-conversion
                                         :command final-command :sentinel sentinel :stderr (format "*%s::stderr*" name) :noquery t))
                     (tcp-proc (progn
                                 (sleep-for 1) ; prevent a connection before SSH has run socat. Ugh.
                                 (lsp--open-network-stream host lsp-port (concat name "::tcp")))))

                ;; TODO: Same :noquery issue (see above)
                (set-process-query-on-exit-flag proc nil)
                (set-process-query-on-exit-flag tcp-proc nil)
                (set-process-filter tcp-proc filter)
                (cons tcp-proc proc)))
   :test? (lambda () t)))


(after! tramp
  (when (require 'lsp-mode nil t)

    (setq lsp-enable-snippet nil
          lsp-log-io nil
          ;; To bypass the "lsp--document-highlight fails if
          ;; textDocument/documentHighlight is not supported" error
          lsp-enable-symbol-highlighting nil)

    (lsp-register-client
     (make-lsp-client
      :new-connection
      (lsp-tramp-connection-over-ssh-port-forwarding "clangd --header-insertion=never")
      :major-modes '(c-mode c++-mode)
      :remote? t
      :server-id 'clangd-remote))))


;; for cpplint
;; see: https://github.com/kkholst/.doom.d/blob/main/config.org
(after! flycheck
  (require 'flycheck-google-cpplint)
  (setq flycheck-c/c++-googlelint-executable "cpplint"
        flycheck-cppcheck-standards '("c++11"))
  (flycheck-add-next-checker 'c/c++-cppcheck '(warning . c/c++-googlelint))
  (add-hook! 'lsp-after-initialize-hook
    (run-hooks (intern (format "%s-lsp-hook" major-mode))))
  (defun my-c++-linter-setup ()
    (flycheck-add-next-checker 'lsp 'c/c++-googlelint))
  (add-hook 'c++-mode-lsp-hook #'my-c++-linter-setup))


(after! lsp-mode
  (setq lsp-log-io nil
        lsp-file-watch-threshold 4000
        lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-icons-enable nil
        lsp-headerline-breadcrumb-segments '(file symbols)
        lsp-imenu-index-symbol-kinds '(File Module Namespace Package Class Method Enum Interface
                                            Function Variable Constant Struct Event Operator TypeParameter)
        )
  (dolist (dir '("[/\\\\]\\.cache\\'"
                 "[/\\\\]bazel-bin\\'"
                 "[/\\\\]bazel-code\\'"
                 "[/\\\\]bazel-genfiles\\'"
                 "[/\\\\]bazel-out\\'"
                 "[/\\\\]bazel-yacl\\'"
                 "[/\\\\]bazel-spu\\'"
                 "[/\\\\]bazel-ppu\\'"
                 "[/\\\\]bazel-testlogs\\'"
                 "[/\\\\]third_party\\'"
                 "[/\\\\]external\\'"
                 "[/\\\\]build\\'"
                 ))
    (push dir lsp-file-watch-ignored-directories))
  )
