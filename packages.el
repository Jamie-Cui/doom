;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
(package! flycheck-google-cpplint)
(package! bazel)
(package! org-download) ;; moving images from point A to point B.
(package! simple-httpd) ;; deps for org-roam-ui, in emacs-web-server
(package! org-roam-ui) ;; use org-roam-ui
(package! protobuf-mode)
(package! meson-mode)
(package! plantuml-mode) ; for drawing diagrams!!!

(package! rime
  :recipe (:host github
           :repo "DogLooksGood/emacs-rime"
           :files ("*.el" "Makefile" "lib.c"))) ; pinyin

(package! xenops) ; Xenops is a LaTeX editing environment for mathematical
(package! keyfreq) ; analysing your key frequency!
(package! gptel)
(package! engrave-faces)
(package! copilot)

;; :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;; :recipe (:host github :repo "username/repo"
;; :files ("some-file.el" "src/lisp/*.el")))

;; (package! holo-layer :recipe (:host github
;;                               :repo "manateelazycat/holo-layer"
;;                               :files ("*")))

;; If you'd like to disable a package included with Doom, you cando so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")

;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED
;; will likely break things)
;; (unpin! t)

;; org-roam-ui tries to keep up with the latest features of org-roam
(unpin! org-roam)

;; HACK see: https://github.com/doomemacs/doomemacs/issues/8333#issuecomment-2765427952
(package! ws-butler
  :recipe (:host github
           :repo "emacsmirror/nongnu_elpa"
           :branch "elpa/ws-butler"
           :local-repo "ws-butler")
  :pin "9ee5a7657a22e836618813c2e2b64a548d27d2ff")


(package! gptel-quick
  :recipe (:host github
           :repo "karthink/gptel-quick"))

(package! edwina
  :recipe (:host github
           :repo "Jamie-Cui/edwina"))
