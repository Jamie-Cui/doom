;;; window.el -*- lexical-binding: t; -*-

;; Manipulate windows
(setq scroll-step            1
      scroll-conservatively  10000
      next-screen-context-lines 5
      ;; move by logical lines rather than visual lines (better for macros)
      line-move-visual nil)
;;

;; see: https://github.com/manateelazycat/holo-layer to get started
;; #+begin_src sh
;; pip3 install epc sexpdata six pynput inflect pyobjc PyQt6 PyQt6-Qt6 PyQt6-sip
;; #+end_src
;; (use-package! holo-layer
;;   :config
;;   (setq holo-layer-enable-cursor-animation t
;;         holo-layer-enable-place-info t
;;         holo-layer-cursor-alpha 100
;;         holo-layer-cursor-animation-interval 25
;;         holo-layer-cursor-animation-type "jelly easing"
;;         holo-layer-python-command "/usr/bin/python3") ; setup python binary path
;;   (holo-layer-enable))

(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; Maximized screen on doom start
(add-to-list 'default-frame-alist '(undecorated . t)) ;; no title bar
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(horizontal-scroll-bars . nil))

(map! :leader
      :desc "Switch to next frame" ;; Switch between different emacs frames
      "F" #'+evil/next-frame)
