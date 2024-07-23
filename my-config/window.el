;;; window.el -*- lexical-binding: t; -*-

;; Manipulate windows
(setq scroll-step            1
      scroll-conservatively  10000
      next-screen-context-lines 5
      ;; move by logical lines rather than visual lines (better for macros)
      line-move-visual nil)

(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; Maximized screen on doom start
(add-to-list 'default-frame-alist '(undecorated . t)) ;; no title bar
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(horizontal-scroll-bars . nil))

(map! :leader
      :desc "Switch to next frame" ;; Switch between different emacs frames
      "F" #'+evil/next-frame)
