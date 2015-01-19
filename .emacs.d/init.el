;;
;; .emacs
;;

(blink-cursor-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
;; no beep
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; key binding
(global-set-key "\C-h" 'delete-backward-char)
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; color
(load-theme 'manoj-dark t)

;; cursor
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)

;; cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)
