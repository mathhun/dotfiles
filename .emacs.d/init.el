;;
;; .emacs
;;

(blink-cursor-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
;; no beep
(setq visible-bell t)
(setq ring-bell-function 'ignore)
;; no backup
(setq make-backup-files nil)
(setq aut-save-default nil)
;; tab
(setq-default tab-width 4
          indent-tabs-mode nil)
;; path
(setq exec-path (cons (concat (getenv "HOME") "/.pyenv/shims") exec-path))

;; frame size
(add-to-list 'default-frame-alist '(left . -1))
(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 130))

;; key binding
(global-set-key "\C-h" 'delete-backward-char)
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; C-w
(defun kill-region-or-backward-word ()
  "If the region is active and non-empty, call `kill-region'. Otherwise, call `backward-kill-word'."
  (interactive)
  (call-interactively
   (if (use-region-p) 'kill-region 'backward-kill-word)))
(global-set-key "\C-w" 'kill-region-or-backward-word)

;; color
(load-theme 'manoj-dark t)

;; cursor
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)

;; dialog
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

;; cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;;
;; programming
;;

(yas-global-mode t)
(require 'auto-complete-config)
(ac-config-default)

;; flymake
(setq flymake-gui-warnings-enabled nil)

;; python
(add-hook 'python-mode-hook
      (lambda ()
        (define-key python-mode-map (kbd "\C-m") 'newline-and-indent)
        (define-key python-mode-map (kbd "RET") 'newline-and-indent)))

(add-hook 'before-save-hook 'py-autopep8-before-save)

(require 'tramp-cmds)
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    ;; Make sure it's not a remote buffer or flymake would not work
    (when (not (subsetp (list (current-buffer)) (tramp-list-remote-buffers)))
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list "pyflakes" (list local-file)))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'python-mode-hook
          (lambda ()
            (flymake-mode t)))

;; dired
(put 'dired-find-alternate-file 'disabled nil)
