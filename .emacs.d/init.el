;;
;; .emacs
;;

(blink-cursor-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(set-scroll-bar-mode nil)
;; no beep
(setq visible-bell t)
(setq ring-bell-function 'ignore)
;; no backup
(setq make-backup-files nil)
(setq auto-save-default nil)
;; tab
(setq-default tab-width 4
              indent-tabs-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq cursor-in-non-selected-windows nil)
(column-number-mode t)
;; always split horizontally
(setq split-height-threshold 0)
(setq split-width-threshold 0)

;; path
(setq exec-path (cons (concat (getenv "HOME") "/.pyenv/shims") exec-path))

;; frame size
(add-to-list 'default-frame-alist '(left . -1))
(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist (cons 'height (if (= (display-pixel-height) 1080) 73 60)))
(add-to-list 'default-frame-alist (cons 'width  (if (= (display-pixel-height) 1080) 170 130)))

;; key binding
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(define-key global-map [?¥] [?\\])
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-m" 'newline-and-indent)

(require 'cl)
(defun ignore-error-wrapper (fn)
  "Funtion return new function that ignore errors.
   The function wraps a function with `ignore-errors' macro."
  (lexical-let ((fn fn))
    (lambda ()
      (interactive)
      (ignore-errors
        (funcall fn)))))

;; window move
(global-set-key (kbd "C-c h") (ignore-error-wrapper 'windmove-left))
(global-set-key (kbd "C-c l") (ignore-error-wrapper 'windmove-right))
(global-set-key (kbd "C-c k") (ignore-error-wrapper 'windmove-up))
(global-set-key (kbd "C-c j") (ignore-error-wrapper 'windmove-down))

;; font
(when (eq window-system 'ns)
  (set-face-attribute 'default nil :family "Migu 1M" :height 120)
  (setq line-spacing 0))

;; color
(load-theme 'deeper-blue t)

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

;;
;; cask
;;

;; curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;;
;; helm
;;
(when (require 'helm-config nil t)
  (helm-mode 1)

  (define-key global-map (kbd "M-x")     'helm-M-x)
  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map (kbd "C-x C-r") 'helm-recentf)
  (define-key global-map (kbd "M-y")     'helm-show-kill-ring)
  (define-key global-map (kbd "C-c i")   'helm-imenu)
  (define-key global-map (kbd "C-x b")   'helm-buffers-list)

  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

  ;; no prompt for new file
  (setq helm-ff-newfile-prompt-p nil)

  ;; Disable helm in some functions
  (add-to-list 'helm-completing-read-handlers-alist '(find-alternate-file . nil))

  ;; Emulate `kill-line' in helm minibuffer
  (setq helm-delete-minibuffer-contents-from-point t)
  (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
    "Emulate `kill-line' in helm minibuffer"
    (kill-new (buffer-substring (point) (field-end))))

  (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
    "Execute command only if CANDIDATE exists"
    (when (file-exists-p candidate)
      ad-do-it))

  (defadvice helm-ff-transform-fname-for-completion (around my-transform activate)
    "Transform the pattern to reflect my intention"
    (let* ((pattern (ad-get-arg 0))
           (input-pattern (file-name-nondirectory pattern))
           (dirname (file-name-directory pattern)))
      (setq input-pattern (replace-regexp-in-string "\\." "\\\\." input-pattern))
      (setq ad-return-value
            (concat dirname
                    (if (string-match "^\\^" input-pattern)
                        ;; '^' is a pattern for basename
                        ;; and not required because the directory name is prepended
                        (substring input-pattern 1)
                      (concat ".*" input-pattern)))))))

;;
;; elscreen
;;
(setq elscreen-prefix-key (kbd "C-z"))
(elscreen-start)
(setq elscreen-tab-display-kill-screen nil)
(setq elscreen-tab-display-control nil)
(setq elscreen-buffer-to-nickname-alist
      '(("^dired-mode$" .
         (lambda ()
           (format "Dired(%s)" dired-directory)))
        ("^Info-mode$" .
         (lambda ()
           (format "Info(%s)" (file-name-nondirectory Info-current-file))))
        ("^mew-draft-mode$" .
         (lambda ()
           (format "Mew(%s)" (buffer-name (current-buffer)))))
        ("^mew-" . "Mew")
        ("^irchat-" . "IRChat")
        ("^liece-" . "Liece")
        ("^lookup-" . "Lookup")))
(setq elscreen-mode-to-nickname-alist
      '(("[Ss]hell" . "shell")
        ("compilation" . "compile")
        ("-telnet" . "telnet")
        ("dict" . "OnlineDict")
        ("*WL:Message*" . "Wanderlust")))


;;
;; pbcopy
;;
(when nil (eq system-type 'darwin)
  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))

  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)


;;
;; programming
;;

(yas-global-mode t)
(require 'auto-complete-config)
(ac-config-default)

;; flymake
;;(setq flymake-gui-warnings-enabled nil)
;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;;
;; python
;;

;; pip install autopep8
;; pip install rope jedi ; auto complete
;; pip install pyflakes  ; flymake

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

;;
;; Lisp
;;

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; dired
(put 'dired-find-alternate-file 'disabled nil)
