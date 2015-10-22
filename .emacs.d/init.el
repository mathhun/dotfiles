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
;; help command
(global-set-key (kbd "C-c h") 'help-command)

;; font
(when (eq window-system 'ns)
  ;;(set-face-attribute 'default nil :family "Migu 1M" :height 120)
  ;;(setq line-spacing 0)

  (set-face-attribute 'default nil :family "Source Han Code JP" :height 100)
  )

;; color
(load-theme 'deeper-blue t)

;; cursor
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)

;; emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))

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

;; PATH
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

;;
;; recentf
;;

(require 'recentf)
(setq recentf-max-saved-items 1000)

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
;; Auto clear up your huge buffer list every 2 hours
;; http://emacswiki.org/emacs/KillingBuffers#toc12
;;

;; midnight mode
(require 'midnight)

;; kill buffers if they were last disabled more than this seconds ago
(setq clean-buffer-list-delay-special 900)

(defvar clean-buffer-list-timer nil
  "Stores clean-buffer-list timer if there is one. You can disable clean-buffer-list by (cancel-timer clean-buffer-list-timer).")

;; run clean-buffer-list every 2 hours
(setq clean-buffer-list-timer (run-at-time t 7200 'clean-buffer-list))

;; kill everything, clean-buffer-list is very intelligent at not killing
;; unsaved buffer.
(setq clean-buffer-list-kill-regexps '("^.*$"))

;; keep these buffer untouched
;; prevent append multiple times
(defvar clean-buffer-list-kill-never-buffer-names-init
  clean-buffer-list-kill-never-buffer-names
  "Init value for clean-buffer-list-kill-never-buffer-names")
(setq clean-buffer-list-kill-never-buffer-names
      (append
       '("*Messages*" "*cmd*" "*scratch*" "*w3m*" "*w3m-cache*" "*Inferior Octave*")
       clean-buffer-list-kill-never-buffer-names-init))

;; prevent append multiple times
(defvar clean-buffer-list-kill-never-regexps-init
  clean-buffer-list-kill-never-regexps
  "Init value for clean-buffer-list-kill-never-regexps")
;; append to *-init instead of itself
(setq clean-buffer-list-kill-never-regexps
      (append '("^\\*EMMS Playlist\\*.*$")
	      clean-buffer-list-kill-never-regexps-init))

;;
;; programming
;;

(yas-global-mode t)
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(global-set-key (kbd "C-c TAB") 'auto-complete)
(setq ac-use-menu-map t)

;; flymake
(require 'flymake)
;;(setq flymake-gui-warnings-enabled nil)

;; flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(mode-enabled save idle-change))
(setq flycheck-idle-change-delay 2)

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

;; etags
(require 'helm-etags+)

(when (require 'swoop)
  (global-set-key (kbd "M-o")   'swoop)
  (global-set-key (kbd "C-M-o") 'swoop-multi)
  ;; (global-set-key (kbd "M-o")   'swoop-pcre-regexp)
  (global-set-key (kbd "C-S-o") 'swoop-back-to-last-position)

  (define-key isearch-mode-map (kbd "M-o") 'swoop-from-isearch)
  (define-key swoop-map (kbd "M-o") 'swoop-multi-from-swoop)

  (setq swoop-font-size-change: nil))

;; imenu
(semantic-mode 1)
(add-hook 'python-mode-hook
          (lambda ()
            (setq imenu-create-index-function 'python-imenu-create-index)))

;; quickrun
(quickrun-add-command "python" '((:command . "python")) :override t)

;;
;; Golang
;;

;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u code.google.com/p/rog-go/exp/cmd/godef
;; go get -u github.com/nsf/gocode
(require 'go-mode-autoloads)
(require 'go-autocomplete)

(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq gofmt-command "goimports")
            (local-set-key (kbd "M-.") 'godef-jump)
            (local-set-key (kbd "M-,") 'pop-tag-mark)
            (local-set-key (kbd "C-c i") 'go-goto-imports)
            (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
            ))

;;
;; JavaScript
;;

;; npm install -g jshint

(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

(require 'js)
(require 'js2-mode)
(define-key js-mode-map "{" 'paredit-open-curly)
(define-key js-mode-map "}" 'paredit-close-curly-and-newline)

(flycheck-add-next-checker 'javascript-jshint 'javascript-gjslint)

;;
;; UnityJS
;;

;; UnityJS mode for emacs
(autoload 'unityjs-mode "unityjs-mode" "Major mode for editing Unity Javascript code." t)
(require 'unityjs-mode)

;;
;; C# / Unity
;;

(require 'csharp-mode)
(add-hook 'csharp-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (setq c-basic-offset 4)
             (c-set-offset 'substatement-open 0)
             (flycheck-mode 1)
             (omnisharp-mode)
             (auto-complete-mode)
             (local-set-key (kbd "C-c i") 'omnisharp-auto-complete)
             (local-set-key (kbd "M-.") 'omnisharp-go-to-definition)
             ))

;; brew install mono
;; git clone https://github.com/nosami/OmniSharpServer.git
;; cd OmniSharpServer
;; git submodule update --init --recursive
;; xbuild
(require 'omnisharp)
(setq omnisharp-server-executable-path
      (expand-file-name "~/dev/src/github.com/nosami/OmniSharpServer/OmniSharp/bin/Debug/OmniSharp.exe"))

;;
;; web-mode
;;

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\.ctp$" . web-mode))
;; indent
(setq web-mode-markup-indent-offset 4)
(setq web-mode-css-indent-offset 4)
(setq web-mode-code-indent-offset 4)
;; padding
(setq web-mode-style-padding 4)
(setq web-mode-script-padding 4)

;;
;; React / JSX
;;

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;; npm install -g jsxhint
(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."

  :command ("jsxhint" source)
  :error-patterns
  ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes (web-mode))

(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "jsx")
              ;; enable flycheck
              (flycheck-select-checker 'jsxhint-checker)
              (flycheck-mode))))

(require 'jsx-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))

;;
;; Haskell
;;

(require 'haskell-mode)
(autoload 'haskell-mode "haskell-mode" nil t)
(autoload 'haskell-cabal "haskell-cabal" nil t)

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode))

;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;
;; Scala
;;

(require 'scala-mode2)
(add-to-list 'auto-mode-alist '("\.sbt$" . scala-mode))

(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(add-hook 'scala-mode-hook
          (lambda ()
            (electric-pair-mode t)))

(custom-set-variables
 '(ensime-sem-high-faces
   '((var . scala-font-lock:var-face))))

;;
;; Elixir
;;

(require 'elixir-mode)
(add-hook 'elixir-mode-hook 'ac-alchemist-setup)

;;
;; Lisp / Scheme / Gauche
;;

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'inferior-scheme-mode-hook  #'enable-paredit-mode)

(add-hook 'paredit-mode-hook
          (lambda ()
            (local-set-key (kbd "C-h") 'paredit-backward-delete)))

;; info
(global-set-key (kbd "C-c h I") 'gauche-info)
(defun gauche-info ()
  (interactive)
  (info "/usr/local/share/info/gauche-refe.info"))

;; run gosh
(global-set-key (kbd "C-x S") (lambda () (interactive) (run-scheme "gosh")))

;; scheme indent
(setq scheme-program-name "gosh")

(put 'and-let* 'scheme-indent-function 1)
(put 'begin0 'scheme-indent-function 0)
(put 'call-with-client-socket 'scheme-indent-function 1)
(put 'call-with-input-conversion 'scheme-indent-function 1)

(put 'call-with-input-file 'scheme-indent-function 1)
(put 'call-with-input-process 'scheme-indent-function 1)
(put 'call-with-input-string 'scheme-indent-function 1)
(put 'call-with-iterator 'scheme-indent-function 1)
(put 'call-with-output-conversion 'scheme-indent-function 1)
(put 'call-with-output-file 'scheme-indent-function 1)
(put 'call-with-output-string 'scheme-indent-function 0)
(put 'call-with-temporary-file 'scheme-indent-function 1)
(put 'call-with-values 'scheme-indent-function 1)
(put 'dolist 'scheme-indent-function 1)
(put 'dotimes 'scheme-indent-function 1)
(put 'if-match 'scheme-indent-function 2)
(put 'let*-values 'scheme-indent-function 1)
(put 'let-args 'scheme-indent-function 2)
(put 'let-keywords* 'scheme-indent-function 2)
(put 'let-match 'scheme-indent-function 2)
(put 'let-optionals* 'scheme-indent-function 2)
(put 'let-syntax 'scheme-indent-function 1)
(put 'let-values 'scheme-indent-function 1)
(put 'let/cc 'scheme-indent-function 1)
(put 'let1 'scheme-indent-function 2)
(put 'letrec-syntax 'scheme-indent-function 1)
(put 'make 'scheme-indent-function 1)
(put 'multiple-value-bind 'scheme-indent-function 2)
(put 'parameterize 'scheme-indent-function 1)
(put 'parse-options 'scheme-indent-function 1)
(put 'receive 'scheme-indent-function 2)
(put 'rxmatch-case 'scheme-indent-function 1)
(put 'rxmatch-cond 'scheme-indent-function 0)
(put 'rxmatch-if  'scheme-indent-function 2)
(put 'rxmatch-let 'scheme-indent-function 2)
(put 'syntax-rules 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'until 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)
(put 'while 'scheme-indent-function 1)
(put 'with-builder 'scheme-indent-function 1)
(put 'with-error-handler 'scheme-indent-function 0)
(put 'with-error-to-port 'scheme-indent-function 1)
(put 'with-input-conversion 'scheme-indent-function 1)
(put 'with-input-from-port 'scheme-indent-function 1)
(put 'with-input-from-process 'scheme-indent-function 1)
(put 'with-input-from-string 'scheme-indent-function 1)
(put 'with-iterator 'scheme-indent-function 1)
(put 'with-module 'scheme-indent-function 1)
(put 'with-output-conversion 'scheme-indent-function 1)
(put 'with-output-to-port 'scheme-indent-function 1)
(put 'with-output-to-process 'scheme-indent-function 1)
(put 'with-output-to-string 'scheme-indent-function 1)
(put 'with-port-locking 'scheme-indent-function 1)
(put 'with-string-io 'scheme-indent-function 1)
(put 'with-time-counter 'scheme-indent-function 1)
(put 'with-signal-handlers 'scheme-indent-function 1)

;; dired
(put 'dired-find-alternate-file 'disabled nil)
