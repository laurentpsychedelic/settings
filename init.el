;; color settings
(setq default-frame-alist
    '((top  . 200) (left  . 400)
      (width. 80 ) (height. 40 )
      (cursor-color . "white")
      (cursor-type . box)
      (foreground-color . "yellow")
      ;;(background-color . "black")
      (font . "-*-Courier-normal-r-*-*-13-*-*-*-c-*-iso8859-1")))
;; window position
(setq initial-frame-alist '((top . 10) (left . 30) (width . 140) (height . 50)))

;; locale
(setenv "LC_TIME" "C")

;; coloring current line
(global-hl-line-mode 1)
;; color
(set-face-background 'hl-line "darkolivegreen")

;; save history between emacs sessions
(savehist-mode 1)

;; save cursor position in file
(setq-default save-place t)
(require 'saveplace)

;; display corresponding parentheses
(show-paren-mode 1)

;; reassign C-h to back delete char
(global-set-key (kbd "C-h") 'delete-backward-char)

;; display line and column number
(line-number-mode 1)
(column-number-mode 1)

;; coloring region
(transient-mark-mode 1) 

;; low GC mode (faster)
(setq gc-cons-threshold (* 10 gc-cons-threshold))

;; max log messages
(setq message-log-max 10000)

;; enable mini-buffer recursive editing
(setq enable-recursive-minibuffers t)

;; disable dialog boxes
(setq use-dialog-box nil)
(defalias 'message-box 'message)

;; save much history
(setq history-length 1000)

;; display key strokes
(setq echo-keystrokes 0.1)

;; large file warning
(setq large-file-warning-threshold (* 25 1024 1024))

;; save minibuffer history even when action cancelled
(defadvice abort-recursive-edit (before minibuffer-save activate)
  (when (eq (selected-window) (active-minibuffer-window))
    (add-to-history minibuffer-history-variable (minibuffer-contents))))

;; replace yes/no reply by y/n reply
(defalias 'yes-or-no-p 'y-or-n-p)

;; tool bar/scroll bar off
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Add dir to load path
(add-to-list 'load-path "~/.emacs.d/")

;; powershell-mode
(autoload 'powershell-mode "powershell-mode" "A editing mode for Microsoft PowerShell." t)
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode)) ; PowerShell script

;; truncation of lines in partial windows
(setq truncate-lines t)
(setq truncate-partial-width-window nil)

;; auto-scroll while compilation
(setq compilation-scroll-output t)

;; move between windows with M-↑→↓←
(windmove-default-keybindings 'meta)


;;;;;;;;;;;;;;;;;;
;; auto-install ;;
;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/auto-install/")
(require 'auto-install)
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;;;;;;;;;;;;;;;;;;
;; auto-complete ;;
;;;;;;;;;;;;;;;;;;;
(require 'auto-complete-config)
(global-auto-complete-mode 1)

;;;;;;;;;;;;;;;;;;;
;; hippie-expand ;;
;;;;;;;;;;;;;;;;;;;
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
	try-complete-file-name
	try-expand-all-abbrevs
	try-expand-dabbrevs
	try-expand-dabbrevs-all-buffers
	try-expand-dabbrevs-from-kill
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol
	))
