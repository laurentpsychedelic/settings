;; tool bar/scroll bar off
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; start with maximized window on Windows
;(w32-send-sys-command 61488)
(defun w32-maximize-frame ()
  "Maximize the current frame"
  (interactive)
  (w32-send-sys-command 61488))
 
(add-hook 'window-setup-hook 'w32-maximize-frame t)

;; color settings
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:background "black" :foreground "white" :font "-outline-Courier New-normal-normal-normal-mono-11-*-*-*-c-*-iso8859-1" :cursor-type "box")))))

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

;; Add dir to load path
(add-to-list 'load-path "~/.emacs.d/")

;; powershell-mode
(autoload 'powershell-mode "powershell-mode" "A editing mode for Microsoft PowerShell." t)
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode)) ; PowerShell script

;; groovy mode
(autoload 'groovy-mode "groovy-mode" "A editing mode for Groovy." t)
(add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode)) ; Groovy source code

;; bat-mode
(setq auto-mode-alist 
       (append 
        (list (cons "\\.[bB][aA][tT]$" 'bat-mode))
        ;; For DOS init files
        (list (cons "CONFIG\\."   'bat-mode))
        (list (cons "AUTOEXEC\\." 'bat-mode))
        auto-mode-alist))

(autoload 'bat-mode "bat-mode"
      "DOS and Windows BAT files" t)

;; truncation of lines in partial windows
(setq truncate-lines t)
(setq truncate-partial-width-window nil)

;; auto-scroll while compilation
(setq compilation-scroll-output t)

;; move between windows with M-arrow
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

;;; shortcuts to enlarge-shrink windows
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs client related ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(server-start)
(defun iconify-emacs-when-server-is-done ()
  (unless server-clients (iconify-frame)))
;; iconify Emacs when editing over
(add-hook 'server-done-hook 'iconify-emacs-when-server-is-done)
;; keybinding : realocate C-x C-c to server-edit
(global-set-key (kbd "C-x C-c") 'server-edit)
;; shut-down Emacs (server) with M-x exit
(defalias 'exit 'save-buffers-kill-emacs)

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

;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-function-mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(which-func-mode 1)
;; aply which-func-mode for all major modes
(setq which-func-modes t)
;; display function name in a bar above screen
(delete (assoc 'which-func-mode mode-line-format) mode-line-format)
(setq-default header-line-format '(which-func-mode ("" which-func-format)))

;;;;;;;;;;;;;;;;;;;
;; flymake-mode ;;
;;;;;;;;;;;;;;;;;;;
(load "flymake")
(add-hook 'c-mode-common-hook (lambda () (flymake-mode t)))
;; redefine to remove "check-syntax" target
(defun flymake-get-make-cmdline (source base-dir)
  (list "make"
	(list "-s"
	      "-C"
	      base-dir
	      (concat "CHK_SOURCES=" source)
	      "SYNTAX_CHECK_MODE=1")))
;; specify that flymake use ant instead of make
(setcdr (assoc "\\.java\\'" flymake-allowed-file-name-masks)
	'(flymake-simple-ant-java-init flymake-simple-java-cleanup))
;; redefine to remove "check-syntax" target
(defun flymake-get-ant-cmdline (source base-dir)
  (list "ant"
	(list "-buildfile"
	      (concat base-dir "/" "build.xml"))))

(add-hook 'java-mode-hook
	  '(lambda ()
	     (flymake-mode)))
(setq flymake-log-level -1)

(defun flymake-display-err-minibuf ()
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count               (length line-err-info-list))
         (text))
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (cur-text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
          (setq text (concat text "\n" (format "[%s] %s" line cur-text)))
          ))
      (setq count (1- count)))
    (message text)
))

(add-hook
 'java-mode-hook
 '(lambda ()
    (define-key java-mode-map "\C-cd" 'flymake-display-err-minibuf)))

;;;;;;;;;;;;
;; hideif ;;
;;;;;;;;;;;;
(require 'hideif)
(add-hook 'c-mode-common-hook 'hide-ifdef-mode)

;;;;;;;;;;;;;;
;; hideshow ;;
;;;;;;;;;;;;;;
(require 'hideshow)
(require 'fold-dwim)

(add-hook 'java-mode-hook 'hs-minor-mode)

(global-set-key (kbd "<f7>")      'fold-dwim-toggle)
(global-set-key (kbd "<M-f7>")    'fold-dwim-hide-all)
(global-set-key (kbd "<S-M-f7>")  'fold-dwim-show-all)
;; (put 'org-mode 'fold-dwim-outline-style 'nested)

;;;;;;;;;;;
;; gtags ;;
;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/glo61wb/share/gtags")
(autoload 'gtags-mode "gtags" "" t)
(require 'gtags)
(add-hook 'c-mode-common-hook 'gtags-mode)
(add-hook 'c++-mode-hook 'gtags-mode)
(add-hook 'java-mode-hook 'gtags-mode)

;;;;;;;;;;;;;
;; tempbuf ;;
;;;;;;;;;;;;;
(require 'tempbuf)
;; enable tempbuf when opening file
(add-hook 'find-file-hooks 'turn-on-tempbuf-mode)
;; enable tempbuf when using dired
(add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)

;; also turn tempbuf on for emacs, W3 (Emacs' Web Browser) buffers,
;; UNIX 'man' documentation buffers, and any buffer with view-mode activated.
(add-hook 'custom-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'w3-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'Man-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'view-mode-hook 'turn-on-tempbuf-mode)
;; * Tricks
;; tempbuf-mode to terminate idle
;;     (defun my-term-on-tempbuf-expire ()
;;       (when (get-buffer-process (current-buffer))
;;         (term-send-eof)))
;;     (defun my-term-mode-patch ()
;;       (turn-on-tempbuf-mode)
;;       (add-hook 'tempbuf-expire-hook 'my-term-on-tempbuf-expire
;;                 nil t))
;;     (add-hook 'term-mode-hook 'my-term-mode-patch)

;;;;;;;;;;;;;;;;;;;;;;;
;; auto-save-buffers ;;
;;;;;;;;;;;;;;;;;;;;;;;
;; original: http://0xcc.net/misc/auto-save/auto-save-buffers.el
(require 'auto-save-buffers)
(run-with-idle-timer 2 t 'auto-save-buffers)    ; auto save after 2s. idle time
;; commands to include or exclude only a certain type of files
;; (run-with-idle-timer 0.5 t 'auto-save-buffers "\\.c$" "^$") ; .c だけ対象
;; (run-with-idle-timer 0.5 t 'auto-save-buffers ""   "\\.h$") ; .h だけ除外


;; abbrev_defs file
(read-abbrev-file "~/.emacs.d/abbrev_defs")
;;; new macro declare-abbrev
(require 'cl)
(defvar my-abbrev-tables nil)
(defun my-abbrev-hook ()
(let ((def (assoc (symbol-name last-abbrev) my-abbrev-tables)))
(when def
(execute-kbd-macro (cdr def)))
t))
(put 'my-abbrev-hook 'no-self-insert t)
(defmacro declare-abbrevs (table abbrevs)
(if (consp table)
`(progn ,@(loop for tab in table
collect `(declare-abbrevs ,tab ,abbrevs)))
`(progn
,@(loop for abbr in abbrevs
do (when (third abbr)
(push (cons (first abbr) (read-kbd-macro (third abbr)))
my-abbrev-tables))
collect `(define-abbrev ,table
,(first abbr) ,(second abbr) ,(and (third abbr)
''my-abbrev-hook))))))
(put 'declare-abbrevs 'lisp-indent-function 2)

;;; sample abbrev code definition
;;; c/c++ mode
(eval-after-load "cc-mode"
  '(declare-abbrevs (c-mode-abbrev-table c++-mode-abbrev-table)
       (("#s"    "#include <>" "C-b")
	("#i"    "#include \"\"" "C-b")
	("#ifn"  "#ifndef")
	("#e"    "#endif /* */" "C-3 C-b")
	("#ifd"  "#ifdef")
	("imain" "int\nmain (int ac, char **av[])\n{\n\n}" "C-p TAB")
	("if"    "if () {\n}\n" "C-M-b C-M-q C-- C-M-d")
	("else"  "else {\n}\n"  "C-M-b C-M-q C-M-d RET")
	("while" "while () {\n}\n" "C-M-b C-M-q C-- C-M-d")
	("for"   "for (;;) {\n}\n" "C-M-b C-M-q C-M-b C-M-d")
	("pr"    "printf (\"\")" "C-2 C-b"))))
;;; java mode
(eval-after-load "cc-mode"
  '(declare-abbrevs (java-mode-abbrev-table)
       (("main" "public static void main(String[] args) {\n\n}" "C-p TAB C-h")
	("println"   "System.out.println()" "C-b")
	("print"   "System.out.print()" "C-b")
	("if"    "if () {\n}\n" "C-M-b C-M-q C-- C-M-d")
	("else"  "else {\n}\n"  "C-M-b C-M-q C-M-d RET")
	("while" "while () {\n}\n" "C-M-b C-M-q C-- C-M-d")
	("for"   "for (;;) {\n}\n" "C-M-b C-M-q C-M-b C-M-d")
	("pr"    "printf (\"\")" "C-2 C-b"))))
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
