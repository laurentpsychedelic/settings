;; -*- coding: utf-8 -*-
;;debug on error
(setq debug-on-error t)
;;exception handling
(defmacro safe-wrap (fn &rest clean-up)
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn ,fn))
           ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)
     ,@clean-up))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; tool bar/scroll bar off
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; start with maximized window on Windows
;(w32-send-sys-command 61488)
(defun w32-maximize-frame ()
  "Maximize the current frame (windows only)"
  (interactive)
  (w32-send-sys-command 61488))
(defun w32-restore-frame ()
  "Restore a minimized/maximized frame (windows only)"
  (interactive)
  (w32-send-sys-command 61728))
 
(defun x11-maximize-frame ()
  "Maximize the current frame (to full screen) X11 version"
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))

(defun maximize-frame ()
  "Maximize current frame"
  (interactive)
  (if (string-equal system-type "gnu/linux")
      (x11-maximize-frame)
    (w32-maximize-frame)))

(defun restore-frame ()
  "Restore a minimized/maximized frame (X11 not supported yet)"
  (interactive)
  (if (string-equal system-type "gnu/linux")
      (message "Unable to use restore function on X11 yet...")
    (w32-restore-frame)))

(defun add-maximize-window-hook ()
  "Mazimize current window"
  (interactive)
  (if (string-equal system-type "gnu/linux")
    (add-hook 'window-setup-hook 'x11-maximize-frame) ; GNU/Linux [Ubuntu]
  (add-hook 'window-setup-hook 'w32-maximize-frame t))) ; Windows

(add-maximize-window-hook)
(maximize-frame)
; (restore-frame)
;; see http://emacsblog.org/2007/02/22/maximize-on-startup-part-2/

;;;;;;;;;;;;;;;;;;
;; auto-install ;;
;;;;;;;;;;;;;;;;;;
(defun load-auto-install-library ()
  (add-to-list 'load-path "~/.emacs.d/auto-install/")
  (require 'auto-install)
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))
(safe-wrap (load-auto-install-library) (message "Unable to load auto-install!"))

;;;;;;;;;;;;;;;;;;
;; myantcompile ;;
;;;;;;;;;;;;;;;;;;
(require 'myantcompile)
;;;;;;;;;;;;;
;; myutils ;;
;;;;;;;;;;;;;
(require 'myutils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gradle build script linked to groovy-mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode)) ; gradle build script

;;;;;;;;;;;;;;
;; org-mode ;;
;;;;;;;;;;;;;;
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
(setq org-directory "~/Dropbox/dev/org/")
(setq org-agenda-files (list org-directory))
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
(if (string-equal system-type "gnu/linux")
    (setq browse-url-browser-function 'browse-url-chromium)
  (setq browse-url-browser-function 'browse-url-default-windows-browser))

;;;;;;;;;;;;;;;
;; yasnippet ;;
;;;;;;;;;;;;;;;
(require 'cl-lib)
(require 'cl)
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(setq yas-snippet-dirs '("~/emacs.d/plugins/yasnippet/snippets"))
(setq yas-snippet-dirs (append yas-snippet-dirs
                               '("~/.emacs.d/snippets")))
(require 'yasnippet)
(yas-global-mode 1)

(auto-insert-mode)
(setq auto-insert-directory "~/.emacs.d/insert/")
;; below is the way to define a snippet by hand on init.el
;; (defun fill-new-java-class-skeleton ()
;;   (interactive)
;;   (yas/expand-snippet
;;    "package ${1:`(file-name-nondirectory (directory-file-name (file-name-directory (file-name-sans-extension (buffer-file-name)))))`};

;; public class ${2:`(file-name-nondirectory (file-name-sans-extension (buffer-file-name)))`} {
;;     $0
;; }"
;;    (point) (point)))
;; add auto-insert snippet from a direct definition above
; (define-auto-insert "\\.java$" 'fill-new-java-class-skeleton)

;; below code uses already registered snippet for auto-insert 
;; add auto-insert snippet when it is already registered
(define-auto-insert "\\.java$" (lambda () (insert "newclass")(yas/expand)))
(define-auto-insert "\\.groovy$" (lambda () (insert "newclass")(yas/expand)))
(define-auto-insert "\\Test.java" (lambda () (insert "newtestclass-java")(yas/expand)))
(define-auto-insert "\\Test.groovy" (lambda () (insert "newtestclass-groovy")(yas/expand)))

; (add-hook 'window-setup-hook 'w32-maximize-frame t)

;; color settings
;;(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
;; '(default ((t (:background "black" :foreground "white" :cursor-type "box")))))
;; '(default ((t (:stipple nil :background "dark blue" :foreground "white" :cursor-type "box")))))
;:font "-outline-Courier New-normal-normal-normal-mono-11-*-*-*-c-*-iso8859-1"
(set-face-attribute 'default nil :stipple nil)

;;;;;;;;;;;;;;;
;; set fonts ;;
;;;;;;;;;;;;;;;
;;(set-face-attribute 'default nil :cursor-type "box")
(set-face-attribute 'default nil :height 100)
;; set font to "Ubunto Mono"
(setq fontsize 14)
(defun setfont (size)
  "Set font size"
(if (string-equal system-type "gnu/linux")
    (safe-wrap (set-face-attribute 'default nil :font (format "Mono-%d" size) (message "Unable to set font!"))) ; GNU/Linux [Ubuntu]
  (safe-wrap (set-face-attribute 'default nil :font (format "Ubuntu Mono-%d:spacing=m:antialias=natural" size)) (message "Unable to set font!")))) ; Windows
(setfont fontsize)
(defun font-size-modify (&optional dec)
  "modify font size; if DEC is t,
    decrease font size, otherwise increase it in 1-steps"
  (let* ((font-size-or-nil fontsize) ; nil before setting
          (oldsize (if font-size-or-nil font-size-or-nil 11))
          (newsize (if dec (- oldsize 1) (+ oldsize 1))))
    (when (and (>= newsize 1) (<= newsize 100))
      (setq fontsize newsize)
      (setfont newsize)
      (restore-frame)
      (maximize-frame))))

 ;; C-> or C-+ will increase font size
 ;; C-< or C-- will decrease font size
(global-set-key (kbd "C->") '(lambda () (interactive) (font-size-modify nil)))
(global-set-key (kbd "C-<") '(lambda () (interactive) (font-size-modify t)))
(global-set-key (kbd "C-+") '(lambda () (interactive) (font-size-modify nil)))
(global-set-key (kbd "C--") '(lambda () (interactive) (font-size-modify t)))

(global-set-key (kbd "C-M-k") (kbd "C-u 0 C-k"))

;;;;;;;;;;;;;;;;;;;;;;
;; set transparency ;;
;;;;;;;;;;;;;;;;;;;;;;
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(transparency '100)
(global-set-key (kbd "C-c t") 'transparency)

(defun djcb-opacity-modify (&optional dec)
  "modify the transparency of the emacs frame; if DEC is t,
    decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
          (oldalpha (if alpha-or-nil alpha-or-nil 100))
          (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

 ;; C-8 will increase opacity (== decrease transparency)
 ;; C-9 will decrease opacity (== increase transparency
 ;; C-0 will returns the state to normal
(global-set-key (kbd "C-8") '(lambda()(interactive)(djcb-opacity-modify)))
(global-set-key (kbd "C-9") '(lambda()(interactive)(djcb-opacity-modify t)))
(global-set-key (kbd "C-0") '(lambda()(interactive)
                               (modify-frame-parameters nil `((alpha . 100)))))

;; locale
(setenv "LC_TIME" "C")

;; save history between emacs sessions
(savehist-mode 1)

;; save cursor position in file
(setq-default save-place t)
(require 'saveplace)

;; display corresponding parentheses
(show-paren-mode 1)

;; reassign C-h to back delete char
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key "\C-xh" 'help-command) ;; reassign help command to C-x h

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

;; show a yank menu
(global-set-key "\C-cy" '(lambda ()
                           (interactive) (popup-menu 'yank-menu)))

;; powershell-mode
(autoload 'powershell-mode "powershell-mode" "A editing mode for Microsoft PowerShell." t)
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode)) ; PowerShell script

;; C# mode
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;; c++ mode (for upcase CPP file extension)
(add-to-list 'auto-mode-alist '("\\.CPP\\'" . c++-mode)) ; C++

;; groovy mode
(add-to-list 'load-path "~/.emacs.d/groovy-mode/")
(autoload 'groovy-mode "groovy-mode" "A editing mode for Groovy." t)
(add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode)) ; Groovy source code
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))
;;; make Groovy mode electric by default.
(add-hook 'groovy-mode-hook
          '(lambda ()
             (require 'groovy-electric)
             (groovy-electric-mode)))

;; Visual Basic mode
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(add-to-list 'auto-mode-alist '("\\.\\(frm\\|bas\\|cls\\|vb\\|vbs\\)$" . visual-basic-mode))

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
(setq truncate-partial-width-windows t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; switch window splitting from vertical/horizontal ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://emacswiki.org/emacs/Rick_Bielawski
;; Idea and starter code from Benjamin Rutt (rutt.4+news@osu.edu) on comp.emacs
(defun rgb-window-horizontal-to-vertical ()
  "Switches from a horizontal split to a vertical split."
  (interactive)
  (let ((one-buf (window-buffer (selected-window)))
        (buf-point (point)))
    (other-window 1)
    (delete-other-windows)
    (split-window-horizontally)
    (switch-to-buffer one-buf)
    (goto-char buf-point)))
(global-set-key (kbd "C-c v") 'rgb-window-horizontal-to-vertical)
;; complement of above created by rgb 11/2004
(defun rgb-window-vertical-to-horizontal ()
  "Switches from a vertical split to a horizontal split."
  (interactive)
  (let ((one-buf (window-buffer (selected-window)))
        (buf-point (point)))
    (other-window 1)
    (delete-other-windows)
    (split-window-vertically)
    (switch-to-buffer one-buf)
    (goto-char buf-point)))
(global-set-key (kbd "C-c h") 'rgb-window-vertical-to-horizontal)

;;;;;;;;;;;;;;;;;;;;
;; ediff settings ;;
;;;;;;;;;;;;;;;;;;;;
(setq ediff-split-window-function 'split-window-horizontally)
;(setq ediff-split-window-function (if (< (frame-width) 150)
;                                      'split-window-horizontally
;                                    'split-window-vertically))
; keybinding for ediff revision "C-x version ediff"
(global-set-key (kbd "C-x v e") 'ediff-revision)
(if (not (string-equal system-type "gnu/linux"))
    (setq ediff-diff-program "C:/cygwin/bin/diff.exe"))
;;(setq ediff-diff-options "--text")
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; auto-scroll while compilation
(setq compilation-scroll-output t)

;; move between windows with M-arrow
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)
(global-set-key (kbd "C-c M-<up>") 'windmove-up)
(global-set-key (kbd "C-c M-<left>") 'windmove-left)
(global-set-key (kbd "C-c M-<right>") 'windmove-right)
(global-set-key (kbd "C-c M-<down>") 'windmove-down)

;;; shortcuts to enlarge-shrink windows
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;;; key binding for imenu
(global-set-key (kbd "M-s") 'imenu)

;;; key binding for find-dired and dired-do-query-replace-regexp
(global-set-key (kbd "C-c s") 'find-dired)
(global-set-key (kbd "C-c r") 'dired-do-query-replace-regexp)


;; jump to corresponding parenthesis
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis. Else go to the
   opening parenthesis one level up."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1))
        (t
         (backward-char 1)
         (cond ((looking-at "\\s\)")
                (forward-char 1) (backward-list 1))
               (t
                (while (not (looking-at "\\s("))
                  (backward-char 1)
                  (cond ((looking-at "\\s\)")
                         (message "->> )")
                         (forward-char 1)
                         (backward-list 1)
                         (backward-char 1)))
                  ))))))
(global-set-key (kbd "C-(") 'goto-match-paren)
(global-set-key (kbd "C-)") 'goto-match-paren)

;;;;;;;;;;;;;;;;;;;;;;
;; buffer switching ;;
;;;;;;;;;;;;;;;;;;;;;;
(iswitchb-mode 1)
(setq read-buffer-function 'iswitchb-read-buffer)
(setq iswitchb-regexp nil)
(setq iswitchb-prompt-newbuffer nil)

;;;;;;;;;;;;;;
;; uniquify ;;
;;;;;;;;;;;;;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

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

;;;;;;;;;;;;;;;;;
;; scilab mode ;;
;;;;;;;;;;;;;;;;;
(require 'scilab-mode)
(autoload 'scilab-mode "scilab-mode" "A editing mode for Scilab scripts." t)
(add-to-list 'auto-mode-alist '("\\.sce\\'" . scilab-mode)) ; Scilab script
(add-to-list 'auto-mode-alist '("\\.sci\\'" . scilab-mode)) ; Scilab script
;;;;;;;;;
;; w3m ;;
;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/emacs-w3m/")
(require 'w3m-load)

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

;;;;;;;;;;;;;;;;
;; point-undo ;;
;;;;;;;;;;;;;;;;
(require 'point-undo)
(define-key global-map (kbd "<f9>") 'point-undo)
(define-key global-map (kbd "C-<f9>") 'point-redo)

;;;;;;;;;;;
;; redo+ ;;
;;;;;;;;;;;
(require 'redo+)
(global-set-key (kbd "C-M-_") 'redo)
(setq undo-no-redo t)
(setq undo-limit 600000)
(setq undo-strong-limit 900000)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-function-mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(which-function-mode 1)

;;;;;;;;;;;;;;;;;;;;
;; highligh-regex ;;
;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "<f6>") 'highlight-regexp)

;;;;;;;;;;;;;
;; compile ;;
;;;;;;;;;;;;;
(global-set-key (kbd "<f5>") 'compile)

;;;;;;;;;;;;;;;;;
;; eval region ;;
;;;;;;;;;;;;;;;;;
(defalias 'er 'eval-region)

;;;;;;;;;;;;;;;;;;;
;; indent-region ;;
;;;;;;;;;;;;;;;;;;;
(defalias 'ir 'indent-region)


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; toggle-truncate-lines ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'ttl 'toggle-truncate-lines)

;;;;;;;;;;;;;;;;;;;;;
;; twittering mode ;;
;;;;;;;;;;;;;;;;;;;;;

(safe-wrap (progn
             (add-to-list 'load-path "~/.emacs.d/plugins/twittering-mode")
             (require 'twittering-mode)
             ;(setq twittering-status-format
             ; "%C{%Y/%m/%d %H:%M:%S} %s > %T // from %f%L%r%R")
             (setq twittering-icon-mode t)                ; Show icons
             (setq twittering-timer-interval 300)         ; Update your timeline each 300 seconds (5 minutes)
             (setq twittering-url-show-status nil)        ; Keeps the echo area from showing all the http processes
             (setq twittering-number-of-tweets-on-retrieval 30))
           (message "Unable to load twittering mode! (Try checking-out submodule with \"git submodule update --init\")"))

;;;;;;;;;;;;;;;;;;;;;
;; indent settings ;;
;;;;;;;;;;;;;;;;;;;;;
(setq-default c-basic-offset 4)
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set time zone to GMT-0009 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-time-zone-rule "GMT-0009")
;(current-time-string)
;(current-time-zone)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autojump             ;;
;; fukuyama.co/autojump ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'recentf)
(recentf-mode t)

(defun open-dired-from-recentf (keyword)
  (interactive "sEnter folder name (part): ")
  (with-temp-buffer
    (mapcar (lambda (x) (insert (format "%s\n" x)))
            recentf-list)
    (goto-char (point-min))
    (search-forward keyword)
    (dired (file-name-directory (thing-at-point 'line)))
    ))
(global-set-key (kbd "C-x C-d") 'open-dired-from-recentf)

;;;;;;;;;;;;
;; sticky ;;
;;;;;;;;;;;;
(require 'sticky)
;;
;; (use-sticky-key ?\; sticky-alist:en)    ; for english keyboards
;;   OR
(use-sticky-key 'muhenkan sticky-alist:ja)    ; for japanese keyboards
(use-sticky-key 'non-convert sticky-alist:ja)

;;;;;;;;
;; bm ;;
;;;;;;;;
(setq-default bm-buffer-persistencenil)
(setq bm-restore-repository-on-load t)
(require 'bm)
(add-hook 'find-file-hooks 'bm-buffer-restore)
(add-hook 'kill-buffer-hook 'bm-buffer-save)
(add-hook 'after-save-hook 'bm-buffer-save)
(add-hook 'after-revert-hook 'bm-buffer-restore)
(add-hook 'vc-before-checkin-hook 'bm-buffer-save)
(global-set-key (kbd "M-SPC") 'bm-toggle)
(global-set-key (kbd "M-[") 'bm-previous)
(global-set-key (kbd "M-]") 'bm-next)
;;   M$ Visual Studio key setup.
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)

;;;;;;;;;;;;;;
;; goto-chg ;;
;;;;;;;;;;;;;;
(require 'goto-chg)
(define-key global-map (kbd "<f8>") 'goto-last-change)
(define-key global-map (kbd "S-<f8>") 'goto-last-change-reverse)

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
(add-hook 'c-mode-common-hook 'hs-minor-mode)

(global-set-key (kbd "<f7>")      'fold-dwim-toggle)
(global-set-key (kbd "<C-f7>")    'fold-dwim-hide-all)
(global-set-key (kbd "<C-S-f7>")  'fold-dwim-show-all)
;; (put 'org-mode 'fold-dwim-outline-style 'nested)

;;;;;;;;;;;;;;;;;;
;; etags-select ;;
;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-.") 'etags-select-find-tag)

;;;;;;;;;;;
;; gtags ;;
;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/glo61wb/share/gtags")
(autoload 'gtags-mode "gtags" "" t)
(require 'gtags)
(add-hook 'c-mode-common-hook 'gtags-mode)
(add-hook 'c++-mode-hook 'gtags-mode)
(add-hook 'java-mode-hook 'gtags-mode)
(add-hook 'gtags-select-mode-hook
   '(lambda ()
     (setq hl-line-face 'underline)
     (hl-line-mode 1)
))
(setq gtags-path-style 'relative)
(setq view-read-only nil)
(setq gtags-read-only nil)
(define-key gtags-select-mode-map "\e*" 'gtags-pop-stack)
(define-key gtags-select-mode-map "\^?" 'scroll-down)
(define-key gtags-select-mode-map " " 'scroll-up)
(define-key gtags-select-mode-map "\C-b" 'scroll-down)
(define-key gtags-select-mode-map "\C-f" 'scroll-up)
(define-key gtags-select-mode-map "k" 'previous-line)
(define-key gtags-select-mode-map "j" 'next-line)
(define-key gtags-select-mode-map "p" 'previous-line)
(define-key gtags-select-mode-map "n" 'next-line)
(define-key gtags-select-mode-map "q" 'gtags-pop-stack)
(define-key gtags-select-mode-map "u" 'gtags-pop-stack)
(define-key gtags-select-mode-map "\C-t" 'gtags-pop-stack)
(define-key gtags-select-mode-map "\C-m" 'gtags-select-tag)
(define-key gtags-select-mode-map "\C-o" 'gtags-select-tag-other-window)
(define-key gtags-select-mode-map "\e." 'gtags-select-tag)

;;;;;;;;;;;;
;; wdired ;;
;;;;;;;;;;;;
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;;;;;;;;;;;;;;
;; summarye ;;
;;;;;;;;;;;;;;
(require 'summarye)
(defalias 'sb 'se/make-summary-buffer)
;(global-set-key (kbd "M-x sb") 'se/make-summary-buffer)
;; (add-to-list 'se/mode-delimiter-alist
;;           '(java-mode "\(\(public\)|\(private\)|\(protected\)\)?.* .*(.*).*")
;; )

;;;;;;;;;;;;;
;; tempbuf ;;
;;;;;;;;;;;;;
(require 'tempbuf)
;; enable tempbuf when opening file: not needed
;; (add-hook 'find-file-hooks 'turn-on-tempbuf-mode)
;; enable tempbuf when using dired
(add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; marmalade package manager ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MELPA package manager ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;
;; solarized-emacs ;;
;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs/solarized-emacs/")
(require 'solarized)
(add-to-list 'custom-theme-load-path "~/.emacs.d/solarized-emacs/")
; (load-theme 'solarized-dark)
(load-theme 'misterioso)
; (load-theme 'tango)

(defun change-theme (theme)
  "Change visual theme"
  (interactive "sTheme name: ")
  (load-theme (intern theme))
  (set-face-background 'highlight-indentation-current-column-face (get-lighter-color (cdr (assoc 'background-color (frame-parameters))) -0.03))
  (set-face-background 'highlight-indentation-face (get-lighter-color (cdr (assoc 'background-color (frame-parameters))) 0.03))
  (set-face-background 'vline-fface (get-lighter-color (cdr (assoc 'background-color (frame-parameters))) -0.03))
  (setq vline-face 'vline-fface)
  (set-face-background 'hl-line (get-lighter-color (cdr (assoc 'background-color (frame-parameters))) -0.3)))
; (change-theme "solarized-dark")

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight-indentation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'highlight-indentation)
(add-hook 'c-mode-common-hook 
          (lambda ()
            (highlight-indentation-current-column-mode)
            (highlight-indentation-mode)))

;;(message (format "%S" (frame-parameters)))
(defun get-lighter-color (color &optional amount)
  "This function get a color name COLOR and return a color slightly lighter (or darker), by the amount AMOUNT (normalized RGB, default: +0.1)"
  (if (not amount) (set amount 0.1))
  (setq color (apply 'color-rgb-to-hex (mapcar (lambda (num) (setq num (+ amount num)) (if (> num 1.0) (setq num 1.0)) num)
                      (color-name-to-rgb (cdr (assoc 'background-color (frame-parameters))))))))

(set-face-background 'highlight-indentation-current-column-face (get-lighter-color (cdr (assoc 'background-color (frame-parameters))) -0.03))
(set-face-background 'highlight-indentation-face (get-lighter-color (cdr (assoc 'background-color (frame-parameters))) 0.03))

;;;;;;;;;;;;;;;;;;;
;; col-highlight ;;
;;;;;;;;;;;;;;;;;;;
(defface vline-fface '((t ()))
  "*Face for current-column highlighting"
  :group 'column-highlight :group 'faces)
(set-face-background 'vline-fface (get-lighter-color (cdr (assoc 'background-color (frame-parameters))) -0.03))
(setq vline-face 'vline-fface)
(require 'col-highlight)
;; 1 or 2 (1: always on)
(column-highlight-mode 2)
;; 2: highlight when idle
(toggle-highlight-column-when-idle 1)
(col-highlight-set-interval 6) ; highlight time in seconds
(setq col-highlight-vline-face-flag nil)

;;;;;;;;;;;;
;; hl-line;;
;;;;;;;;;;;;
(global-hl-line-mode nil)
;(setq hl-line-face 'underline)
(set-face-background 'hl-line (get-lighter-color (cdr (assoc 'background-color (frame-parameters))) -0.3))

;;;;;;;;;;;
;; align ;;
;;;;;;;;;;;
(global-set-key (kbd "C-M-a") 'align)

;;;;;;;;;;;;;;;;;;;;;;
;; Numbered backups ;;
;;;;;;;;;;;;;;;;;;;;;;
(setq version-control t)

;;;;;;;;;;;;;;;;;;;;;;;;
;; java documentation ;;
;;;;;;;;;;;;;;;;;;;;;;;;
;; ~/.emacs.d/java-doc/allclasses-noframe.html
(add-to-list 'load-path "~/.emacs.d/java-doc/")
(require 'java-class-doc)
(defalias 'jdoc 'java-class-doc)

;(autoload 'java-class-doc "java-class-doc" "Open documentation for class in Java API docs" t)

;; also turn tempbuf on for emacs, W3 (Emacs' Web Browser) buffers,
;; UNIX 'man' documentation buffers, and any buffer with view-mode activated.
(add-hook 'custom-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'w3-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'Man-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'view-mode-hook 'turn-on-tempbuf-mode)

;;;;;;;;;;;;;;;;;;;;
;; beautiful-json ;;
;;;;;;;;;;;;;;;;;;;;
(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; open current buffer with default program ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun open-in-browser()
  (interactive)
  (let ((filename (buffer-file-name)))
    (print (concat "file://" filename))
    (browse-url (concat "file://" filename))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; open file with default program in dired ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun uenox-dired-winstart () 
  "Type '[uenox-dired-winstart]': win-start the current line's file." 
  (interactive) 
  (if (eq major-mode 'dired-mode) 
      (let ((fname (dired-get-filename))) 
        (w32-shell-execute "open" fname) 
        (message "win-started %s" fname))))
;; dired key-bindings
(add-hook 'dired-mode-hook 
          (lambda () 
            (define-key dired-mode-map "z" 'uenox-dired-winstart)))

;;;;;;;;;;;;
;; popwin ;;
;;;;;;;;;;;;
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

;;;;;;;;;;;
;; direx ;;
;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/direx-el/")
(require 'direx)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)
(push '(direx:direx-mode :position left :width 35 :dedicated t)
      popwin:special-display-config)
(setq direx:leaf-icon "  "
      direx:open-icon "↓ "
      direx:closed-icon "→ ")

;;;;;;;;;;;
;; magit ;;
;;;;;;;;;;;
;(add-to-list 'load-path "~/.emacs.d/magit-1.2.0/")
;(require 'magit)
;(require 'magit-svn)
;(global-set-key (kbd "C-x v q") 'magit-status)

;; automatically make script files executable
(add-hook 'after-save-hook
   'executable-make-buffer-file-executable-if-script-p)

;;;;;;;;;;;;;;;;;;;;;;;
;; auto-save-buffers ;;
;;;;;;;;;;;;;;;;;;;;;;;
;; original: http://0xcc.net/misc/auto-save/auto-save-buffers.el
(require 'auto-save-buffers)
(run-with-idle-timer 5 t 'auto-save-buffers)    ; auto save after 2s. idle time
;; commands to include or exclude only a certain type of files
;; (run-with-idle-timer 0.5 t 'auto-save-buffers "\\.c$" "^$") ; .c files IN
;; (run-with-idle-timer 0.5 t 'auto-save-buffers ""   "\\.h$") ; .h files OUT

(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xeu_elisp_util (http://code.google.com/p/ergoemacs/source/browse/packages/xeu_elisp_util.el) ;;
;;   >> block at point, etc...                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'xfrp_find_replace_pairs)
(require 'xeu_elisp_util)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subword-mode ON in Java mode (camel case) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'java-mode-hook 'subword-mode)
(add-hook 'groovy-mode-hook 'subword-mode)
(add-hook 'c-mode-common-hook 'subword-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse method arguments in Java ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-java-method-arguments (text)
  "This function return a list of the arguments in the function defined in the text"
  (let (result eles params)
    (setq case-fold-search nil)
    (when (string-match "[^(]*([^()].*" (nth 0 (split-string text "[\r\n]"))) ;; verify if it is function
      (when (string-match "(\\([^()]*\\))" text) ;; get the list of parameters between "(" and ")"
        (setq result (match-string 1 text))
        (setq eles (split-string result "[[:space:]\r\n\t]*,[[:space:]\r\n\t]*")) ;; split with ","
        (setq params (map 'list (lambda (ele)
                                  (car (last (split-string ele "[[:space:]]+")))) ;; retrieve the parameter name part, without the type
                          eles))
        (when (< (length params) 2) ;; avoid list with a single empty string
          (if (equal "" (nth 0 params))
              (setq params '())))))
      params))

(defun get-java-method-throws (text)
  "This function return a list of the exceptions thrown by the function defined in the text"
  (let (result throws)
    (setq case-fold-search nil)
    (when (string-match "[^(]*([^()].*" (nth 0 (split-string text "[\r\n]"))) ;; verify if it is function
      (when (string-match "([^()]*)[[:space:]]*throws[[:space:]]*\\([^{;]*[^[:space:]]\\)[[:space:]]*[{;]" text) ;; get the list of throw clauses after keyword "throws"
        (setq result (match-string 1 text))
        (setq throws (split-string result "[[:space:]\r\n\t]*,[[:space:]\r\n\t]*")))) ;; split with ","
    throws))

(defun get-if-javadoc-has-return-clause (text)
  "This function if the Javadoc string of a Java method has a return clause (return type is non-void)"
  (let ((result nil))
    (setq case-fold-search nil)
    (when (string-match "[^(]*([^)]*).*" (nth 0 (split-string text "[\r\n]"))) ;; verify if it is function)
      (if (string-match ".*void.*" (nth 0 (split-string text "[\r\n]")))
          (setq result nil)
        (setq result t)))
    result))

;;;;;;;;;;;;;;;;
;; camel case ;;
;;;;;;;;;;;;;;;;
(defun camel-case-get-initials (text)
  "This function return the initials of each word in a camel cased expression"
  (let ((result "") (index 0))
    ; (message (format "text: %S" text))
    (setq case-fold-search nil)
    (while (setq index (string-match "\\([A-Z]\\)" text index))
      (setq index (1+ index))
      (setq result (concat result (match-string 1 text))))
    result))

;;;;;;;;;;;;;
;; recentf ;;
;;;;;;;;;;;;;
;; set the number of recent files to be saved
(setq recentf-max-saved-items 3000)

;; set files to be excluded
;; (setq recentf-exclude '("/TAGS$" "/var/tmp/"))
(require 'recentf-ext)
(global-set-key (kbd "C-x C-o") 'recentf-open-files)

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
        ("imain" "int main(int argc, char *argv[]) {\n\n}" "C-p TAB"))))
;;; java mode
(eval-after-load "cc-mode"
  '(declare-abbrevs (java-mode-abbrev-table)
       (("imain" "public static void main(String[] args) {\n\n}" "C-p TAB C-h"))))
(eval-after-load "cc-mode"
  '(declare-abbrevs (groovy-mode-abbrev-table)
       (("imain" "public static void main(String[] args) {\n\n}" "C-p TAB C-h"))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(inhibit-startup-screen t))

;; eval-and-replace
(defun eval-and-replace (value)
  "Evaluate the sexp at point and replace it with its value"
  (interactive (list (eval-last-sexp nil)))
  (kill-sexp -1)
  (insert (format "%S" value)))
(global-set-key (kbd "C-x C-r") 'eval-and-replace)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;
;; uncrustify-mode ;;
;;;;;;;;;;;;;;;;;;;;;
(require 'uncrustify-mode)
(eval-after-load "cc-mode"
  '(setq uncrustify-config-path "~/settings/bin/phl-indent.cfg"))

