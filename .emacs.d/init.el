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

;; tool bar/scroll bar off
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; start with maximized window on Windows
;(w32-send-sys-command 61488)
(defun w32-maximize-frame ()
  "Maximize the current frame"
  (interactive)
  (w32-send-sys-command 61488))
 
(defun x11-maximize-frame ()
  "Maximize the current frame (to full screen)"
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))

(if (string-equal system-type "gnu/linux")
    (add-hook 'window-setup-hook 'x11-maximize-frame) ; GNU/Linux [Ubuntu]
  (add-hook 'window-setup-hook 'w32-maximize-frame t)) ; Windows

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

;;;;;;;;;;;;;;;
;; yasnippet ;;
;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet/")
(require 'yasnippet-config)
(yas/setup "~/.emacs.d/plugins/yasnippet")

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

;; coloring current line
(global-hl-line-mode 1)


;;;;;;;;;;;;;;;;;;;
;; col-highlight ;;
;;;;;;;;;;;;;;;;;;;
(require 'col-highlight)
;; 1 or 2 (1: always on)
(column-highlight-mode 1)
;; 2: highlight when idle
(toggle-highlight-column-when-idle 1)
(col-highlight-set-interval 6) ; highlight time in seconds


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight-indentation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'highlight-indentation)
(add-hook 'c-mode-common-hook 
          (lambda ()
            (highlight-indentation-current-column-mode)
            (highlight-indentation-mode)))

(setq radiance-dark-theme-background "#484850")
(setq dark-theme-background "black")
(setq light-theme-background "light grey")
(setq radiance-dark-theme-foreground "#000000")
(setq dark-theme-foreground "white")
(setq light-theme-foreground "black")
(setq radiance-dark-theme-hlline "#FFFFFF")
(setq dark-theme-hlline "darkolivegreen")
(setq light-theme-hlline "darkolivegreen")
(setq radiance-dark-theme-indentface "#CCCCCC")
(setq dark-theme-indentface "#FFFFFF")
(setq light-theme-indentface "#000000")
(setq radiance-dark-theme-currentindentface "#AAAAAA")
(setq dark-theme-currentindentface "#DDDDDD")
(setq light-theme-currentindentface "#222222")

(defun set-radiance-dark-theme ()
  ;;set theme to radiance dark
  (set-face-attribute 'default nil :background radiance-dark-theme-background)
  (set-face-attribute 'default nil :foreground radiance-dark-theme-foreground)
  (set-face-background 'hl-line radiance-dark-theme-hlline)
  (set-face-background 'col-highlight radiance-dark-theme-hlline)
  (set-face-background 'highlight-indentation-face radiance-dark-theme-indentface)
  (set-face-background 'highlight-indentation-current-column-face radiance-dark-theme-currentindentface))

(defun set-dark-theme ()
  ;;set theme to dark
  (set-face-attribute 'default nil :background dark-theme-background)
  (set-face-attribute 'default nil :foreground dark-theme-foreground)
  (set-face-background 'hl-line dark-theme-hlline)
  (set-face-background 'col-highlight dark-theme-hlline)
  (set-face-background 'highlight-indentation-face dark-theme-indentface)
  (set-face-background 'highlight-indentation-current-column-face dark-theme-currentindentface))
(defun set-light-theme ()
  ;;set theme to dark
  (set-face-attribute 'default nil :background light-theme-background)
  (set-face-attribute 'default nil :foreground light-theme-foreground)
  (set-face-background 'hl-line light-theme-hlline)
  (set-face-background 'col-highlight light-theme-hlline)
  (set-face-background 'highlight-indentation-face light-theme-indentface)
  (set-face-background 'highlight-indentation-current-column-face light-theme-currentindentface))
;;(set-radiance-dark-theme)
;;(set-light-theme)
;;(set-dark-theme)
(defalias 'dt 'set-dark-theme)
(defalias 'lt 'set-light-theme)
(defalias 'rdt 'set-radiance-dark-theme)

(if (window-system)
    (set-dark-theme)
  (set-face-attribute 'default nil :foreground "white"))

;;(set-face-attribute 'default nil :cursor-type "box")
(set-face-attribute 'default nil :height 100)
;;set font to ubuntu (try)
(if (string-equal system-type "gnu/linux")
    (safe-wrap (set-face-attribute 'default nil :font "Ubuntu") (message "Unable to set font!")) ; GNU/Linux [Ubuntu]
  (safe-wrap (set-face-attribute 'default nil :font "Ubuntu Mono-10:spacing=m:antialias=natural") (message "Unable to set font!"))) ; Windows
(if (string-equal system-type "gnu/linux")
    (safe-wrap (set-face-attribute 'default nil :font "Ubuntu Mono") (message "Unable to set font!"))) ; GNU/Linux [Ubuntu]
;;(set-face-attribute 'default nil :stipple "c:/Documents and Settings/LFabre/My Documents/Downloads/1328746948_crkoinarwhal_442920.jpeg")

;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(transparency '80)
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

;; Add dir to load path
(add-to-list 'load-path "~/.emacs.d/")

;; powershell-mode
(autoload 'powershell-mode "powershell-mode" "A editing mode for Microsoft PowerShell." t)
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode)) ; PowerShell script

;; c++ mode (for upcase CPP file extension)
(add-to-list 'auto-mode-alist '("\\.CPP\\'" . c++-mode)) ; C++

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
(which-func-mode 1)
;; aply which-func-mode for all major modes
(setq which-func-modes t)
;; display function name in a bar above screen
(delete (assoc 'which-func-mode mode-line-format) mode-line-format)
(setq-default header-line-format '(which-func-mode ("" which-func-format)))

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

;;;;;;;;;;;;;;;;;;;;;
;; twittering mode ;;
;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/twittering-mode-2.0.0")
(require 'twittering-mode)
;(setq twittering-status-format
;      "%C{%Y/%m/%d %H:%M:%S} %s > %T // from %f%L%r%R")
(setq twittering-icon-mode t)                ; Show icons
(setq twittering-timer-interval 300)         ; Update your timeline each 300 seconds (5 minutes)
(setq twittering-url-show-status nil)        ; Keeps the echo area from showing all the http processes
(setq twittering-number-of-tweets-on-retrieval 30)

;;;;;;;;;;;;;;;;;;;;;
;; indent settings ;;
;;;;;;;;;;;;;;;;;;;;;
(setq-default c-basic-offset 4)
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
(defun c-java-mode-untabify ()
  (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[ \t]+$" nil t)
        (delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      (if (search-forward "\t" nil t)
          (untabify (1- (point)) (point-max))))
    nil)

(add-hook 'java-mode-hook 
	  '(lambda ()
	     (make-local-variable 'write-contents-hooks)
	     (add-hook 'write-contents-hooks 'c-java-mode-untabify)))
(add-hook 'c-mode-common-hook 
	  '(lambda ()
	     (make-local-variable 'write-contents-hooks)
    	     (add-hook 'write-contents-hooks 'c-java-mode-untabify)))

;;;;;;;;;;;;;;;;;;;
;; flymake-mode ;;;
;;;;;;;;;;;;;;;;;;;
(load "flymake")
(add-hook 'java-mode-hook (lambda () (flymake-mode t)))
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
  (list "bash"
        (list "ant-emacs.sh"
              "-buildfile"
;             (concat base-dir "/" "build.xml")
              "../../build.xml"
              "compile"
              "|" "sed" "-e" "\"s/groovyc/javac/g\"")))

(add-hook 'java-mode-hook
          '(lambda ()
             (flymake-mode)))
(setq flymake-log-level 3)

(defun my-read-file (file)
  "read file contents"
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      ;(while (not (eobp)) ;;used when scanning the whole file
      (thing-at-point  'line))))
      ;(forward-line)))))

(defun flymake-vc-init ()
  (let*  ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
;       (list "cl" (list "/nologo" "/W4" "/Wp64" "/Zs" local-file))))
        (list "cl" (nconc (split-string (my-read-file "cl_settings.cl") "|") (list local-file)))))
(push '("\\.c\\'" flymake-vc-init) flymake-allowed-file-name-masks)
(push '("\\.cpp\\'" flymake-vc-init) flymake-allowed-file-name-masks)
(push '("\\.CPP\\'" flymake-vc-init) flymake-allowed-file-name-masks)

(defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
  (setq flymake-check-was-interrupted t))
(ad-activate 'flymake-post-syntax-check)

(add-hook 'c-mode-common-hook (lambda () (flymake-mode t)))


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

(global-set-key (kbd "<f7>")      'fold-dwim-toggle)
(global-set-key (kbd "<C-f7>")    'fold-dwim-hide-all)
(global-set-key (kbd "<C-S-f7>")  'fold-dwim-show-all)
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
;; enable tempbuf when opening file
;; tempbuf turned off for normal files (there are annoying
;; side effects with flymake and files opened with emacsclient)
;; (add-hook 'find-file-hooks 'turn-on-tempbuf-mode)
;; enable tempbuf when using dired
(add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)

;;;;;;;;;;;;;;;;;;;;;;;;
;; java documentation ;;
;;;;;;;;;;;;;;;;;;;;;;;;
;; ~/.emacs.d/java-doc/allclasses-noframe.html
(add-to-list 'load-path "~/.emacs.d/java-doc/")
(autoload 'java-class-doc "java-class-doc" "Open documentation for class in Java API docs" t)

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
;; (run-with-idle-timer 0.5 t 'auto-save-buffers "\\.c$" "^$") ; .c ÇæÇØëŒè€
;; (run-with-idle-timer 0.5 t 'auto-save-buffers ""   "\\.h$") ; .h ÇæÇØèúäO

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
        ("imain" "int main(int ac, char **av[]) {\n\n}" "C-p TAB")
        ("if"    "if () {\n}\n" "C-M-b C-M-q C-- C-M-d")
        ("else"  "else {\n}\n"  "C-M-b C-M-q C-M-d RET")
        ("elseif" "else if () {\n}\n" "C-M-b C-M-q C-- C-M-d")
        ("while" "while () {\n}\n" "C-M-b C-M-q C-- C-M-d")
        ("for"   "for (;;) {\n}\n" "C-M-b C-M-q C-M-b C-M-d")
        ("printf"    "printf(\"\")" "C-2 C-b"))))
;;; java mode
(eval-after-load "cc-mode"
  '(declare-abbrevs (java-mode-abbrev-table)
       (("imain" "public static void main(String[] args) {\n\n}" "C-p TAB C-h")
        ("println"   "System.out.println()" "C-b")
        ("print"   "System.out.print()" "C-b")
        ("if"    "if () {\n}\n" "C-M-b C-M-q C-- C-M-d")
        ("else"  "else {\n}\n"  "C-M-b C-M-q C-M-d RET")
        ("elseif" "else if () {\n}\n" "C-M-b C-M-q C-- C-M-d")
        ("while" "while () {\n}\n" "C-M-b C-M-q C-- C-M-d")
        ("for"   "for (;;) {\n}\n" "C-M-b C-M-q C-M-b C-M-d"))))
(eval-after-load "cc-mode"
  '(declare-abbrevs (groovy-mode-abbrev-table)
       (("imain" "public static void main(String[] args) {\n\n}" "C-p TAB C-h")
        ("println"   "System.out.println()" "C-b")
        ("print"   "System.out.print()" "C-b")
        ("if"    "if () {\n}\n" "C-M-b C-M-q C-- C-M-d")
        ("else"  "else {\n}\n"  "C-M-b C-M-q C-M-d RET")
        ("elseif" "else if () {\n}\n" "C-M-b C-M-q C-- C-M-d")
        ("while" "while () {\n}\n" "C-M-b C-M-q C-- C-M-d")
        ("for"   "for (;;) {\n}\n" "C-M-b C-M-q C-M-b C-M-d"))))
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
