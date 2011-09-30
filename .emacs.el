(add-to-list 'load-path "~/emacs")

(defun delete-trailing-whitespace-hook ()
    (setq show-trailing-whitespace t)
    (make-local-hook 'before-save-hook)
    (add-hook 'before-save-hook 'delete-trailing-whitespace))

(when (file-exists-p "~/emacs/lang.el")
  (load "~/emacs/lang.el"))

(when (file-exists-p "~/emacs/priv.el")
  (load "~/emacs/priv.el"))


;; Access to remote files
(defun init-tramp ()
  "Set tramp configuration."
  (setq tramp-default-user "khia")
  (setq tramp-default-method "ssh")
  (require 'tramp))  
(when (locate-library "tramp")
  (init-tramp))

(set 'man-path "/usr/share/man")

;; ============================
;; Printer
;; ============================
;;(setq printer-name "Phaser_6350DT")
;;printer-name
(setq printer-name "PDF")

;; ============================
;; Look & Feel
;; ============================
;; disable startup message
(setq inhibit-startup-message t)
;; Misc
;;    (global-set-key [C-tab] "\C-q\t")   ; Control tab quotes a tab.

;; Tag Completion
(global-set-key [C-tab] 'complete-tag)   ; Control tab quotes a tab.
(setq tags-table-list
           '("~/emacs" "~/dev"))

(global-set-key "\M-\C-g" 'goto-line)
(setq visible-bell t)
;; Highlight the current line
(global-hl-line-mode 1)
;; Show paren
(show-paren-mode 1)
;; Status line
(setq-default column-number-mode t)
(setq-default line-number-mode t)
;; Window size
(enlarge-window 25)
;;(enlarge-window-horizontally 80)

;; filename in the window title
;;(setq frame-title-format '(buffer-file-name "%b - Emacs" "%b - Emacs"))
(setq frame-title-format '
      (buffer-file-name "%f - Emacs" 
			(dired-directory dired-directory "%b")))
;;(setq icon-title-format '(buffer-file-name "%b - Emacs" "%b - Emacs"))


;; ========== Line by line scrolling ==========
;; This makes the buffer scroll by only a single line when the up or
;; down cursor keys push the cursor (tool-bar-mode) outside the
;; buffer. The standard emacs behaviour is to reposition the cursor in
;; the center of the screen, but this can make the scrolling confusing
(setq scroll-step 1)

;; enable visual feedback on selections
(setq transient-mark-mode t)

;; check spelling
(defun init-spell-checking ()
  (autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
  (add-hook 'text-mode-hook 'flyspell-mode)

  (add-hook 'c-mode 'flyspell-mode)
  ;;(autoload 'flyspell-delay-command "flyspell" "Delay on command." t) 
  ;;(autoload 'text-mode-flyspell-verify "flyspell" "" t) 
  ;;(flyspell-mode 1)
  (global-set-key [backtab] 'ispell-complete-word)
  (setq-default ispell-program-name "aspell")
  (setq flyspell-delay '3))
(init-spell-checking)

;; WoMan
(defun my-woman-pre-format-fn ()
  "Function added to `woman-pre-format-hook'."
;;  (copy-face 'my-Man-overstrike-face 'woman-bold-face)
;;  (copy-face 'my-Man-underline-face 'woman-italic-face)
  (face-spec-set 'woman-addition-face '((t (:foreground "orange" :background "pink"))))
  (face-spec-set 'woman-unknown-face  '((t (:foreground "cyan" :background "pink")))))

(add-hook 'woman-pre-format-hook 'my-woman-pre-format-fn)

;; So that each instance will pop up a new frame.
;; Maybe `special-display-regexps' would be better?
(add-hook 'woman-post-format-hook (lambda () (setq woman-frame nil)))


;(defun get-user-mail-address ()
;  user-mail-address
;)  
;(get-user-mail-address)
  
;; Snippets
(defun init-msf-abbrev ()
  "Set msf-abbrev configuration."
  (require 'msf-abbrev)
  (setq-default abbrev-mode t)
  (setq save-abbrevs nil)
  (setq msf-abbrev-root "~/emacs/abb")
  (global-set-key (kbd "C-c a") 'msf-abbrev-define-new-abbrev-this-mode)
  (global-set-key (kbd "C-c s") 'abbrev-mode)
  (global-set-key (kbd "C-x a u") 'unexpand-abbrev))
(when (locate-library "msf-abbrev")
  (init-msf-abbrev))

;; dvc - distributed version control system incldes support for hg
(defun init-dvc ()
  "Set dvc configuration."
  (add-to-list 'load-path "~/emacs/dvc")
  (add-to-list 'Info-default-directory-list "/usr/local/share/info/")
  (require 'dvc-autoloads))
(when (file-exists-p "~/emacs/dvc")
  (init-dvc))

; disable vc (version control) module as we have DVC (distributed version control) installed
;(setq vc-handled-backends nil)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; internal functions 
(defun generate-tag-table (dir)
  "Generate tag tables under current directory(Linux)."
  (interactive "DGnerate tags in: ")
  (setq exp
;;        (read-from-minibuffer "suffix: " "*.[chCH]*"))
        (read-from-minibuffer "suffix: " "*.[eh]rl"))
  (with-temp-buffer
    (shell-command
     (concat "find " dir " -name \"" exp "\" -print | xargs etags -o " dir "/TAGS -")
     ))
  (add-to-list 'tags-table-list dir))

;; for changing default compile command uncoment next line
;; (defvar compile-command "scons") 
;; OR place next lines into stub Makefile
;; main:
;;    * scons 
;;; SCons builds into a 'build' subdir, but we want to find the errors
;;; in the regular source dir.  So we remove build/XXX/YYY/{dbg,final}/ from the
;;; filenames.
(defun process-error-filename (filename)
  (let ((case-fold-search t))
    (setq f (replace-regexp-in-string
             "[Ss]?[Bb]uild[\\/].*\\(final\\|dbg\\)[^\\/]*[\\/]" "" filename))
    (cond ((file-exists-p f)
           f)
          (t filename))))

(setq compilation-parse-errors-filename-function 'process-error-filename)

(when (file-exists-p "~/emacs/elisp")
    (add-to-list 'load-path "~/emacs/elisp"))

;; Desktop mode
; save sessions
;(load "desktop")
;(desktop-save-mode t)
(defun close-all-buffers ()
    (interactive)
    (mapc 'kill-buffer (buffer-list)))

;; Save temp file in different directory
;; make-directory
(defun init-backups ()
  (unless (file-exists-p "~/emacs/backups") (make-directory "~/emacs/backups"))
  (setq backup-directory "~/emacs/backups")
  (setq backup-directory-alist
	`((".*" . ,backup-directory)))
  (setq auto-save-file-name-transforms
	`((".*" ,backup-directory t)))
)
(init-backups)

(defun init-color-theme ()
  (add-to-list 'load-path "~/emacs/color-theme")
  (require 'color-theme)
  (color-theme-initialize)
  (color-theme-bharadwaj-slate))

(when (file-exists-p "~/emacs/color-theme")
  (init-color-theme))

;; =================================================
;; Modes specific configuration
;; ----------------------------

;; C & C++ & Java specific
(when (file-exists-p "~/emacs/c.el")
  (load "~/emacs/c.el"))

;;; graphwiz mode
(autoload 'graphviz-dot-mode "graphviz-dot-mode" "graphviz-dot Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))

;; erlang specific
(when (file-exists-p "~/emacs/erlang.el")
  (load "~/emacs/erlang.el"))

;; SCONS
 (setq auto-mode-alist
      (cons '("SConstruct" . python-mode) auto-mode-alist))
 (setq auto-mode-alist
      (cons '("SConscript" . python-mode) auto-mode-alist))

(when (file-exists-p "~/emacs/track.el")
  (load "~/emacs/track.el"))


