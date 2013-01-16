;; check spelling
(add-hook 'erlang-mode-hook 'flyspell-mode)
(add-hook 'erlang-mode 'flyspell-prog-mode 1)

;; msf
(add-hook 'erlang-mode-hook 'msf-abbrev-load)

(add-hook 'erlang-mode-hook 'delete-trailing-whitespace-hook)

;; =================== Helper functions ==================
(defun set-otp-variables ()
  "Set otp variables."
  (setq otp_version "R15B03")
  (setq otp_preffix "/opt/")
  ;;(setq erlang-root-dir (concat otp_preffix otp_version "/lib/erlang/lib/"))
  (setq erlang-root-dir (concat otp_preffix otp_version))
  (setq erlang-otp-lib (concat otp_preffix otp_version "/lib/erlang/lib/"))
  (setq erlang_mode_path
	(concat (car (directory-files erlang-otp-lib t "^tools-*")) "/emacs"))
  ;;    (concat (car (directory-files erlang-root-dir t "^tools-*")) "/emacs"))
  (setq otp_bin (concat otp_preffix otp_version "/lib/erlang/bin/"))
  (setq otp-man-path (concat erlang-root-dir "/lib/erlang/man")))

(defun init-refactorerl ()
  "Set refactorerl configuration."
  (setq refactorerl-path (concat (getenv "HOME") "/bin/refactorerl/lib/referl_ui/emacs"))
  (add-to-list 'load-path refactorerl-path)
  (message (concat "Path: " refactorerl-path " was added to load-path."))
  (when (locate-library "refactorerl")
    (require 'refactorerl))
)

(defun init-distel ()
  "Set distel configuration."
  (setq distel-path (concat (getenv "HOME") "/emacs/distel-wrangler/elisp"))
  (add-to-list 'load-path distel-path)
  (message (concat "Path: " distel-path " was added to load-path."))
  (when (locate-library "distel")
    (require 'distel))

(defun init-fun-hide ()
   (setq hs-special-modes-alist
 	(cons '(erlang-mode
 		"^\\([a-z][a-zA-Z0-9_]*\\|'[^\n']*[^\\]'\\)\\s *(" nil "%"
 		erlang-end-of-clause) hs-special-modes-alist))
   (hs-minor-mode 1)
   (local-set-key [?\M-s] 'hs-toggle-hiding)
   (local-set-key [?\M-h] 'hs-hide-all)
   (local-set-key [?\M-u] 'hs-show-all))

(defun init-shell ()
  (add-hook 'erlang-shell-mode-hook 'erlang-shell-init)
  (defun erlang-shell-init ()
    (setq comint-dynamic-complete-functions
	  '(erl-nodename-complete  comint-replace-by-expanded-history)))
  (defun erl-nodename-complete ()
    "Call erl-complete if we have an Erlang node name"
    (if erl-nodename-cache
        (erl-complete erl-nodename-cache)
      nil))
  (defconst distel-shell-keys
    '(("\C-\M-i"   erl-complete)
      ("\M-?"      erl-complete)
      ("\M-."      erl-find-source-under-point)
      ("\M-,"      erl-find-source-unwind)
      ("\M-*"      erl-find-source-unwind)
      )
    "Additional keys to bind when in Erlang shell.")

  (add-hook 'erlang-shell-mode-hook
	    (lambda ()
	      ;; add some Distel bindings to the Erlang shell
	      (dolist (spec distel-shell-keys)
		(define-key erlang-shell-mode-map (car spec) (cadr spec)))))
  (distel-setup))
)

;; ===================== flymake ====================================
(defun flymake-create-temp-intemp (file-name prefix)
  "Return file name in temporary directory for checking FILE-NAME.
This is a replacement for `flymake-create-temp-inplace'. The
difference is that it gives a file name in
`temporary-file-directory' instead of the same directory as
FILE-NAME.

For the use of PREFIX see that function.

Note that not making the temporary file in another directory
\(like here) will not if the file you are checking depends on
relative paths to other files \(for the type of checks flymake
makes)."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((name (concat
                (file-name-nondirectory
                 (file-name-sans-extension file-name))
                "_" prefix))
         (ext  (concat "." (file-name-extension file-name)))
         (temp-name (make-temp-file name nil ext))
         )
    (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
    temp-name))

(defun flymake-erlang-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-intemp))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "~/emacs/bin/eflymake" (list local-file))))

(defun init-flymake ()
	(when (locate-library "flymake")
		(require 'flymake)
		(add-to-list 'flymake-allowed-file-name-masks
								 '("\\.erl\\'" flymake-erlang-init))
		(flymake-mode 1))
)
;; ------------------------------------------------------------------

;; esense specific
(defun init-esense ()
  "Set esense configuration."
  (add-to-list 'load-path "~/emacs/esense")
  (require 'esense-start)
  (setq esense-indexer-program "~/emacs/esense/esense.sh"))

(when (file-exists-p "~/emacs/esense/esense.sh")
  (init-esense))

(defun init-erlang ()
  (setq indent-tabs-mode nil)
  (setq-default tab-width 2)
	(setq erlang-indent-level 2)
  (setq inferior-erlang-machine-options '("-sname" "emacs"))
  ;; add Erlang functions to an imenu menu
  (imenu-add-to-menubar "imenu")
  (init-fun-hide)
  (init-shell)
  (init-flymake)
)
(add-hook 'erlang-mode-hook 'init-erlang)

;; ========================== MAIN ==============================
(set-otp-variables)
;;(message otp-man-path)
(add-to-list 'load-path erlang_mode_path)
;;(setq load-path (cons erlang_mode_path load-path))
(message (concat "Path: " erlang_mode_path " was added to load-path."))
(add-to-list 'exec-path otp_bin)
(message (concat "Path: " otp_bin " was added to exec-path."))
(require 'erlang-start)
(init-distel)
(init-refactorerl)

;; ====================== END OF MAIN ===========================
(message otp-man-path)
(defun get-erl-man ()
  (interactive)
  (let* ((man-path otp-man-path) ;;“/opt/R11B-5/lib/erlang/man”)
         (man-args (format “-M %s %s” man-path (current-word))))
    (man man-args)))

(global-set-key [(f6)] (lambda () (interactive) (get-erl-man)))


;; Key bindings
 (global-set-key "\M-\C-i" 'erlang-indent-current-buffer)

;;  ;; Distel
;;  (defvar erlang-distel-path
;;    (concat (getenv "HOME") "/emacs/distel-wrangler/elisp"))
;;  (add-to-list 'load-path erlang-distel-path)

;; use to start an erlang shell with boot flags
(defun erl-shell (flags)
  "Start an erlang shell with flags"
  (interactive (list (read-string "Flags: ")))
  (set 'inferior-erlang-machine-options (split-string flags))
  (erlang-shell))

;; (add-hook 'erlang-shell-mode-hook 'my-erlang-load-hook)
;; ;; when loading a beam file from emacs, add the path to erlang
;;   (setq erl-reload-dwim t)
;; ;; distel The Right Way(tm)
;;    (local-set-key [(meta l)] 'erl-find-mod)
;;    (local-set-key [(meta \()] 'erl-openparent)
;;    (local-set-key [(meta /)]  'erl-complete)
;;    (local-set-key [(control x) (\?)] 'erlang-man-function)

;;    ;; make hack for compile command
;;     ;; uses Makefile if it exists, else looks for ../inc & ../ebin
;;    (unless (null buffer-file-name)
;;      (make-local-variable 'compile-command)
;;      (setq compile-command
;;  	  (if (file-exists-p "Makefile") "make -k"
;;  	    (concat
;;  	     (concat
;;  	      "erlc "
;;  	      (concat
;;  	       (if (file-exists-p "../ebin") "-o ../ebin " "")
;;  	       (if (file-exists-p "../inc") "-I ../inc " ""))
;;  	      "+debug_info -W " buffer-file-name))))))

 ;; A number of the erlang-extended-mode key bindings are useful in the shell too

;==== help for erlang will be accessible on F6 ==============
;;(defun get-erl-man ()
;;  (interactive)
;;  (let* ((man-path “/opt/R11B-5/lib/erlang/man”)
;;         (man-args (format “-M %s %s” man-path (current-word))))
;;    (man man-args)))

(global-set-key [(f6)] (lambda () (interactive) (get-erl-man)))
;=================================================