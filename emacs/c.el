;=================================================
; C & C++ & Java specific
;-----------------
(defun my-bind-clb ()
  (define-key c-mode-base-map (kbd "RET") 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-bind-clb)

(defun my-indent-setup ()
  (c-set-offset 'arglist-intro '8)
  (c-set-offset 'arglist-cont '0)
  (setq-default c-basic-offset 2
		tab-width 2
		;; Use spaces instead of tabs
		indent-tabs-mode nil) 
  )
(add-hook 'c-mode-hook 'my-indent-setup)
(add-hook 'c-mode-hook 'delete-trailing-whitespace-hook)

; "gnu" | "k&r" | "linux" | "bsd" | "stroustrup" | "python" | "java" | "user"
(setq c-default-style "k&r") 
(global-set-key (kbd "C-c u") 'uncomment-region)
;=================================================
