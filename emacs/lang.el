;; UTF-8 specific
;; Enable UTF-8 environment
(set-language-environment 'UTF-8)

;;-----------------------------------------------------------------------------
;; ================
;; Home
;; ----------------
;;(set-default-font"-*-fixed-medium-r-normal-*-20-*-*-*-*-*-iso10646-1")
(set-frame-font "DejaVu Sans Mono")

;; ================
;; Work
;; ----------------
;;(set-default-font "-*-fixed-medium-r--*-20-*-*-*-m-*-iso10646-1")
;;(set-default-font "-*-terminal-medium-r--*-20-*-*-*-m-*-iso10646-1")
;;-----------------------------------------------------------------------------

;;(set-default-font "fontset-standard")

;; UTF-8 for screen output
(set-terminal-coding-system 'utf-8)
;; UTF-8 for input from keyboard
(set-keyboard-coding-system 'utf-8)
;; UTF-8 for clipboard (does not work with emacs 21!)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
;; Keyboard layout "as in Windows" (does not work for emacs 21!)
(setq default-input-method 'russian-computer)

(prefer-coding-system 'mule-utf-8)



;;(setup-Cyrillic-ISO-environment)
;;(custom-set-variables
;; '(current-language-environment "UTF-8")
;; '(default-input-method "russian-computer")
;; '(x-select-enable-clipboard t)
;;)

;;(set-input-method "russian-computer" nil)


;; (set-language-environment 'Russian UTF-8)
;; (set-language-info "Russian UTF-8" 'tutorial "TUTORIAL.ru")
;; (set-language-info "Russian UTF-8" 'documentation "Support for Russian using utf-8 and the russian-computer input method.")
;; (set-language-info "Russian UTF-8" 'sample-text "Russian (Ú’””À… ) ˙ƒ“¡◊”‘◊’ ‘≈!")
;; (set-language-info "Russian UTF-8" 'coding-priority '(mule-utf-8 cyrillic-koi8 cyrillic-iso-8bit))
;; (set-language-info "Russian UTF-8" 'coding-system '(mule-utf-8))
;; (set-language-info "Russian UTF-8" 'unibyte-display 'mule-utf-8)
;; (set-language-info "Russian UTF-8" 'charset '(mule-utf-8))
;; (set-language-info "Russian UTF-8" 'input-method "russian-computer")
;; (set-language-info "Russian UTF-8" 'setup-function (lambda nil
;; (if (and (fboundp 'w32-add-charset-info)
;; (not
;; (boundp 'w32-unicode-charset-defined)))
;; (w32-add-charset-info "iso10646-1" 'w32-charset-ansi t))))


;; Support for codepages cp866 and cp1251
;;(codepage-setup 1251)
;;(define-coding-system-alias 'windows-1251 'cp1251)
;;(codepage-setup 866)
;; Codepage autodetection
;; prefer-coding-system places codepage in the begining of the lilst of prefered codepages
;; So in this case first codepage will be utf-8-unix
;;(prefer-coding-system 'cp866)
;;(prefer-coding-system 'koi8-r-unix)
;;(prefer-coding-system 'windows-1251-dos)
;; Font replacing
;;  (create-fontset-from-fontset-spec
;;     "-b&h-*-medium-r-*-*-12-*-*-*-*-*-fontset-unicode,
;;  latin-iso8859-1:-b&h-*-medium-r-*-*-12-*-*-*-*-*-iso8859-1,
;;  latin-iso8859-15:-b&h-*-medium-r-*-*-12-*-*-*-*-*-iso8859-15,
;;  cyrillic-iso8859-5:-b&h-*-medium-r-*-*-12-*-*-*-*-*-iso8859-5" t)
;; by default
;;(set-default-font "fontset-unicode")
;; by default for frames
;;(add-to-list ‚Äôdefault-frame-alist ‚Äô(font . "fontset-unicode"))
