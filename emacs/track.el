;; Trac wiki
;;add new projects here
(defun init-track ()
  (add-to-list 'load-path "~/emacs/w3m")
  (require 'trac-wiki)
  (require 'w3m-load)
  ;;(require 'mime-w3m)
  ;;(trac-wiki-define-multiple-project '("ROOT")
  ;;                                    "https://localhost:4443/trac/" t)
  (trac-wiki-define-project "root" "https://localhost:4443/trac/root" t)
  (autoload 'trac-wiki "trac-wiki" "Trac wiki editing entry-point." t))

(when (locate-library "trac-wiki")
  (when (locate-library "w3m-load")
    (init-track)))


