;;; adjust-parens-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "adjust-parens" "adjust-parens.el" (0 0 0 0))
;;; Generated autoloads from adjust-parens.el

(autoload 'adjust-parens-mode "adjust-parens" "\
Indent and dedent Lisp code, automatically adjust close parens.

If called interactively, enable Adjust-Parens mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "adjust-parens" '("adjust-" "last-sexp-with-relative-depth" "lisp-")))

;;;***

;;;### (autoloads nil "adjust-parens-tests" "adjust-parens-tests.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from adjust-parens-tests.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "adjust-parens-tests" '("apt-check-buffer")))

;;;***

;;;### (autoloads nil nil ("adjust-parens-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; adjust-parens-autoloads.el ends here
