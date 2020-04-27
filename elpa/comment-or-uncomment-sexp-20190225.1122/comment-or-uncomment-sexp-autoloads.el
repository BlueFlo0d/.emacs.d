;;; comment-or-uncomment-sexp-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "comment-or-uncomment-sexp" "comment-or-uncomment-sexp.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from comment-or-uncomment-sexp.el

(autoload 'comment-or-uncomment-sexp "comment-or-uncomment-sexp" "\
Comment the sexp at point and move past it.
If already inside (or before) a comment, uncomment instead.
With a prefix argument N, (un)comment that many sexps.

\(fn &optional N)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "comment-or-uncomment-sexp" '("comment-or-uncomment-sexp-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; comment-or-uncomment-sexp-autoloads.el ends here
