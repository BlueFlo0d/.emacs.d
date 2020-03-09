;;; autotetris-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "autotetris-mode" "autotetris-mode.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from autotetris-mode.el

(autoload 'autotetris-mode "autotetris-mode" "\
Automatically play tetris in the current buffer.

\(fn &optional ARG)" t nil)

(autoload 'autotetris "autotetris-mode" "\
Automatically play a game of tetris.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "autotetris-mode" '("autotetris-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; autotetris-mode-autoloads.el ends here
