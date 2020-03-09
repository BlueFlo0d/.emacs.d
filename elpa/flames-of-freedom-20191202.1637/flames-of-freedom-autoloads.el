;;; flames-of-freedom-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flames-of-freedom" "flames-of-freedom.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from flames-of-freedom.el

(autoload 'flames-of-freedom-my-message "flames-of-freedom" "\
Displays the Flames of Freedom.

These are the eternal flames of freedom (and an homage to RMS who
is having tough times in this year 2019).

THE-MESSAGE is displayed.  It is a list of sentences separated by
\"|\".  If you just want to stare at a comforting fire, just
leave the message empty.

If TESTING is set, then some debugging information is displayed.

\(fn &optional THE-MESSAGE TESTING)" t nil)

(autoload 'flames-of-freedom-default "flames-of-freedom" "\
Displays the Flames of Freedom.

These are the eternal flames of freedom (and an homage to RMS who
is having tough times in this year 2019).

A little poem is displayed.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flames-of-freedom" '("flames-of-freedom-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flames-of-freedom-autoloads.el ends here
