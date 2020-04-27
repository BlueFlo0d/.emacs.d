;;; company-fuzzy-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "company-fuzzy" "company-fuzzy.el" (0 0 0 0))
;;; Generated autoloads from company-fuzzy.el

(autoload 'company-fuzzy-mode "company-fuzzy" "\
Minor mode 'company-fuzzy-mode'.

\(fn &optional ARG)" t nil)

(defvar global-company-fuzzy-mode nil "\
Non-nil if Global Company-Fuzzy mode is enabled.
See the `global-company-fuzzy-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-company-fuzzy-mode'.")

(custom-autoload 'global-company-fuzzy-mode "company-fuzzy" nil)

(autoload 'global-company-fuzzy-mode "company-fuzzy" "\
Toggle Company-Fuzzy mode in all buffers.
With prefix ARG, enable Global Company-Fuzzy mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Company-Fuzzy mode is enabled in all buffers where
`company-fuzzy-turn-on-company-fuzzy-mode' would do it.
See `company-fuzzy-mode' for more information on Company-Fuzzy mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-fuzzy" '("company-fuzzy-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; company-fuzzy-autoloads.el ends here
