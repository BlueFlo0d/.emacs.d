;;; helm-sly-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helm-sly" "helm-sly.el" (0 0 0 0))
;;; Generated autoloads from helm-sly.el

(autoload 'helm-sly-list-connections "helm-sly" "\
List Lisp connections with Helm.

\(fn)" t nil)

(autoload 'helm-sly-apropos "helm-sly" "\
Yet another Apropos with `helm'.

\(fn)" t nil)

(autoload 'helm-sly-mode "helm-sly" "\
Use Helm for Lisp xref selections.
Note that the local minor mode has a global effect, thus making
`global-helm-sly-mode' and `helm-sly-mode' equivalent.

\(fn &optional ARG)" t nil)

(defvar global-helm-sly-mode nil "\
Non-nil if Global Helm-Sly mode is enabled.
See the `global-helm-sly-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-helm-sly-mode'.")

(custom-autoload 'global-helm-sly-mode "helm-sly" nil)

(autoload 'global-helm-sly-mode "helm-sly" "\
Toggle Helm-Sly mode in all buffers.
With prefix ARG, enable Global Helm-Sly mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Helm-Sly mode is enabled in all buffers where
`helm-sly-mode' would do it.
See `helm-sly-mode' for more information on Helm-Sly mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-sly" '("helm-sly-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-sly-autoloads.el ends here
