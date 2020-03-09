;;; sly-asdf-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "sly-asdf" "sly-asdf.el" (0 0 0 0))
;;; Generated autoloads from sly-asdf.el

(with-eval-after-load 'sly (add-to-list 'sly-contribs 'sly-asdf 'append))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sly-asdf" '("sly-asdf")))

;;;***

;;;### (autoloads nil "sly-asdf-flymake" "sly-asdf-flymake.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from sly-asdf-flymake.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sly-asdf-flymake" '("sly-asdf-" "create-flymake-report-fn" "*sly-asdf-")))

;;;***

;;;### (autoloads nil "sly-asdf-flymake-base" "sly-asdf-flymake-base.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from sly-asdf-flymake-base.el

(autoload 'sly-asdf-flymake-base-log "sly-asdf-flymake-base" "\
Log, at level LEVEL, the message MSG formatted with ARGS.
LEVEL is passed to `display-warning', which is used to display
the warning.  If this form is included in a byte-compiled file,
the generated warning contains an indication of the file that
generated it.

\(fn LEVEL MSG &rest ARGS)" nil t)

(autoload 'sly-asdf-flymake-base-make-diagnostic "sly-asdf-flymake-base" "\
Make a Sly-Asdf-Flymake-Base diagnostic for BUFFER's region from BEG to END.
TYPE is a key to `sly-asdf-flymake-base-diagnostic-types-alist' and TEXT is a
description of the problem detected in this region.

\(fn BUFFER BEG END TYPE TEXT)" nil nil)

(autoload 'sly-asdf-flymake-base-diagnostics "sly-asdf-flymake-base" "\
Get Sly-Asdf-Flymake-Base diagnostics in region determined by BEG and END.

If neither BEG or END is supplied, use the whole buffer,
otherwise if BEG is non-nil and END is nil, consider only
diagnostics at BEG.

\(fn &optional BEG END)" nil nil)

(autoload 'sly-asdf-flymake-base-diag-region "sly-asdf-flymake-base" "\
Compute BUFFER's region (BEG . END) corresponding to LINE and COL.
If COL is nil, return a region just for LINE.  Return nil if the
region is invalid.

\(fn BUFFER LINE &optional COL)" nil nil)

(autoload 'sly-asdf-flymake-base-mode "sly-asdf-flymake-base" "\
Toggle Sly-Asdf-Flymake-Base mode on or off.
With a prefix argument ARG, enable Sly-Asdf-Flymake-Base mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.

Sly-Asdf-Flymake-Base is an Emacs minor mode for on-the-fly syntax checking.
Sly-Asdf-Flymake-Base collects diagnostic information from multiple sources,
called backends, and visually annotates the buffer with the
results.

Sly-Asdf-Flymake-Base performs these checks while the user is editing.  The
customization variables `sly-asdf-flymake-base-start-on-sly-asdf-flymake-base-mode',
`sly-asdf-flymake-base-no-changes-timeout' and
`sly-asdf-flymake-base-start-syntax-check-on-newline' determine the exact
circumstances whereupon Sly-Asdf-Flymake-Base decides to initiate a check of
the buffer.

The commands `sly-asdf-flymake-base-goto-next-error' and
`sly-asdf-flymake-base-goto-prev-error' can be used to navigate among Sly-Asdf-Flymake-Base
diagnostics annotated in the buffer.

The visual appearance of each type of diagnostic can be changed
in the variable `sly-asdf-flymake-base-diagnostic-types-alist'.

Activation or deactivation of backends used by Sly-Asdf-Flymake-Base in each
buffer happens via the special hook
`sly-asdf-flymake-base-diagnostic-functions'.

Some backends may take longer than others to respond or complete,
and some may decide to disable themselves if they are not
suitable for the current buffer. The commands
`sly-asdf-flymake-base-running-backends', `sly-asdf-flymake-base-disabled-backends' and
`sly-asdf-flymake-base-reporting-backends' summarize the situation, as does the
special *Sly-Asdf-Flymake-Base log* buffer.

\(fn &optional ARG)" t nil)

(autoload 'sly-asdf-flymake-base-mode-on "sly-asdf-flymake-base" "\
Turn Sly-Asdf-Flymake-Base mode on.

\(fn)" nil nil)

(autoload 'sly-asdf-flymake-base-mode-off "sly-asdf-flymake-base" "\
Turn Sly-Asdf-Flymake-Base mode off.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sly-asdf-flymake-base" '("sly-asdf-flymake-base-")))

;;;***

;;;### (autoloads nil nil ("sly-asdf-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sly-asdf-autoloads.el ends here
