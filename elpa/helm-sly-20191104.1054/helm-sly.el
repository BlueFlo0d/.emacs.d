;;; helm-sly.el --- Helm sources and some utilities for SLY. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;
;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-helm/helm-sly
;; Package-Version: 20191104.1054
;; Version: 0.4.1
;; Keywords: convenience, helm, sly, lisp
;; Package-Requires: ((emacs "25.1") (helm "3.2") (cl-lib "0.5") (sly "0.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; A Helm for using SLY.
;;
;; The complete command list:
;;
;;  `helm-sly-list-connections'
;;    Yet another Lisp connection list with `helm'.
;;  `helm-sly-apropos'
;;    Yet another `apropos' with `helm'.
;;  `helm-sly-mini'
;;    Like ~helm-sly-list-connections~, but include an extra source of
;;    Lisp-related buffers, like the events buffer or the scratch buffer.

;;; Installation:
;;
;; Add helm-sly.el to your load-path.
;; Set up SLY properly.
;;
;; To use Helm instead of the Xref buffer, enable `global-helm-sly-mode'.
;;
;; To enable Helm for completion, two options:
;;
;; - Without company:
;;
;;   (add-hook 'sly-mrepl-hook #'helm-sly-disable-internal-completion)
;;
;;   If you want fuzzy-matching:
;;
;;   (setq helm-completion-in-region-fuzzy-match t)
;;
;;
;; - With company, install helm-company
;;   (https://github.com/Sodel-the-Vociferous/helm-company), then:
;;
;;   (add-hook 'sly-mrepl-hook #'company-mode)
;;   (require 'helm-company)
;;   (define-key sly-mrepl-mode-map (kbd "<tab>") 'helm-company)

;;; Code:

(require 'helm)
(require 'helm-buffers)
(require 'sly)
(require 'cl-lib)

(declare-function sly-mrepl--find-buffer "sly-mrepl.el")
(declare-function sly-mrepl-new "sly-mrepl.el")

(defun helm-sly-disable-internal-completion ()
  "Disable SLY own's completion system, e.g. to use Helm instead.
This is mostly useful when added to `sly-mrepl-hook'."
  (sly-symbol-completion-mode -1))

(defgroup helm-sly nil
  "Helm sources and some utilities for SLY."
  :prefix "helm-sly-"
  :group 'helm
  :link '(url-link :tag "GitHub" "https://github.com/emacs-helm/helm-sly"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar helm-sly-connections-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-D") 'helm-sly-run-delete-buffers)
    (define-key map (kbd "M-R") 'helm-sly-run-rename-connection-buffer)
    map)
  "Keymap for Lisp connection source in Helm.")

(defun helm-sly-output-buffer (&optional connection)
  "Generic wrapper for CONNECTION output buffer."
  (sly-mrepl--find-buffer (or connection (sly-current-connection))))

(defun helm-sly-go-to-repl (_candidate)
  "Switched to marked REPL(s)."
  (helm-window-show-buffers
   (mapcar (lambda (candidate)
             (let ((buffer (nth 1 candidate))
                   (connection (nth 0 candidate)))
               (unless buffer
                 (sly-mrepl-new connection))
               buffer))
           (helm-marked-candidates))))
(put 'helm-sly-go-to-repl 'helm-only t)

(defun helm-sly-process (connection)
  "Generic wrapper for CONNECTION process."
  (sly-process connection))

(defun helm-sly-connection-number (connection)
  "Generic wrapper for CONNECTION number."
  (sly-connection-number connection))

(defun helm-sly-connection-name (connection)
  "Generic wrapper for CONNECTION name."
  (sly-connection-name connection))

(defun helm-sly-pid (connection)
  "Generic wrapper for CONNECTION pid."
  (sly-pid connection))

(defun helm-sly-implementation-type (connection)
  "Generic wrapper for CONNECTION implementation type."
  (sly-lisp-implementation-type connection))

(defun helm-sly-debug-buffers (connection)
  "Generic wrapper for CONNECTION debug buffers."
  (sly-db-buffers connection))

(defun helm-sly-buffer-connection (buffer)
  "Generic wrapper for BUFFER's connection."
  (when (bufferp buffer)
    (with-current-buffer buffer
      sly-buffer-connection)))

(defun helm-sly-go-to-inferior (_candidate)
  "Switched to inferior Lisps associated with the marked connections."
  (helm-window-show-buffers
   (cl-loop for c in (helm-marked-candidates)
            collect (process-buffer (helm-sly-process
                                     (helm-sly-buffer-connection c))))))
(put 'helm-sly-go-to-inferior 'helm-only t)

(defun helm-sly-go-to-debug (_candidate)
  "Switched to debug buffers associated with the marked connections."
  (helm-window-show-buffers
   (cl-loop for c in (helm-marked-candidates)
            append (helm-sly-debug-buffers (car c)))))
(put 'helm-sly-go-to-debug 'helm-only t)

(defun helm-sly-run-delete-buffers ()
  "Run `helm-sly-delete-buffers' action from `helm-sly--c-source-connection'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-sly-delete-buffers)))
(put 'helm-sly-run-delete-buffers 'helm-only t)

(defun helm-sly-set-default-connection (candidate)
  "Set connection to use by default to that of CANDIDATE buffer."
  (let ((connection (car candidate)))
    (sly-select-connection connection)))
(put 'helm-sly-rename-connection-buffer 'helm-only t)

(defun helm-sly--net-processes ()
  "Generic wrapper around connection processes."
  sly-net-processes)

(defun helm-sly-delete-buffers (&optional _candidate)
  "Kill marked REPL(s).
Kill their inferior Lisps as well if they are the last buffer
connected to it."
  (let ((repl-buffers (helm-sly--repl-buffers)))
    (dolist (c (helm-marked-candidates))
      (let ((last-connection?
             (not (memq (car c)
                        (mapcar #'helm-sly-buffer-connection
                                (delete (cadr c) repl-buffers))))))
        (when last-connection?
          (let ((sly-dispatching-connection (car c)))
            (sly-quit-lisp t)))
        (kill-buffer (cadr c))))))
(put 'helm-sly-delete-buffers 'helm-only t)

(defun helm-sly-restart-connections (_candidate)
  "Restart marked REPLs' inferior Lisps."
  (dolist (c (helm-marked-candidates))
    (sly-restart-connection-at-point (helm-sly-buffer-connection c))))
(put 'helm-sly-restart-connections 'helm-only t)

(defun helm-sly-run-rename-connection-buffer ()
  "Run `helm-sly-rename-connection-buffer' action from `helm-sly--c-source-connection'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-sly-rename-connection-buffer)))
(put 'helm-sly-run-rename-connection 'helm-only t)

(defun helm-sly-rename-connection-buffer (candidate)
  "Rename REPL buffer for CANDIDATE."
  (let* ((sly-dispatching-connection (car candidate)))
    (when (cadr candidate)
      (with-current-buffer (cadr candidate)
        (rename-buffer (helm-read-string "New name: " (buffer-name)))))))
(put 'helm-sly-rename-connection-buffer 'helm-only t)

(defcustom helm-sly-connection-actions
  `(("Go to REPL" . helm-sly-go-to-repl)
    ("Set default" . helm-sly-set-default-connection)
    ("Restart" . helm-sly-restart-connections)
    (,(substitute-command-keys "Rename REPL buffer \\<helm-sly-connections-map>`\\[helm-sly-run-rename-connection-buffer]'")
     . helm-sly-rename-connection-buffer)
    (,(substitute-command-keys "Quit \\<helm-sly-connections-map>`\\[helm-sly-run-delete-buffers]'")
     . helm-sly-delete-buffers)
    ("Go to inferior Lisp" . helm-sly-go-to-inferior)
    ("Go to debug buffers" . helm-sly-go-to-debug))
  "Actions for `helm-sly-list-connections`."
  :group 'helm-sly
  :type '(alist :key-type string :value-type function))

(defun helm-sly--connection-candidates (p &optional buffer)
  "Return (DISPLAY-VALUE . REAL-VALUE) for connection P.
The REAL-VALUE is (P BUFFER)."
  (setq buffer (or buffer
                   (helm-sly-output-buffer p)))
  (let ((fstring "%s%2s  %-10s  %-17s  %-7s %-s %s"))
    (cons
     (format fstring
             (if (eq sly-default-connection p)
                 "*"
               " ")
             (helm-sly-connection-number p)
             (helm-sly-connection-name p)
             (or (process-id p) (process-contact p))
             (helm-sly-pid p)
             (helm-sly-implementation-type p)
             buffer)
     (list p buffer))))

(defun helm-sly--repl-buffers (&optional connection thread)
  "Return the list of buffers association to CONNECTION and/or THREAD.
Inspired by `sly-mrepl--find-buffer'."
  (cl-remove-if-not
   (lambda (x)
     (with-current-buffer x
       (and (eq major-mode 'sly-mrepl-mode)
            (or (not connection)
                (eq sly-buffer-connection
                    connection))
            (or (not thread)
                (eq thread sly-current-thread)))))
   (buffer-list)))

(defun helm-sly--repl-buffer-candidates ()
  "Return buffer/connection candidates.
It returns all REPL buffer candidates + connections without buffers."
  (let* ((repl-buffers (helm-sly--repl-buffers))
         (buffer-connections (cl-delete-duplicates
                              (mapcar #'helm-sly-buffer-connection
                                      repl-buffers))))
    (append (mapcar (lambda (b)
                      (helm-sly--connection-candidates
                       (helm-sly-buffer-connection b)
                       b))
                    repl-buffers)
            (mapcar #'helm-sly--connection-candidates
                    (cl-set-difference
                     (reverse (helm-sly--net-processes))
                     buffer-connections)))))

(defun helm-sly--c-source-connection ()
  "Build Helm source of Lisp connections."
  (helm-build-sync-source "Lisp connections"
    :candidates (helm-sly--repl-buffer-candidates)
    :action helm-sly-connection-actions
    :keymap helm-sly-connections-map))

;;;###autoload
(defun helm-sly-list-connections ()
  "List Lisp connections with Helm."
  (interactive)
  (helm :sources (list (helm-sly--c-source-connection))
        :buffer "*helm-sly-list-connections*"))

(defun helm-sly--buffer-candidates ()
  "Collect Lisp-related buffers, like the `events' buffer.
If the buffer does not exist, we use the associated function to generate it.

The list is in the (DISPLAY . REAL) form.  Because Helm seems to
require that REAL be a string, we need to (funcall (intern
\"function\")) in `helm-sly-switch-buffers' to generate the
buffer."
  (list (cons (sly-buffer-name :events :connection (sly-current-connection))
              "sly-pop-to-events-buffer")
        (cons (sly-buffer-name :threads :connection (sly-current-connection))
              "sly-list-threads")
        (cons (sly-buffer-name :scratch :connection (sly-current-connection))
              "sly-scratch")))

(defun helm-sly-switch-buffers (_candidate)
  "Switch to buffer candidates and replace current buffer.

If more than one buffer marked switch to these buffers in separate windows.
If a prefix arg is given split windows vertically."
  (helm-window-show-buffers
   (cl-loop for b in (helm-marked-candidates)
            collect (funcall (intern b)))))

(defun helm-sly-build-buffers-source ()
  "Build Helm source of Lisp buffers."
  (helm-build-sync-source "Lisp buffers"
    :candidates (helm-sly--buffer-candidates)
    :action `(("Switch to buffer(s)" . helm-sly-switch-buffers))))

(defun helm-sly-new-repl (name)
  "Spawn new SLY REPL with name NAME."
  (sly-mrepl-new (sly-current-connection) name))

(defun helm-sly-new-repl-choose-lisp ()
  "Spawn new SLY REPL on a new connection."
  (let ((current-prefix-arg '-))
    (call-interactively #'sly)))

(defvar helm-sly-new
  (helm-build-dummy-source "Open new REPL"
    :action (helm-make-actions
             "Open new REPL" 'helm-sly-new-repl
             "Open new REPL with chosen Lisp" 'helm-sly-new-repl-choose-lisp))
  "Helm source to make new REPLs.")

(defun helm-sly-mini ()
  "Helm for Lisp connections and buffers."
  (interactive)
  (helm :sources (list (helm-sly--c-source-connection)
                       helm-sly-new
                       (helm-sly-build-buffers-source))
        :buffer "*helm-sly-mini*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass helm-sly-apropos-type (helm-source-sync)
  ((action :initform `(("Describe SYMBOL" . sly-describe-symbol)
                       ("Edit definition" . sly-edit-definition)))
   (persistent-action :initform #'sly-describe-symbol)
   ;;(volatile :initform t)
   (requires-pattern :initform 2))
  "Lisp apropos.")

(defun helm-sly--format-designator (designator)
  "Generic wrapper around apropos' DESIGNATOR formatter."
  (let ((package (cadr designator))
        (name (car designator)))
    (format "%s:%s" package name)))

(defun helm-sly--apropos-source (name external-only case-sensitive current-package)
  "Build source that provides Helm completion against `apropos'.
- NAME: name of the source.
- EXTERNAL-ONLY: only search external symbols.
- CASE-SENSITIVE: match case in apropos search.
- CURRENT-PACKAGE: only search symbols in current package."
  (helm-make-source name 'helm-sly-apropos-type
    :candidates `(lambda ()
                   (with-current-buffer helm-current-buffer
                     (cl-loop for plist in (sly-eval (list 'slynk-apropos:apropos-list-for-emacs
                                                           helm-pattern
                                                           ,(not (null external-only))
                                                           ,(not (null case-sensitive))
                                                           (when ,current-package
                                                             (helm-sly-current-package))))
                              collect (helm-sly--format-designator (plist-get plist :designator)))))))

(defun helm-sly-current-package ()
  "Return the Common Lisp package in the current context."
  (or sly-buffer-package
      (sly-current-package)))

(defvar helm-sly--c-source-apropos-symbol-current-package
  (helm-sly--apropos-source
   "Apropos (current package)"
   nil
   nil
   (helm-sly-current-package)))

(defvar helm-sly--c-source-apropos-symbol-current-external-package
  (helm-sly--apropos-source
   "Apropos (current external package)"
   'external-only
   nil
   (helm-sly-current-package)))

(defvar helm-sly--c-source-apropos-symbol-all-external-package
  (helm-sly--apropos-source
   "Apropos (all external packages)"
   'external-only
   nil
   nil))

(defvar helm-sly--c-source-apropos-symbol-all-package
  (helm-sly--apropos-source
   "Apropos (all packages)"
   nil
   nil
   nil))

(defvar helm-sly-apropos-sources
  '(helm-sly--c-source-apropos-symbol-current-package
    helm-sly--c-source-apropos-symbol-current-external-package
    helm-sly--c-source-apropos-symbol-all-external-package
    helm-sly--c-source-apropos-symbol-all-package)
  "List of Helm sources for `helm-sly-apropos'.")

;;;###autoload
(defun helm-sly-apropos ()
  "Yet another Apropos with `helm'."
  (interactive)
  (helm :sources helm-sly-apropos-sources
        :buffer "*helm lisp apropos*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun helm-sly-normalize-xrefs (xref-alist)
  "Like `sly-insert-xrefs' but return a formatted list of strings instead.
XREF-ALIST is as per `sly-insert-xrefs'.
The strings are formatted as \"GROUP: LABEL\"."
  (cl-loop for (group . refs) in xref-alist
           append
           (cl-loop for (label location) in refs
                    collect
                    (list group label location))))

(defun helm-sly-xref-lineno (location)
  "Return line number of xref's LOCATION.
Return 0 if location does not refer to a proper file."
  (or
   (ignore-errors
     (save-window-excursion
       (sly--pop-to-source-location location
                                    'sly-xref)
       (line-number-at-pos
        (car (alist-get :position (cdr location))))))
   0))

(defun helm-sly-xref-transformer (candidates)
  "Transform CANDIDATES to \"GROUP: LABEL\".
CANDIDATES is a list of (GROUP LABEL LOCATION) as per
`helm-sly-normalize-xrefs'."
  (cl-loop for (group label location) in candidates
           collect (cons (concat (propertize (abbreviate-file-name group)
                                             'face 'helm-grep-file)
                                 ":"
                                 (propertize
                                  (number-to-string (helm-sly-xref-lineno location))
                                  'face 'helm-grep-lineno)
                                 ":"
                                 (sly-one-line-ify label))
                         (list group label location))))

(defun helm-sly-xref-goto (candidate)
  "Go to CANDIDATE xref location."
  (let ((location (nth 2 candidate)))
    (switch-to-buffer
     (save-window-excursion
       (sly--pop-to-source-location location 'sly-xref)))))

(defun helm-sly-build-xref-source (xrefs)
  "Return a Helm source of XREFS."
  (helm-build-sync-source "Lisp xrefs"
    :candidates (helm-sly-normalize-xrefs xrefs)
    :candidate-transformer 'helm-sly-xref-transformer
    :action `(("Switch to buffer(s)" . helm-sly-xref-goto))))

(defun helm-sly-show-xref-buffer (xrefs _type _symbol _package &optional _method)
  "Show XREFS.
See `sly-xref--show-results'."
  (helm :sources (list (helm-sly-build-xref-source xrefs))
        :buffer "*helm-sly-xref*"))

;;;###autoload
(define-minor-mode helm-sly-mode
  "Use Helm for Lisp xref selections.
Note that the local minor mode has a global effect, thus making
`global-helm-sly-mode' and `helm-sly-mode' equivalent."
  ;; TODO: Is it possible to disable the local minor mode?
  :init-value nil
  (let ((target 'sly-xref--show-results))
    (if (advice-member-p #'helm-sly-show-xref-buffer target)
        (advice-remove target #'helm-sly-show-xref-buffer)
      (advice-add target :override #'helm-sly-show-xref-buffer))))

;;;###autoload
(define-globalized-minor-mode global-helm-sly-mode
  helm-sly-mode
  helm-sly-mode
  :require 'helm-sly)

(provide 'helm-sly)
;;; helm-sly.el ends here
