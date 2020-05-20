(require 'package)

(setq default-directory "~/")
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (not package-archive-contents)
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/custom")
(add-to-list 'load-path "~/.emacs.d/lilypond")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq k--imac-pro-p (string-equal system-type "darwin"))
(setq k--x1c6-p (not k--imac-pro-p))

(when k--x1c6-p
  (require 'exwm)
  (require 'exwm-randr)
  (setq exwm-workspace-number 4)
  (exwm-enable)
  ;(exwm-workspace-detach-minibuffer)
  ;(setq exwm-workspace-minibuffer-position 'bottom)

  (setq exwm-randr-workspace-output-plist
        '(0 "DP-2-1" 1 "HDMI-1" 2 "eDP-1"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output DP-2-1 --output HDMI-1 --output eDP-1 --auto")))
  (exwm-randr-enable)
  (setq exwm-input-global-keys
        `(([?\s-e] . exwm-reset)
          ([?\s-w] . exwm-workspace-switch)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))
  (setq exwm-input-simulation-keys
        '(([?\C-b] . [left])
          ([?\C-f] . [right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])))
  )
(require 'setup-general)
(setq async-bytecomp-allowed-packages '(all))
(if (version< emacs-version "24.4")
    (require 'setup-ivy-counsel)
  (require 'setup-helm)
  (require 'setup-helm-gtags))
(setq history-delete-duplicates t)
;; (require 'setup-ggtags)
(require 'setup-cedet)
(require 'setup-editing)
(require 'setup-c)


(require 'cl)
(require 'color)

;; function-args
;; (require 'function-args)
;; (fa-config-default)
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(cond
 (k--imac-pro-p
  (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))))
(setenv "CC" "clang")
(setenv "CXX" "clang++")
(setenv "QT_SCALE_FACTOR" "2.0")
(setenv "GDK_DPI_SCALE" "2.0")
(setq exec-path (append exec-path
                        (cond
                         (k--imac-pro-p
                          '("/Library/TeX/texbin" "/usr/local/bin"))
                         (t '("/usr/local/bin")))
                        ))
(require 'geiser)
(if k--imac-pro-p
    (progn
      (setq geiser-chez-binary "chez")
      (setq geiser-mit-binary "/Applications/MIT:GNU Scheme 10.1.10.app/Contents/Resources/mit-scheme")
      (setq geiser-active-implementations '(chez mit)))
  (progn
    (require 'geiser-guile)
    (add-to-list 'geiser-guile-load-path "~/src/guix")))
;; (with-eval-after-load "tex"
;;   (add-to-list 'TeX-view-program-list '("open" "open %o"))
;;   (setcdr (assq 'output-pdf TeX-view-program-selection) '("open")))

;; to use pdfview with auctex
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
      TeX-source-correlate-start-server t) ;; not sure if last line is neccessary

;; to have the buffer refresh after compilation
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

(add-hook 'LaTeX-mode-hook (lambda()
                             (turn-on-cdlatex)
                             (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
                             (add-to-list 'TeX-command-list '("PDFLaTeX" "%`pdflatex -shell-escape%(mode)%' %t" TeX-run-TeX nil t))
                             (setq TeX-command-default "XeLaTeX")
                             (setq TeX-save-query  nil )
                             (setq TeX-show-compilation t)))
(add-hook 'python-mode-hook (lambda ()
                              (electric-pair-mode 1)))
;; Key bindings
(global-set-key (kbd "s-o") 'ace-window)
(global-set-key (kbd "s-s") 'helm-do-ag-project-root)
(global-set-key (kbd "s-S") 'helm-swoop)
(global-set-key (kbd "s-m") 'magit-status)
(require 'helm)
(setq completion-styles
      (when (version< emacs-version "27.0")
        '(flex)
        '(helm-flex)))
(setq helm-follow-mode-persistent t)
(define-key helm-find-files-map (kbd "s-d") 'helm-ff-run-delete-file)
(define-key helm-find-files-map (kbd "s-w") 'helm-ff-run-copy-file)
(define-key helm-find-files-map (kbd "s-l") 'helm-ff-run-symlink-file)
(define-key helm-find-files-map (kbd "s-r") 'helm-ff-run-rename-file)
(global-set-key (kbd "s-v")
                (lambda ()
                  (interactive)
                  (if (projectile-project-p)
                      (helm-projectile-find-file)
                    (helm-projectile-switch-project))))
(global-set-key (kbd "s-V") #'helm-projectile-switch-project)
(require 'paredit)
(define-key paredit-mode-map (kbd "M-;") #'comment-or-uncomment-sexp)
(define-key paredit-mode-map (kbd "C-M-;") (lambda () (interactive)
                                             (save-excursion
                                               (beginning-of-line)
                                               (if (eq (char-after) ?\;)
                                                   (comment-or-uncomment-sexp)
                                                 (beginning-of-defun)
                                                 (comment-or-uncomment-sexp)))))
(define-key paredit-mode-map (kbd "M-c") #'paredit-convolute-sexp)
(pop minibuffer-setup-hook)
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (if (memq this-command
                      '(eval-expression
                        sly-inspect
                        sly-interactive-eval))
                (paredit-mode 1))))
(defun cloc-magit-root ()
  (interactive)
  (message (shell-command-to-string
            (concat "cloc " (magit-toplevel)))))
(add-hook 'magit-post-commit-hook 'cloc-magit-root)
(defun next-workspace (direction)
  (case direction
    (left (exwm-workspace-switch (1- exwm-workspace-current-index)))
    (right (exwm-workspace-switch (1+ exwm-workspace-current-index)))))
(if k--imac-pro-p
    (require 'framemove)
  (defadvice windmove-do-window-select (around do-window-select-wrapper activate)
    "Let windmove do its own thing, if there is an error, try framemove in that direction."
    (condition-case err
        ad-do-it
      (error
       (next-workspace (ad-get-arg 0))))))
(setq framemove-hook-into-windmove t)
(global-set-key (kbd "s-p") 'windmove-up)
(global-set-key (kbd "s-n") 'windmove-down)
(global-set-key (kbd "s-r") 'windmove-right)
(global-set-key (kbd "s-l") 'windmove-left)
(global-set-key (kbd "C-z") (kbd "C-x u"))
(require 'buffer-move)
(global-set-key (kbd "C-s-p") #'buf-move-up)
(global-set-key (kbd "C-s-n") #'buf-move-down)
(global-set-key (kbd "C-s-r") #'buf-move-right)
(global-set-key (kbd "C-s-l") #'buf-move-left)
(defun buf-move-to (direction)
  "override buffer-move to support inter-frame buffer movement"
  (let* ((this-win (selected-window))
         (buf-this-buf (window-buffer this-win))
         (other-win
          (let ((buf-this-window (windmove-find-other-window direction)))
            (if (null buf-this-window)
                (progn
                  (if k--imac-pro-p
                      (fm-next-frame direction)
                    (next-workspace direction))
                  (selected-window))
              buf-this-window))))
    (if (null other-win)
        (error "No window in this direction")
      (if (window-dedicated-p other-win)
          (error "The window in this direction is dedicated"))
      (if (string-match "^ \\*Minibuf" (buffer-name (window-buffer other-win)))
          (error "The window in this direction is the Minibuf"))
      (if (eq buffer-move-behavior 'move)
          ;; switch selected window to previous buffer (moving)
          (switch-to-prev-buffer this-win)
        ;; switch selected window to buffer of other window (swapping)
        (set-window-buffer this-win (window-buffer other-win)))

      ;; switch other window to this buffer
      (set-window-buffer other-win buf-this-buf)

      (when (or (null buffer-move-stay-after-swap)
                (eq buffer-move-behavior 'move))
        (select-window other-win)))))
;; setup avy jump
(let ((hyper-mask (- ?\H-a ?a)))
  (dolist (x (number-sequence ?a ?z))
    (eval
     `(global-set-key (vector (+ hyper-mask x))
                      (lambda ()
                        (interactive)
                        (avy-goto-word-1 ,x))))))
(setq avy-keys (number-sequence ?a ?z))
(add-hook 'cdlatex-mode-hook (lambda()
                               (local-set-key [C-tab] (quote cdlatex-tab))
                               (setq cdlatex-env-alist
                                     '(("split" "\\begin{split}\nAUTOLABEL\n?\n\\end{split}\n" nil)
                                       ))
                               (setq cdlatex-command-alist
                                     '(("spl" "Insert split env" "" cdlatex-environment ("split") t nil)
                                       ))))
;;(setq cmake-ide-build-dir "~/cmake-build/")
;; (desktop-save-mode 1)

(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Switch to interactive Scheme buffer." t)
(setq auto-mode-alist (cons '("\\.ss" . scheme-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.sls" . scheme-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.lisp" . lisp-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.z" . z3-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ly" . lilypond-mode) auto-mode-alist))
(setq auto-mode-alist (delq (assoc "\\.rkt\\'" auto-mode-alist) auto-mode-alist))
(setq yascroll:delay-to-hide nil)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-yascroll-bar-mode 1)

(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)
(if k--imac-pro-p
    (add-to-list 'default-frame-alist '(font . "Courier-20"))
  (add-to-list 'default-frame-alist '(font . "Courier-20"))
  (set-fontset-font t 'symbol "Unifont" nil 'append)
  (set-fontset-font t 'symbol "Noto Emoji" nil 'append)
  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
  (set-fontset-font t 'symbol "Noto Sans Symbols" nil 'append)
  (set-fontset-font t nil "Noto Sans CJK" nil 'append))
(print (font-family-list))
;; (setq exec-path (append exec-path '("/usr/local/bin")))
;; (add-to-list 'load-path "~/emms/lisp")
;; (require 'emms-setup)
;; (require 'emms-player-mplayer)
;; (emms-standard)
;; (emms-default-players)
;; (define-emms-simple-player mplayer '(file url)
;;   (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv" ".wma"
;;                ".mov" ".avi" ".divx" ".ogm" ".asf" ".mkv" "http://" "mms://"
;;                ".rm" ".rmvb" ".mp4" ".flac" ".vob" ".m4a" ".flv" ".ogv" ".pls"))
;;   "mplayer" "-slave" "-quiet" "-really-quiet" "-fullscreen")

(defmacro globalize (mode)
  (let ((%global-mode-symbol (intern (concat "global-" (symbol-name mode)))))
    `(progn
       (define-globalized-minor-mode ,%global-mode-symbol ,mode
         (lambda () (,mode 1)))
       (,%global-mode-symbol 1))))

(require 'highlight-tail)
(setq highlight-tail-timer 0.1)
(setq highlight-tail-steps 20)
(defvar blink-cursor-colors (list  "#92c48f" "#6785c5" "#be369c" "#d9ca65"))
(defvar blink-highlight-colors (list "#5D7E79" "#475E94" "#733780" "#808164"))
(setq highlight-tail-colors '(("#5D7E79" . 0)))
(highlight-tail-mode 1)
(setq blink-cursor-count 0)
(defun blink-cursor-timer-function ()
  (when (not (internal-show-cursor-p))
    (when (>= blink-cursor-count (length blink-cursor-colors))
      (setq blink-cursor-count 0))
    (let ((color (nth blink-cursor-count blink-cursor-colors))
          (hl-color (nth blink-cursor-count blink-highlight-colors)))
      (set-cursor-color color)
      (setq highlight-tail-colors `((,hl-color . 0)))
      (setq highlight-tail-colors-fade-list nil
            highlight-tail-nonhtfaces-bgcolors nil
            highlight-tail-const-overlays-list nil
            highlight-tail-update-const-overlays-to-this-list nil
            highlight-tail-face-max nil)
      (let* ((background-color-name (if (featurep 'xemacs)
                                        (face-background-name 'default)
                                      (cdr (assoc 'background-color
                                                  (frame-parameters)))))
             (background-color-hex (highlight-tail-hex-from-colorname
                                    background-color-name)))
        (setq highlight-tail-default-background-color background-color-name))
      (setq highlight-tail-colors-with-100
            (if (= (cdr (nth (1- (length highlight-tail-colors))
                             highlight-tail-colors))
                   100)
                highlight-tail-colors
              (append highlight-tail-colors (list '(null . 100)))))
      (setq highlight-tail-face-max highlight-tail-steps)
      (highlight-tail-add-colors-fade-table 'start)
      (highlight-tail-add-colors-fade-table 'default)
      (highlight-tail-make-faces
       (highlight-tail-get-colors-fade-table-with-key 'default))
      (setq blink-cursor-count (+ 1 blink-cursor-count))))
  (internal-show-cursor nil (not (internal-show-cursor-p))))

(globalize highlight-tail-mode)

(helm-adaptive-mode 1)
(require 'highlight-indent-guides)
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-responsive 'top)
(setq highlight-indent-guides-delay 0)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'k-bgex t)

(require 'company)

(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(company-flx-mode 1)
(setq company-backends (delete 'company-semantic company-backends))
(add-to-list
 'company-backends 'company-files)

(setq company-eclim-auto-save nil)

;;(setq doc-view-resolution 200)
(eval-after-load "term"
  '(progn (term-set-escape-char ?\C-x)
          (define-key term-raw-map (kbd "C-c") nil)))
(add-hook 'scheme-mode-hook (lambda ()
                              (geiser-mode)
                              (local-set-key (kbd "M-l") 'geiser-load-current-buffer)))

(setq geiser-mode-start-repl-p t)
(global-hl-line-mode 1)
(set-face-background 'hl-line "#1D294A")
(global-flycheck-mode 1)
(setq cmake-ide-flags-c '("-I/usr/local/include"
                           "-I/Library/Developer/CommandLineTools/usr/include"
                           "-I/Library/Developer/CommandLineTools/usr/lib/clang/10.0.1/include"
                           "-I/Library/Developer/CommandLineTools/usr/include"
                           "-I/Library/Developer/CommandLineTools/SDKs/MacOSX10.14.sdk/System/Library/Frameworks"))
(cmake-ide-setup)
(setq-default mode-line-format
      '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
       (vc-mode vc-mode)
       "  " (mode-line-process ("(" mode-name ":" mode-line-process  ")")
                               mode-name)
       mode-line-misc-info mode-line-end-spaces))

;; Email setup
(setq smtpmail-default-stmp-server "smtp.exchange.mit.edu"
      smtpmail-local-domain "mit.edu")
(load-library "smtpmail")
(setq send-mail-function 'smtpmail-send-it)

(setq notmuch-search-oldest-first nil)

                                        ;(setq x-super-keysym 'meta)
                                        ;(setq x-meta-keysym 'super)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq mac-right-option-modifier 'hyper)
(define-key input-decode-map (kbd "(") (kbd "["))
(define-key input-decode-map (kbd "[") (kbd "("))
(define-key input-decode-map (kbd ")") (kbd "]"))
(define-key input-decode-map (kbd "]") (kbd ")"))
(define-key input-decode-map (kbd "C-(") (kbd "C-["))
(define-key input-decode-map (kbd "C-[") (kbd "C-("))
(define-key input-decode-map (kbd "C-)") (kbd "C-]"))
(define-key input-decode-map (kbd "C-]") (kbd "C-)"))

(use-package multi-vterm
  :load-path "~/.emacs.d/multi-libvterm")
(setenv "ENV" (cond
               (k--imac-pro-p "/Users/hongqiantan/.bashrc")
               (t "/usr/home/qhong/.bashrc")))
(when k--imac-pro-p
  (setq multi-vterm-program "/bin/bash"))
(setq gc-cons-threshold 8000000)
(setq gc-cons-percentage 0.25)

;; (setq k--brightness 40000)
;; (defun k--refresh-brightness ()
;;   (start-process-shell-command
;;    "xbrightness" nil (format "xbrightness %d" k--brightness)))
;; (defun k--increase-brightness ()
;;   (interactive)
;;     (setq k--brightness (+ 2000 k--brightness))
;;   (if (> k--brightness 65535)
;;       (setq k--brightness 65535))
;;   (k--refresh-brightness))
;; (defun k--decrease-brightness ()
;;   (interactive)
;;   (setq k--brightness (- k--brightness 2000))
;;     (if (< k--brightness 10000)
;;         (setq k--brightness 10000))
;;     (k--refresh-brightness))
;; (global-set-key (kbd "<f5>") 'k--decrease-brightness)
;; (global-set-key (kbd "<f6>") 'k--increase-brightness)
;; Now with native support, no need

;; Switching workflow
(require 'nswbuff)
(setq nswbuff-buffer-list-function #'nswbuff-projectile-buffer-list)
(setq nswbuff-recent-buffers-first nil)
(setq nswbuff-start-with-current-centered t)
(setq nswbuff-exclude-mode-regexp "helm\\|vterm\\|compilation")
(setq nswbuff-exclude-buffer-regexps '("*Flycheck" "*Backtrace" "*Message"))
(setq nswbuff-status-window-layout 'adjust)
(setq nswbuff-display-intermediate-buffers t)
(global-set-key (kbd "s-f") #'nswbuff-switch-to-next-buffer)
(global-set-key (kbd "s-b") #'nswbuff-switch-to-previous-buffer)

(global-set-key (kbd "s-x") 'multi-vterm-next)
(global-set-key (kbd "s-X") 'multi-vterm)
(define-key vterm-mode-map (kbd "C-c C-t") nil)
(define-key vterm-mode-map (kbd "C-c C-j") 'vterm-copy-mode)
(define-key vterm-mode-map (kbd "C-d") (lambda () (interactive) (vterm-send-key "d" nil nil t)))
(define-key vterm-copy-mode-map (kbd "C-c C-k") (lambda () (interactive) (vterm-copy-mode -1)))
(defun execute-without-multivterm (k)
  (eval `(lambda ()
           (interactive)
           (let ((current-frame (selected-frame)))
             (set-frame-parameter current-frame 'buffer-predicate (lambda (buffer) t))))))

(defun multi-vterm-set-custom-keys ()
  ""
  (local-set-key (kbd "s-x") 'multi-vterm)
  (local-set-key (kbd "s-f") 'multi-vterm-next)
  (local-set-key (kbd "s-b") 'multi-vterm-prev)
  (local-set-key (kbd "s-g") (execute-without-multivterm `(switch-to-buffer (other-buffer)))))
(setq vterm-max-scrollback 1000000)
(advice-add 'multi-vterm :after  (lambda ()
                                   (interactive)
                                   (multi-vterm-set-custom-keys)))
(advice-add 'vterm-copy-mode :after  (lambda (&optional on)
                                       (interactive)
                                       (when
                                           (or (not on) (> on 0))
                                         (multi-vterm-set-custom-keys))))
(advice-add 'term-char-mode :after (lambda ()
                                     (local-set-key (kbd "C-c C-j") 'term-line-mode)))

(require 'eww)
(setq browse-url-browser-function 'eww-browse-url)
(defadvice eww-tag-title (after eww-rename-buffer-ad (cont))
  "Update EWW buffer title with new page load."
  (let ((eww-current-title (plist-get eww-data :title)))
    (rename-buffer (format "*eww: %s*" eww-current-title) t)))
(ad-activate 'eww-tag-title)
(defun eww-new-buffer (url)
  (interactive
   (let* ((uris (eww-suggested-uris))
          (prompt (concat "Enter URL or keywords"
                          (if uris (format " (default %s)" (car uris)) "")
                          ": ")))
     (list (read-string prompt nil nil uris))))
  (with-temp-buffer
    (eww url)))
;; Quick launch apps
(global-set-key (kbd "s-g") #'eww-new-buffer)
(global-set-key (kbd "s-L")
                (lambda ()
                  (interactive)
                  (call-process-shell-command
                   "/home/qh/Resources/i3.sh" nil nil)))

(require 'adjust-parens)
(mapc (lambda (h)
        (add-hook h #'paredit-mode)
        (add-hook h #'adjust-parens-mode)
        (add-hook h #'highlight-indent-guides-mode))
      '(emacs-lisp-mode-hook
        lisp-mode-hook
        scheme-mode-hook
        sly-mrepl-mode-hook))
(setq backward-delete-char-untabify-method 'hungry)
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (require 'company-elisp)
                                  (push 'company-elisp company-backends)
                                  (push 'company-capf company-backends)))
(add-hook 'emacs-lisp-mode-hook #'rainbow-mode)

(add-hook 'scheme-mode-hook #'paredit-mode)
(add-hook 'sly-mrepl-mode-hook #'paredit-mode)

(defvar  sly-mrepl--sylvesters
  (with-temp-buffer
    (insert-file-contents-literally
     (expand-file-name "better-sly-motd.txt"
                       (file-name-directory load-file-name)))
    (cl-loop while (< (point) (point-max))
             for start = (point)
             do (search-forward "\n\n" nil 'noerror)
             collect (buffer-substring-no-properties start (- (point) 2)))))
(require 'sly)
(remove-hook 'lisp-mode-hook #'sly-editing-mode)
(add-hook 'lisp-mode-hook (lambda ()
                            (interactive)
                            (unless (eq 'z3-mode major-mode)
                              (sly-editing-mode))))
(define-key sly-mode-map (kbd "s-h") 'sly-hyperspec-lookup)
(define-key sly-mode-map (kbd "s-x") 'sly-mrepl)
(global-set-key (kbd "s-w") nil)
(define-key sly-mode-map (kbd "s-w") 'helm-sly-apropos)
(define-key sly-mode-map (kbd "s-W") 'sly-apropos-package)
(add-hook 'sly-mode-hook
          (lambda ()
            (unless (sly-connected-p)
              (save-excursion (sly)))))

;; Emacs server
(server-start)
(add-hook 'server-switch-hook
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
	        (when server-buffer-clients
		      (local-set-key (kbd "C-x k") 'server-edit))))
;; Sudo edit better default dir
(add-hook 'find-file-hook
          (lambda ()
              (setq default-directory
                    (replace-regexp-in-string "^/sudo:root@localhost:" "" default-directory))))
(require 'vlf-setup)
(setq vlf-application 'dont-ask)
(ace-link-setup-default)

;; Using external programs for opening some files
;; (require 'openwith)
;; (setq openwith-associations
;;       (list
;;        (list (openwith-make-extension-regexp
;;               '("pdf" "dvi"))
;;              "zathura"
;;              '(file))
;;        (list (openwith-make-extension-regexp
;;               '("mp4" "avi" "webm" "mkv"))
;;              "mplayer"
;;              '(file))
;;        ))
(use-package pdf-tools
  :ensure t
  :config
  (custom-set-variables
   '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo"))
;(pdf-tools-install)
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

;; Print screen
(global-set-key (kbd "<print>")
  (lambda ()
    (interactive)
    (let ((path (concat "~/Documents/Screenshot-" (format-time-string "%Y-%m-%d,%H:%M:%S") ".png")))
      (start-process-shell-command
       "import" nil (concat "import -window root " path))
    (message (concat "Screenshot saved to " path)))
    ))

(setq undo-limit 1000000)
(setq undo-strong-limit 10000000)
(setq undo-outer-limit 100000000)
(setq company-dabbrev-downcase nil)
(when (string-equal system-type "berkeley-unix") (openwith-mode t))

(when k--imac-pro-p (require 'blog))
(if k--imac-pro-p
    (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (setq inferior-lisp-program "~/.guix-profile/bin/sbcl"))
;; (setq sly-contribs '(sly-fancy sly-quicklisp sly-macrostep))
;; (setq sly-lisp-implementations
;;       '((ccl ("/usr/local/Cellar/clozure-cl/1.11.6/bin/ccl64"))
;;         (sbcl ("/usr/local/bin/sbcl"))))
(require 'org)
(require 'ox-latex)
(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass\[presentation\]\{beamer\}"
               ("\\section\{%s\}" . "\\section*\{%s\}")
               ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
               ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))
(setq org-latex-create-formula-image-program 'dvisvgm)
(setq-default org-html-with-latex 'dvisvgm)
(org-babel-do-load-languages 'org-babel-load-languages '((latex . t)))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((latex . t)))
(global-subword-mode 1)
(require 'highlight-parentheses)
(setq hl-paren-colors '(nil))
(set-face-attribute 'hl-paren-face nil :inherit 'show-paren-match)
(show-paren-mode 1)
(globalize highlight-parentheses-mode)
(require 'paredit)
(define-key paredit-mode-map (kbd "M-q")
  (lambda () (interactive)
    (paredit-backward-up)
    (kill-sexp)))
(save-place-mode 1)

(define-key indent-rigidly-map (kbd "C-b") 'indent-rigidly-left)
(define-key indent-rigidly-map (kbd "C-f") 'indent-rigidly-right)
(define-key indent-rigidly-map (kbd "M-b") 'indent-rigidly-left-to-tab-stop)
(define-key indent-rigidly-map (kbd "M-f") 'indent-rigidly-right-to-tab-stop)
(add-hook 'lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'scheme-mode-hook #'aggressive-indent-mode)

(require 'telega)
(setq telega-squash-message-mode-hook nil)
(add-hook 'telega-root-mode-hook
          (lambda ()
            (interactive)
            (setq default-directory "~/.telega/")))

(require 'bgex)
(bgex-set-image-default "~/Resources/wallpaper-blurred.jpg")
(setq default-frame-alist
      (append '((alpha . 70)
                (left-fringe . 1)
                (right-fringe . 5))
              default-frame-alist))
(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4bdc036ccf4ec5fc246cba3fcb5d18852d88026a77074209ebecdf9d8dbf1c75" default)))
 '(debug-on-error t)
 '(helm-ls-git-default-sources
   (quote
    (helm-source-ls-git-buffers helm-source-ls-git-status helm-source-ls-git)))
 '(jka-compr-compression-info-list
   (quote
    (["\\.Z\\'" "compressing" "compress"
      ("-c")
      "uncompressing" "gzip"
      ("-c" "-q" "-d")
      nil t "\235"]
     ["\\.bz2\\'" "bzip2ing" "bzip2" nil "bunzip2ing" "bzip2"
      ("-d")
      nil t "BZh"]
     ["\\.tbz2?\\'" "bzip2ing" "bzip2" nil "bunzip2ing" "bzip2"
      ("-d")
      nil nil "BZh"]
     ["\\.\\(?:tgz\\|svgz\\|sifz\\)\\'" "compressing" "gzip"
      ("-c" "-q")
      "uncompressing" "gzip"
      ("-c" "-q" "-d")
      t nil "\213"]
     ["\\.gz\\'" "compressing" "gzip"
      ("-c" "-q")
      "uncompressing" "gzip"
      ("-c" "-q" "-d")
      t t "\213"]
     ["\\.lz\\'" "Lzip compressing" "lzip"
      ("-c" "-q")
      "Lzip uncompressing" "lzip"
      ("-c" "-q" "-d")
      t t "LZIP"]
     ["\\.lzma\\'" "LZMA compressing" "lzma"
      ("-c" "-q" "-z")
      "LZMA uncompressing" "lzma"
      ("-c" "-q" "-d")
      t t ""]
     ["\\.xz\\'" "XZ compressing" "xz"
      ("-c" "-q")
      "XZ uncompressing" "xz"
      ("-c" "-q" "-d")
      t t "\3757zXZ "]
     ["\\.txz\\'" "XZ compressing" "xz"
      ("-c" "-q")
      "XZ uncompressing" "xz"
      ("-c" "-q" "-d")
      t nil "\3757zXZ "]
     ["\\.dz\\'" nil nil nil "uncompressing" "gzip"
      ("-c" "-q" "-d")
      nil t "\213"]
     ["\\.zst\\'" "zstd compressing" "zstd"
      ("-c" "-q")
      "zstd uncompressing" "zstd"
      ("-c" "-q" "-d")
      t t "(\265/\375"]
     ["\\.tzst\\'" "zstd compressing" "zstd"
      ("-c" "-q")
      "zstd uncompressing" "zstd"
      ("-c" "-q" "-d")
      t nil "(\265/\375"])))
 '(package-selected-packages
   (quote
    (guix company-flx comment-dwim-2 pdf-tools xwwp-follow-link-helm helm-swoop helm-bibtex helm-gtags helm-ag helm-projectile helm-ls-git helm-sly helm adjust-parens buffer-move comment-or-uncomment-sexp gnu-elpa-keyring-update svg-clock ansi package-build shut-up epl git commander dash s visible-mark highlight-parentheses nswbuff flames-of-freedom autotetris-mode highlight-indent-guides aggressive-indent ace-link vlf rainbow-mode magithub avy sly-macrostep sly-quicklisp sly-asdf sly vterm tuareg z3-mode ox-latex-subfigure htmlize org-static-blog company-ghci dash-functional rainbow-identifiers tracking anaphora ace-window openwith notmuch sudo-edit exwm magit flycheck-irony irony flycheck ggtags paredit geiser cdlatex auctex racket-mode zygospore yascroll xwidgete ws-butler volatile-highlights use-package undo-tree steam slime-volleyball proof-general org neotree mines magit-popup iedit haskell-mode haskell-emacs git-commit ghub ghci-completion f exec-path-from-shell emms elpy dtrt-indent dired-du company-rtags company-c-headers color-theme cmake-ide clean-aindent-mode chess anzu 2048-game)))
 '(pdf-tools-handle-upgrades nil)
 '(safe-local-variable-values
   (quote
    ((cmake-ide-project-dir . ~/ksi)
     (cmake-ide-build-dir . ~/ksi/build)
     (eval setq cmake-ide-build-dir
           (concat my-project-path "build"))
     (cmake-ide-project-dir . my-project-path)
     (eval set
           (make-local-variable
            (quote my-project-path))
           (file-name-directory
            (let
                ((d
                  (dir-locals-find-file ".")))
              (if
                  (stringp d)
                  d
                (car d)))))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
