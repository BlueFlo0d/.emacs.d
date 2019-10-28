(require 'package)
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
;;(load-theme 'sourcerer t)
(require 'setup-general)
(if (version< emacs-version "24.4")
    (require 'setup-ivy-counsel)
  (require 'setup-helm)
  (require 'setup-helm-gtags))
;; (require 'setup-ggtags)
(require 'setup-cedet)
(require 'setup-editing)
(require 'setup-c)


;; function-args
;; (require 'function-args)
;; (fa-config-default)

(cond
 ((string-equal system-type "darwin")
  (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))))
(setenv "CC" "clang")
(setenv "CXX" "clang++")
(setenv "QT_SCALE_FACTOR" "2.0")
(setenv "GDK_DPI_SCALE" "2.0")
(setq exec-path (append exec-path
                        (cond
                         ((string-equal system-type "darwin")
                          '("/Library/TeX/texbin" "/usr/local/bin"))
                         (t '("/usr/local/bin")))
))
(setq scheme-program-name "chez-scheme")
(setq geiser-chez-binary "chez-scheme")
(setq geiser-active-implementations '(chez))
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
                             (setq TeX-show-compilation t)
                             ))
(add-hook 'python-mode-hook (lambda ()
                              (electric-pair-mode 1)
                              ))
;; Key bindings
(global-set-key (kbd "s-o") 'ace-window)
(global-set-key (kbd "s-j") 'ace-jump-mode)
(global-set-key (kbd "s-s") 'helm-swoop)
(require 'framemove)
(setq framemove-hook-into-windmove t)
(global-set-key (kbd "s-p") 'windmove-up)
(global-set-key (kbd "s-n") 'windmove-down)
(global-set-key (kbd "s-r") 'windmove-right)
(global-set-key (kbd "s-l") 'windmove-left)
(global-set-key (kbd "C-z") (kbd "C-x u"))
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
(setq auto-mode-alist (delq (assoc "\\.rkt\\'" auto-mode-alist) auto-mode-alist))
(require 'cl)
(require 'color)
(setq yascroll:delay-to-hide nil)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-yascroll-bar-mode 1)

 (define-key c-mode-map  [(tab)] 'company-complete)
 (define-key c++-mode-map  [(tab)] 'company-complete)
(add-to-list 'default-frame-alist '(font . "Courier-20") )
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
(defvar blink-cursor-colors (list  "#92c48f" "#6785c5" "#be369c" "#d9ca65")
  "On each blink the cursor will cycle to the next color in this list.")

(setq blink-cursor-count 0)
(defun blink-cursor-timer-function ()
  "Zarza wrote this cyberpunk variant of timer `blink-cursor-timer'.
Warning: overwrites original version in `frame.el'.

This one changes the cursor color on each blink. Define colors in `blink-cursor-colors'."
  (when (not (internal-show-cursor-p))
    (when (>= blink-cursor-count (length blink-cursor-colors))
      (setq blink-cursor-count 0))
    (set-cursor-color (nth blink-cursor-count blink-cursor-colors))
    (setq blink-cursor-count (+ 1 blink-cursor-count))
    )
  (internal-show-cursor nil (not (internal-show-cursor-p)))
  )
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4bdc036ccf4ec5fc246cba3fcb5d18852d88026a77074209ebecdf9d8dbf1c75" default)))
 '(package-selected-packages
   (quote
    (ox-latex-subfigure htmlize slime-company slime org-static-blog telega company-ghci dash-functional rainbow-identifiers tracking anaphora pdf-tools ace-window openwith notmuch sudo-edit multi-term exwm magit flycheck-irony irony flycheck ggtags paredit geiser cdlatex auctex ace-jump-mode helm racket-mode zygospore yascroll xwidgete ws-butler volatile-highlights use-package undo-tree steam slime-volleyball proof-general org neotree mines magit-popup iedit helm-swoop helm-projectile helm-gtags haskell-mode haskell-emacs git-commit ghub ghci-completion f exec-path-from-shell emms elpy dtrt-indent dired-du company-rtags company-c-headers color-theme cmake-ide clean-aindent-mode chess anzu 2048-game)))
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
(load-theme 'k)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(setq company-backends (delete 'company-semantic company-backends))
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-irony))
;;(setq doc-view-resolution 200)
(eval-after-load "term"
  '(progn (term-set-escape-char ?\C-x)
          (define-key term-raw-map (kbd "C-c") nil)))
(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'racket-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook (lambda ()
                              (geiser-mode)
                              (paredit-mode)
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

;; Email setup
(setq smtpmail-default-stmp-server "smtp.exchange.mit.edu"
      smtpmail-local-domain "mit.edu")
(load-library "smtpmail")
(setq send-mail-function 'smtpmail-send-it)

(setq notmuch-search-oldest-first nil)

(setq x-super-keysym 'meta)
(setq x-meta-keysym 'super)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(keyboard-translate ?\( ?\[)
(keyboard-translate ?\[ ?\()
(keyboard-translate ?\) ?\])
(keyboard-translate ?\] ?\))

(require 'exwm)(require 'exwm-randr)
(setq exwm-workspace-number 4)
(exwm-enable)

(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist
      '(0 "DP-2-1" 1 "HDMI-1" 2 "eDP-1"))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output DP-2-1 --output HDMI-1 --output eDP-1 --auto")))
(exwm-randr-enable)

(setq exwm-input-global-keys
      `(([?\s-r] . exwm-reset)
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

(require 'multi-term)
(setenv "ENV" (cond
               ((string-equal system-type "darwin") "/Users/hongqiantan/.bashrc")
               (t "/usr/home/qhong/.bashrc")))
(setq multi-term-program "/bin/bash")
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
(defun execute-without-multiterm (k)
  (eval `(lambda ()
    (interactive)
    (let ((current-frame (selected-frame)))
      (set-frame-parameter current-frame
                           'buffer-predicate
                           (lambda (buffer)
                             (not (string= (buffer-local-value 'major-mode buffer) "term-mode"))))
      (,@k)
      (set-frame-parameter current-frame 'buffer-predicate (lambda (buffer) t))))))
(global-set-key (kbd "s-f") (execute-without-multiterm `(next-buffer)))
(global-set-key (kbd "s-b") (execute-without-multiterm `(previous-buffer)))
(global-set-key (kbd "s-x") 'multi-term-next)
(global-set-key (kbd "s-X") 'multi-term)
(advice-add 'multi-term :after  (lambda ()
                            (local-set-key (kbd "s-x") 'multi-term)
                            (local-set-key (kbd "s-f") 'multi-term-next)
                            (local-set-key (kbd "s-b") 'multi-term-prev)
                            (local-set-key (kbd "M-DEL") (lambda ()
                                                           (interactive)
                                                           (term-send-raw-string "\e\C-?")))
                            (local-set-key (kbd "s-g") (execute-without-multiterm `(switch-to-buffer (other-buffer))))
                            ))
(advice-add 'term-char-mode :after (lambda ()
                                     (local-set-key (kbd "C-c C-j") 'term-line-mode)))

;; Quick launch apps
(global-set-key (kbd "s-G")
                (lambda (url)
                  (interactive "sEnter URL or keywords: ")
                  (with-current-buffer
                      (generate-new-buffer "*eww*")
                    (eww-mode)
                    (eww url))))
(add-hook 'emacs-lisp-mode-hook 'electric-pair-mode)

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
(pdf-tools-install)
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

(when (string-equal system-type "darwin") (require 'blog))
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(slime-setup '(slime-fancy slime-company))
(require 'org)
(require 'ox-latex)
(setq org-latex-create-formula-image-program 'dvisvgm)
(setq-default org-html-with-latex 'dvisvgm)
(org-babel-do-load-languages 'org-babel-load-languages '((latex . t)))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((latex . t)))
(require 'lilypond-mode)
(provide 'init)
;;; init.el ends here
