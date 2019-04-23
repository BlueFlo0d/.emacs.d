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

(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin/"))
(setq exec-path (append exec-path '("/Library/TeX/texbin/" "/usr/local/bin/")))
(setq scheme-program-name "chez")
(setq geiser-chez-binary "chez")
(setq geiser-active-implementations '(chez))
(add-hook 'LaTeX-mode-hook (lambda()
                             (turn-on-cdlatex)
                             (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
                             (setq TeX-command-default "XeLaTeX")
                             (setq TeX-save-query  nil )
                             (setq TeX-show-compilation t)
                             ))
(add-hook 'python-mode-hook (lambda ()
                              (electric-pair-mode 1)
                              ))

;; Key bindings
(global-set-key (kbd "M-n") (kbd "C-x 5 o"))
(global-set-key (kbd "C-z") (kbd "C-x u"))
(add-hook 'cdlatex-mode-hook (lambda()
                               (global-set-key [C-tab] (quote cdlatex-tab))
                               (setq cdlatex-env-alist
                                     '(("split" "\\begin{split}\nAUTOLABEL\n?\n\\end{split}\n" nil)
                                       ))
                               (setq cdlatex-command-alist
                                     '(("spl" "Insert split env" "" cdlatex-environment ("split") t nil)
                                       ))))
(setq cmake-ide-build-dir "~/cmake-build/")
(setenv "CC" "")
;; (desktop-save-mode 1)
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Switch to interactive Scheme buffer." t)
(setq auto-mode-alist (cons '("\\.ss" . scheme-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.sls" . scheme-mode) auto-mode-alist))
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-envs '("PATH" "CC" "CXX"))
(require 'cl)
(require 'color)
(setq yascroll:delay-to-hide nil)
(scroll-bar-mode -1)
(global-yascroll-bar-mode 1)

 (define-key c-mode-map  [(tab)] 'company-complete)
 (define-key c++-mode-map  [(tab)] 'company-complete)
(add-to-list 'default-frame-alist '(font . "Courier-20") )
;;(require 'emms-setup)
;;(emms-all)
;;(emms-default-players 'mplayer)
(setq exec-path (append exec-path '("/usr/local/bin")))
(add-to-list 'load-path "~/emms/lisp")
(require 'emms-setup)
(require 'emms-player-mplayer)
(emms-standard)
(emms-default-players)
;;(define-emms-simple-player mplayer '(file url)
  ;;(regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv" ".wma"
  ;;              ".mov" ".avi" ".divx" ".ogm" ".asf" ".mkv" "http://" "mms://"
  ;;              ".rm" ".rmvb" ".mp4" ".flac" ".vob" ".m4a" ".flv" ".ogv" ".pls"))
  ;;"mplayer" "-slave" "-quiet" "-really-quiet" "-fullscreen")
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
    (flycheck ggtags paredit geiser cdlatex auctex ace-jump-mode helm racket-mode zygospore yascroll xwidgete ws-butler volatile-highlights use-package undo-tree steam slime-volleyball proof-general org neotree mines magit-popup iedit helm-swoop helm-projectile helm-gtags haskell-mode haskell-emacs git-commit ghub ghci-completion f exec-path-from-shell emms elpy dtrt-indent dired-du company-rtags company-c-headers color-theme cmake-ide clean-aindent-mode chess anzu 2048-game))))
(load-theme 'k)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(setq doc-view-resolution 200)
(eval-after-load "term"
  '(progn (term-set-escape-char ?\C-x)
          (define-key term-raw-map (kbd "C-c") nil)))
(add-hook 'scheme-mode-hook (lambda ()
                              (geiser-mode)
                              (paredit-mode)
                              (local-set-key (kbd "M-l") 'geiser-load-current-buffer)))
(setq geiser-mode-start-repl-p t)
(global-hl-line-mode 1)
(set-face-background 'hl-line "#1D294A")
(global-flycheck-mode 1)
(cmake-ide-setup)

(provide 'init)
;;; init.el ends here
