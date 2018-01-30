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
(load-theme 'sourcerer t)

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
 (define-key c-mode-map  [(tab)] 'company-complete)
 (define-key c++-mode-map  [(tab)] 'company-complete)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (sourcerer-theme color-theme neotree magit dired-du cmake-ide company-rtags auto-complete-clang-async auto-complete-clang markdown-toc markdown-mode+ markdown-mode cdlatex emms 2048-game xwidgete zygospore helm-gtags helm yasnippet ws-butler volatile-highlights use-package undo-tree iedit dtrt-indent counsel-projectile company clean-aindent-mode anzu)))
 '(tramp-syntax (quote default) nil (tramp)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(add-to-list 'default-frame-alist
             '(font . "Courier-20"))
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
(set-background-color "black")
(set-foreground-color "white")
(add-to-list 'load-path "~/.emacs.d/mu4e")
(require 'mu4e)
;; default

(setq mu4e-maildir "~/Maildir")
(setq mu4e-drafts-folder "/[ART].Drafts")
(setq mu4e-sent-folder   "/[ART].Sent Mail")
(setq mu4e-trash-folder  "/[ART].Trash")

(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin/"))
(setq exec-path (append exec-path '("/Library/TeX/texbin/")))
(add-hook 'LaTeX-mode-hook (lambda()
                             (turn-on-cdlatex)
                             (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
                             (setq TeX-command-default "XeLaTeX")
                             (setq TeX-save-query  nil )
                             (setq TeX-show-compilation t)
                             ))

;; Key bindings
(global-set-key (kbd "M-n") (kbd "C-x 5 o"))

(add-hook 'cdlatex-mode-hook (lambda()
                               (global-set-key [C-tab] (quote cdlatex-tab))
                               (setq cdlatex-env-alist
                                     '(("split" "\\begin{split}\nAUTOLABEL\n?\n\\end{split}\n" nil)
                                       ))
                               (setq cdlatex-command-alist
                                     '(("spl" "Insert split env" "" cdlatex-environment ("split") t nil)
))))
(cmake-ide-setup)
