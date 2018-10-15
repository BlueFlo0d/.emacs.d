;;; steam-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "steam" "steam.el" (0 0 0 0))
;;; Generated autoloads from steam.el

(autoload 'steam-launch "steam" "\
Launch a game in your Steam library.

\(fn)" t nil)

(autoload 'steam-insert-org-text "steam" "\
Insert each Steam game as an org heading.
The heading contains the game's name and a link to execute the game.
Entries already existing in the buffer will not be duplicated.

\(fn)" t nil)

(autoload 'steam-insert-org-images "steam" "\
Insert each Steam game as an org heading.
The heading contains an image of the game's logo and a link to execute the game.
Entries already existing in the buffer will not be duplicated.

\(fn)" t nil)

(autoload 'steam-browse-at-point "steam" "\
Open the Steam store for the Steam org link at point.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "steam" '("steam-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; steam-autoloads.el ends here
