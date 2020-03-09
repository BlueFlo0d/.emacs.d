;;; telega-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "telega" "telega.el" (0 0 0 0))
;;; Generated autoloads from telega.el

(autoload 'telega "telega" "\
Start telegramming.
If prefix ARG is given, then will not pop to telega root buffer.

\(fn ARG)" t nil)

(autoload 'telega-kill "telega" "\
Kill currently running telega.
With prefix arg FORCE quit without confirmation.

\(fn FORCE)" t nil)

(autoload 'telega-report-bug "telega" "\
Create bug report for https://github.com/zevlg/telega.el/issues.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telega" '("telega-")))

;;;***

;;;### (autoloads nil "telega-chat" "telega-chat.el" (0 0 0 0))
;;; Generated autoloads from telega-chat.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telega-chat" '("telega-" "with-telega-chatbuf-action")))

;;;***

;;;### (autoloads nil "telega-company" "telega-company.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from telega-company.el

(autoload 'telega-company-emoji "telega-company" "\
Backend for `company' to complete emojis.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(autoload 'telega-company-telegram-emoji "telega-company" "\
Backend for `company' to complete emojis using `searchEmojis' TDLib method.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(autoload 'telega-company-username "telega-company" "\
Backend for `company' to complete usernames.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(autoload 'telega-company-hashtag "telega-company" "\
Backend for `company' to complete recent hashtags.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(autoload 'telega-company-botcmd "telega-company" "\


\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telega-company" '("telega-company-")))

;;;***

;;;### (autoloads nil "telega-core" "telega-core.el" (0 0 0 0))
;;; Generated autoloads from telega-core.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telega-core" '("telega-" "with-telega-")))

;;;***

;;;### (autoloads nil "telega-customize" "telega-customize.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from telega-customize.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telega-customize" '("telega-")))

;;;***

;;;### (autoloads nil "telega-ffplay" "telega-ffplay.el" (0 0 0 0))
;;; Generated autoloads from telega-ffplay.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telega-ffplay" '("telega-ffplay-")))

;;;***

;;;### (autoloads nil "telega-filter" "telega-filter.el" (0 0 0 0))
;;; Generated autoloads from telega-filter.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telega-filter" '("has-" "any" "all" "archive" "main" "custom" "contact" "chat-list" "label" "last-message-by-me" "type" "top" "tracking" "telega-" "search" "saved-messages" "restriction" "pin" "permission" "ids" "verified" "online-status" "unread" "unmuted" "not" "name" "define-telega-filter")))

;;;***

;;;### (autoloads nil "telega-i18n" "telega-i18n.el" (0 0 0 0))
;;; Generated autoloads from telega-i18n.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telega-i18n" '("telega-i18n")))

;;;***

;;;### (autoloads nil "telega-info" "telega-info.el" (0 0 0 0))
;;; Generated autoloads from telega-info.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telega-info" '("telega-")))

;;;***

;;;### (autoloads nil "telega-inline" "telega-inline.el" (0 0 0 0))
;;; Generated autoloads from telega-inline.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telega-inline" '("telega-")))

;;;***

;;;### (autoloads nil "telega-ins" "telega-ins.el" (0 0 0 0))
;;; Generated autoloads from telega-ins.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telega-ins" '("telega-")))

;;;***

;;;### (autoloads nil "telega-media" "telega-media.el" (0 0 0 0))
;;; Generated autoloads from telega-media.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telega-media" '("telega-")))

;;;***

;;;### (autoloads nil "telega-modes" "telega-modes.el" (0 0 0 0))
;;; Generated autoloads from telega-modes.el

(defvar telega-mode-line-mode nil "\
Non-nil if Telega-Mode-Line mode is enabled.
See the `telega-mode-line-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `telega-mode-line-mode'.")

(custom-autoload 'telega-mode-line-mode "telega-modes" nil)

(autoload 'telega-mode-line-mode "telega-modes" "\
Toggle display of the unread chats/mentions in the modeline.

\(fn &optional ARG)" t nil)

(defvar telega-autoplay-mode nil "\
Non-nil if Telega-Autoplay mode is enabled.
See the `telega-autoplay-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `telega-autoplay-mode'.")

(custom-autoload 'telega-autoplay-mode "telega-modes" nil)

(autoload 'telega-autoplay-mode "telega-modes" "\
Automatically play animation messages.

\(fn &optional ARG)" t nil)

(autoload 'telega-url-shorten-mode "telega-modes" "\
Toggle URLs shortening mode.

\(fn &optional ARG)" t nil)

(defvar global-telega-url-shorten-mode nil "\
Non-nil if Global Telega-Url-Shorten mode is enabled.
See the `global-telega-url-shorten-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-telega-url-shorten-mode'.")

(custom-autoload 'global-telega-url-shorten-mode "telega-modes" nil)

(autoload 'global-telega-url-shorten-mode "telega-modes" "\
Global mode to shorten the URLs.

\(fn &optional ARG)" t nil)

(autoload 'telega-squash-message-mode "telega-modes" "\
Toggle message squashing minor mode.

\(fn &optional ARG)" t nil)

(defvar global-telega-squash-message-mode nil "\
Non-nil if Global Telega-Squash-Message mode is enabled.
See the `global-telega-squash-message-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-telega-squash-message-mode'.")

(custom-autoload 'global-telega-squash-message-mode "telega-modes" nil)

(autoload 'global-telega-squash-message-mode "telega-modes" "\
Global mode to squashing messages.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telega-modes" '("telega-")))

;;;***

;;;### (autoloads nil "telega-msg" "telega-msg.el" (0 0 0 0))
;;; Generated autoloads from telega-msg.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telega-msg" '("telega-")))

;;;***

;;;### (autoloads nil "telega-notifications" "telega-notifications.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from telega-notifications.el

(defvar telega-notifications-mode nil "\
Non-nil if Telega-Notifications mode is enabled.
See the `telega-notifications-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `telega-notifications-mode'.")

(custom-autoload 'telega-notifications-mode "telega-notifications" nil)

(autoload 'telega-notifications-mode "telega-notifications" "\
Telega D-Bus notifications.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telega-notifications" '("telega-")))

;;;***

;;;### (autoloads nil "telega-obsolete" "telega-obsolete.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from telega-obsolete.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telega-obsolete" '("telega-obsolete--")))

;;;***

;;;### (autoloads nil "telega-root" "telega-root.el" (0 0 0 0))
;;; Generated autoloads from telega-root.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telega-root" '("telega-")))

;;;***

;;;### (autoloads nil "telega-server" "telega-server.el" (0 0 0 0))
;;; Generated autoloads from telega-server.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telega-server" '("telega-" "with-telega-deferred-events")))

;;;***

;;;### (autoloads nil "telega-sort" "telega-sort.el" (0 0 0 0))
;;; Generated autoloads from telega-sort.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telega-sort" '("join-date" "order" "online-members" "member-count" "telega-" "title" "unread-count" "define-telega-sorter")))

;;;***

;;;### (autoloads nil "telega-sticker" "telega-sticker.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from telega-sticker.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telega-sticker" '("telega-")))

;;;***

;;;### (autoloads nil "telega-tdlib" "telega-tdlib.el" (0 0 0 0))
;;; Generated autoloads from telega-tdlib.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telega-tdlib" '("telega-" "with-telega-server-reply")))

;;;***

;;;### (autoloads nil "telega-tme" "telega-tme.el" (0 0 0 0))
;;; Generated autoloads from telega-tme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telega-tme" '("telega-tme-")))

;;;***

;;;### (autoloads nil "telega-ton" "telega-ton.el" (0 0 0 0))
;;; Generated autoloads from telega-ton.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telega-ton" '("telega-ton-")))

;;;***

;;;### (autoloads nil "telega-user" "telega-user.el" (0 0 0 0))
;;; Generated autoloads from telega-user.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telega-user" '("telega-")))

;;;***

;;;### (autoloads nil "telega-util" "telega-util.el" (0 0 0 0))
;;; Generated autoloads from telega-util.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telega-util" '("telega-")))

;;;***

;;;### (autoloads nil "telega-voip" "telega-voip.el" (0 0 0 0))
;;; Generated autoloads from telega-voip.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telega-voip" '("telega-")))

;;;***

;;;### (autoloads nil "telega-vvnote" "telega-vvnote.el" (0 0 0 0))
;;; Generated autoloads from telega-vvnote.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telega-vvnote" '("telega-vvnote-")))

;;;***

;;;### (autoloads nil "telega-webpage" "telega-webpage.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from telega-webpage.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "telega-webpage" '("telega-")))

;;;***

;;;### (autoloads nil nil ("telega-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; telega-autoloads.el ends here
