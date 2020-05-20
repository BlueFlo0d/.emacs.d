(deftheme k-bgex)

(let ((class '((class color) (min-colors 89)))
      (background   "#273763")
      (fringe "#1e2d57")
      (brighter-bg  "#435ca3")
      (highlight    "#1e2d57")
      (contrast-bg "#4a587d")
      (active-modeline  "#2e1e57")
      (foreground   "#d5dbfb")
      (comment      "#7e8db4")
      (red          "#aa4450")
      (orange       "#fdcb6e")
      (lime       "#ffeaa7")
      (lpurple        "#a29bfe")
      (brightgreen  "#ff4b33")
      (yellow         "#ffcb8f")
      (pink         "#fd79a8")
      (skyblue     "#2ec4ff")
      (lgreen       "#55efc4"))

  (custom-theme-set-faces
   'k-bgex
   `(default ((,class (:foreground ,foreground :background ,background))))
   `(bold ((,class (:weight bold))))
   `(bold-italic ((,class (:slant italic :weight bold))))
   `(underline ((,class (:underline t))))
   `(italic ((,class (:slant italic))))
   `(font-lock-builtin-face ((,class (:foreground ,pink))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,comment :slant italic))))
   `(font-lock-comment-face ((,class (:foreground ,comment :slant italic))))
   `(font-lock-doc-face ((,class (:foreground ,lgreen))))
   `(font-lock-doc-string-face ((,class (:foreground ,lime))))
   `(font-lock-function-name-face ((,class (:foreground ,skyblue))))
   `(font-lock-keyword-face ((,class (:foreground ,pink))))
   `(font-lock-negation-char-face ((,class (:foreground "#afafaf"))))
   `(font-lock-reference-face ((,class (:foreground ,lime))))
   `(font-lock-constant-face ((,class (:foreground ,yellow))))
   `(font-lock-preprocessor-face ((,class (:foreground ,brightgreen))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,lime))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,lgreen))))
   `(font-lock-string-face ((,class (:foreground ,lpurple))))
   `(font-lock-type-face ((,class (:foreground ,lgreen))))
   `(font-lock-variable-name-face ((,class (:foreground ,skyblue))))
   `(font-lock-warning-face ((,class (:weight bold :foreground ,orange))))
   `(shadow ((,class (:foreground ,comment))))
   `(success ((,class (:foreground ,brightgreen))))
   `(error ((,class (:foreground ,red))))
   `(warning ((,class (:foreground ,orange))))

   ;; web mode
   `(web-mode-variable-name-face ((,class (:foreground ,lgreen))))
   `(web-mode-css-property-name-face ((,class (:foreground ,lgreen))))
   `(web-mode-function-name-face ((,class (:foreground ,pink))))
   `(web-mode-filter-face ((,class (:foreground ,pink))))
   `(web-mode-filter-face ((,class (:foreground ,pink))))
   `(web-mode-html-attr-name-face ((,class (:foreground ,lgreen))))
   `(web-mode-html-tag-face ((,class (:foreground ,pink))))

   ;; Flycheck
   `(flycheck-error ((,class (:underline (:style wave :color ,red)))))
   `(flycheck-info ((,class (:underline (:style wave :color ,yellow)))))
   `(flycheck-warning ((,class (:underline (:style wave :color ,orange)))))
   `(flycheck-fringe-error ((,class (:foreground ,red))))
   `(flycheck-fringe-info ((,class (:foreground ,yellow))))
   `(flycheck-fringe-warning ((,class (:foreground ,orange))))

   ;; Flymake
   `(flymake-warnline ((,class (:underline (:style wave :color ,orange) :background ,background))))
   `(flymake-errline ((,class (:underline (:style wave :color ,red) :background ,background))))

   ;; Flyspell
   `(flyspell-incorrect ((,class (:underline (:style wave :color ,red)))))

   ;; Clojure errors
   `(clojure-test-failure-face ((,class (:background nil :inherit flymake-warnline))))
   `(clojure-test-error-face ((,class (:background nil :inherit flymake-errline))))
   `(clojure-test-success-face ((,class (:background nil :foreground nil :underline ,lpurple))))

   ;; EDTS errors
   `(edts-face-warning-line ((,class (:background nil :inherit flymake-warnline))))
   `(edts-face-warning-mode-line ((,class (:background nil :foreground ,orange :weight bold))))
   `(edts-face-error-line ((,class (:background nil :inherit flymake-errline))))
   `(edts-face-error-mode-line ((,class (:background nil :foreground ,red :weight bold))))

   ;; For Brian Carper's extended clojure syntax table
   `(clojure-keyword ((,class (:foreground ,lime))))
   `(clojure-parens ((,class (:foreground ,foreground))))
   `(clojure-braces ((,class (:foreground ,lpurple))))
   `(clojure-brackets ((,class (:foreground ,lime))))
   `(clojure-double-quote ((,class (:foreground ,yellow :background nil))))
   `(clojure-special ((,class (:foreground ,pink))))
   `(clojure-java-call ((,class (:foreground ,lgreen))))

   ;; Rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,foreground))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,yellow))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,lime))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,brightgreen))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,pink))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,foreground))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,yellow))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,lime))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,lpurple))))
   `(rainbow-delimiters-unmatched-face ((,class (:foreground ,red))))

   ;; MMM-mode
   `(mmm-code-submode-face ((,class (:background ,contrast-bg))))
   `(mmm-comment-submode-face ((,class (:inherit font-lock-comment-face))))
   `(mmm-output-submode-face ((,class (:background ,contrast-bg))))

   ;; Search
   `(match ((,class (:foreground ,pink :background ,background :inverse-video t))))
   `(isearch ((,class (:foreground ,lime :background ,background :inverse-video t))))
   `(isearch-lazy-highlight-face ((,class (:foreground ,yellow :background ,background :inverse-video t))))
   `(isearch-fail ((,class (:background ,background :inherit font-lock-warning-face :inverse-video t))))

   ;; Anzu
   `(anzu-mode-line ((,class (:foreground ,orange))))
   `(anzu-replace-highlight ((,class (:inherit isearch-lazy-highlight-face))))
   `(anzu-replace-to ((,class (:inherit isearch))))

   ;; IDO
   `(ido-subdir ((,class (:foreground ,lgreen))))
   `(ido-first-match ((,class (:foreground ,orange))))
   `(ido-only-match ((,class (:foreground ,lpurple))))
   `(ido-indicator ((,class (:foreground ,red :background ,background))))
   `(ido-virtual ((,class (:foreground ,comment))))

   ;; flx-ido
   `(flx-highlight-face ((,class (:inherit nil :foreground ,lime :weight bold :underline nil))))

   ;; which-function
   `(which-func ((,class (:foreground ,pink :background nil :weight bold))))

   ;; Emacs interface
   `(cursor ((,class (:background "#626262"))))
   `(fringe ((,class (:background ,fringe :foreground ,lpurple))))
   `(linum ((,class (:background ,fringe :foreground ,lpurple :italic nil :underline nil))))
   `(vertical-border ((,class (:foreground ,active-modeline))))
   `(border ((,class (:background ,fringe :foreground ,highlight))))
   `(border-glyph (nil))
   `(highlight ((,class (:inverse-video t :background ,highlight))))
   `(gui-element ((,class (:background ,contrast-bg))))
   `(mode-line ((,class (:background ,active-modeline :weight normal))))
   `(mode-line-buffer-id ((,class (:foreground ,lgreen :background nil))))
   `(mode-line-inactive ((,class (:inherit mode-line
                                           :foreground ,comment
                                           :background ,fringe :weight normal))))
   `(mode-line-emphasis ((,class (:foreground ,foreground :slant italic))))
   `(mode-line-highlight ((,class (:foreground ,lgreen :box nil :weight bold))))
   `(minibuffer-prompt ((,class (:foreground ,pink))))
   `(region ((,class (:background ,contrast-bg :inverse-video nil))))
   `(secondary-selection ((,class (:background ,highlight))))

   `(header-line ((,class (:inherit mode-line-inactive :foreground ,yellow :background nil))))

   `(trailing-whitespace ((,class (:background ,orange :foreground ,lime))))
   `(whitespace-empty ((,class (:foreground ,orange :background ,lime))))
   `(whitespace-hspace ((,class (:background ,contrast-bg))))
   `(whitespace-indentation ((,class (:background ,contrast-bg))))
   `(whitespace-line ((,class (:background ,contrast-bg))))
   `(whitespace-newline ((,class (:background ,contrast-bg))))
   `(whitespace-space ((,class (:background ,contrast-bg))))
   `(whitespace-space-after-tab ((,class (:background ,contrast-bg))))
   `(whitespace-space-before-tab ((,class (:background ,contrast-bg))))
   `(whitespace-tab ((,class (:background ,contrast-bg))))
   `(whitespace-trailing ((,class (:background ,contrast-bg))))

   ;; Parenthesis matching (built-in)))
   `(show-paren-match ((,class (:background ,brighter-bg))))
   `(show-paren-mismatch ((,class (:background ,red :foreground ,background))))

   ;; Smartparens paren matching
   `(sp-show-pair-match-face ((,class (:foreground nil :background nil :inherit show-paren-match))))
   `(sp-show-pair-mismatch-face ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))

   ;; Parenthesis matching (mic-paren)))
   `(paren-face-match ((,class (:foreground nil :background nil :inherit show-paren-match))))
   `(paren-face-mismatch ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))
   `(paren-face-no-match ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))

   ;; Parenthesis dimming (parenface)))
   `(paren-face ((,class (:foreground ,comment :background nil))))

   `(sh-heredoc ((,class (:foreground nil :inherit font-lock-string-face :weight normal))))
   `(sh-quoted-exec ((,class (:foreground nil :inherit font-lock-preprocessor-face))))
   `(slime-highlight-edits-face ((,class (:weight bold))))
   `(slime-repl-input-face ((,class (:weight normal :underline nil))))
   `(slime-repl-prompt-face ((,class (:underline nil :weight bold :foreground ,lgreen))))
   `(slime-repl-result-face ((,class (:foreground ,lpurple))))
   `(slime-repl-output-face ((,class (:foreground ,pink :background ,background))))
   `(slime-repl-inputed-output-face ((,class (:foreground ,comment))))

   `(csv-separator-face ((,class (:foreground ,orange))))

   `(diff-added ((,class (:foreground ,lpurple))))
   `(diff-changed ((,class (:foreground ,lgreen))))
   `(diff-removed ((,class (:foreground ,orange))))
   `(diff-header ((,class (:foreground ,yellow :background nil))))
   `(diff-file-header ((,class (:foreground ,pink :background nil))))
   `(diff-hunk-header ((,class (:foreground ,lgreen))))
   `(diff-refine-added ((,class (:inherit diff-added :inverse-video t))))
   `(diff-refine-removed ((,class (:inherit diff-removed :inverse-video t))))

   `(diff-hl-insert ((,class (:background ,lpurple))))
   `(diff-hl-change ((,class (:background ,pink))))
   `(diff-hl-delete ((,class (:background ,orange))))
   `(diff-hl-unknown ((,class (:background ,lgreen))))

   `(ediff-even-diff-A ((,class (:foreground nil :background nil :inverse-video t))))
   `(ediff-even-diff-B ((,class (:foreground nil :background nil :inverse-video t))))
   `(ediff-odd-diff-A  ((,class (:foreground ,comment :background nil :inverse-video t))))
   `(ediff-odd-diff-B  ((,class (:foreground ,comment :background nil :inverse-video t))))

   `(eldoc-highlight-function-argument ((,class (:foreground ,lpurple :weight bold))))

   ;; macrostep
   `(macrostep-expansion-highlight-face ((,class (:inherit highlight :foreground nil))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face ((,class (:foreground ,foreground))))
   `(undo-tree-visualizer-current-face ((,class (:foreground ,lpurple :weight bold))))
   `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,red))))
   `(undo-tree-visualizer-register-face ((,class (:foreground ,lime))))

   ;; dired+
   `(diredp-compressed-file-suffix ((,class (:foreground ,pink))))
   `(diredp-deletion ((,class (:inherit error :inverse-video t))))
   `(diredp-deletion-file-name ((,class (:inherit error))))
   `(diredp-date-time ((,class (:foreground ,pink))))
   `(diredp-dir-heading ((,class (:foreground ,lpurple :weight bold))))
   `(diredp-dir-name ((,class (:foreground ,yellow))))
   `(diredp-dir-priv ((,class (:foreground ,yellow :background nil))))
   `(diredp-exec-priv ((,class (:foreground ,orange :background nil))))
   `(diredp-executable-tag ((,class (:foreground ,red :background nil))))
   `(diredp-file-name ((,class (:foreground ,lime))))
   `(diredp-file-suffix ((,class (:foreground ,lpurple))))
   `(diredp-flag-mark ((,class (:foreground ,lpurple :inverse-video t))))
   `(diredp-flag-mark-line ((,class (:background nil :inherit highlight))))
   `(diredp-ignored-file-name ((,class (:foreground ,comment))))
   `(diredp-link-priv ((,class (:background nil :foreground ,lgreen))))
   `(diredp-mode-line-flagged ((,class (:foreground ,red))))
   `(diredp-mode-line-marked ((,class (:foreground ,lpurple))))
   `(diredp-no-priv ((,class (:background nil))))
   `(diredp-number ((,class (:foreground ,lime))))
   `(diredp-other-priv ((,class (:background nil :foreground ,lgreen))))
   `(diredp-rare-priv ((,class (:foreground ,red :background nil))))
   `(diredp-read-priv ((,class (:foreground ,lpurple :background nil))))
   `(diredp-symlink ((,class (:foreground ,lgreen))))
   `(diredp-write-priv ((,class (:foreground ,lime :background nil))))

   ;; Magit
   `(magit-blame-heading ((,class (:background ,highlight :foreground ,orange))))
   `(magit-blame-date ((,class (:foreground ,red))))
   `(magit-header-line ((,class (:inherit nil :weight bold))))
   `(magit-dimmed ((,class (:foreground ,comment))))
   `(magit-hash ((,class (:foreground ,comment))))
   `(magit-tag ((,class (:foreground ,lime))))
   `(magit-branch-local ((,class (:foreground ,yellow))))
   `(magit-branch-remote ((,class (:foreground ,lpurple))))
   `(magit-branch-current ((,class (:foreground ,pink))))
   `(magit-refname ((,class (:inherit comment))))
   `(magit-signature-good ((,class (:inherit success))))
   `(magit-signature-bad ((,class (:inherit error))))
   `(magit-signature-untrusted ((,class (:foreground ,yellow))))
   `(magit-signature-unmatched ((,class (:foreground ,yellow))))
   `(magit-cherry-equivalent ((,class (:foreground ,lgreen))))

   `(magit-log-graph ((,class (:foreground ,comment))))
   `(magit-log-author ((,class (:foreground ,orange))))
   `(magit-log-date ((,class (:foreground ,pink))))

   `(magit-process-ok ((,class (:inherit success))))
   `(magit-process-ng ((,class (:inherit error))))
   `(magit-section-heading ((,class (:foreground ,lime :weight bold))))
   `(magit-section-heading-selection ((,class (:foreground ,orange :weight bold))))
   `(magit-section-highlight ((,class (:inherit highlight))))

   ;; git-gutter
   `(git-gutter:modified ((,class (:foreground ,lgreen :weight bold))))
   `(git-gutter:added ((,class (:foreground ,lpurple :weight bold))))
   `(git-gutter:deleted ((,class (:foreground ,red :weight bold))))
   `(git-gutter:unchanged ((,class (:background ,lime))))

   ;; git-gutter-fringe
   `(git-gutter-fr:modified ((,class (:foreground ,lgreen :weight bold))))
   `(git-gutter-fr:added ((,class (:foreground ,lpurple :weight bold))))
   `(git-gutter-fr:deleted ((,class (:foreground ,red :weight bold))))

   ;; guide-key
   `(guide-key/prefix-command-face ((,class (:foreground ,pink))))
   `(guide-key/highlight-command-face ((,class (:foreground ,lpurple))))
   `(guide-key/key-face ((,class (:foreground ,comment))))

   `(link ((,class (:foreground nil :underline t))))
   `(widget-button ((,class (:underline t))))
   `(widget-field ((,class (:background ,contrast-bg :box (:line-width 1 :color ,foreground)))))

   ;; Compilation (most faces politely inherit from 'success, 'error, 'warning etc.)))
   `(compilation-column-number ((,class (:foreground ,lime))))
   `(compilation-line-number ((,class (:foreground ,lime))))
   `(compilation-message-face ((,class (:foreground ,pink))))
   `(compilation-mode-line-exit ((,class (:foreground ,lpurple))))
   `(compilation-mode-line-fail ((,class (:foreground ,red))))
   `(compilation-mode-line-run ((,class (:foreground ,pink))))

   ;; Grep
   `(grep-context-face ((,class (:foreground ,comment))))
   `(grep-error-face ((,class (:foreground ,red :weight bold :underline t))))
   `(grep-hit-face ((,class (:foreground ,pink))))
   `(grep-match-face ((,class (:foreground nil :background nil :inherit match))))

   `(regex-tool-matched-face ((,class (:foreground nil :background nil :inherit match))))

   ;; mark-multiple
   `(mm/master-face ((,class (:inherit region :foreground nil :background nil))))
   `(mm/mirror-face ((,class (:inherit region :foreground nil :background nil))))

   ;; helm
   `(helm-buffer-saved-out ((,class (:inherit warning))))
   `(helm-buffer-size ((,class (:foreground ,lime))))
   `(helm-buffer-not-saved ((,class (:foreground ,orange))))
   `(helm-buffer-process ((,class (:foreground ,yellow))))
   `(helm-buffer-directory ((,class (:foreground ,pink))))
   `(helm-ff-dotted-directory ((,class (:foreground ,comment))))
   `(helm-ff-dotted-symlink-directory ((,class (:foreground ,comment))))
   `(helm-ff-directory ((,class (:foreground ,yellow))))
   `(helm-candidate-number ((,class (:foreground ,red))))
   `(helm-match ((,class (:inherit match))))
   `(helm-selection ((,class (:background ,active-modeline))))
   `(helm-separator ((,class (:foreground ,lgreen))))
   `(helm-source-header ((,class (:weight bold :foreground ,orange :height 1.44))))

   ;; company
   `(company-preview ((,class (:foreground ,comment :background ,contrast-bg))))
   `(company-preview-common ((,class (:inherit company-preview :foreground ,lgreen))))
   `(company-preview-search ((,class (:inherit company-preview :foreground ,pink))))
   `(company-tooltip ((,class (:background ,contrast-bg))))
   `(company-tooltip-selection ((,class (:background ,highlight))))
   `(company-tooltip-common ((,class (:inherit company-tooltip :foreground ,lgreen))))
   `(company-tooltip-common-selection ((,class (:inherit company-tooltip-selection :foreground ,lgreen))))
   `(company-tooltip-search ((,class (:inherit company-tooltip :foreground ,pink))))
   `(company-tooltip-annotation ((,class (:inherit company-tooltip :foreground ,lpurple))))
   `(company-scrollbar-bg ((,class (:inherit 'company-tooltip :background ,fringe))))
   `(company-scrollbar-fg ((,class (:background ,lpurple))))
   `(company-echo-common ((,class (:inherit company-echo :foreground ,skyblue))))

   `(org-agenda-structure ((,class (:foreground ,lgreen))))
   `(org-agenda-date ((,class (:foreground ,pink :underline nil))))
   `(org-agenda-done ((,class (:foreground ,lpurple))))
   `(org-agenda-dimmed-todo-face ((,class (:foreground ,comment))))
   `(org-block ((,class (:foreground ,orange))))
   `(org-code ((,class (:foreground ,lime))))
   `(org-column ((,class (:background ,contrast-bg))))
   `(org-column-title ((,class (:inherit org-column :weight bold :underline t))))
   `(org-date ((,class (:foreground ,pink :underline t))))
   `(org-document-info ((,class (:foreground ,yellow))))
   `(org-document-info-keyword ((,class (:foreground ,lpurple))))
   `(org-document-title ((,class (:weight bold :foreground ,orange :height 1.44))))
   `(org-done ((,class (:foreground ,lpurple))))
   `(org-ellipsis ((,class (:foreground ,comment))))
   `(org-footnote ((,class (:foreground ,yellow))))
   `(org-formula ((,class (:foreground ,red))))
   `(org-hide ((,class (:foreground ,background :background ,background))))
   `(org-link ((,class (:foreground ,pink :underline t))))
   `(org-scheduled ((,class (:foreground ,lpurple))))
   `(org-scheduled-previously ((,class (:foreground ,yellow))))
   `(org-scheduled-today ((,class (:foreground ,lpurple))))
   `(org-special-keyword ((,class (:foreground ,orange))))
   `(org-table ((,class (:foreground ,lgreen))))
   `(org-todo ((,class (:foreground ,red))))
   `(org-upcoming-deadline ((,class (:foreground ,orange))))
   `(org-warning ((,class (:weight bold :foreground ,red))))

   `(markdown-url-face ((,class (:inherit link))))
   `(markdown-link-face ((,class (:foreground ,pink :underline t))))

   ;; hl-line-mode
   `(hl-line ((,class (:background ,highlight))))
   `(hl-sexp-face ((,class (:background ,contrast-bg))))
   `(highlight-symbol-face ((,class (:inherit isearch-lazy-highlight-face))))
   `(highlight-80+ ((,class (:background ,contrast-bg))))

   ;; Hydra
   `(hydra-face-blue ((,class (:foreground ,pink))))
   `(hydra-face-teal ((,class (:foreground ,yellow))))
   `(hydra-face-pink ((,class (:foreground ,lgreen))))
   `(hydra-face-red ((,class (:foreground ,red))))
   `(hydra-face-amaranth ((,class (:foreground ,orange))))

   ;; Python-specific overrides
   `(py-builtins-face ((,class (:foreground ,orange :weight normal))))

   ;; js2-mode
   `(js2-warning ((,class (:underline ,orange))))
   `(js2-error ((,class (:foreground nil :underline ,red))))
   `(js2-external-variable ((,class (:foreground ,lgreen))))
   `(js2-function-param ((,class (:foreground ,pink))))
   `(js2-instance-member ((,class (:foreground ,pink))))
   `(js2-private-function-call ((,class (:foreground ,red))))
   ;; js2-mode additional attributes for better syntax highlight in javascript
   `(js2-jsdoc-tag ((,class (:foreground ,yellow))))
   `(js2-jsdoc-type ((,class (:foreground ,orange))))
   `(js2-jsdoc-value ((,class (:foreground ,orange))))
   `(js2-function-call ((,class (:foreground ,foreground))))
   `(js2-object-property ((,class (:foreground ,foreground))))
   `(js2-private-member ((,class (:foreground ,lgreen))))
   `(js2-jsdoc-html-tag-name ((,class (:foreground ,orange))))
   `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,orange))))


   ;; js3-mode
   `(js3-warning-face ((,class (:underline ,orange))))
   `(js3-error-face ((,class (:foreground nil :underline ,red))))
   `(js3-external-variable-face ((,class (:foreground ,lgreen))))
   `(js3-function-param-face ((,class (:foreground ,pink))))
   `(js3-jsdoc-tag-face ((,class (:foreground ,orange))))
   `(js3-jsdoc-type-face ((,class (:foreground ,yellow))))
   `(js3-jsdoc-value-face ((,class (:foreground ,lime))))
   `(js3-jsdoc-html-tag-name-face ((,class (:foreground ,pink))))
   `(js3-jsdoc-html-tag-delimiter-face ((,class (:foreground ,lpurple))))
   `(js3-instance-member-face ((,class (:foreground ,pink))))
   `(js3-private-function-call-face ((,class (:foreground ,red))))

   ;; coffee-mode
   `(coffee-mode-class-name ((,class (:foreground ,orange :weight bold))))
   `(coffee-mode-function-param ((,class (:foreground ,lgreen))))

   ;; nxml
   `(nxml-name-face ((,class (:foreground unspecified :inherit font-lock-constant-face))))
   `(nxml-attribute-local-name-face ((,class (:foreground unspecified :inherit font-lock-variable-name-face))))
   `(nxml-ref-face ((,class (:foreground unspecified :inherit font-lock-preprocessor-face))))
   `(nxml-delimiter-face ((,class (:foreground unspecified :inherit font-lock-keyword-face))))
   `(nxml-delimited-data-face ((,class (:foreground unspecified :inherit font-lock-string-face))))
   `(rng-error-face ((,class (:underline ,red))))

   ;; RHTML
   `(erb-delim-face ((,class (:background ,contrast-bg))))
   `(erb-exec-face ((,class (:background ,contrast-bg :weight bold))))
   `(erb-exec-delim-face ((,class (:background ,contrast-bg))))
   `(erb-out-face ((,class (:background ,contrast-bg :weight bold))))
   `(erb-out-delim-face ((,class (:background ,contrast-bg))))
   `(erb-comment-face ((,class (:background ,contrast-bg :weight bold :slant italic))))
   `(erb-comment-delim-face ((,class (:background ,contrast-bg))))

   ;; Message-mode
   `(message-header-other ((,class (:foreground nil :background nil :weight normal))))
   `(message-header-subject ((,class (:inherit message-header-other :weight bold :foreground ,lime))))
   `(message-header-to ((,class (:inherit message-header-other :weight bold :foreground ,orange))))
   `(message-header-cc ((,class (:inherit message-header-to :foreground nil))))
   `(message-header-name ((,class (:foreground ,pink :background nil))))
   `(message-header-newsgroups ((,class (:foreground ,yellow :background nil :slant normal))))
   `(message-separator ((,class (:foreground ,lgreen))))

   ;; Jabber
   `(jabber-chat-prompt-local ((,class (:foreground ,lime))))
   `(jabber-chat-prompt-foreign ((,class (:foreground ,orange))))
   `(jabber-chat-prompt-system ((,class (:foreground ,lime :weight bold))))
   `(jabber-chat-text-local ((,class (:foreground ,lime))))
   `(jabber-chat-text-foreign ((,class (:foreground ,orange))))
   `(jabber-chat-text-error ((,class (:foreground ,red))))

   `(jabber-roster-user-online ((,class (:foreground ,lpurple))))
   `(jabber-roster-user-xa ((,class (:foreground ,comment))))
   `(jabber-roster-user-dnd ((,class (:foreground ,lime))))
   `(jabber-roster-user-away ((,class (:foreground ,orange))))
   `(jabber-roster-user-chatty ((,class (:foreground ,lgreen))))
   `(jabber-roster-user-error ((,class (:foreground ,red))))
   `(jabber-roster-user-offline ((,class (:foreground ,comment))))

   `(jabber-rare-time-face ((,class (:foreground ,comment))))
   `(jabber-activity-face ((,class (:foreground ,lgreen))))
   `(jabber-activity-personal-face ((,class (:foreground ,yellow))))

   ;; Outline
   `(outline-1 ((,class (:inherit nil :foreground ,pink))))
   `(outline-2 ((,class (:inherit nil :foreground ,lgreen))))
   `(outline-3 ((,class (:inherit nil :foreground ,yellow))))
   `(outline-4 ((,class (:inherit nil :foreground ,skyblue))))
   `(outline-5 ((,class (:inherit nil :foreground ,orange))))
   `(outline-6 ((,class (:inherit nil :foreground ,pink))))
   `(outline-7 ((,class (:inherit nil :foreground ,lgreen))))
   `(outline-8 ((,class (:inherit nil :foreground ,yellow))))
   `(outline-9 ((,class (:inherit nil :foreground ,lime))))

   ;; Ledger-mode
   `(ledger-font-comment-face ((,class (:inherit font-lock-comment-face))))
   `(ledger-font-occur-narrowed-face ((,class (:inherit font-lock-comment-face :invisible t))))
   `(ledger-font-occur-xact-face ((,class (:inherit highlight))))
   `(ledger-font-payee-cleared-face ((,class (:foreground ,lpurple))))
   `(ledger-font-payee-uncleared-face ((,class (:foreground ,yellow))))
   `(ledger-font-posting-date-face ((,class (:foreground ,orange))))
   `(ledger-font-posting-amount-face ((,class (:foreground ,foreground))))
   `(ledger-font-posting-account-cleared-face ((,class (:foreground ,pink))))
   `(ledger-font-posting-account-face ((,class (:foreground ,lgreen))))
   `(ledger-font-posting-account-pending-face ((,class (:foreground ,lime))))
   `(ledger-font-xact-highlight-face ((,class (:inherit highlight))))
   `(ledger-occur-narrowed-face ((,class (:inherit font-lock-comment-face :invisible t))))
   `(ledger-occur-xact-face ((,class (:inherit highlight))))

   ;; EMMS
   `(emms-browser-artist-face ((,class (:inherit outline-2))))
   `(emms-browser-album-face ((,class (:inherit outline-3))))
   `(emms-browser-track-face ((,class (:inherit outline-4))))
   `(emms-browser-year/genre-face ((,class (:inherit outline-1))))
   `(emms-playlist-selected-face ((,class (:inverse-video t))))
   `(emms-playlist-track-face ((,class (:inherit outline-4))))

   ;; mu4e
   `(mu4e-header-highlight-face ((,class (:underline nil :inherit region))))
   `(mu4e-header-marks-face ((,class (:underline nil :foreground ,lime))))
   `(mu4e-flagged-face ((,class (:foreground ,orange :inherit nil))))
   `(mu4e-replied-face ((,class (:foreground ,pink :inherit nil))))
   `(mu4e-unread-face ((,class (:foreground ,lime :inherit nil))))
   `(mu4e-cited-1-face ((,class (:inherit outline-1 :slant normal))))
   `(mu4e-cited-2-face ((,class (:inherit outline-2 :slant normal))))
   `(mu4e-cited-3-face ((,class (:inherit outline-3 :slant normal))))
   `(mu4e-cited-4-face ((,class (:inherit outline-4 :slant normal))))
   `(mu4e-cited-5-face ((,class (:inherit outline-5 :slant normal))))
   `(mu4e-cited-6-face ((,class (:inherit outline-6 :slant normal))))
   `(mu4e-cited-7-face ((,class (:inherit outline-7 :slant normal))))
   `(mu4e-ok-face ((,class (:foreground ,lpurple))))
   `(mu4e-view-contact-face ((,class (:inherit nil :foreground ,lime))))
   `(mu4e-view-link-face ((,class (:inherit link :foreground ,pink))))
   `(mu4e-view-url-number-face ((,class (:inherit nil :foreground ,yellow))))
   `(mu4e-view-attach-number-face ((,class (:inherit nil :foreground ,orange))))
   `(mu4e-highlight-face ((,class (:inherit highlight))))
   `(mu4e-title-face ((,class (:inherit nil :foreground ,lpurple))))

   ;; Gnus
   `(gnus-cite-1 ((,class (:inherit outline-1 :foreground nil))))
   `(gnus-cite-2 ((,class (:inherit outline-2 :foreground nil))))
   `(gnus-cite-3 ((,class (:inherit outline-3 :foreground nil))))
   `(gnus-cite-4 ((,class (:inherit outline-4 :foreground nil))))
   `(gnus-cite-5 ((,class (:inherit outline-5 :foreground nil))))
   `(gnus-cite-6 ((,class (:inherit outline-6 :foreground nil))))
   `(gnus-cite-7 ((,class (:inherit outline-7 :foreground nil))))
   `(gnus-cite-8 ((,class (:inherit outline-8 :foreground nil))))
   `(gnus-header-content ((,class (:inherit message-header-other))))
   `(gnus-header-subject ((,class (:inherit message-header-subject))))
   `(gnus-header-from ((,class (:inherit message-header-other-face :weight bold :foreground ,orange))))
   `(gnus-header-name ((,class (:inherit message-header-name))))
   `(gnus-button ((,class (:inherit link :foreground nil))))
   `(gnus-signature ((,class (:inherit font-lock-comment-face))))

   `(gnus-summary-normal-unread ((,class (:foreground ,pink :weight normal))))
   `(gnus-summary-normal-read ((,class (:foreground ,foreground :weight normal))))
   `(gnus-summary-normal-ancient ((,class (:foreground ,yellow :weight normal))))
   `(gnus-summary-normal-ticked ((,class (:foreground ,orange :weight normal))))
   `(gnus-summary-low-unread ((,class (:foreground ,comment :weight normal))))
   `(gnus-summary-low-read ((,class (:foreground ,comment :weight normal))))
   `(gnus-summary-low-ancient ((,class (:foreground ,comment :weight normal))))
   `(gnus-summary-high-unread ((,class (:foreground ,lime :weight normal))))
   `(gnus-summary-high-read ((,class (:foreground ,lpurple :weight normal))))
   `(gnus-summary-high-ancient ((,class (:foreground ,lpurple :weight normal))))
   `(gnus-summary-high-ticked ((,class (:foreground ,orange :weight normal))))
   `(gnus-summary-cancelled ((,class (:foreground ,red :background nil :weight normal))))

   `(gnus-group-mail-low ((,class (:foreground ,comment))))
   `(gnus-group-mail-low-empty ((,class (:foreground ,comment))))
   `(gnus-group-mail-1 ((,class (:foreground nil :weight normal :inherit outline-1))))
   `(gnus-group-mail-2 ((,class (:foreground nil :weight normal :inherit outline-2))))
   `(gnus-group-mail-3 ((,class (:foreground nil :weight normal :inherit outline-3))))
   `(gnus-group-mail-4 ((,class (:foreground nil :weight normal :inherit outline-4))))
   `(gnus-group-mail-5 ((,class (:foreground nil :weight normal :inherit outline-5))))
   `(gnus-group-mail-6 ((,class (:foreground nil :weight normal :inherit outline-6))))
   `(gnus-group-mail-1-empty ((,class (:inherit gnus-group-mail-1 :foreground ,comment))))
   `(gnus-group-mail-2-empty ((,class (:inherit gnus-group-mail-2 :foreground ,comment))))
   `(gnus-group-mail-3-empty ((,class (:inherit gnus-group-mail-3 :foreground ,comment))))
   `(gnus-group-mail-4-empty ((,class (:inherit gnus-group-mail-4 :foreground ,comment))))
   `(gnus-group-mail-5-empty ((,class (:inherit gnus-group-mail-5 :foreground ,comment))))
   `(gnus-group-mail-6-empty ((,class (:inherit gnus-group-mail-6 :foreground ,comment))))
   `(gnus-group-news-1 ((,class (:foreground nil :weight normal :inherit outline-5))))
   `(gnus-group-news-2 ((,class (:foreground nil :weight normal :inherit outline-6))))
   `(gnus-group-news-3 ((,class (:foreground nil :weight normal :inherit outline-7))))
   `(gnus-group-news-4 ((,class (:foreground nil :weight normal :inherit outline-8))))
   `(gnus-group-news-5 ((,class (:foreground nil :weight normal :inherit outline-1))))
   `(gnus-group-news-6 ((,class (:foreground nil :weight normal :inherit outline-2))))
   `(gnus-group-news-1-empty ((,class (:inherit gnus-group-news-1 :foreground ,comment))))
   `(gnus-group-news-2-empty ((,class (:inherit gnus-group-news-2 :foreground ,comment))))
   `(gnus-group-news-3-empty ((,class (:inherit gnus-group-news-3 :foreground ,comment))))
   `(gnus-group-news-4-empty ((,class (:inherit gnus-group-news-4 :foreground ,comment))))
   `(gnus-group-news-5-empty ((,class (:inherit gnus-group-news-5 :foreground ,comment))))
   `(gnus-group-news-6-empty ((,class (:inherit gnus-group-news-6 :foreground ,comment))))

   `(erc-direct-msg-face ((,class (:foreground ,orange))))
   `(erc-error-face ((,class (:foreground ,red))))
   `(erc-header-face ((,class (:foreground ,foreground :background ,highlight))))
   `(erc-input-face ((,class (:foreground ,lpurple))))
   `(erc-keyword-face ((,class (:foreground ,lime))))
   `(erc-current-nick-face ((,class (:foreground ,lpurple))))
   `(erc-my-nick-face ((,class (:foreground ,lpurple))))
   `(erc-nick-default-face ((,class (:weight normal :foreground ,lgreen))))
   `(erc-nick-msg-face ((,class (:weight normal :foreground ,lime))))
   `(erc-notice-face ((,class (:foreground ,comment))))
   `(erc-pal-face ((,class (:foreground ,orange))))
   `(erc-prompt-face ((,class (:foreground ,pink))))
   `(erc-timestamp-face ((,class (:foreground ,yellow))))
   `(erc-keyword-face ((,class (:foreground ,lpurple))))

   ;; twittering-mode
   `(twittering-username-face ((,class (:inherit erc-pal-face))))
   `(twittering-uri-face ((,class (:foreground ,pink :inherit link))))
   `(twittering-timeline-header-face ((,class (:foreground ,lpurple :weight bold))))
   `(twittering-timeline-footer-face ((,class (:inherit twittering-timeline-header-face))))

   `(custom-variable-tag ((,class (:foreground ,pink))))
   `(custom-group-tag ((,class (:foreground ,pink))))
   `(custom-state ((,class (:foreground ,lpurple))))

   ;; ansi-term
   `(term ((,class (:foreground nil :background nil :inherit default))))
   `(term-color-black   ((,class (:foreground ,foreground :background ,foreground))))
   `(term-color-red     ((,class (:foreground ,red :background ,red))))
   `(term-color-green   ((,class (:foreground ,lpurple :background ,lpurple))))
   `(term-color-yellow  ((,class (:foreground ,lime :background ,lime))))
   `(term-color-blue    ((,class (:foreground ,pink :background ,pink))))
   `(term-color-magenta ((,class (:foreground ,lgreen :background ,lgreen))))
   `(term-color-cyan    ((,class (:foreground ,yellow :background ,yellow))))
   `(term-color-white   ((,class (:foreground ,background :background ,background))))

   ;; e2wm
   `(e2wm:face-history-list-normal ((,class (:foreground ,foreground :background ,background))))
   `(e2wm:face-history-list-select1 ((,class (:foreground ,yellow :background ,background))))
   `(e2wm:face-history-list-select2 ((,class (:foreground ,lime :background ,background))))

   ;; rpm-spec-mode
   `(rpm-spec-dir-face ((,class (:foreground ,lpurple))))
   `(rpm-spec-doc-face ((,class (:foreground ,lpurple))))
   `(rpm-spec-ghost-face ((,class (:foreground ,red))))
   `(rpm-spec-macro-face ((,class (:foreground ,lime))))
   `(rpm-spec-obsolete-tag-face ((,class (:foreground ,red))))
   `(rpm-spec-package-face ((,class (:foreground ,red))))
   `(rpm-spec-section-face ((,class (:foreground ,lime))))
   `(rpm-spec-tag-face ((,class (:foreground ,pink))))
   `(rpm-spec-var-face ((,class (:foreground ,red))))

   ;; sx
   `(sx-question-mode-content-face ((,class (:background ,highlight))))
   `(sx-question-list-answers ((,class (:height 1.0 :inherit sx-question-list-parent :foreground ,lpurple))))
   `(sx-question-mode-accepted ((,class (:height 1.5 :inherit sx-question-mode-title :foreground ,lpurple))))
   `(sx-question-mode-kbd-tag ((,class (:height 0.9 :weight semi-bold :box (:line-width 3 :style released-button :color ,contrast-bg)))))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'k-bgex)
