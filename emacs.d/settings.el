; various mac setup things
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; use inconsolata if it is available
(defvar font-name "Inconsolata:style=Medium:size=18")

(when (window-system)
  (require 'color-theme)
  (color-theme)
  (when (not (null (font-info font-name)))
    (add-to-list 'default-frame-alist `(font . ,font-name))))

(setq whitespace-style '(lines-tail))

(blink-cursor-mode nil)

(setq uniquify-buffer-name-style 'post-forward)

; disable alarms completely
(setq ring-bell-function 'ignore)

;; Don't clutter up directories with files~
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))

(setq-default save-place t)
(setq save-place-file "~/.places.emacs.sav")

(setq line-move-visual nil)
(setq kill-whole-line t)

;; More standard mouse scrolling style
(setq mouse-wheel-progressive-speed nil)

(iswitchb-mode t)

(setq-default indent-tabs-mode nil
              c-default-style "k&r"
              indent-level 2
              c-indent-level 2
              css-indent-offset 2
              c-basic-offset 2
              sh-basic-offset 2
              tab-width 2 ;for external files with \t
              cperl-indent-level 2)

(setq cperl-under-as-char t
      cperl-indent-parens-as-block nil
      cperl-continued-statement-offset 2
      cperl-brace-offset 0
      cperl-close-paren-offset -2
      cperl-electric-parens nil
      cperl-autoindent-on-semi t
      cperl-auto-newline nil
      cperl-auto-newline-after-colon nil
      cperl-auto-newline-after-brace nil
      cperl-electric-linefeed nil
      cperl-electric-lbrace-space nil
      cperl-break-one-line-blocks-when-indent nil
      cperl-fix-hanging-brace-when-indent nil
      cperl-use-syntax-table-text-property t
      cperl-use-syntax-table-text-property-for-tags t
      cperl-invalid-face nil)

; show lines and columns
(line-number-mode t)
(column-number-mode t)

; short answers
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default show-trailing-whitespace t)

; modes
(add-to-list 'auto-mode-alist '("Makefile" . makefile-mode))
(add-to-list 'auto-mode-alist '("bash" . sh-mode))
(add-to-list 'auto-mode-alist '("emacs" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("elisp" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("/tmp/diff$" . diff-mode))

(add-to-list 'auto-mode-alist '(".json$" . js-mode))

;; lisp debug print entire expression
(setq eval-expression-print-length nil)
(setq print-length nil)

; font lock in all major modes
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)

;; always end a file with a newline
(setq require-final-newline t)

; don't want any startup message
(setq inhibit-startup-message t)

; tail compilation buffers
(setq compilation-scroll-output t)

; find-grep ignore gitfiles
(require 'grep)
(grep-apply-setting 'grep-find-command '("find . -path '*/.git' -prune -o -name development.log -prune -o -name test.log -prune -o -type f -exec grep -nH -e  {} /dev/null \\;" . 116))

(grep-apply-setting 'grep-find-command nil)

; scroll settings
(setq scroll-conservatively 500
      scroll-preserve-screen-position t)

; tab completion
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(require 'smart-tab)
(global-smart-tab-mode 1)
(setq hippie-expand-try-functions-list (list
  'try-expand-dabbrev-visible
  'try-expand-dabbrev
  'try-expand-dabbrev-all-buffers
  'try-expand-dabbrev-from-kill
  'try-complete-file-name-partially
  'try-complete-file-name
))

(setq smart-tab-using-hippie-expand t)
(setq smart-tab-disabled-major-modes '(term-mode inf-ruby-mode org-mode eshell-mode))

(defvar delete-trailing-whitespace-on-save t)
(defun toggle-buffer-delete-trailing-whitespace ()
  (interactive)
  (set
   (make-local-variable 'delete-trailing-whitespace-on-save)
   (not delete-trailing-whitespace-on-save))
  (message "delete whitespace %s"
           (if delete-trailing-whitespace-on-save
               "enabled"
             "disabled")))

(add-hook 'before-save-hook (lambda ()
                              (if delete-trailing-whitespace-on-save
                                  (delete-trailing-whitespace))))
(add-hook 'term-mode-hook (lambda () (setq show-trailing-whitespace nil)))

;; terminal colors
(setq ansi-term-color-vector
  [unspecified "black" "red3" "green3" "yellow3" "RoyalBlue1"
   "magenta3" "cyan3" "white"])

;; terminal back/forward word
(define-key term-raw-map (kbd "C-<left>") '(lambda () (interactive) (term-send-raw-string "\e[1;5D")))
(define-key term-raw-map (kbd "C-<right>") '(lambda () (interactive) (term-send-raw-string "\e[1;5C")))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

; make isearch put you at the start of the search, not the end
(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
(defun my-goto-match-beginning ()
  (if (and isearch-success isearch-forward isearch-other-end)
      (goto-char isearch-other-end)))

;; highlight commit long lines in magit
(add-hook 'magit-log-edit-mode-hook
          (lambda ()
            (whitespace-mode 1)
            (make-local-variable 'whitespace-style)
            (setq whitespace-style '(lines))))

;; use the latest ruby
(rvm-use "ruby-2.0.0-p353" "")

;; remember previous open buffers
(setq desktop-dirname "~/.emacs.d/")
(desktop-save-mode 1)

(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-expert t)

; turn off new delete behavior
(setq delete-active-region nil)

(electric-indent-mode 't)

(server-start)

(provide 'settings)
