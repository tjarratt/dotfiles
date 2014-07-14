;;; bindings.el ---
(define-prefix-command 'tj-map)
(global-set-key (kbd "s-j") 'tj-map)
(global-set-key (kbd "s-a") 'tj-map)

(define-key tj-map (kbd "d w") 'delete-trailing-whitespace)
(define-key tj-map (kbd "f c") 'findcode)
(define-key tj-map (kbd "g d") 'git-diff)
(define-key tj-map (kbd "g s") 'magit-status)
(define-key tj-map (kbd "i r") 'run-ruby)
(define-key tj-map (kbd "p g") 'psql)
(define-key tj-map (kbd "r i") 'random-ip)
(define-key tj-map (kbd "r m") 'random-mac)
(define-key tj-map (kbd "r n") 'random-number)
(define-key tj-map (kbd "r r") 'random-rhyme)
(define-key tj-map (kbd "r s") 'random-string)
(define-key tj-map (kbd "r u") 'random-unicode-string)
(define-key tj-map (kbd "t q") 'toggle-quotes)
(define-key tj-map (kbd "u b") 'update-buffers)
(define-key tj-map (kbd "u u") 'new-uuid)

(global-set-key (kbd "<f5>") 'previous-error)
(global-set-key (kbd "<f6>") 'next-error)
(global-set-key (kbd "<f8>") 'kill-current-buffer)

(global-set-key (kbd "s-<left>") 'git-time-machine-diff-backwards)
(global-set-key (kbd "s-<right>") 'git-time-machine-diff-forwards)

(global-set-key (kbd "C-x f") 'find-file-in-project)
(global-set-key (kbd "M-T") 'ff-find-other-file)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "M-g") 'set-mark-and-goto-line)

(global-set-key (kbd "C-'") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "M-'") 'kmacro-end-or-call-macro)

(global-set-key (kbd "<C-backspace>") 'backward-kill-word)
(global-set-key (kbd "<C-kp-delete>") 'kill-word)

(global-set-key (kbd "M-c") 'kill-ring-save)

(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)

(global-unset-key (kbd "C-t"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(global-set-key (kbd "<kp-delete>") 'delete-char)

(add-hook 'iswitchb-define-mode-map-hook
          '(lambda ()
             (define-key iswitchb-mode-map " " 'iswitchb-next-match)
             (define-key iswitchb-mode-map "\C-j" 'iswitchb-exit-minibuffer)))

(provide 'bindings)
