(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit"))
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit/themes"))

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq custom-file (concat dotfiles-dir "custom.el"))

(require 'ruby-mode)
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)

(setq starter-kit-packages (list
                                 'magit
                                 'gist
                                 'deft
                                 'maxframe
                                 'inf-ruby
                                 'js2-mode
                                 'rvm
                                 'smex
                                 'rainbow-mode
                                 'full-ack
                                 ))
(require 'starter-kit-elpa)
(require 'cl)
(require 'ffap)
(require 'uniquify)
(require 'starter-kit-defuns)
(regen-autoloads)
(require 'defuns)
(require 'bindings)
(require 'js)
(require 'c)
(require 'ansi-term)
(require 'random-string)
(require 'cal)
(require 'settings)
(require 'ruby)
(require 'irc)

(load custom-file 'noerror)
(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-config) (load user-specific-config))
(if (file-exists-p user-specific-dir)
  (mapc #'load (directory-files user-specific-dir nil ".*el$")))

(require 'rainbow-mode)
(add-hook 'css-mode-hook (lambda () (rainbow-mode 1)))
(add-hook 'html-mode-hook (lambda () (rainbow-mode 1)))

(setq visible-bell 'top-bottom)
(setq font-height 130)

;; todo: break this out into a separate file
(require 'deft)
(setq deft-directory "~/.db/deft/")
(global-set-key [f1] 'deft)

(defun transpose-windows ()
  (interactive)
  (let ((this-buffer (window-buffer)))
    (switch-to-buffer (other-buffer this-buffer t))
    (switch-to-buffer-other-window this-buffer)
    (other-window -1)))

(require 'maxframe)
(defvar my-fullscreen-p t "Check if fullscreen is on or off")

(defun toggle-fullscreen ()
  (interactive)
  (setq my-fullscreen-p (not my-fullscreen-p))
  (if my-fullscreen-p
	  (restore-frame)
	(maximize-frame)))

(define-key tj-map (kbd "s b") 'transpose-windows)
(define-key tj-map (kbd "t f") 'toggle-fullscreen)

(when (window-system)
  (load-theme 'tomorrow-night)
  (let ((font-name "Inconsolata-dz:style=dz:size=14"))
    (when (not (null (font-info font-name)))
      (add-to-list 'default-frame-alist `(font . ,font-name))
      (set-face-attribute 'default nil :height 180))))

(setq shell-file-name "/bin/sh") ; zsh, compile and rvm don't play nice
