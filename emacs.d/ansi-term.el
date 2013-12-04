(require 'term)

(setq term-default-bg-color nil)
(setq term-default-fg-color nil)

;; Define additional terminal escape sequences:
;;   ^[AnSiXb <buffer name>
(defadvice term-handle-ansi-terminal-messages (after tj-escape activate)
  (while (string-match "\eAnSiX.+\n" ad-return-value)
    (let* ((start (match-beginning 0))
           (end (match-end 0))
           (command-code (aref ad-return-value (+ start 6)))
           (argument
            (save-match-data
              (substring ad-return-value
                         (+ start 8)
                         (string-match "\r?\n" ad-return-value (+ start 8)))))
           handled)
      (setq ad-return-value (replace-match "" t t ad-return-value))

      (cond ((= command-code ?b)
             (setq handled t)
             (rename-buffer (generate-new-buffer-name "*tmp*"))
             (rename-buffer (generate-new-buffer-name (concat "*" argument "*")))))
      (if handled
          (setq ad-return-value (replace-regexp-in-string "\032.+\n" "" ad-return-value))))))

(defun ansi-term-from-command (command buffer-name &rest switches)
  "Run a command in an ansi-term"
  (interactive)
  (if buffer-name
      (setq term-ansi-buffer-name buffer-name)
    (setq term-ansi-buffer-name "*ansi-term*"))
  (setq term-ansi-buffer-name (generate-new-buffer-name term-ansi-buffer-name))
  (apply 'term-ansi-make-term term-ansi-buffer-name command nil switches)
  (set-buffer term-ansi-buffer-name)
  (term-mode)
  (term-char-mode)
  (term-set-escape-char ?\C-x)
  (switch-to-buffer term-ansi-buffer-name))

(defun login-shell (&optional buffer-name)
  "Opens a login shell in an ansi-term buffer"
  (interactive)
  (let ((user-shell (shell-command-to-string "ruby -r etc -e 'print Etc.getpwuid(Process.uid).shell'")))
    (message user-shell)

    (ansi-term-from-command user-shell buffer-name "-l")))

(defun psql (&optional db-name)
  "Interactive psql session"
  (interactive)
  (ansi-term-from-command "/usr/local/pgsql/bin/psql" (concat "*psql-" db-name "*") db-name))

(provide 'ansi-term)
