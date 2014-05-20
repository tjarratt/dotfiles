(defun activate-hacker-mode ()
  (interactive)
  (message "Scott detected. Let's make this a bit easier...")
  (setq fake-hacker-type-map (make-sparse-keymap))

  (setq shouldBailWhenDone 0)
  (write-region "" nil "/Users/tjarratt/go/src/github.com/tjarratt/cli/cf/app/app.go")
  (find-file "/Users/tjarratt/go/src/github.com/tjarratt/cli/cf/app/app.go")
  (setq filename "/Users/tjarratt/hackz/app.go")
  (setq fileContents (get-lines-from-file filename))
  (setq lineNumber 0)

  (setq start 0)
  (setq jumpBy 3)

  (define-key fake-hacker-type-map [remap self-insert-command] 'inject_contents)
  (use-local-map fake-hacker-type-map)
)

(defun deactivate-hacker-mode ()
  (interactive)
  (message "Scott-Mode deactivated. Standing down.")
  (define-key fake-hacker-type-map [remap self-insert-command] nil))

(global-set-key (kbd "<f1>") 'activate-hacker-mode)
(global-set-key (kbd "<f2>") 'deactivate-hacker-mode)
(global-set-key (kbd "<f5>") 'turbo-mode)

(defun turbo-mode ()
  (interactive)
  (message "TURBO MODE ACTIVATED")
  (setq jumpBy 22))

(defun inject_contents ()
  (interactive)

  (if (< lineNumber (length fileContents))
      (insert-next-piece)))

(defun insert-next-piece ()
  (setq substr (substring (elt fileContents lineNumber) start (min (+ start jumpBy) (length (elt fileContents lineNumber)))))
  (insert substr)

  (setq start (+ start jumpBy))

  (if (>= start (length (elt fileContents lineNumber)))
      (jump-to-next-line)))

(defun jump-to-next-line ()
  (insert "\n")
  (setq start 0)
  (setq lineNumber (+ lineNumber 1)))


(defun get-lines-from-file (filePath)
  "Returns the lines in the file $filePath"
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))
