(defun deactivate-hacker-type ()
  (interactive)
  (define-key fake-hacker-type-map [remap self-insert-command] nil)
)

(defun inject_contents (&optional n)
  (interactive)
  (insert (elt fileContents lineNumber))
  (insert "\n")
  (setq lineNumber (+ lineNumber 1))

  (if (>= lineNumber (length fileContents))
      (switch-to-app-dot-go)))

(defun hacker-type (&optional filename)
  (interactive)
  (setq fake-hacker-type-map (make-sparse-keymap))

  (setq shouldBailWhenDone 0)
  (setq filename "/Users/tjarratt/go/src/github.com/tjarratt/cli/cf/app/app_test.go")
  (setq fileContents (get-lines-from-file filename))
  (setq lineNumber 0)
  (setq insert_by 22)

  (define-key fake-hacker-type-map [remap self-insert-command] 'inject_contents)
  (use-local-map fake-hacker-type-map)
)

(defun get-lines-from-file (filePath)
  "Returns the lines in the file $filePath"
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun switch-to-app-dot-go
  (setq filename "/Users/tjarratt/go/src/github.com/tjarratt/cli/cf/app/app.go")
  (setq lineNumber 0))
