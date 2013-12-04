(defun calc-eval-region (start end)
  "evaluate an expression in a given region and replace it with the result"
  (interactive "r")
  (let ((value (calc-eval (buffer-substring start end))))
    (delete-region start end)
    (insert value)))

(defun new-uuid ()
  (interactive)
  (insert (chomp (shell-command-to-string "uuidgen"))))

(defun ruby-compile (cmd)
  "compile with ruby syntax"
  (compile cmd 'ruby-compilation))

(defun run-buffer ()
  "run a buffer from a file"
  (interactive)
  (compile (buffer-file-name))
)

(defun kill-current-buffer ()
  "kill the active buffer"
  (interactive)
  (if (window-minibuffer-p) (keyboard-escape-quit)
    (if (string= "*Buffer List*" (buffer-name)) (keyboard-quit)
      (progn
        (kill-buffer (current-buffer))))))

(defun uniq-lines-in-region (start end)
  "run uniq on a region instead of an entire buffer"
  (save-excursion
    (let ((query-replace-highlight nil))
    (replace-regexp "\\(^.*$\\)\n\\(\\1\n\\)+" "\\1\n" nil start end))))

(defun findcode (search)
  "run findcode in another buffer"
  (interactive
   (list (read-string "Search: "
                      (if (region-active-p)
                          (buffer-substring (region-beginning) (region-end))
                        (current-word)))))
  (let ((command (concat "findcode " search))
        (compilation-buffer-name-function
         (lambda (mode-name)
           (format "*%s*" command))))
    (grep command)))

(defun update-buffers ()
  "reload all buffers"
  (interactive)
  (let ((list (buffer-list)) buffer errmesg)
    (loop for buffer in (buffer-list) do
          (if (and (not (string-match "\\*" (buffer-name buffer)))
                   (buffer-file-name buffer)
                   (file-exists-p (buffer-file-name buffer)))
              (if (and (not (verify-visited-file-modtime buffer))
                       (buffer-modified-p buffer))
                  (setq errmesg (concat errmesg (format "buffer '%s' has conflicting changes from buffer and file\n" buffer)))
                (if (not (verify-visited-file-modtime buffer))
                    (progn
                      (message "%s %s %s" (buffer-name buffer) (verify-visited-file-modtime buffer) (buffer-modified-p buffer))
                      (set-buffer buffer)
                      (revert-buffer t t t))))))
    (if errmesg (setq errmesg (chomp errmesg)))
    (message "%s" (or errmesg "Done"))
))

(provide 'defuns)
