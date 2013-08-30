(defun calc-eval-region (beg end)
  "Eval the arithmetic expression in the region and replace it with the result"
  (interactive "r")
  (let ((val (calc-eval (buffer-substring beg end))))
    (delete-region beg end)
    (insert val)))

(defun new-uuid ()
  (interactive)
  (insert (chomp (shell-command-to-string "uuidgen"))))

(defun ruby-compile (cmd)
  "Run compile with ruby test highlighting"
  (compile cmd 'ruby-compilation))

(defun run-buffer ()
  "Run the buffer"
  (interactive)
  (compile (buffer-file-name))
)

(defun kill-current-buffer ()
  "Kill current buffer"
  (interactive)
  (if (window-minibuffer-p) (keyboard-escape-quit)
    (if (string= "*Buffer List*" (buffer-name)) (keyboard-quit)
      (progn
        (kill-buffer (current-buffer))))))

(defun sort-lines-by-length-in-region (start end)
  (save-excursion
    (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (sort-subr nil
               'forward-line 'end-of-line
               nil nil
               (function (lambda (a b)
                           (let ((len-a (- (cdr a) (car a)))
                                 (len-b (- (cdr b) (car b))))
                             (cond
                              ((= len-a len-b)
                               (> 0 (compare-buffer-substrings
                                     nil (car a) (cdr a)
                                     nil (car b) (cdr b))))
                              (t (< len-a len-b))))))))))

(defun uniq-lines-in-region (start end)
  "Like running uniq on a region"
  (save-excursion
    (let ((query-replace-highlight nil))
    (replace-regexp "\\(^.*$\\)\n\\(\\1\n\\)+" "\\1\n" nil start end))))

(defun findcode (search)
  "Run a findcode in separate buffer"
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
  "Reload buffers from disk"
  (interactive)
  (let ((list (buffer-list)) buffer errmesg)
    (loop for buffer in (buffer-list) do
          (if (and (not (string-match "\\*" (buffer-name buffer)))
                   (buffer-file-name buffer)
                   (file-exists-p (buffer-file-name buffer)))
              (if (and (not (verify-visited-file-modtime buffer))
                       (buffer-modified-p buffer))
                  (setq errmesg (concat errmesg (format "Buffer '%s' has file and buffer changes!\n" buffer)))
                (if (not (verify-visited-file-modtime buffer))
                    (progn
                      (message "%s %s %s" (buffer-name buffer) (verify-visited-file-modtime buffer) (buffer-modified-p buffer))
                      (set-buffer buffer)
                      (message "Refreshing %s" (buffer-file-name buffer))
                      (revert-buffer t t t))))))
    (if errmesg (setq errmesg (chomp errmesg)))
    (message "%s" (or errmesg "Done"))
))
