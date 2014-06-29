(require 'go-mode)

(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

;; running tests
(defun go-test ()
  "Runs tests for package in current buffer"
  (interactive)
  (compile (concat "ginkgo " (file-name-directory (buffer-file-name)))))

(defun go-unfocus ()
  "Unfocuses tests for the current file"
  (interactive)
  (compile (concat "ginkgo unfocus " (file-name-directory (buffer-file-name))))
  (update-buffers))

;; test toggling
(eval-after-load 'find-file
  '(progn
     (add-to-list 'cc-other-file-alist '(".go" ("_test.go")))
     (add-to-list 'cc-other-file-alist '("_test.go" (".go")))
     ))

(define-key tj-map (kbd "r t") 'go-test)
(define-key tj-map (kbd "u f") 'go-unfocus)
(global-set-key (kbd "M-T") 'ff-find-other-file)

(provide 'tj-go)
