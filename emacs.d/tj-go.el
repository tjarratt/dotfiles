(require 'go-mode)

(setq gofmt-command "goimports")
(add-to-list 'load-path "/home/you/goroot/misc/emacs/")
(add-hook 'before-save-hook 'gofmt-before-save)

(defun go-test ()
  "Runs tests for package in current buffer"
  (interactive)
  (compile (concat "ginkgo " (file-name-directory (buffer-file-name)))))

(provide 'tj-go)
