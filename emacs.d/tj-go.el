(require 'go-mode)

(setq gofmt-command "goimports")
(add-to-list 'load-path "/home/you/goroot/misc/emacs/")
(add-hook 'before-save-hook 'gofmt-before-save)

(provide 'tj-go)
