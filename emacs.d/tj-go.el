(require 'go-mode)

;; running tests
(defun go-test ()
  "Runs ginkgo specs for the current buffer's file's package"
  (interactive)
  (compile (concat "ginkgo " (file-name-directory (buffer-file-name)))))

(defun go-unfocus ()
  "Unfocuses ginkgo specs for the current file"
  (interactive)
  (compile (concat "ginkgo unfocus " (file-name-directory (buffer-file-name))))
  (update-buffers))

;; test toggling
(eval-after-load 'find-file
  '(progn
     (add-to-list 'cc-other-file-alist '(".go" ("_test.go")))
     (add-to-list 'cc-other-file-alist '("_test.go" (".go")))
     ))

;; keybindings
(define-prefix-command 'tj-golang-prefix-map)
(defvar tj-golang-mode-map (make-sparse-keymap)
  "Bindings for tj-golang-mode")

(define-key tj-golang-prefix-map (kbd "r t") 'go-test)
(define-key tj-golang-prefix-map (kbd "u f") 'go-unfocus)

(define-key tj-golang-mode-map (kbd "s-a") 'tj-golang-prefix-map)
(define-key tj-golang-mode-map (kbd "s-j") 'tj-golang-prefix-map)
(define-key tj-golang-mode-map (kbd "s-n") 'tj-golang-prefix-map)

;; setup minor mode for before-save-hook and other goodies
(define-minor-mode tj-golang-mode
  "Minor mode for golang-specific behavior

Commands:
\\{tj-golang-mode-map}
"
  :init-value nil
  :keymap tj-golang-mode-map
  (make-local-variable 'hippie-expand-try-functions-list))

;; setup before save hook when in go-mode
(add-hook 'go-mode-hook
          (lambda ()
            (tj-golang-mode t)
            (setq gofmt-command "goimports")
            (add-hook 'before-save-hook 'gofmt-before-save)))

(provide 'tj-go)
