(require 'ruby-mode)
;; inf-ruby
(add-hook 'comint-mode-hook '(lambda () (setq comint-process-echo t)))

;; allow ansi-colors in inf-ruby modes
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on)

(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.feature$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rxml$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.t$" . ruby-mode))

;; test toggling
(eval-after-load 'find-file
  '(progn
     (add-to-list 'cc-other-file-alist '("\\.rb$" (".t")))
     (add-to-list 'cc-other-file-alist '("\\.t$" (".rb")))))

(defun tj-ruby-completion (&optional arg)
  (interactive "P")
  (if (string-equal (current-word) "end")
      nil
    (hippie-expand arg)))

(eval-after-load 'smart-tab
  '(progn
     (add-to-list 'smart-tab-completion-functions-alist
                  '(ruby-mode . tj-ruby-completion))))

;; ruby-mode setup
(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key "\r" 'newline-and-indent)
            (tj-ruby-mode t)))
(setq ruby-deep-indent-paren nil)

;; nb ruby minor mode
(defvar tj-ruby-mode-map (make-sparse-keymap)
  "Bindings for tj-ruby-mode with prefix maps")

(define-prefix-command 'tj-ruby-prefix-map)
(define-key tj-ruby-prefix-map (kbd "a r") 'tj-ruby-add-requires)
(define-key tj-ruby-prefix-map (kbd "a u") 'tj-ruby-add-requires)
(define-key tj-ruby-prefix-map (kbd "c c") 'tj-ruby-insert-current-class)
(define-key tj-ruby-prefix-map (kbd "d m") 'tj-ruby-declare-module)
(define-key tj-ruby-prefix-map (kbd "f t") 'tj-ruby-full-test)
(define-key tj-ruby-prefix-map (kbd "p t") 'tj-ruby-partial-test)
(define-key tj-ruby-prefix-map (kbd "s c") 'tj-ruby-superclass)
(define-key tj-ruby-prefix-map (kbd "s u") 'tj-ruby-sort-uselines)
(define-key tj-ruby-prefix-map (kbd "t r") 'tj-ruby-time-region)
(define-key tj-ruby-prefix-map (kbd "d r") 'tj-ruby-deparse)

(define-key tj-ruby-mode-map (kbd "s-n") 'tj-ruby-prefix-map)
(define-key tj-ruby-mode-map (kbd "s-a") 'tj-ruby-prefix-map)
(define-key tj-ruby-mode-map (kbd "s-j") 'tj-ruby-prefix-map)

(define-key tj-ruby-mode-map (kbd "<S-f5>") '(lambda ()
                                            (interactive)
                                            (ff-find-other-file nil t)))
(define-key tj-ruby-mode-map (kbd "<f4>") 'tj-ruby-open-module-at-point)
(define-key tj-ruby-mode-map (kbd "<f5>") 'tj-ruby-run-current-buffer-action)
(define-key tj-ruby-mode-map (kbd "C-c C-c") 'comment-dwim)

(modify-syntax-entry ?@' "w" ruby-mode-syntax-table)

(defun tj-ruby-run-current-buffer-action ()
  "Runs test file or other action for special buffers"
  (interactive)
    (run-test))

(defun tj-ruby-current-class ()
  (let ((case-fold-search nil)
        (class-name
         (save-excursion
           (skip-chars-backward "A-Za-z0-9:_")
           (let ((beg (point)))
             (skip-chars-forward "A-Za-z0-9:_")
             (buffer-substring beg (point))))))
    (cond ((and
            (string-match "^\\(.*\\)::\\([A-Z_]+\\)$" class-name)
            (> (length (match-string 2 class-name)) 5))
           (match-string 1 class-name))
          (t class-name))))

(defun tj-ruby-declare-module ()
  (interactive)
  (save-restriction
    (narrow-to-current-line)
    (beginning-of-line)
    (insert "module ")
    (let ((depth 0))
      (while (search-forward "::" nil t)
        (backward-delete-char 2)
        (insert "\nmodule ")
        (indent-for-tab-command)
        (setq depth (+ 1 depth)))
      (end-of-line)
      (while (>= depth 0)
        (insert "\nend")
        (indent-for-tab-command)
        (setq depth (- depth 1))))))

(defun tj-ruby-add-module-declaration ()
  (interactive)
  (save-restriction
    (goto-char (point-min))
    (search-forward-regexp "^\\(class\\|module\\) ::[^:
 ]+\\>")
    (let ((classname (match-string 2)))
      (forward-line -1)
      (insert "\n")
      (insert classname)
      (beginning-of-line)
      (tj-ruby-declare-module)
      (insert "\n"))))

(defun tj-ruby-guess-namespace ()
  (let* ((current-file (buffer-file-name))
         (start) (module-text "")
         (ruby-files
          (remove-if-not
           '(lambda (file)
              (or (string-match "^[^#]+\.rb$" file)
                  (string-equal file current-file)))
           (directory-files (file-name-directory (buffer-file-name))))))
    (catch 'break
      (mapc '(lambda (file)
               (condition-case nil
                   (save-excursion
                     (with-temp-buffer
                       (insert-file-contents file)
                       (goto-char (point-min))
                       (search-forward-regexp "^module\s*$")
                       (beginning-of-line)
                       (setq start (point))
                       (search-forward-regexp "^end\s*$")
                       (setq module-text (buffer-substring start (point)))
                       (throw 'break nil)))))
            ruby-files))
    (remove-if
     '(lambda (part) (string-equal part ""))
     (split-string
      (replace-regexp-in-string "\\(\s+\\|module\\|end\\)" "" module-text)))))

(defun tj-ruby-guess-declare-module ()
  "Insert module prefix declaration.

Guess based on what other files in the current directory"
  (interactive)
  (let ((depth 0)
        (namespace (tj-ruby-guess-namespace)))
    (insert
     (mapconcat '(lambda (part)
                   (let ((str (concat (make-string (* 2 depth) ? ) "module " part)))
                     (incf depth)
                     str))
                namespace
                "\n"))
    (insert "\n"
     (mapconcat '(lambda (part)
                   (decf depth)
                   (concat (make-string (* 2 depth) ? ) "end"))
                namespace
                "\n"))))

(defun tj-ruby-guess-class-prefix ()
  "Insert module prefix declaration inline.

Guess based on what other files in the current directory"
  (interactive)
  (insert (mapconcat 'identity (tj-ruby-guess-namespace) "::") "::"))

(defun tj-ruby-sort-uselines ()
  "Sort uselines by length"
  (interactive)
  (save-excursion
    (let (beg end)
      (goto-char (point-min))
      (while (not (looking-at "require '"))
        (forward-line 1))
      (setq beg (point))
      (while (looking-at "require '")
        (forward-line 1))
      (setq end (point))
      (sort-lines-by-length-in-region beg end)
      (uniq-lines-in-region beg end))))

(defun tj-ruby-add-requires ()
  "Add require line for class at point"
  (interactive)
  (save-excursion
    (let ((filename
           (downcase (replace-regexp-in-string "::" "/" (tj-ruby-current-class)))))
      (or
       (search-backward-regexp "^require " nil t)
       (progn
         (goto-char (point-min))
         (while (looking-at "^#")
           (forward-line 1))
         (forward-line -1)))
      (end-of-line)
      (insert "\nrequire '" filename "'")
      (tj-ruby-sort-uselines))))

(defun tj-ruby-partial-test ()
  "Only run the enclosing test"
  (interactive)
  (save-excursion
    (search-backward "def test")
    (save-excursion
      (goto-line 0)
      (replace-string "def test" "def _test"))
    (save-restriction
      (narrow-to-current-line)
      (beginning-of-line)
      (replace-string "def _test" "def test")))
  (save-buffer)
  (run-test))

(defun tj-ruby-full-test ()
  "Remove partial test restriction"
  (interactive)
  (save-excursion
    (goto-line 0)
    (replace-string "def _test" "def test"))
  (save-buffer))

(defun tj-ruby-current-class-for-file ()
  "Trys to find the current class name"
  (let ((class-name))
    (save-excursion
      (when (and
             (buffer-file-name)
             (string-match "\\(.*\\).t$" (buffer-file-name))
             (file-exists-p (concat (match-string 1 (buffer-file-name)) ".rb")))
        (set-buffer (find-file-noselect
                     (concat (match-string 1 (buffer-file-name)) ".rb"))))
      (goto-char (point-min))
      (while (not (or (looking-at "^class\s+\\")
                      (eobp)))
        (forward-line))
      (if (not (eobp))
          (setq class-name (match-string 1))))
    class-name))

(defun tj-ruby-he-class-beg ()
  (save-excursion (skip-chars-backward "A-Za-z0-9:") (point)))
(defun tj-ruby-he-class-end ()
  (save-excursion (skip-chars-forward "A-Za-z0-9:") (point)))

(defvar tj-ruby-hippie-expand-syntax-table
  (let ((map (copy-syntax-table ruby-mode-syntax-table)))
    (modify-syntax-entry ?:' "w" map)
    map))

(defadvice hippie-expand (around tj-ruby-hippie-syntax activate)
  (if (not tj-ruby-mode)
      ad-do-it
    (with-syntax-table tj-ruby-hippie-expand-syntax-table
      ad-do-it)))

(defun tj-ruby-module-at-point ()
  (save-excursion
    (skip-chars-backward "A-Za-z0-9:_")
    (let ((beg (point)) module)
      (skip-chars-forward "A-Za-z0-9:_")
      (setq module (buffer-substring beg (point)))
      module)))

(defun tj-ruby-insert-current-class ()
  "Inserts current class name or class for current .t file"
  (interactive)
  (let ((class-name (tj-ruby-current-class-for-file)))
    (if (not (null class-name))
        (insert class-name))))

(defun tj-ruby-file-for-module-at-point ()
  "Filename for module at point"
  (let ((root (or
               (root)
               (root-prompt)))
        (file (concat (replace-regexp-in-string "::" "/" (downcase (tj-ruby-module-at-point))) ".rb")))
    (with-temp-buffer
      (shell-command (format "find %s/*/lib %s/*/app -path '*%s'" root root file)
                     (current-buffer))
      (beginning-of-buffer)
      (buffer-substring (point) (line-end-position)))))

(defun tj-ruby-open-module-at-point ()
  "Open the module at point"
  (interactive)
  (find-file (tj-ruby-file-for-module-at-point)))

(defvar tj-ruby-add-buffer-requires-map
  (let ((map (make-sparse-keymap)))
    (define-key map "y" 'act)
    (define-key map "Y" 'act)
    (define-key map "n" 'skip)
    (define-key map "N" 'skip)
    (define-key map "q" 'exit)
    (define-key map "Q" 'exit)
    (define-key map "!" 'automatic)
    map))

(setq tj-ruby-time-region-counter 0)
(defun tj-ruby-time-region ()
  "Add debugging code to time region execution"
  (interactive)
  (save-excursion
    (let ((beg (region-beginning)) (end (region-end)))
      (goto-char end)
      (insert (format "puts \"Region #%d took #{Time.now-__start}s\"\n" tj-ruby-time-region-counter))
      (goto-char beg)
      (insert "__start = Time.now\n")
      (setq tj-ruby-time-region-counter (+ 1 tj-ruby-time-region-counter)))))

(defun tj-ruby-deparse (beg end)
  (interactive "r")
  (let ((region (buffer-substring beg end)) (buffer (get-buffer-create "*deparse-ruby*")))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (call-process "ruby" nil buffer nil "-rripper" "-e"
                    (format "p Ripper.sexp('%s')"
                            (replace-regexp-in-string "'" "\\\\'" region)))
      (display-message-or-buffer buffer))))

(define-minor-mode tj-ruby-mode
  "Minor mode for ruby-specific functions

Commands:
\\{tj-ruby-mode-map}
"
  :init-value nil
  :keymap tj-ruby-mode-map
  (make-local-variable 'hippie-expand-try-functions-list))

;; make inf-ruby use hippie expand completions on <TAB>
(defvar inf-ruby-he-expand-completions nil)
(defun inf-ruby-try-expand (old)
  (unless old
    (he-init-string (line-beginning-position) (point))
    (setq he-expand-list inf-ruby-he-expand-completions))
  (while (and he-expand-list
              (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn (if old (he-reset-string)) nil)
    (progn
      (he-substitute-string (car he-expand-list))
      (setq he-expand-list (cdr he-expand-list))
      t)))
(defadvice inf-ruby-complete-or-tab (around inf-ruby-hippie-complete activate)
  "Use hippie completion instead of completing read"
  (interactive)
  (let* ((command)
         (curr (thing-at-point 'line))
         (inf-ruby-he-expand-completions (inf-ruby-completions curr))
         (hippie-expand-try-functions-list '(inf-ruby-try-expand)))
    (hippie-expand nil)))

(defalias 'irb 'run-ruby)

(add-to-list 'compilation-error-regexp-alist-alist
             '(ruby-tap-testrunner "\\(/[^\s]+\\):\\([0-9]+\\)" 1 2))

(define-compilation-mode ruby-compilation "Ruby Compile"
  "Compilation mode for ruby"

  (set (make-local-variable 'compilation-error-regexp-alist) '(ruby-tap-testrunner ruby-Test::Unit)))


(provide 'tj-ruby)
