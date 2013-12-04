(eval-after-load 'find-file
  '(progn
     (add-to-list 'cc-other-file-alist '("\\.cu\\'"  (".h")))
     (add-to-list 'cc-other-file-alist '("\\.m\\'"  (".h")))
     (add-to-list 'cc-other-file-alist '("\\.mm\\'"  (".h")))

     (add-to-list 'cc-other-file-alist
                  `("\\.h\\'"
                    ,(append  '(".cu") (cadr (assoc "\\.h\\'" cc-other-file-alist)))))))

(add-hook 'c-mode-common-hook 'c-mode)
(defvar c-mode-map (make-sparse-keymap)
  "Bindings for c-mode with prefix maps")

(define-prefix-command 'c-prefix-map)
(define-key c-prefix-map (kbd "s u") 'c-sort-includes)
(define-key c-mode-map (kbd "<f7>") '(lambda ()
                                            (interactive)
                                            (ff-find-other-file nil t)))

(define-key c-mode-map (kbd "s-n") 'c-prefix-map)
(define-key c-mode-map (kbd "s-a") 'c-prefix-map)
(define-key c-mode-map (kbd "s-j") 'c-prefix-map)

(defun c-sort-includes ()
  "Sorts #include statements by length"
  (interactive)
  (save-excursion
    (let (beg end)
      (goto-char (point-min))
      (while (not (looking-at "#include "))
        (forward-line 1))
      (setq beg (point))
      (while (looking-at "#include ")
        (forward-line 1))
      (setq end (point))
      (sort-lines-by-length-in-region beg end)
      (uniq-lines-in-region beg end))))

(defun c-sort-imports ()
  "Sort #import statements by length"
  (interactive)
  (save-excursion
    (let (beg end)
      (goto-char (point0min))
      (while (not (looking-at "#import "))
        (forward-line 1))
      (setq beg (point))
      (while (looking-at "#import ")
        (forward-line 1))
      (setq end (point))
      (sort-lines-by-length-in-region beg end)
      (uniq-lines-in-region beg end))))

(define-minor-mode c-mode
  "Minor mode for c-specific functions

Commands:
\\{c-mode-map}
"
  :init-value nil
  :keymap c-mode-map)

(provide 'c)
