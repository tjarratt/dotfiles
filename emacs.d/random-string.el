(defvar random-string-strategy 'vanilla)
(defun random-string ()
  "Insert a new random string"
  (interactive)
  (let ((string (funcall (intern (concat "random-string-" (symbol-name random-string-strategy))))))
    (insert string)))

(defun random-unicode-string ()
  "Insert a random string with some gratuitious Unicode"
  (interactive)
  (insert (random-string-with-symbol)))

(defun random-string-shell ()
  "Shell out to produce a random string"
  (interactive)
  (replace-regexp-in-string "'" "\\\\'" (shell-command-to-string "random-string -i")))

(defvar vanilla-dictionary nil)
(defun load-vanilla-dictionary ()
  (when (null vanilla-dictionary)
    (with-temp-buffer
      (insert-file-contents "/usr/share/dict/words")
      (setq vanilla-dictionary (split-string (buffer-string) "\n" t)))))

(defun random-word ()
  (nth (random (length vanilla-dictionary)) vanilla-dictionary))

(defun random-string-vanilla ()
  "Just a vanilla random-string, from elisp."
  (load-vanilla-dictionary)
  (concat (random-word) "-" (random-word)))

(defvar adj-dictionary nil)
(defvar noun-dictionary nil)
(defun random-string-np ()
  "Insert a noun phrase"
  (when (null adj-dictionary)
    (with-temp-buffer
      (insert-file-contents "/usr/share/dict/adj")
      (setq adj-dictionary (split-string (buffer-string) "\n" t)))
    (with-temp-buffer
      (insert-file-contents "/usr/share/dict/noun")
      (setq noun-dictionary (split-string (buffer-string) "\n" t))))
  (concat (nth (random (length adj-dictionary)) adj-dictionary)
          "-"
          (nth (random (length noun-dictionary)) noun-dictionary)))

(defvar rhyme-dictionary nil)
(defun random-string-rhyme ()
  "Insert a random rhyme"
  (when (null rhyme-dictionary)
    (with-temp-buffer
      (insert-file-contents "/usr/share/dict/rhymes")
      (setq rhyme-dictionary (split-string (buffer-string) "\n" t))))
  (let* ((words (shuffle-vector (apply 'vector (split-string (nth (random (length rhyme-dictionary)) rhyme-dictionary) " ")))))
    (concat (aref words 0)
            "-"
            (aref words 1))))

(defvar alliteration-dictionary nil)
(defun random-string-alliteration ()
  "Generates a random alliteration"
  (save-match-data
    (when (null alliteration-dictionary)
      (with-temp-buffer
        (insert-file-contents "/usr/share/dict/words")
        (setq alliteration-dictionary
              (remove-if '(lambda (str) (string-match "'" str))
                         (split-string (buffer-string) "\n" t)))))
    (let* ((char (char-to-string (+ 97 (random 26))))
           (words (remove-if-not '(lambda (str) (string-match (concat "^" char) str))
                                 alliteration-dictionary)))
      (concat (nth (random (length words)) words)
              "-"
              (nth (random (length words)) words)))))

(defun random-string-with-symbol ()
  "Just a vanilla random-string, with a little extra in the middle."
  (load-vanilla-dictionary)
  (concat (random-word) "-" (exciting-symbol) "-" (random-word)))

(defun exciting-symbol ()
  "Unicode miscellaneous symbols block data"
  (char-to-string (+ 9728 (random 178))))

(defun random-string-flippy-floppy ()
  "Lemme tell you a little story about how my strings got flipped turned upside down..."
  (replace-regexp-in-string "'" "\\\\'"
                            (concat (mapcar 'flippify-char
                                            (reverse (string-to-list
                                                      (downcase (shell-command-to-string "random-string -i"))))))))

(defun flippify-char (char)
  (cond
   ((equal char 97) #x0250) ;250
   ((equal char 98) #x0071)
   ((equal char 99) #x0254) ;254
   ((equal char 100) #x0070)
   ((equal char 101) #x01DD) ;ǝ
   ((equal char 102) #x025F) ;ɟ
   ((equal char 103) #x0183) ;ƃ
   ((equal char 104) #x0265) ;ɥ
   ((equal char 105) #x0131) ;ı
   ((equal char 106) #x027E) ;ɾ
   ((equal char 107) #x029E) ;ʞ
   ((equal char 108) #x006C) ;l
   ((equal char 109) #x026F) ;ɯ
   ((equal char 110) #x0075) ;u
   ((equal char 111) #x006F) ;o
   ((equal char 112) #x0064) ;d
   ((equal char 113) #x0062) ;b
   ((equal char 114) #x0279) ;ɹ
   ((equal char 115) #x0073) ;s
   ((equal char 116) #x0287) ;ʇ
   ((equal char 117) #x006E) ;n
   ((equal char 118) #x028C) ;ʌ
   ((equal char 119) #x028D) ;ʍ
   ((equal char 120) #x0078) ;x
   ((equal char 121) #x028E) ;ʎ
   ((equal char 122) #x02D9) ;z
   (t char))
)

(defvar haiku-dict-5 nil)
(defvar haiku-dict-7 nil)
(defun random-string-haiku ()
  "5-7-5"
  (when (null haiku-dict-5)
    (with-temp-buffer
      (insert-file-contents "/usr/share/dict/5-sylable")
      (setq haiku-dict-5 (split-string (buffer-string) "\n" t)))
    (with-temp-buffer
      (insert-file-contents "/usr/share/dict/7-sylable")
      (setq haiku-dict-7 (split-string (buffer-string) "\n" t))))
  (concat (nth (random (length haiku-dict-5)) haiku-dict-5)
          "-"
          (nth (random (length haiku-dict-7)) haiku-dict-7)
          "-"
          (nth (random (length haiku-dict-5)) haiku-dict-5)))

(provide 'random-string)
