(require 'erc)

(defun fn-connect ()
  "Connect to Freenode.net"
  (interactive)
  (erc-tls :server "chat.freenode.net" :port 6697 :nick (user-login-name) :password (read-passwd "IRC Password: ")))

(setq erc-track-exclude-server-buffer 't)
(setq erc-track-shorten-function nil)
(setq erc-autojoin-channels-alist '(("chat.freenode.net" "#techendo" "#ratchet" "#twirck")))

(provide 'irc)
