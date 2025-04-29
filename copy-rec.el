(setq copy-marked-files (dired-get-marked-files))

(setq copy-dest-prefix "")
(setq copy-roots-alist '(("/home/xoey/" . "home")))

(setq copy-roots (mapcar #'car copy-roots-alist))


(cl-loop for file in copy-marked-files
         collect (cl-loop for (root . repl) in copy-roots-alist
                          when (string-prefix-p root file)
                          return (replace-regexp-in-string (rx string-start (literal root)) repl file)))
