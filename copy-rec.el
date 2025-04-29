(require 'dired)

(defun crf-transform-path (file transform)
  (cl-loop for (src-pre . dst-pre) in transforms
           for srx-rx = (rx string-start (literal src-pre))
           when (string-match srx-rx file)
           return (and-let* ((src-trunc (replace-match "" nil :lit file 0))
                             (dst (file-name-concat dst-pre src-trunc))))))

(defun crf--read-transforms ()
  "Read a list of source->destination filename prefix transformations, and return them as an alist. Never returns nil."
  (and-let* ((src-root (expand-file-name (read-directory-name "root source: ")))
             (dst-root (expand-file-name (read-directory-name "root destination: ")))
             (xf (cl-loop
                  ;; not strictly necessary but it's nice to forward-declare
                  with count = 0
                  ;; ask if we're reading more transforms
                  for more = (pcase count
                               (0 (y-or-n-p "specific transforms?"))
                               (_ (y-or-n-p "more transforms?")))
                  ;; this when-collect takes the role of the nonexistent
                  ;; finally-collect clause
                  when (and
                        ;; no more transformations
                        (not more)
                        (or
                         ;; and either no transformations so far (ensures we never return nil)
                         (= count 0)
                         ;; or we opt into the bare transform after listing specific transforms
                         (y-or-n-p "include root -> root?")))
                  ;; add in a final element
                  collect (cons src-root dst-root)
                  ;; count transforms so far
                  count t into count
                  while more
                  for src = (expand-file-name (read-directory-name "sub-source: " src-root))
                  for dst = (expand-file-name (read-directory-name "sub-destination: " dst-root))
                  collect (cons src dst))))))

;;;###autoload
(defun crf-copy-recursive-filter (files transforms)
  "Copy all the given FILES, determining the source for each according to the alist TRANSFORMS.

When called interactively, FILES is all marked files in the current `dired' buffer.
If a FILE is a directory, its contents are not filtered by TRANSFORMS - the directory is simply copied wholesale."
  (interactive (list (dired-get-marked-files)
                     (crf--read-transforms)))
  (cl-loop for src in files
           for dst = (crf-transform-path src transforms)
           if dst
           do
           (message "copying %S -> %S" src dst)
           (crf-do-copy src dst)
           else do
           (message "skipping %S: no destination" src)
           finally do
           (message "done")))

(defun crf-do-copy (src dst)
  (let ((dst-dir (file-name-directory dst))
        (dired-recursive-copies 'always))
    (mkdir dst-dir t)
    (dired-copy-file src dst 0) ; integer third arg (mostly) means ask before overwriting
    ))

(provide 'crf)
