(when-let* ((this-file (or load-file-name
                           (user-error "This installation file is expected to be loaded")))
            (this-file-base-ext (file-name-nondirectory this-file))
            (this-directory (file-name-directory this-file))
            (dir-files (directory-files default-directory t directory-files-no-dot-files-regexp)))
  (dolist (file dir-files)
    (and-let* ((file-base-ext (file-name-nondirectory file))
               ((not (string-equal file-base-ext this-file-base-ext)))
               ((string-match-p "^install-.*\\.el$" file-base-ext)))
      (load file))))

