;; -*- lexical-binding: t; -*-
;; local-mu4e.el

(require 'elfeed)


;;; Feeds
;; IACR ePrint
(defconst IACR_EPRINT_FEED_ATOM  "https://eprint.iacr.org/rss/atom.xml?order=recent"
  "Atom feed for IACR Cryptology ePrint Archive.")

(defconst IACR_EPRINT_CATEGORY_TAGS
  '(("Applications" . iacr-applications)
    ("Foundations" . iacr-foundations)
    ("Implementation" . iacr-implementation)
    ("Public-key cryptography" . iacr-public-key)
    ("Secret-key cryptography" . iacr-secret-key)
    ("Cryptographic protocols" . iacr-protocols)
    ("Attacks and cryptanalysis" . iacr-attacks-cryptanalysis))
  "Mapping from IACR ePrint categories to Elfeed tags.")

(defun an-elfeed-tag-iacr-eprint-entry (entry)
  "Tag IACR ePrint ENTRY according to its upstream category, as per
`IACR_EPRINT_CATEGORY_TAGS'."
  (when (equal (elfeed-feed-url (elfeed-entry-feed entry)) IACR_EPRINT_FEED_ATOM)
    (dolist (category (elfeed-meta entry :categories))
      (when-let* ((tag (cdr (assoc-string category IACR_EPRINT_CATEGORY_TAGS t))))
        (elfeed-tag entry tag)))))


(provide 'local-elfeed)

;;; local-elfeed.el ends here
