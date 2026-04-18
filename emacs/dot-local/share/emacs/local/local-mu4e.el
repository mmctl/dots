;; -*- lexical-binding: t; -*-
;; local-mu4e.el

(require 'message)
(require 'mail-extr)
(require 'mu4e)


;;; Addresses and meta-level configuration
;; Defaults (per provider/domain)
(defconst ADDRESS_CONFIG_DEFAULTS
  '((:domain "mmeijers.com"
             :smtpserver "mail.your-server.de"
             :smtpport 465
             :smtptype ssl
             :sent "/Sent"
             :drafts "/Drafts"
             :trash "/Trash"
             :refile ask))
  "Default configuration for addresses per domain.")

(defun an-address-configuration-with-domain-defaults (domain address maildir &rest overrides)
  "Constructs property list starting from defaults for
DOMAIN for email address ADDRESS and maildir MAILDIR, potentially
overriding/adding property entries as per the property/value pairs in
OVERRIDES."
  (let* ((plbase (seq-find (lambda (domdfl)
                             (equal (plist-get domdfl :domain) domain))
                           ADDRESS_CONFIG_DEFAULTS))
         (adrspl (copy-sequence plbase)))
    (setq adrspl (plist-put adrspl :address address))
    (setq adrspl (plist-put adrspl :maildir maildir))
    (while overrides
      (setq adrspl (plist-put adrspl (pop overrides) (pop overrides))))
    adrspl))

;; Email address configurations
(defconst PERSONAL_ADDRESS_CONFIGS
  (list
   (an-address-configuration-with-domain-defaults "mmeijers.com" "personal@mmeijers.com" "/personal-mmeijers")
   (an-address-configuration-with-domain-defaults "mmeijers.com" "kernel@mmeijers.com" "/kernel-mmeijers")
   (an-address-configuration-with-domain-defaults "mmeijers.com" "kem@mmeijers.com" "/kem-mmeijers"))
  "List of (property lists representing) personal email addresses and
corresponding configuration.")

(defconst PERSONAL_ADDRESSES
  (mapcar (lambda (adrspl) (plist-get adrspl :address)) PERSONAL_ADDRESS_CONFIGS)
  "List of personal email addresses.")

(defconst PERSONAL_MAILROOTS
  (mapcar (lambda (adrspl) (plist-get adrspl :maildir)) PERSONAL_ADDRESS_CONFIGS)
  "List of maildir roots corresponding to personal email addresses.")

(defconst WORK_ADDRESS_CONFIGS
  (list
   (an-address-configuration-with-domain-defaults "mmeijers.com" "research@mmeijers.com" "/research-mmeijers")
   (an-address-configuration-with-domain-defaults "mmeijers.com" "teaching@mmeijers.com" "/teaching-mmeijers"))
  "List of (property lists representing) work email addresses and
corresponding configuration.")

(defconst WORK_ADDRESSES
  (mapcar (lambda (adrspl) (plist-get adrspl :address)) WORK_ADDRESS_CONFIGS)
  "List of work email addresses.")

(defconst WORK_MAILROOTS
  (mapcar (lambda (adrspl) (plist-get adrspl :maildir)) WORK_ADDRESS_CONFIGS)
  "List of maildir roots corresponding to work email addresses.")

(defconst MISCELLANEOUS_ADDRESS_CONFIGS
  (list
   (an-address-configuration-with-domain-defaults "mmeijers.com" "host@mmeijers.com" "/host-mmeijers")
   (an-address-configuration-with-domain-defaults "mmeijers.com" "dump@mmeijers.com" "/dump-mmeijers"))
  "List of (property lists representing) miscellaneous email addresses and
corresponding configuration.")

(defconst MISCELLANEOUS_ADDRESSES
  (mapcar (lambda (adrspl) (plist-get adrspl :address)) MISCELLANEOUS_ADDRESS_CONFIGS)
  "List of miscellaneous email addresses.")

(defconst MISCELLANEOUS_MAILROOTS
  (mapcar (lambda (adrspl) (plist-get adrspl :maildir)) MISCELLANEOUS_ADDRESS_CONFIGS)
  "List of maildir roots corresponding to miscellaneous email addresses.")

(defconst ALL_ADDRESS_CONFIGS
  (append PERSONAL_ADDRESS_CONFIGS WORK_ADDRESS_CONFIGS MISCELLANEOUS_ADDRESS_CONFIGS)
  "List of (property lists representing) all email addresses and
corresponding configuration.")

(defconst ALL_ADDRESSES
  (append PERSONAL_ADDRESSES WORK_ADDRESSES MISCELLANEOUS_ADDRESSES)
  "List of all email addresses.")

(defconst ALL_MAILROOTS
  (append PERSONAL_MAILROOTS WORK_MAILROOTS MISCELLANEOUS_MAILROOTS)
  "List of all maildir roots corresponding to email addresses.")

(defconst ALL_ADDRESS_CONFIGS_ADRS_ASSOC
  (mapcar (lambda (adrspl) (cons (plist-get adrspl :address) adrspl))
          ALL_ADDRESS_CONFIGS)
  "Association list mapping addresses to their property lists (as in
`ALL_ADDRESS_CONFIGS').")

(defconst ALL_ADDRESS_CONFIGS_MAILDIR_ASSOC
  (mapcar (lambda (adrspl) (cons (plist-get adrspl :maildir) adrspl))
          ALL_ADDRESS_CONFIGS)
  "Association list mapping maildirs to their property lists (as in
`ALL_ADDRESS_CONFIGS').")


;;; Utilities
(defun a-mailroot-from-maildir (maildir)
  "Extract Maildir root from MAILDIR; i.e., extract `/root' from
`/root/sub1/.../subn'."
  (when (and maildir (string-match "^\\(/[^/]+\\)\\(/\\|$\\)" maildir))
    (match-string 1 maildir)))

(defun a-mailroot-from-mu4e-message (msg)
  "Extract Maildir root from MSG."
  (a-mailroot-from-maildir (mu4e-message-field msg :maildir)))

(defun a-mailroot-from-mu4e-or-buffer-message-or-user-mail (&optional msg)
  "Attempts to extract Maildir root from (in order):
- MSG
- Message in buffer (based on From address)
- `user-mail-address'

If none of these succeed, return nil."
  (or (when-let* ((maildir (and msg (mu4e-message-field msg :maildir))))
        (a-mailroot-from-maildir maildir))
      (when-let* ((fromhdr (message-field-value "From"))
                  (from (cadr (mail-extract-address-components fromhdr)))
                  (frompl (cdr (assoc from ALL_ADDRESS_CONFIGS_ADRS_ASSOC))))
        (plist-get frompl :maildir))
      (when-let* ((userpl (cdr (assoc user-mail-address ALL_ADDRESS_CONFIGS_ADRS_ASSOC))))
        (plist-get userpl :maildir))))

(defun a-completing-read-mu4e-address (&optional prompt context-only)
  "Completing read asking for addresses using PROMPT,
defaulting to first address from context (if available).

If CONTEXT-ONLY is non-nil, only consider addresses from current context as
options (if no context is current, then this will fallback to all addresses)."
  (let* ((contadrs (an-addresses-mu4e-context))
         (choices (if context-only contadrs ALL_ADDRESSES))
         (def (car contadrs)))
    (completing-read (format-prompt (or prompt "Choose address") def)
                     choices nil t nil nil def)))


;;; Contexts
(defun an-addresses-mu4e-context ()
  "Provides list of addresses corresponding to current context, falling
back to all addresses if no context is current."
  (let* ((ctx (mu4e-context-current))
         (ctxname (and ctx (mu4e-context-name ctx))))
    (pcase ctxname
      ("Personal" PERSONAL_ADDRESSES)
      ("Work" WORK_ADDRESSES)
      ("Miscellaneous" MISCELLANEOUS_ADDRESSES)
      (_ ALL_ADDRESSES))))


;;; Bookmarks/queries
(defun a-mu4e-inbox-roots-query (roots)
  "Query matching all INBOXes under ROOTS."
  (mapconcat (lambda (r) (format "maildir:%s/INBOX" r)) roots " OR "))


;;; Special folders
(defun a-determine-mu4e-special-folder (type &optional msg)
  "Determines special folder of TYPE (sent, drafts, trash, and refile)
for MSG, relative to the root Maildir. TYPE is one of `:sent',
`:drafts' `:trash', or `:refile'."
  (let* ((mailroot (a-mailroot-from-mu4e-or-buffer-message-or-user-mail msg)))
    (if (null mailroot)
        (mu4e-ask-maildir-check-exists (format "Failed to detect Maildir root. Choose Maildir (for %s):" type))
      (let* ((folder (plist-get (cdr (assoc mailroot ALL_ADDRESS_CONFIGS_MAILDIR_ASSOC)) type))
             (mu4e-maildir-initial-input mailroot))
        (cond
         ((stringp folder)
          (concat mailroot folder))
         ((eq folder 'ask)
          (mu4e-ask-maildir-check-exists (format "Choose Maildir (for %s):" type)))
         (t
          (mu4e-ask-maildir-check-exists (format "Failed to determine Maildir. Choose Maildir (for %s):" type))))))))

(defun a-determine-mu4e-sent-folder (msg)
  "Determines sent folder for MSG, relative to the root Maildir.

Meant for `mu4e-sent-folder', which see."
  (a-determine-mu4e-special-folder :sent msg))

(defun a-determine-mu4e-drafts-folder (msg)
  "Determines drafts folder for MSG, relative to the root Maildir.

Meant for `mu4e-drafts-folder', which see."
  (a-determine-mu4e-special-folder :drafts msg))
(defun a-determine-mu4e-trash-folder (msg)
  "Determines trash folder for MSG, relative to the root Maildir.

Meant for `mu4e-trash-folder', which see."
  (a-determine-mu4e-special-folder :trash msg))

(defun a-determine-mu4e-refile-folder (msg)
  "Determines refile folder for MSG, relative to the root Maildir.

Meant for `mu4e-refile-folder', which see."
  (a-determine-mu4e-special-folder :refile msg))


;;; Composing/sending
(defun a-determine-mu4e-compose-from-address ()
  "Determines From address to use for composition of mail,
either based on `mu4e-compose-parent-message' (set when
composing), or by asking."
  (if mu4e-compose-parent-message
      (let* ((mailroot (when-let* ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                         (a-mailroot-from-maildir maildir)))
             (byroot (and mailroot (plist-get (cdr (assoc mailroot ALL_ADDRESS_CONFIGS_MAILDIR_ASSOC)) :address)))
             (recipients (mapcar #'mu4e-contact-email
                                 (append (mu4e-message-field mu4e-compose-parent-message :to)
                                         (mu4e-message-field mu4e-compose-parent-message :cc)
                                         (mu4e-message-field mu4e-compose-parent-message :bcc))))
             (senders (mapcar #'mu4e-contact-email
                              (mu4e-message-field mu4e-compose-parent-message :from)))
             (contadrs (an-addresses-mu4e-context))
             (byrecip (seq-find (lambda (adrs) (member adrs contadrs)) recipients))
             (bysend (seq-find (lambda (adrs) (member adrs contadrs)) senders)))
        (or byroot
            byrecip
            bysend
            (a-completing-read-mu4e-address "Couldn't determine `From' address. Choose")))
    (a-completing-read-mu4e-address "Choose `From' address")))

(defun an-around-advice-draft-configure (mud comp-type comp-func &optional parent)
  "Performs configuration, particularly `user-mail-address',
before starting composition of new mail, so that the From address is set
correctly.

Meant as advice around `mu4e--draft'."
  (let* ((mu4e-compose-parent-message parent)
         (mu4e-compose-type comp-type)
         (user-mail-address (a-determine-mu4e-compose-from-address)))
    (funcall mud comp-type comp-func parent)))

(defun an-smtpmail-configure-and-send-it ()
  "Wrapper around `smtpmail-send-it' that first configures
the relevant SMTP-related settings based on the From
field in the current message.

Meant as replacement for `smtpmail-send-it', e.g., in
`message-send-mail-function'."
  (let* ((fromhdr (message-field-value "From"))
         (from (and fromhdr (cadr (mail-extract-address-components fromhdr))))
         (frompl (cdr (assoc from ALL_ADDRESS_CONFIGS_ADRS_ASSOC))))
    (unless from
      (error "`From' field non-existent or not parseable"))
    (unless frompl
      (error "Failed to find SMTP configuration for %s"))
    (let* ((smtpmail-smtp-user from)
           (smtpmail-smtp-server (plist-get frompl :smtpserver))
           (smtpmail-smtp-service (plist-get frompl :smtpport))
           (smtpmail-stream-type (plist-get frompl :smtptype)))
      (smtpmail-send-it))))

;; Contexts (Mu4e)
(defconst PERSONAL_MU4E_CONTEXT
  (make-mu4e-context
   :name "Personal"
   :enter-func (lambda () (mu4e-message "Entering context: Personal"))
   :leave-func (lambda () (mu4e-message "Leaving context: Personal"))
   :match-func (lambda (msg)
                 (when msg
                   (member (a-mailroot-from-mu4e-message msg)
                           PERSONAL_MAILROOTS)))
   :vars
   `((mu4e-maildir-shortcuts . ((:maildir "/personal-mmeijers/INBOX" :key ?p)
                                (:maildir "/kernel-mmeijers/INBOX" :key ?k)
                                (:maildir "/kem-mmeijers/INBOX" :key ?r)))
     (mu4e-bookmarks . ((:name "All" :key ?a :query ,(a-mu4e-inbox-roots-query PERSONAL_MAILROOTS))
                        (:name "All unread" :key ?u :query ,(concat "("
                                                                    (a-mu4e-inbox-roots-query PERSONAL_MAILROOTS)
                                                                    ") AND flag:unread"))
                        (:name "Personal unread" :key ?p :query "maildir:/personal-mmeijers/INBOX AND flag:unread")
                        (:name "Kernel unread" :key ?k :query "maildir:/kernel-mmeijers/INBOX AND flag:unread")
                        (:name "KeM unread" :key ?r :query "maildir:/kem-mmeijers/INBOX AND flag:unread")))
     (mu4e-get-mail-command . ,(concat "mbsync"
                                       (when-let* ((xdgcnf (getenv "XDG_CONFIG_HOME")))
                                         (concat " -c " (shell-quote-argument (expand-file-name "isyncrc" xdgcnf))))
                                       " personal"))))
  "Mu4e context for personal addresses.")

(defconst WORK_MU4E_CONTEXT
  (make-mu4e-context
   :name "Work"
   :enter-func (lambda () (mu4e-message "Entering context: Work"))
   :leave-func (lambda () (mu4e-message "Leaving context: Work"))
   :match-func (lambda (msg)
                 (when msg
                   (member (a-mailroot-from-mu4e-message msg)
                           PERSONAL_MAILROOTS)))
   :vars
   `((mu4e-maildir-shortcuts . ((:maildir "/research-mmeijers/INBOX" :key ?r)
                                (:maildir "/teaching-mmeijers/INBOX" :key ?t)))
     (mu4e-bookmarks . ((:name "All" :key ?a :query ,(a-mu4e-inbox-roots-query WORK_MAILROOTS))
                        (:name "All unread" :key ?u :query ,(concat "("
                                                                    (a-mu4e-inbox-roots-query WORK_MAILROOTS)
                                                                    ") AND flag:unread"))
                        (:name "Research unread" :key ?r :query "maildir:/research-mmeijers/INBOX AND flag:unread")
                        (:name "Teaching unread" :key ?t :query "maildir:/reaching-mmeijers/INBOX AND flag:unread")))
     (mu4e-get-mail-command . ,(concat "mbsync"
                                       (when-let* ((xdgcnf (getenv "XDG_CONFIG_HOME")))
                                         (concat " -c " (shell-quote-argument (expand-file-name "isyncrc" xdgcnf))))
                                       " work"))))
  "Mu4e context for work addresses.")

(provide 'local-mu4e)
;;; local-mu4e.el ends here
