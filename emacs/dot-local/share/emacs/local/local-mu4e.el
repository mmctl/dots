;; -*- lexical-binding: t; -*-
;; local-mu4e.el

(require 'message)
(require 'mail-extr)
(require 'mu4e)

(require 'cl-lib)


;;; Data structures
;; Backends
(cl-defstruct a-mail-backend-config
  "Shared configuration for a mail backend."
  (id nil
      :documentation
      "Keyword identifying the backend configuration.")
  (smtp-server nil
               :documentation
               "SMTP server hostname.")
  (smtp-port nil
             :documentation
             "SMTP server port.")
  (smtp-type nil
             :documentation
             "SMTP connection type.")
  (sent-folder nil
               :documentation
               "Maildir folder or policy for sent messages.")
  (drafts-folder nil
                 :documentation
                 "Maildir folder or policy  for draft messages.")
  (trash-folder nil
                :documentation
                "Maildir folder or policy for trashed messages.")
  (refile-folder nil
                 :documentation
                 "Maildir folder or policy for refiled messages."))

(defconst MMEIJERS_MAIL_BACKEND_CONFIG
  (make-a-mail-backend-config
   :id :mmeijers.com
   :smtp-server "mail.your-server.de"
   :smtp-port 465
   :smtp-type 'ssl
   :sent-folder "/Sent"
   :drafts-folder "/Drafts"
   :trash-folder "/Trash"
   :refile-folder 'ask)
  "Mail backend configuration for mmeijers.com.")

(defconst PROTONMAIL_BRIDGE_MAIL_BACKEND_CONFIG
  (make-a-mail-backend-config
   :id :protonmail-bridge
   :smtp-server "127.0.0.1"
   :smtp-port 1025
   :smtp-type 'starttls
   :sent-folder "/Sent"
   :drafts-folder "/Drafts"
   :trash-folder "/Trash"
   :refile-folder 'ask)
  "Mail backend configuration for protonmail-bridge.")

(defconst ALL_MAIL_BACKEND_CONFIGS
  (list MMEIJERS_MAIL_BACKEND_CONFIG PROTONMAIL_BRIDGE_MAIL_BACKEND_CONFIG)
  "All mail backend configurations.")

(defun a-mail-backend-config-for-id (id)
  "Return the mail backend configuration identified by ID
in `ALL_MAIL_BACKEND_CONFIGS'.

Signal an error if no such backend configuration exists."
  (or (cl-find id ALL_MAIL_BACKEND_CONFIGS
               :key #'a-mail-backend-config-id
               :test #'eq)
      (error "No mail backend configuration known for id %S" id)))

;; Addresses
(cl-defstruct (a-mail-address-config
               (:constructor make--a-mail-address-config))
  "Resolved configuration for an email address."
  (group nil
         :documentation
         "Logical address group, such as `:personal' or `:research'.")
  (address nil
           :documentation
           "Email address.")
  (maildir nil
           :documentation
           "Root maildir corresponding to the email address.")
  (backend nil
           :documentation
           "Identifier of the backend from which defaults were inherited.")
  (smtp-server nil
               :documentation
               "SMTP server hostname.")
  (smtp-port nil
             :documentation
             "SMTP server port.")
  (smtp-type nil
             :documentation
             "SMTP connection type.")
  (sent-folder nil
               :documentation
               "Maildir folder or policy for sent messages.")
  (drafts-folder nil
                 :documentation
                 "Maildir folder or policy for draft messages.")
  (trash-folder nil
                :documentation
                "Maildir folder or policy for trashed messages.")
  (refile-folder nil
                 :documentation
                 "Maildir folder or policy for refiled messages."))

(defconst ALL_MAIL_ADDRESS_GROUPS
  '(:personal :research :business :miscellaneous)
  "Supported logical mail address groups.")

(cl-defun make-a-mail-address-config
    (&key group address maildir backend-id
          (smtp-server :backend)
          (smtp-port :backend)
          (smtp-type :backend)
          (sent-folder :backend)
          (drafts-folder :backend)
          (trash-folder :backend)
          (refile-folder :backend))
  "Create a resolved mail address configuration.

GROUP identifies the logical role of the address.  ADDRESS is the
email address, MAILDIR is its maildir root, and BACKEND-ID identifies
an entry in `ALL_MAIL_BACKEND_CONFIGS'.

The remaining keyword arguments override the corresponding backend
defaults when explicitly supplied."
  (unless (memq group ALL_MAIL_ADDRESS_GROUPS)
    (error "Unsupported mail address group: %S" group))
  (unless (stringp address)
    (error "Mail address must be a string: %S" address))
  (unless (stringp maildir)
    (error "Maildir must be a string: %S" maildir))

  (let ((defaults (a-mail-backend-config-for-id backend-id)))
    (make--a-mail-address-config
     :group group
     :address address
     :maildir maildir
     :backend backend-id
     :smtp-server
     (if (eq smtp-server :backend)
         (a-mail-backend-config-smtp-server defaults)
       smtp-server)
     :smtp-port
     (if (eq smtp-port :backend)
         (a-mail-backend-config-smtp-port defaults)
       smtp-port)
     :smtp-type
     (if (eq smtp-type :backend)
         (a-mail-backend-config-smtp-type defaults)
       smtp-type)
     :sent-folder
     (if (eq sent-folder :backend)
         (a-mail-backend-config-sent-folder defaults)
       sent-folder)
     :drafts-folder
     (if (eq drafts-folder :backend)
         (a-mail-backend-config-drafts-folder defaults)
       drafts-folder)
     :trash-folder
     (if (eq trash-folder :backend)
         (a-mail-backend-config-trash-folder defaults)
       trash-folder)
     :refile-folder
     (if (eq refile-folder :backend)
         (a-mail-backend-config-refile-folder defaults)
       refile-folder))))

(defconst ALL_MAIL_ADDRESS_CONFIGS
  (list
   (make-a-mail-address-config
         :group :personal
         :address "matthiasmeijers@proton.me"
         :maildir "/matthiasmeijers-proton"
         :backend-id :protonmail-bridge)

   (make-a-mail-address-config
    :group :personal
    :address "personal@mmeijers.com"
    :maildir "/personal-mmeijers"
    :backend-id :mmeijers.com)

   (make-a-mail-address-config
    :group :personal
    :address "kernel@mmeijers.com"
    :maildir "/kernel-mmeijers"
    :backend-id :mmeijers.com)

   (make-a-mail-address-config
    :group :personal
    :address "kem@mmeijers.com"
    :maildir "/kem-mmeijers"
    :backend-id :mmeijers.com)

   (make-a-mail-address-config
    :group :research
    :address "mmeijersres@protonmail.com"
    :maildir "/mmeijersres-protonmail"
    :backend-id :protonmail-bridge)

   (make-a-mail-address-config
    :group :research
    :address "research@mmeijers.com"
    :maildir "/research-mmeijers"
    :backend-id :mmeijers.com)

   (make-a-mail-address-config
    :group :research
    :address "teaching@mmeijers.com"
    :maildir "/teaching-mmeijers"
    :backend-id :mmeijers.com)

   (make-a-mail-address-config
    :group :miscellaneous
    :address "host@mmeijers.com"
    :maildir "/host-mmeijers"
    :backend-id :mmeijers.com)

   (make-a-mail-address-config
    :group :miscellaneous
    :address "dump@mmeijers.com"
    :maildir "/dump-mmeijers"
    :backend-id :mmeijers.com))
  "Configurations for all active email addresses.")

;; Mu4e configurations
(cl-defstruct a-mu4e-mailbox-config
  "Configuration for one mailbox displayed in a mu4e context."
  (label nil
         :documentation
         "Human-readable mailbox label.")
  (key nil
       :documentation
       "Character used for the mailbox shortcut and bookmark (within logical group).")
  (mailroot nil
            :documentation
            "Root Maildir corresponding to the mailbox."))

(cl-defstruct a-mu4e-context-config
  "Configuration from which to construct a mu4e context."
  (group nil
         :documentation
         "Logical mail address group represented by the context.")
  (name nil
        :documentation
        "Display name of the mu4e context.")
  (sync-group nil
              :documentation
              "sync group used to synchronize this context.")
  (mailboxes nil
             :documentation
             "Mailbox configurations displayed in this context."))

(defconst PERSONAL_MU4E_CONTEXT_CONFIG
  (make-a-mu4e-context-config
   :group :personal
   :name "Personal"
   :sync-group "personal"
   :mailboxes
   (list (make-a-mu4e-mailbox-config
          :label "Main"
          :key ?m
          :mailroot "/matthiasmeijers-proton")
         (make-a-mu4e-mailbox-config
          :label "Personal"
          :key ?p
          :mailroot "/personal-mmeijers")
         (make-a-mu4e-mailbox-config
          :label "Kernel"
          :key ?k
          :mailroot "/kernel-mmeijers")
         (make-a-mu4e-mailbox-config
          :label "KeM"
          :key ?r
          :mailroot "/kem-mmeijers")))
  "Configuration for personal mu4e context.")

(defconst RESEARCH_MU4E_CONTEXT_CONFIG
  (make-a-mu4e-context-config
   :group :research
   :name "Research"
   :sync-group "research"
   :mailboxes
   (list (make-a-mu4e-mailbox-config
          :label "Main"
          :key ?m
          :mailroot "/mmeijersres-protonmail")
         (make-a-mu4e-mailbox-config
          :label "Research"
          :key ?r
          :mailroot "/research-mmeijers")
         (make-a-mu4e-mailbox-config
          :label "Teaching"
          :key ?t
          :mailroot "/teaching-mmeijers")))
  "Configuration for research mu4e context.")

(defconst MISCELLANEOUS_MU4E_CONTEXT_CONFIG
  (make-a-mu4e-context-config
   :group :miscellaneous
   :name "Miscellaneous"
   :sync-group "miscellaneous"
   :mailboxes
   (list (make-a-mu4e-mailbox-config
          :label "Host"
          :key ?h
          :mailroot "/host-mmeijers")
         (make-a-mu4e-mailbox-config
          :label "Dump/Catchall"
          :key ?d
          :mailroot "/dump-mmeijers")))
  "Configuration for miscellaneous mu4e context.")

(defconst ALL_MU4E_CONTEXT_CONFIGS
  (list PERSONAL_MU4E_CONTEXT_CONFIG
        RESEARCH_MU4E_CONTEXT_CONFIG
        MISCELLANEOUS_MU4E_CONTEXT_CONFIG)
  "Configurations identifying all supported mu4e contexts.")


;;; Utilities
(defun a-mailroot-from-maildir (maildir)
  "Extracts Maildir root from MAILDIR; i.e., extract `/root' from
`/root/sub1/.../subn'."
  (when (and maildir
             (string-match "^\\(/[^/]+\\)\\(/\\|$\\)" maildir))
    (match-string 1 maildir)))

(defun a-mailroot-from-mu4e-message (msg)
  "Extracts Maildir root from MSG."
  (a-mailroot-from-maildir (mu4e-message-field msg :maildir)))

(defun a-mailroot-from-mu4e-or-buffer-message-or-user-mail (&optional msg)
  "Attempts to extract Maildir root from (in order):
- MSG
- Message in buffer (based on From address)
- `user-mail-address'

If none of these succeed, return nil."
  (or (when msg
        (a-mailroot-from-mu4e-message msg))
      (when-let* ((fromhdr (message-field-value "From"))
                  (from (cadr (mail-extract-address-components fromhdr)))
                  (config (a-mail-address-config-for-address from)))
        (a-mail-address-config-maildir config))
      (when-let* ((config (a-mail-address-config-for-address user-mail-address)))
        (a-mail-address-config-maildir config))))

(defun a-completing-read-mu4e-address (&optional prompt context-only)
  "Completing read asking for addresses using PROMPT,
defaulting to first address from context (if available).

If CONTEXT-ONLY is non-nil, only consider addresses from current context as
options (if no context is current, then this will fallback to all addresses)."
  (let* ((contadrs (an-addresses-mu4e-context))
         (choices (if context-only
                      contadrs
                    (a-mail-addresses-from-configs ALL_MAIL_ADDRESS_CONFIGS)))
         (def (car contadrs)))
    (completing-read (format-prompt (or prompt "Choose address") def)
                     choices nil t nil nil def)))


;;; Getters
(defun a-mail-address-config-for-address (address)
  "Returns the mail address configuration for ADDRESS.

Returns nil if ADDRESS is not configured."
  (cl-find address ALL_MAIL_ADDRESS_CONFIGS
           :key #'a-mail-address-config-address
           :test #'string=))

(defun a-mail-address-config-for-mailroot (mailroot)
  "Returns the mail address configuration for MAILROOT.

Returns nil if MAILROOT is not configured."
  (cl-find mailroot ALL_MAIL_ADDRESS_CONFIGS
           :key #'a-mail-address-config-maildir
           :test #'string=))

(defun a-mail-address-configs-for-group (group)
  "Returns mail address configurations belonging to GROUP."
  (cl-remove-if-not (lambda (config)
                      (eq (a-mail-address-config-group config) group))
                    ALL_MAIL_ADDRESS_CONFIGS))

(defun a-mail-addresses-from-configs (configs)
  "Returns the mail addresses represented by CONFIGS."
  (mapcar #'a-mail-address-config-address configs))

(defun a-mailroots-from-configs (configs)
  "Returns the Maildir roots represented by CONFIGS."
  (mapcar #'a-mail-address-config-maildir configs))

(defun a-mail-addresses-for-group (group)
  "Returns configured email addresses belonging to GROUP."
  (a-mail-addresses-from-configs (a-mail-address-configs-for-group group)))

(defun a-mailroots-for-group (group)
  "Returns configured Maildir roots belonging to GROUP."
  (a-mailroots-from-configs (a-mail-address-configs-for-group group)))

(defun a-mu4e-context-config-for-name (name)
  "Returns the mu4e context configuration named NAME."
  (cl-find name ALL_MU4E_CONTEXT_CONFIGS
           :key #'a-mu4e-context-config-name
           :test #'string=))

(defun an-addresses-mu4e-context ()
  "Provides list of addresses corresponding to current context, falling
back to all addresses if no context is current."
  (if-let* ((ctx (mu4e-context-current))
            (ctx-config (a-mu4e-context-config-for-name (mu4e-context-name ctx))))
      (a-mail-addresses-for-group (a-mu4e-context-config-group ctx-config))
    (a-mail-addresses-from-configs ALL_MAIL_ADDRESS_CONFIGS)))


;;; Bookmarks/queries
(defun a-mu4e-inbox-roots-query (roots)
  "Query matching all INBOXes under ROOTS."
  (mapconcat
   (lambda (root)
     (format "maildir:%s/INBOX" root))
   roots
   " OR "))


;;; Special folders
(defun a-mail-address-config-special-folder (config type)
  "Return special folder of TYPE from CONFIG.

TYPE is one of `:sent', `:drafts', `:trash', or `:refile'."
  (pcase type
    (:sent
     (a-mail-address-config-sent-folder config))
    (:drafts
     (a-mail-address-config-drafts-folder config))
    (:trash
     (a-mail-address-config-trash-folder config))
    (:refile
     (a-mail-address-config-refile-folder config))
    (_
     (error "Unsupported special folder type: %S" type))))

(defun a-determine-mu4e-special-folder (type &optional msg)
  "Determines special folder of TYPE (sent, drafts, trash, and refile)
for MSG, relative to the root Maildir. TYPE is one of `:sent',
`:drafts' `:trash', or `:refile'."
  (let ((mailroot (a-mailroot-from-mu4e-or-buffer-message-or-user-mail msg)))
    (if (null mailroot)
        (mu4e-ask-maildir-check-exists
         (format "Failed to detect Maildir root. Choose Maildir (for %s):" type))
      (let* ((config (a-mail-address-config-for-mailroot mailroot))
             (folder (and config (a-mail-address-config-special-folder config type)))
             (mu4e-maildir-initial-input mailroot))
        (cond
         ((stringp folder)
          (concat mailroot folder))
         ((eq folder 'ask)
          (mu4e-ask-maildir-check-exists
           (format "Choose Maildir (for %s):" type)))
         (t
          (mu4e-ask-maildir-check-exists
           (format "Failed to determine Maildir. Choose Maildir (for %s):" type))))))))

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
      (let* ((mailroot (when-let* ((maildir (mu4e-message-field
                                             mu4e-compose-parent-message
                                             :maildir)))
                         (a-mailroot-from-maildir maildir)))
             (byroot (when-let* ((config (and mailroot
                                              (a-mail-address-config-for-mailroot mailroot))))
                       (a-mail-address-config-address config)))
             (recipients (mapcar #'mu4e-contact-email
                                 (append (mu4e-message-field mu4e-compose-parent-message :to)
                                         (mu4e-message-field mu4e-compose-parent-message :cc)
                                         (mu4e-message-field mu4e-compose-parent-message :bcc))))
             (senders (mapcar #'mu4e-contact-email
                              (mu4e-message-field mu4e-compose-parent-message :from)))
             (contadrs (an-addresses-mu4e-context))
             (byrecip (seq-find (lambda (adrs)
                                  (member adrs contadrs))
                                recipients))
             (bysend (seq-find (lambda (adrs)
                                 (member adrs contadrs))
                               senders)))
        (or byroot
            byrecip
            bysend
            (a-completing-read-mu4e-address "Couldn't determine `From' address. Choose")))
    (a-completing-read-mu4e-address "Choose `From' address")))

(defun an-around-advice-draft-configure (draft-func compose-type compose-func &optional parent)
  "Performs configuration, particularly `user-mail-address',
before starting composition of new mail, so that the From address is set
correctly.

Meant as advice around `mu4e--draft'."
  (let* ((mu4e-compose-parent-message parent)
         (mu4e-compose-type compose-type)
         (user-mail-address (a-determine-mu4e-compose-from-address)))
    (funcall draft-func compose-type compose-func parent)))

(defun an-smtpmail-configure-and-send-it ()
  "Wrapper around `smtpmail-send-it' that first configures
the relevant SMTP-related settings based on the From
field in the current message.

Meant as replacement for `smtpmail-send-it', e.g., in
`message-send-mail-function'."
  (let* ((fromhdr (message-field-value "From"))
         (from (and fromhdr
                    (cadr (mail-extract-address-components fromhdr))))
         (config (and from
                      (a-mail-address-config-for-address from))))
    (unless from
      (error "`From' field non-existent or not parseable"))
    (unless config
      (error "Failed to find SMTP configuration for %s" from))
    (let* ((smtpmail-smtp-user from)
           (smtpmail-smtp-server (a-mail-address-config-smtp-server config))
           (smtpmail-smtp-service (a-mail-address-config-smtp-port config))
           (smtpmail-stream-type (a-mail-address-config-smtp-type config)))
      (smtpmail-send-it))))


;;; Context construction (Mu4e)
(defun a-mu4e-maildir-shortcuts (mailboxes)
  "Construct mu4e Maildir shortcuts from MAILBOXES."
  (mapcar (lambda (mailbox)
            (list :maildir (concat (a-mu4e-mailbox-config-mailroot mailbox) "/INBOX")
                  :key (a-mu4e-mailbox-config-key mailbox)))
          mailboxes))

(defun a-mu4e-bookmarks (context-config)
  "Construct mu4e bookmarks from CONTEXT-CONFIG."
  (let* ((group (a-mu4e-context-config-group context-config))
         (mailroots (a-mailroots-for-group group))
         (inbox-query (a-mu4e-inbox-roots-query mailroots))
         (mailboxes (a-mu4e-context-config-mailboxes context-config)))
    (append `((:name "All" :key ?a :query ,inbox-query)
              (:name "All unread" :key ?u :query ,(concat "(" inbox-query ") AND flag:unread")))
            (mapcar (lambda (mailbox)
                      `(:name ,(concat (a-mu4e-mailbox-config-label mailbox) " unread")
                              :key ,(a-mu4e-mailbox-config-key mailbox)
                              :query ,(format "maildir:%s/INBOX AND flag:unread"
                                              (a-mu4e-mailbox-config-mailroot mailbox))))
                    mailboxes))))

(defun a-mu4e-get-mail-command (&optional sync-group)
  "Construct the sync command for SYNC-GROUP, or the general
sync command if SYNC-GROUP is nil."
  (concat "mbsync"
          (when-let* ((xdgcnf (getenv "XDG_CONFIG_HOME")))
            (concat " -c " (shell-quote-argument (expand-file-name "isyncrc" xdgcnf))))
          (when (stringp sync-group)
            (concat " " sync-group))))

(defun a-mu4e-context-status-message (action name)
  "Report ACTION for mu4e context NAME."
  (mu4e-message "%s context: %s" action name))

(defun a-mu4e-context-matches-message-p (mailroots msg)
  "Return non-nil if MSG belongs to one of MAILROOTS."
  (when msg
    (member (a-mailroot-from-mu4e-message msg) mailroots)))

(defun a-make-mu4e-context (context-config)
  "Construct a mu4e context from CONTEXT-CONFIG."
  (let* ((name (a-mu4e-context-config-name context-config))
         (group (a-mu4e-context-config-group context-config))
         (mailroots (a-mailroots-for-group group))
         (mailboxes (a-mu4e-context-config-mailboxes context-config)))
    (make-mu4e-context
     :name name

     :enter-func
     (apply-partially #'a-mu4e-context-status-message "Entering" name)
     :leave-func
     (apply-partially #'a-mu4e-context-status-message "Leaving" name)
     :match-func
     (apply-partially #'a-mu4e-context-matches-message-p mailroots)

     :vars
     (list (cons 'mu4e-maildir-shortcuts (a-mu4e-maildir-shortcuts mailboxes))
           (cons 'mu4e-bookmarks (a-mu4e-bookmarks context-config))
           (cons 'mu4e-get-mail-command
                 (a-mu4e-get-mail-command (a-mu4e-context-config-sync-group context-config)))))))

(defconst PERSONAL_MU4E_CONTEXT (a-make-mu4e-context PERSONAL_MU4E_CONTEXT_CONFIG)
  "Mu4e context for personal addresses.")

(defconst RESEARCH_MU4E_CONTEXT (a-make-mu4e-context RESEARCH_MU4E_CONTEXT_CONFIG)
  "Mu4e context for research addresses.")

(defconst MISCELLANEOUS_MU4E_CONTEXT (a-make-mu4e-context MISCELLANEOUS_MU4E_CONTEXT_CONFIG)
  "Mu4e context for miscellaneous addresses.")


;;; Miscellaneous
(defun a-mu4e-align-header-line ()
  "Precisely align mu4e column headings with message fields.

Meant for `mu4e-headers-found-hook'."
  (when mu4e-headers-precise-alignment
    (setq header-line-format
          (cons (car header-line-format)
                (cl-mapcar (lambda (heading field-spec)
                             (pcase-let ((`(,field . ,width) field-spec))
                               (if (null width)
                                   heading
                                 (let* ((heading (substring heading 0 -1)) ; Remove the separator appended by `mu4e~header-line-format'.
                                        (properties (text-properties-at 0 heading))
                                        (heading (mu4e~headers-truncate-field-precise field heading width))
                                        (padding-position (1- (length heading))))
                                   ;; Keep the heading's keymap, mouse highlighting,
                                   ;; help text, face and field identity on the
                                   ;; precisely aligned padding space.
                                   (add-text-properties padding-position (1+ padding-position) properties heading)
                                   ;; Restore the ordinary inter-field separator.
                                   (concat heading " ")))))
                           (cdr header-line-format)
                           mu4e-headers-fields)))))

(defun a-mu4e-headers-set-truncate-string-ellipsis ()
  "Use a reliably measured ellipsis in mu4e headers.

Meant for `mu4e-headers-mode-hook'"
  (setq-local truncate-string-ellipsis ".."))


(provide 'local-mu4e)

;;; local-mu4e.el ends here
