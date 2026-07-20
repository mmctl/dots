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
  (list (make-a-mail-address-config
         :group :personal
         :address "matthiasmeijers@proton.me"
         :maildir "/matthiasmeijers-proton"
         :backend-id :protonmail-bridge)

        ;; (make-a-mail-address-config
        ;;  :group :personal
        ;;  :address "personal@mmeijers.com"
        ;;  :maildir "/personal-mmeijers"
        ;;  :backend-id :mmeijers.com)

        ;; (make-a-mail-address-config
        ;;  :group :personal
        ;;  :address "kernel@mmeijers.com"
        ;;  :maildir "/kernel-mmeijers"
        ;;  :backend-id :mmeijers.com)

        ;; (make-a-mail-address-config
        ;;  :group :personal
        ;;  :address "kem@mmeijers.com"
        ;;  :maildir "/kem-mmeijers"
        ;;  :backend-id :mmeijers.com)

        (make-a-mail-address-config
         :group :research
         :address "mmeijersres@protonmail.com"
         :maildir "/mmeijersres-protonmail"
         :backend-id :protonmail-bridge)

        ;; (make-a-mail-address-config
        ;;  :group :research
        ;;  :address "research@mmeijers.com"
        ;;  :maildir "/research-mmeijers"
        ;;  :backend-id :mmeijers.com)

        ;; (make-a-mail-address-config
        ;;  :group :research
        ;;  :address "teaching@mmeijers.com"
        ;;  :maildir "/teaching-mmeijers"
        ;;  :backend-id :mmeijers.com)

        (make-a-mail-address-config
         :group :business
         :address "mmeijersbsn@protonmail.com"
         :maildir "/mmeijersbsn-protonmail"
         :backend-id :protonmail-bridge)

        ;; (make-a-mail-address-config
        ;;  :group :business
        ;;  :address "contracting@mmeijers.com"
        ;;  :maildir "/contracting-mmeijers"
        ;;  :backend-id :mmeijers.com)

        ;; (make-a-mail-address-config
        ;;  :group :miscellaneous
        ;;  :address "host@mmeijers.com"
        ;;  :maildir "/host-mmeijers"
        ;;  :backend-id :mmeijers.com)

        ;; (make-a-mail-address-config
        ;;  :group :miscellaneous
        ;;  :address "dump@mmeijers.com"
        ;;  :maildir "/dump-mmeijers"
        ;;  :backend-id :mmeijers.com)
        )
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

(defconst BUSINESS_MU4E_CONTEXT_CONFIG
  (make-a-mu4e-context-config
   :group :business
   :name "Business"
   :sync-group "business"
   :mailboxes
   (list (make-a-mu4e-mailbox-config
          :label "Main"
          :key ?m
          :mailroot "/mmeijersbsn-protonmail")
         (make-a-mu4e-mailbox-config
          :label "Contracting"
          :key ?c
          :mailroot "/contracting-mmeijers")))
  "Configuration for business mu4e context.")

(defconst MISCELLANEOUS_MU4E_CONTEXT_CONFIG
  (make-a-mu4e-context-config
   :group :miscellaneous
   :name "Miscellaneous")
  "Configuration for miscellaneous mu4e context.")

(defconst ALL_MU4E_CONTEXT_CONFIGS
  (list PERSONAL_MU4E_CONTEXT_CONFIG
        RESEARCH_MU4E_CONTEXT_CONFIG
        BUSINESS_MU4E_CONTEXT_CONFIG
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
  "Returns the mu4e context configuration named NAME.

Returns nil if NAME does not identify a configured context."
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
  (let ((mu4e-compose-parent-message parent)
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
    (let ((smtpmail-smtp-user from)
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

(defconst BUSINESS_MU4E_CONTEXT (a-make-mu4e-context BUSINESS_MU4E_CONTEXT_CONFIG)
  "Mu4e context for business addresses.")


;; ;;; Addresses and meta-level configuration
;; ;; Defaults (per provider/domain)
;; (defconst ADDRESS_CONFIG_DEFAULTS
;;   '((:domain "mmeijers.com"
;;              :smtpserver "mail.your-server.de"
;;              :smtpport 465
;;              :smtptype ssl
;;              :sent "/Sent"
;;              :drafts "/Drafts"
;;              :trash "/Trash"
;;              :refile ask)
;;     (:domain "protonmail-bridge"
;;              :smtpserver "127.0.0.1"
;;              :smtpport 1025
;;              :smtptype starttls
;;              :sent "/Sent"
;;              :drafts "/Drafts"
;;              :trash "/Trash"
;;              :refile ask))
;;   "Default configuration for addresses per domain.")

;; (defun an-address-configuration-with-domain-defaults (domain address maildir &rest overrides)
;;   "Constructs property list starting from defaults for
;; DOMAIN for email address ADDRESS and maildir MAILDIR, potentially
;; overriding/adding property entries as per the property/value pairs in
;; OVERRIDES."
;;   (let* ((plbase (seq-find (lambda (domdfl)
;;                              (equal (plist-get domdfl :domain) domain))
;;                            ADDRESS_CONFIG_DEFAULTS))
;;          (adrspl (copy-sequence plbase)))
;;     (setq adrspl (plist-put adrspl :address address))
;;     (setq adrspl (plist-put adrspl :maildir maildir))
;;     (while overrides
;;       (setq adrspl (plist-put adrspl (pop overrides) (pop overrides))))
;;     adrspl))

;; ;; Email address configurations
;; (defconst PERSONAL_ADDRESS_CONFIGS
;;   (list
;;    (an-address-configuration-with-domain-defaults "protonmail-bridge" "matthiasmeijers@proton.me" "/matthiasmeijers-proton")
;;    ;; (an-address-configuration-with-domain-defaults "mmeijers.com" "personal@mmeijers.com" "/personal-mmeijers")
;;    ;; (an-address-configuration-with-domain-defaults "mmeijers.com" "kernel@mmeijers.com" "/kernel-mmeijers")
;;    ;; (an-address-configuration-with-domain-defaults "mmeijers.com" "kem@mmeijers.com" "/kem-mmeijers")
;;    )
;;   "List of (property lists representing) personal email addresses and
;; corresponding configuration.")

;; (defconst PERSONAL_ADDRESSES
;;   (mapcar (lambda (adrspl) (plist-get adrspl :address)) PERSONAL_ADDRESS_CONFIGS)
;;   "List of personal email addresses.")

;; (defconst PERSONAL_MAILROOTS
;;   (mapcar (lambda (adrspl) (plist-get adrspl :maildir)) PERSONAL_ADDRESS_CONFIGS)
;;   "List of maildir roots corresponding to personal email addresses.")

;; (defconst RESEARCH_ADDRESS_CONFIGS
;;   (list
;;    (an-address-configuration-with-domain-defaults "protonmail-bridge" "mmeijersres@protonmail.com" "/mmeijersres-protonmail")
;;    ;; (an-address-configuration-with-domain-defaults "mmeijers.com" "research@mmeijers.com" "/research-mmeijers")
;;    ;; (an-address-configuration-with-domain-defaults "mmeijers.com" "teaching@mmeijers.com" "/teaching-mmeijers")
;;    )
;;   "List of (property lists representing) research email addresses and
;; corresponding configuration.")

;; (defconst RESEARCH_ADDRESSES
;;   (mapcar (lambda (adrspl) (plist-get adrspl :address)) RESEARCH_ADDRESS_CONFIGS)
;;   "List of research email addresses.")

;; (defconst RESEARCH_MAILROOTS
;;   (mapcar (lambda (adrspl) (plist-get adrspl :maildir)) RESEARCH_ADDRESS_CONFIGS)
;;   "List of maildir roots corresponding to research email addresses.")

;; (defconst BUSINESS_ADDRESS_CONFIGS
;;   (list
;;    (an-address-configuration-with-domain-defaults "protonmail-bridge" "mmeijersbsn@protonmail.com" "/mmeijersbsn-protonmail")
;;    ;; (an-address-configuration-with-domain-defaults "mmeijers.com" "contracting@mmeijers.com" "/contracting-mmeijers")
;;    )
;;   "List of (property lists representing) business email addresses and
;; corresponding configuration.")

;; (defconst BUSINESS_ADDRESSES
;;   (mapcar (lambda (adrspl) (plist-get adrspl :address)) BUSINESS_ADDRESS_CONFIGS)
;;   "List of business email addresses.")

;; (defconst BUSINESS_MAILROOTS
;;   (mapcar (lambda (adrspl) (plist-get adrspl :maildir)) BUSINESS_ADDRESS_CONFIGS)
;;   "List of maildir roots corresponding to business email addresses.")

;; (defconst MISCELLANEOUS_ADDRESS_CONFIGS
;;   (list
;;    ;; (an-address-configuration-with-domain-defaults "mmeijers.com" "host@mmeijers.com" "/host-mmeijers")
;;    ;; (an-address-configuration-with-domain-defaults "mmeijers.com" "dump@mmeijers.com" "/dump-mmeijers")
;;    )
;;   "List of (property lists representing) miscellaneous email addresses and
;; corresponding configuration.")

;; (defconst MISCELLANEOUS_ADDRESSES
;;   (mapcar (lambda (adrspl) (plist-get adrspl :address)) MISCELLANEOUS_ADDRESS_CONFIGS)
;;   "List of miscellaneous email addresses.")

;; (defconst MISCELLANEOUS_MAILROOTS
;;   (mapcar (lambda (adrspl) (plist-get adrspl :maildir)) MISCELLANEOUS_ADDRESS_CONFIGS)
;;   "List of maildir roots corresponding to miscellaneous email addresses.")

;; (defconst ALL_ADDRESS_CONFIGS
;;   (append PERSONAL_ADDRESS_CONFIGS RESEARCH_ADDRESS_CONFIGS
;;           BUSINESS_ADDRESS_CONFIGS MISCELLANEOUS_ADDRESS_CONFIGS)
;;   "List of (property lists representing) all email addresses and
;; corresponding configuration.")

;; (defconst ALL_ADDRESSES
;;   (append PERSONAL_ADDRESSES RESEARCH_ADDRESSES
;;           BUSINESS_ADDRESSES MISCELLANEOUS_ADDRESSES)
;;   "List of all email addresses.")

;; (defconst ALL_MAILROOTS
;;   (append PERSONAL_MAILROOTS RESEARCH_MAILROOTS
;;           BUSINESS_MAILROOTS MISCELLANEOUS_MAILROOTS)
;;   "List of all maildir roots corresponding to email addresses.")

;; (defconst ALL_ADDRESS_CONFIGS_ADRS_ASSOC
;;   (mapcar (lambda (adrspl) (cons (plist-get adrspl :address) adrspl))
;;           ALL_ADDRESS_CONFIGS)
;;   "Association list mapping addresses to their property lists (as in
;; `ALL_ADDRESS_CONFIGS').")

;; (defconst ALL_ADDRESS_CONFIGS_MAILDIR_ASSOC
;;   (mapcar (lambda (adrspl) (cons (plist-get adrspl :maildir) adrspl))
;;           ALL_ADDRESS_CONFIGS)
;;   "Association list mapping maildirs to their property lists (as in
;; `ALL_ADDRESS_CONFIGS').")


;; ;;; Utilities
;; (defun a-mailroot-from-maildir (maildir)
;;   "Extract Maildir root from MAILDIR; i.e., extract `/root' from
;; `/root/sub1/.../subn'."
;;   (when (and maildir (string-match "^\\(/[^/]+\\)\\(/\\|$\\)" maildir))
;;     (match-string 1 maildir)))

;; (defun a-mailroot-from-mu4e-message (msg)
;;   "Extract Maildir root from MSG."
;;   (a-mailroot-from-maildir (mu4e-message-field msg :maildir)))

;; (defun a-mailroot-from-mu4e-or-buffer-message-or-user-mail (&optional msg)
;;   "Attempts to extract Maildir root from (in order):
;; - MSG
;; - Message in buffer (based on From address)
;; - `user-mail-address'

;; If none of these succeed, return nil."
;;   (or (when-let* ((maildir (and msg (mu4e-message-field msg :maildir))))
;;         (a-mailroot-from-maildir maildir))
;;       (when-let* ((fromhdr (message-field-value "From"))
;;                   (from (cadr (mail-extract-address-components fromhdr)))
;;                   (frompl (cdr (assoc from ALL_ADDRESS_CONFIGS_ADRS_ASSOC))))
;;         (plist-get frompl :maildir))
;;       (when-let* ((userpl (cdr (assoc user-mail-address ALL_ADDRESS_CONFIGS_ADRS_ASSOC))))
;;         (plist-get userpl :maildir))))

;; (defun a-completing-read-mu4e-address (&optional prompt context-only)
;;   "Completing read asking for addresses using PROMPT,
;; defaulting to first address from context (if available).

;; If CONTEXT-ONLY is non-nil, only consider addresses from current context as
;; options (if no context is current, then this will fallback to all addresses)."
;;   (let* ((contadrs (an-addresses-mu4e-context))
;;          (choices (if context-only contadrs ALL_ADDRESSES))
;;          (def (car contadrs)))
;;     (completing-read (format-prompt (or prompt "Choose address") def)
;;                      choices nil t nil nil def)))


;; ;;; Contexts
;; (defun an-addresses-mu4e-context ()
;;   "Provides list of addresses corresponding to current context, falling
;; back to all addresses if no context is current."
;;   (let* ((ctx (mu4e-context-current))
;;          (ctxname (and ctx (mu4e-context-name ctx))))
;;     (pcase ctxname
;;       ("Personal" PERSONAL_ADDRESSES)
;;       ("Research" RESEARCH_ADDRESSES)
;;       ("Miscellaneous" MISCELLANEOUS_ADDRESSES)
;;       (_ ALL_ADDRESSES))))


;; ;;; Bookmarks/queries
;; (defun a-mu4e-inbox-roots-query (roots)
;;   "Query matching all INBOXes under ROOTS."
;;   (mapconcat (lambda (r) (format "maildir:%s/INBOX" r)) roots " OR "))


;; ;;; Special folders
;; (defun a-determine-mu4e-special-folder (type &optional msg)
;;   "Determines special folder of TYPE (sent, drafts, trash, and refile)
;; for MSG, relative to the root Maildir. TYPE is one of `:sent',
;; `:drafts' `:trash', or `:refile'."
;;   (let* ((mailroot (a-mailroot-from-mu4e-or-buffer-message-or-user-mail msg)))
;;     (if (null mailroot)
;;         (mu4e-ask-maildir-check-exists (format "Failed to detect Maildir root. Choose Maildir (for %s):" type))
;;       (let* ((folder (plist-get (cdr (assoc mailroot ALL_ADDRESS_CONFIGS_MAILDIR_ASSOC)) type))
;;              (mu4e-maildir-initial-input mailroot))
;;         (cond
;;          ((stringp folder)
;;           (concat mailroot folder))
;;          ((eq folder 'ask)
;;           (mu4e-ask-maildir-check-exists (format "Choose Maildir (for %s):" type)))
;;          (t
;;           (mu4e-ask-maildir-check-exists (format "Failed to determine Maildir. Choose Maildir (for %s):" type))))))))

;; (defun a-determine-mu4e-sent-folder (msg)
;;   "Determines sent folder for MSG, relative to the root Maildir.

;; Meant for `mu4e-sent-folder', which see."
;;   (a-determine-mu4e-special-folder :sent msg))

;; (defun a-determine-mu4e-drafts-folder (msg)
;;   "Determines drafts folder for MSG, relative to the root Maildir.

;; Meant for `mu4e-drafts-folder', which see."
;;   (a-determine-mu4e-special-folder :drafts msg))
;; (defun a-determine-mu4e-trash-folder (msg)
;;   "Determines trash folder for MSG, relative to the root Maildir.

;; Meant for `mu4e-trash-folder', which see."
;;   (a-determine-mu4e-special-folder :trash msg))

;; (defun a-determine-mu4e-refile-folder (msg)
;;   "Determines refile folder for MSG, relative to the root Maildir.

;; Meant for `mu4e-refile-folder', which see."
;;   (a-determine-mu4e-special-folder :refile msg))


;; ;;; Composing/sending
;; (defun a-determine-mu4e-compose-from-address ()
;;   "Determines From address to use for composition of mail,
;; either based on `mu4e-compose-parent-message' (set when
;; composing), or by asking."
;;   (if mu4e-compose-parent-message
;;       (let* ((mailroot (when-let* ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
;;                          (a-mailroot-from-maildir maildir)))
;;              (byroot (and mailroot (plist-get (cdr (assoc mailroot ALL_ADDRESS_CONFIGS_MAILDIR_ASSOC)) :address)))
;;              (recipients (mapcar #'mu4e-contact-email
;;                                  (append (mu4e-message-field mu4e-compose-parent-message :to)
;;                                          (mu4e-message-field mu4e-compose-parent-message :cc)
;;                                          (mu4e-message-field mu4e-compose-parent-message :bcc))))
;;              (senders (mapcar #'mu4e-contact-email
;;                               (mu4e-message-field mu4e-compose-parent-message :from)))
;;              (contadrs (an-addresses-mu4e-context))
;;              (byrecip (seq-find (lambda (adrs) (member adrs contadrs)) recipients))
;;              (bysend (seq-find (lambda (adrs) (member adrs contadrs)) senders)))
;;         (or byroot
;;             byrecip
;;             bysend
;;             (a-completing-read-mu4e-address "Couldn't determine `From' address. Choose")))
;;     (a-completing-read-mu4e-address "Choose `From' address")))

;; (defun an-around-advice-draft-configure (mud comp-type comp-func &optional parent)
;;   "Performs configuration, particularly `user-mail-address',
;; before starting composition of new mail, so that the From address is set
;; correctly.

;; Meant as advice around `mu4e--draft'."
;;   (let* ((mu4e-compose-parent-message parent)
;;          (mu4e-compose-type comp-type)
;;          (user-mail-address (a-determine-mu4e-compose-from-address)))
;;     (funcall mud comp-type comp-func parent)))

;; (defun an-smtpmail-configure-and-send-it ()
;;   "Wrapper around `smtpmail-send-it' that first configures
;; the relevant SMTP-related settings based on the From
;; field in the current message.

;; Meant as replacement for `smtpmail-send-it', e.g., in
;; `message-send-mail-function'."
;;   (let* ((fromhdr (message-field-value "From"))
;;          (from (and fromhdr (cadr (mail-extract-address-components fromhdr))))
;;          (frompl (cdr (assoc from ALL_ADDRESS_CONFIGS_ADRS_ASSOC))))
;;     (unless from
;;       (error "`From' field non-existent or not parseable"))
;;     (unless frompl
;;       (error "Failed to find SMTP configuration for %s" from))
;;     (let* ((smtpmail-smtp-user from)
;;            (smtpmail-smtp-server (plist-get frompl :smtpserver))
;;            (smtpmail-smtp-service (plist-get frompl :smtpport))
;;            (smtpmail-stream-type (plist-get frompl :smtptype)))
;;       (smtpmail-send-it))))

;; ;; Contexts (Mu4e)
;; (defconst PERSONAL_MU4E_CONTEXT
;;   (make-mu4e-context
;;    :name "Personal"
;;    :enter-func (lambda () (mu4e-message "Entering context: Personal"))
;;    :leave-func (lambda () (mu4e-message "Leaving context: Personal"))
;;    :match-func (lambda (msg)
;;                  (when msg
;;                    (member (a-mailroot-from-mu4e-message msg)
;;                            PERSONAL_MAILROOTS)))
;;    :vars
;;    `((mu4e-maildir-shortcuts . ((:maildir "/matthiasmeijers-proton/INBOX" :key ?m)
;;                                 (:maildir "/personal-mmeijers/INBOX" :key ?p)
;;                                 (:maildir "/kernel-mmeijers/INBOX" :key ?k)
;;                                 (:maildir "/kem-mmeijers/INBOX" :key ?r)))
;;      (mu4e-bookmarks . ((:name "All" :key ?a :query ,(a-mu4e-inbox-roots-query PERSONAL_MAILROOTS))
;;                         (:name "All unread" :key ?u :query ,(concat "("
;;                                                                     (a-mu4e-inbox-roots-query PERSONAL_MAILROOTS)
;;                                                                     ") AND flag:unread"))
;;                         (:name "Main unread" :key ?m :query "maildir:/matthiasmeijers-proton/INBOX AND flag:unread")
;;                         (:name "Personal unread" :key ?p :query "maildir:/personal-mmeijers/INBOX AND flag:unread")
;;                         (:name "Kernel unread" :key ?k :query "maildir:/kernel-mmeijers/INBOX AND flag:unread")
;;                         (:name "KeM unread" :key ?r :query "maildir:/kem-mmeijers/INBOX AND flag:unread")))
;;      (mu4e-get-mail-command . ,(concat "mbsync"
;;                                        (when-let* ((xdgcnf (getenv "XDG_CONFIG_HOME")))
;;                                          (concat " -c " (shell-quote-argument (expand-file-name "isyncrc" xdgcnf))))
;;                                        " personal"))))
;;   "Mu4e context for personal addresses.")

;; (defconst RESEARCH_MU4E_CONTEXT
;;   (make-mu4e-context
;;    :name "Research"
;;    :enter-func (lambda () (mu4e-message "Entering context: Research"))
;;    :leave-func (lambda () (mu4e-message "Leaving context: Research"))
;;    :match-func (lambda (msg)
;;                  (when msg
;;                    (member (a-mailroot-from-mu4e-message msg)
;;                            RESEARCH_MAILROOTS)))
;;    :vars
;;    `((mu4e-maildir-shortcuts . ((:maildir "/mmeijersres-protonmail/INBOX" :key ?m)
;;                                 (:maildir "/research-mmeijers/INBOX" :key ?r)
;;                                 (:maildir "/teaching-mmeijers/INBOX" :key ?t)))
;;      (mu4e-bookmarks . ((:name "All" :key ?a :query ,(a-mu4e-inbox-roots-query RESEARCH_MAILROOTS))
;;                         (:name "All unread" :key ?u :query ,(concat "("
;;                                                                     (a-mu4e-inbox-roots-query RESEARCH_MAILROOTS)
;;                                                                     ") AND flag:unread"))
;;                         (:name "Main unread" :key ?m :query "maildir:/mmeijersres-protonmail/INBOX AND flag:unread")
;;                         (:name "Research unread" :key ?r :query "maildir:/research-mmeijers/INBOX AND flag:unread")
;;                         (:name "Teaching unread" :key ?t :query "maildir:/teaching-mmeijers/INBOX AND flag:unread")))
;;      (mu4e-get-mail-command . ,(concat "mbsync"
;;                                        (when-let* ((xdgcnf (getenv "XDG_CONFIG_HOME")))
;;                                          (concat " -c " (shell-quote-argument (expand-file-name "isyncrc" xdgcnf))))
;;                                        " research"))))
;;   "Mu4e context for research addresses.")

;; (defconst BUSINESS_MU4E_CONTEXT
;;   (make-mu4e-context
;;    :name "Research"
;;    :enter-func (lambda () (mu4e-message "Entering context: Business"))
;;    :leave-func (lambda () (mu4e-message "Leaving context: Business"))
;;    :match-func (lambda (msg)
;;                  (when msg
;;                    (member (a-mailroot-from-mu4e-message msg)
;;                            BUSINESS_MAILROOTS)))
;;    :vars
;;    `((mu4e-maildir-shortcuts . ((:maildir "/mmeijersbsn-protonmail/INBOX" :key ?m)
;;                                 (:maildir "/contracting-mmeijers/INBOX" :key ?c)))
;;      (mu4e-bookmarks . ((:name "All" :key ?a :query ,(a-mu4e-inbox-roots-query RESEARCH_MAILROOTS))
;;                         (:name "All unread" :key ?u :query ,(concat "("
;;                                                                     (a-mu4e-inbox-roots-query RESEARCH_MAILROOTS)
;;                                                                     ") AND flag:unread"))
;;                         (:name "Main unread" :key ?m :query "maildir:/mmeijersbsn-protonmail/INBOX AND flag:unread")
;;                         (:name "Contracting unread" :key ?c :query "maildir:/contracting-mmeijers/INBOX AND flag:unread")))
;;      (mu4e-get-mail-command . ,(concat "mbsync"
;;                                        (when-let* ((xdgcnf (getenv "XDG_CONFIG_HOME")))
;;                                          (concat " -c " (shell-quote-argument (expand-file-name "isyncrc" xdgcnf))))
;;                                        " business"))))
;;   "Mu4e context for business addresses.")


(provide 'local-mu4e-new)
;;; local-mu4e.el ends here
