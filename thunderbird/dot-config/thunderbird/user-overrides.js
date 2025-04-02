user_pref("browser.display.use_system_colors", true);                   // whether to use the system color theme
user_pref("calendar.timezone.local", "Europe/Amsterdam");               // set to "" to allow TB to aquire local time zone, or "UTC", or your local timezone
user_pref("devtools.debugger.remote-enabled", true);                    // [*SAFE=false] whether to enable remote debugging (needed for developer tools)

user_pref("mail.collect_email_address_outgoing", true);                 // whether to save outgoing mail address in the "Collected Address" address book
user_pref("mail.phishing.detection.enabled", false);                    // whether to enable phishing detection
user_pref("mailnews.headers.showUserAgent", true);                      // whether to display the user-agent string of the senders email client

user_pref("mailnews.start_page.enabled", false);                        // whether to display the default start page before a mail is selected
user_pref("mailnews.start_page.url", "");                               // redundant, just in case - see: "mailnews.start_page.enabled"

user_pref("privacy.userContext.enabled", false);                        // whether to enable containers
user_pref("privacy.userContext.ui.enabled", false);                     // whether to enable the UI for containers
user_pref("security.external_protocol_requires_permission", false);     // [*SAFE=true] whether to prompt when opening a link in an external program

user_pref("accessibility.tabfocus", 3);                             // which elements can be focused using the Tab key - 1=text fields, 2=all form fields except text, 4=links (can be added)

user_pref("app.update.auto", false);                                // [SET] [*SAFE=true] whether to enable automatic updates (non-Windows)

user_pref("browser.safebrowsing.blockedURIs.enabled", false);       // [SET] [*SAFE=true] it is not suggested to disable these safebrowsing features
user_pref("browser.safebrowsing.downloads.enabled", false);         // [SET] [*SAFE=true] "
user_pref("browser.safebrowsing.malware.enabled", false);           // [SET] [*SAFE=true] "
user_pref("browser.safebrowsing.phishing.enabled", false);          // [SET] [*SAFE=true] "
user_pref("browser.search.update", false);                          // whether to disable search engine plugin updates
user_pref("browser.triple_click_selects_paragraph", false);         // whether to select entire paragraph when text is triple clicked

user_pref("clipboard.plainTextOnly", true);                         // whether to strip text formatting when copying(?)/pasting text

user_pref("extensions.getAddons.cache.enabled", false);             // whether to enable extension metadata (extension detail tab)
user_pref("extensions.update.autoUpdateDefault", false);            // [SET] whether to automatically install extension updates (after checking for updates)

user_pref("general.useragent.compatMode.firefox", true);            // [*PRIV=true] whether to limit sending extra user-agent data

user_pref("image.animation_mode", "none");                          // how to display animated GIF images - none=do not animate, once=play animation once, normal=play the animation normally

user_pref("intl.date_time.pattern_override.date_short", "yyyy/MM/dd");
user_pref("intl.date_time.pattern_override.date_medium", "yyyy/MM/dd eee");
user_pref("intl.date_time.pattern_override.time_medium", "HH:mm:ss"); // force 24 hr. time format for mail and calendar (may only be necessary on Linux)
user_pref("intl.date_time.pattern_override.time_short", "HH:mm"); // force 24 hr. time format for mail and calendar (may only be necessary on Linux)

user_pref("mail.mdn.report.enabled", false);                        // whether to enable sending return receipts
user_pref("mailnews.database.global.indexer.enabled", false);       // whether to enable the search indexer
user_pref("offline.autoDetect", false);                             // whether to auto-detect if Thunderbird is on/off line

user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true); // whether to load user styles from chrome folder
user_pref("view_source.syntax_highlight", true);                    // whether to highlight the source code of a document when viewing it

/**
 * [SET] the following preferences adjusts the smooth scrolling feature of
 * Thunderbird when using a mouse wheel or keyboard keys to scroll
 */
user_pref("general.smoothscroll", true);                            // whether to enable smooth scrolling
user_pref("general.smoothScroll.lines.durationMaxMS", 400);         // smooth the start/end of line scrolling operations in ms (up/down arrow/page keys)
user_pref("general.smoothScroll.lines.durationMinMS", 200);         // smooth the start/end of line scrolling operations in ms (up/down arrow/page keys)
user_pref("general.smoothScroll.mouseWheel.durationMaxMS", 600);    // smooth the start/end of scrolling operations in ms
user_pref("general.smoothScroll.mouseWheel.durationMinMS", 300);    // smooth the start/end of scrolling operations in ms
user_pref("general.smoothScroll.other.durationMaxMS", 400);         // smooth the start/end of other scrolling operations in ms
user_pref("general.smoothScroll.other.durationMinMS", 200);         // smooth the start/end of other scrolling operations in ms
user_pref("general.smoothScroll.pages.durationMaxMS", 400);         // smooth the start/end of page scrolling operations in ms (PgUp/PgDn keys)
user_pref("general.smoothScroll.pages.durationMinMS", 200);         // smooth the start/end of page scrolling operations in ms (PgUp/PgDn keys)
user_pref("mousewheel.acceleration.factor", 10);                    // sets acceleration factor if mouse wheel.acceleration.start > -1
user_pref("mousewheel.acceleration.start", 0);                      // when to apply mouse wheel.acceleration.factor (after how many scroll clicks of mouse wheel) - value must be greater than -1
user_pref("mousewheel.default.delta_multiplier_x", 85);             // sets the x-axis step size
user_pref("mousewheel.default.delta_multiplier_y", 85);             // sets the y-axis step size
user_pref("mousewheel.default.delta_multiplier_z", 85);             // sets the z-axis step size
user_pref("mousewheel.min_line_scroll_amount", 10);                 // if the CSS line height is smaller than this value in pixels, each scroll click will scroll this amount

user_pref("widget.use-xdg-desktop-portal.file-picker", 1);

// Check if "_user.js.parrot" field in about:config is set to this value
user_pref("_user.js.parrot", "SUCCESS: Overrides loaded!");
