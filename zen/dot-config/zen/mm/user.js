// Required for userChrome.css / generated theme files in Zen
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);

// Find behavior
user_pref("accessibility.typeaheadfind", true);
user_pref("accessibility.typeaheadfind.flashBar", 0);

// UI choices
user_pref("browser.toolbars.bookmarks.visibility", "always");
user_pref("sidebar.visibility", "hide-sidebar");

// Address bar behavior
user_pref("browser.urlbar.suggest.clipboard", false);
user_pref("browser.urlbar.suggest.engines", false);

// Privacy / shutdown cleanup
user_pref("privacy.sanitize.sanitizeOnShutdown", true);
user_pref("privacy.clearHistory.formdata", true);
user_pref("privacy.clearOnShutdown_v2.formdata", true);
user_pref("privacy.clearOnShutdown_v2.browsingHistoryAndDownloads", false);

// Password / autofill behavior
user_pref("signon.rememberSignons", false);
user_pref("signon.autofillForms", false);
user_pref("signon.generation.enabled", false);
user_pref("extensions.formautofill.creditCards.enabled", false);

// Zen behavior
user_pref("zen.view.compact.enable-at-startup", true);
user_pref("zen.tabs.ctrl-tab.ignore-essential-tabs", true);
user_pref("browser.ctrlTab.sortByRecentlyUsed", true);
