/******************************************************************************
 * 1. CUSTOM BROWSER CHROME
 ******************************************************************************/

/*
 * Load chrome/userChrome.css from the active Firefox profile.
 */
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);


/******************************************************************************
 * 2. NATIVE VERTICAL TABS/SIDEBAR
 ******************************************************************************/

/*
 * Enable Firefox's redesigned sidebar and native vertical tabs.
 */
user_pref("sidebar.revamp", true);
user_pref("sidebar.verticalTabs", true);

/*
 * Keep the vertical-tabs sidebar visible. The sidebar button can still be
 * used to switch between its collapsed and expanded states.
 */
user_pref("sidebar.visibility", "always-show");

/*
 * Place the sidebar at the logical start of the window: the left side in a
 * left-to-right locale.
 */
user_pref("sidebar.position_start", true);

/*
 * Comma-separated list of built-in tools shown in the sidebar launcher.
 *
 * Available identifiers currently include:
 *
 *   history
 *   bookmarks
 *   syncedtabs
 *   aichat
 *
 * The order here also determines their order in the sidebar.
 */
user_pref("sidebar.main.tools", "history,bookmarks");


/******************************************************************************
 * 3. PASSWORDS, ADDRESSES AND PAYMENT AUTOFILL
 ******************************************************************************/

/*
 * Disable Firefox's built-in password manager.
 *
 *   - do not remember logins;
 *   - disable access to saved passwords through the preferences UI and
 *     about:logins.
 */
user_pref("signon.rememberSignons", false);
user_pref("pref.privacy.disable_button.view_passwords", true);

/*
 * Disable login autofill and Firefox's generated-password functionality.
 */
user_pref("signon.autofillForms", false);
user_pref("signon.generation.enabled", false);

/*
 * Do not store or autofill addresses or payment-card information.
 */
user_pref("extensions.formautofill.addresses.enabled", false);
user_pref("extensions.formautofill.creditCards.enabled", false);


/******************************************************************************
 * 4. SHUTDOWN SANITIZATION
 ******************************************************************************/

/*
 * Clear transient browsing data when Firefox shuts down normally.
 *
 * Retained:
 *
 *   - browsing history;
 *   - bookmarks;
 *   - extensions and extension configuration;
 *   - Firefox preferences;
 *   - site permissions and shutdown-clearing exceptions;
 *   - cookies and storage belonging to explicitly exempted sites.
 *
 */
user_pref("privacy.history.custom", true);
user_pref("privacy.sanitize.sanitizeOnShutdown", true);

/*
 * Current sanitization preferences.
 */
user_pref("privacy.clearOnShutdown_v2.browsingHistoryAndDownloads", false);
user_pref("privacy.clearOnShutdown_v2.cookiesAndStorage", true);
user_pref("privacy.clearOnShutdown_v2.cache", true);
user_pref("privacy.clearOnShutdown_v2.formdata", true);

/*
 * Site settings must be retained because Firefox stores shutdown-clearing
 * exceptions as site permissions.
 */
user_pref("privacy.clearOnShutdown_v2.siteSettings", false);

/*
 * Legacy and compatibility preferences still consulted by parts of Firefox
 * and by migration code.
 */
user_pref("privacy.clearOnShutdown.cache", true);
user_pref("privacy.clearOnShutdown.cookies", true);
user_pref("privacy.clearOnShutdown.formdata", true);
user_pref("privacy.clearOnShutdown.history", false);
user_pref("privacy.clearOnShutdown.downloads", false);
user_pref("privacy.clearOnShutdown.sessions", true);
user_pref("privacy.clearOnShutdown.offlineApps", true);
user_pref("privacy.clearOnShutdown.siteSettings", false);

/*
 * Do not automatically restore the previous browsing session after a normal
 * restart. Crash recovery remains available.
 */
user_pref("browser.startup.page", 1);


/******************************************************************************
 * 5. ENHANCED TRACKING PROTECTION AND COOKIES
 ******************************************************************************/

/*
 * Use Firefox's current Strict Enhanced Tracking Protection definition.
 */
user_pref("browser.contentblocking.category", "strict");

/*
 * Explicitly enable the principal tracking-protection categories
 */
user_pref("privacy.trackingprotection.enabled", true);
user_pref("privacy.trackingprotection.pbmode.enabled", true);
user_pref("privacy.trackingprotection.socialtracking.enabled", true);
user_pref("privacy.trackingprotection.cryptomining.enabled", true);
user_pref("privacy.trackingprotection.fingerprinting.enabled", true);
user_pref("privacy.trackingprotection.emailtracking.enabled", true);
user_pref("privacy.trackingprotection.emailtracking.pbmode.enabled", true);

/*
 * Reject cookies from known trackers and partition other third-party cookies.
 *
 * Value 5 corresponds to Firefox's Total Cookie Protection behavior:
 * tracker-cookie rejection plus partitioning of third-party storage.
 */
user_pref("network.cookie.cookieBehavior", 5);
user_pref("network.cookie.cookieBehavior.pbmode", 5);


/******************************************************************************
 * 6. HTTPS AND NETWORK PREDICTION
 ******************************************************************************/

/*
 * Upgrade navigations to HTTPS when possible
 */
user_pref("dom.security.https_only_mode", true);

/*
 * Disable DNS prefetching, matching the NetworkPrediction=false policy.
 */
user_pref("network.dns.disablePrefetch", true);
user_pref("network.dns.disablePrefetchFromHTTPS", true);

/*
 * Disable speculative connections initiated from address-bar predictions.
 */
user_pref("browser.urlbar.speculativeConnect.enabled", false);


/******************************************************************************
 * 7. TELEMETRY AND STUDIES
 ******************************************************************************/

/*
 * Disable Firefox telemetry upload and local telemetry archives.
 *
 * These are the preferences affected by DisableTelemetry.
 */
user_pref("datareporting.healthreport.uploadEnabled", false);
user_pref("datareporting.policy.dataSubmissionEnabled", false);
user_pref("datareporting.usage.uploadEnabled", false);
user_pref("toolkit.telemetry.archive.enabled", false);

/*
 * Opt out of Firefox studies and experiments.
 */
user_pref("app.shield.optoutstudies.enabled", false);

/*
 * Disable contextual feature and extension recommendations.
 */
user_pref("browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons", false);
user_pref("browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features", false);


/******************************************************************************
 * 8. POCKET, ONBOARDING AND PROMOTIONAL UI
 ******************************************************************************/

/*
 * Disable the remaining Pocket integration.
 */
user_pref("extensions.pocket.enabled", false);

/*
 * Disable onboarding and Mozilla promotional surfaces.
 */
user_pref("browser.aboutwelcome.enabled", false);
user_pref("browser.preferences.moreFromMozilla", false);
user_pref("browser.preferences.experimental", false);

/*
 * Suppress the first-run and post-update promotional pages.
 */
user_pref("startup.homepage_welcome_url", "");
user_pref("startup.homepage_override_url", "");

/*
 * Use Firefox's normal new-tab page for newly opened windows rather than the
 * distribution-provided openSUSE homepage.
 */
user_pref("browser.startup.homepage", "about:newtab");

/*
 * On a fresh browser startup, open the configured homepage rather than
 * restoring the previous session.
 */
user_pref("browser.startup.page", 1);

/*
 * Do not trigger restoration of Firefox's bundled default bookmarks.
 */
user_pref("browser.bookmarks.restore_default_bookmarks", false);


/******************************************************************************
 * 9. FIREFOX HOME AND NEW-TAB PAGE
 ******************************************************************************/

/*
 * Keep only the search field on Firefox Home.
 */
user_pref("browser.newtabpage.enabled", true);
user_pref("browser.newtabpage.activity-stream.showSearch", true);

user_pref("browser.newtabpage.activity-stream.feeds.topsites", false);
user_pref("browser.newtabpage.activity-stream.feeds.section.highlights",false);
user_pref("browser.newtabpage.activity-stream.feeds.section.topstories",false);
user_pref("browser.newtabpage.activity-stream.feeds.snippets", false);

/*
 * Disable sponsored content.
 */
user_pref("browser.newtabpage.activity-stream.showSponsoredTopSites", false);
user_pref("browser.newtabpage.activity-stream.showSponsored", false);
user_pref("browser.topsites.contile.enabled", false);


/******************************************************************************
 * 10. SEARCH AND ADDRESS-BAR PRIVACY
 ******************************************************************************/

/*
 * Do not send partial search queries to the configured search engine.
 */
user_pref("browser.search.suggest.enabled", false);
user_pref("browser.search.suggest.enabled.private", false);
user_pref("browser.urlbar.suggest.searches", false);

/*
 * Disable Firefox Suggest, sponsored and non-sponsored remote suggestions,
 * trending suggestions, and related data collection.
 */
user_pref("browser.urlbar.quicksuggest.enabled", false);
user_pref("browser.urlbar.suggest.quicksuggest.all", false);
user_pref("browser.urlbar.suggest.quicksuggest.sponsored", false);
user_pref("browser.urlbar.quicksuggest.dataCollection.enabled", false);
user_pref("browser.urlbar.suggest.trending", false);

/*
 * Older Firefox releases used this preference for non-sponsored Quick
 * Suggest results. Keeping it false is harmless where it is no longer used.
 */
user_pref("browser.urlbar.suggest.quicksuggest.nonsponsored", false);

/*
 * Continue showing useful local results from browsing history, bookmarks and
 * currently open tabs.
 */
user_pref("browser.urlbar.suggest.history", true);
user_pref("browser.urlbar.suggest.bookmark", true);
user_pref("browser.urlbar.suggest.openpage", true);

/*
 * Do not show frequently visited sites merely by focusing an empty URL bar.
 */
user_pref("browser.urlbar.suggest.topsites", false);

/*
 * Display full URLs rather than visually removing their scheme.
 */
user_pref("browser.urlbar.trimURLs", false);

/*
 * Compatibility preference used by Firefox versions that separately control
 * trimming of the HTTPS scheme.
 */
user_pref("browser.urlbar.trimHttps", false);


/******************************************************************************
 * 11. SEARCH AND NEW-TAB KEYBOARD WORKFLOW
 ******************************************************************************/

/*
 * Searches submitted from Firefox's optional dedicated search bar open in a
 * new tab.
 */
user_pref("browser.search.openintab", true);

/*
 * Keep normal URL-bar Enter behavior:
 *
 *   Enter             open in the current tab
 *   Alt+Enter         open in a new foreground tab
 *   Alt+Shift+Enter   open in a new background tab
 */
user_pref("browser.urlbar.openintab", false);


/******************************************************************************
 * 12. TAB BEHAVIOR
 ******************************************************************************/

/*
 * Switch with Ctrl+Tab in most-recently-used order.
 */
user_pref("browser.ctrlTab.sortByRecentlyUsed", true);

/*
 * Closing the final tab leaves Firefox open with a new blank tab.
 */
user_pref("browser.tabs.closeWindowWithLastTab", false);

/*
 * Place newly opened tabs immediately after the current tab.
 *
 * insertAfterCurrent applies this to all new tabs and therefore takes
 * precedence over insertRelatedAfterCurrent.
 */
user_pref("browser.tabs.insertAfterCurrent", true);
user_pref("browser.tabs.insertRelatedAfterCurrent", true);

/*
 * Return to the originating/owner tab when a related tab is closed.
 */
user_pref("browser.tabs.selectOwnerOnClose", true);

/*
 * Retain warnings for potentially destructive bulk tab operations.
 */
user_pref("browser.tabs.warnOnClose", true);
user_pref("browser.tabs.warnOnCloseOtherTabs", true);
user_pref("browser.tabs.warnOnOpen", true);

/*
 * Select ordinary newly opened tabs rather than always loading them silently
 * in the background.
 */
user_pref("browser.tabs.loadInBackground", false);
user_pref("browser.tabs.loadDivertedInBackground", false);

/*
 * Open bookmarks in tabs rather than replacing the current page.
 */
user_pref("browser.tabs.loadBookmarksInTabs", true);
user_pref("browser.tabs.loadBookmarksInBackground", false);

/*
 * Firefox does not currently provide Zen-style workspaces as a native
 * abstraction. Native tab groups are the closest built-in alternative.
 */
user_pref("browser.tabs.groups.enabled", true);
user_pref("browser.tabs.dragDrop.createGroup.enabled", true);

/*
 * Disable ML-based tab-group naming and tab suggestions. Ordinary manually
 * managed tab groups remain enabled.
 */
user_pref("browser.tabs.groups.smart.enabled", false);
user_pref("browser.tabs.groups.smart.userEnabled", false);
user_pref("browser.tabs.groups.smart.optin", false);


/******************************************************************************
 * 13. DOWNLOADS
 ******************************************************************************/

/*
 * Ask where each download should be saved.
 */
user_pref("browser.download.useDownloadDir", false);


/******************************************************************************
 * 14. FULL-SCREEN SECURITY INDICATION
 ******************************************************************************/

/*
 * Keep Firefox's full-screen transition warning visible briefly.
 */
user_pref("full-screen-api.warning.timeout", 1500);


/******************************************************************************
 * 15. MISCELLANEOUS SETTINGS
 ******************************************************************************/

/*
 * Global Privacy Control sends an explicit request not to
 * sell or share personal data.
 */
user_pref("privacy.globalprivacycontrol.enabled", true);

/*
 * Disable Firefox's integrated third-party chatbot feature entirely, rather
 * than merely hiding its sidebar button.
 */
user_pref("browser.ml.chat.enabled", false);
