-- Yazi initialization 
--- Plugins
---- Border
require("full-border"):setup({
	type = ui.Border.ROUNDED,
})

---- Yatline (header and status line)
----- Nord 
local yatline_nord_theme = require("yatline-nord-theme"):setup()
require("yatline"):setup(yatline_nord_theme)

---- Augmented commands
require("augment-command"):setup({
    prompt = false,
    default_item_group_for_prompt = "hovered",
    smart_enter = true,
    smart_paste = false,
    smart_tab_create = false,
    smart_tab_switch = true,
    confirm_on_quit = false,
    open_file_after_creation = false,
    enter_directory_after_creation = false,
    use_default_create_behaviour = false,
    enter_archives = false,
    extract_retries = 3,
    recursively_extract_archives = false,
    preserve_file_permissions = false,
    must_have_hovered_item = false,
    skip_single_subdirectory_on_enter = true,
    skip_single_subdirectory_on_leave = true,
    smooth_scrolling = true,
    scroll_delay = 0.01,
    wraparound_file_navigation = false,
})

---- Clipboard
require("copy-file-contents"):setup({
	append_char = "\n",
	notification = true,
})

---- Archives
require("archivemount"):setup()

--- Custom Functions
---- Linemode showing both size and modification time
function Linemode:size_and_mtime()
	local time = math.floor(self._file.cha.mtime or 0)
	if time == 0 then
		time = ""
	else
--		time = os.date("%d/%m/%y | %H:%M", time)
--		time = os.date("%d %b %Y @ %H:%M", time)
		time = os.date("%d/%m/%y @ %H:%M", time)
	end

	local size = self._file:size()
	return string.format("%s | %s", size and ya.readable_size(size) or "-", time)
end
