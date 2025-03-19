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

---- Archives
require("fuse-archive"):setup({
  smart_enter = true,
})

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
