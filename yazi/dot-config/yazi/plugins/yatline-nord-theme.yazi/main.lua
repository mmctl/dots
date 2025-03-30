local nord_palette = {
        nord0 = "#2e3440",
        nord1 = "#3b4252",
        nord2 = "#434c5e",
        nord3 = "#4c566a",
        nord4 = "#d8dee9",
        nord5 = "#e5e9f0",
        nord6 = "#eceff4",
        nord7 = "#8fbcbb",
        nord8 = "#88c0d0",
        nord9 = "#81a1c1",
        nord10 = "#5e81ac",
        nord11 = "#bf616a",
        nord12 = "#d08770",
        nord13 = "#ebcb8b",
        nord14 = "#a3be8c",
        nord15 = "#b48ead",
}

local yatline_nord_theme = {
        section_separator = { open = "", close = "" },
	part_separator = { open = "", close = "" },
	inverse_separator = { open = "", close = "" },

	style_a = {
		fg = nord_palette.nord3,
		bg_mode = {
			normal = nord_palette.nord7,
			select = nord_palette.nord6,
			un_set = nord_palette.nord15
		}
	},
	style_b = { bg = nord_palette.nord9, fg = nord_palette.nord1 },
	style_c = { bg = nord_palette.nord10, fg = nord_palette.nord0 },

	permissions_t_fg = nord_palette.nord14,
	permissions_r_fg = nord_palette.nord13,
	permissions_w_fg = nord_palette.nord12,
	permissions_x_fg = nord_palette.nord11,
	permissions_s_fg = nord_palette.nord4,

	tab_width = 16,
	tab_use_inverse = false,

	selected = { icon = "󰻭", fg = nord_palette.nord13 },
	copied = { icon = "", fg = nord_palette.nord14 },
	cut = { icon = "", fg = nord_palette.nord12 },

	total = { icon = "󰮍", fg = nord_palette.nord4 },
	succ = { icon = "", fg = nord_palette.nord14 },
	fail = { icon = "", fg = nord_palette.nord11 },
	found = { icon = "󰮕", fg = nord_palette.nord4 },
	processed = { icon = "󰐍", fg = nord_palette.nord14 },

	show_background = true,

	display_header_line = true,
	display_status_line = true,

	component_positions = { "header", "tab", "status" },

	header_line = {
		left = {
			section_a = {
        			{type = "line", custom = false, name = "tabs", params = {"left"}},
			},
			section_b = {
			},
			section_c = {
			}
		},
		right = {
			section_a = {
        			{type = "string", custom = false, name = "cursor_position"},
			},
			section_b = {
        			{type = "string", custom = false, name = "cursor_percentage"},
			},
			section_c = {
        			{type = "coloreds", custom = false, name = "task_states"},
			}
		}
	},

	status_line = {
		left = {
			section_a = {
        			{type = "string", custom = false, name = "tab_mode"},
			},
			section_b = {
        			{type = "string", custom = false, name = "tab_num_files"},
			},
			section_c = {
        			{type = "coloreds", custom = false, name = "count"},
			}
		},
		right = {
			section_a = {
        			{type = "string", custom = false, name = "hovered_path", params = {{trimed = true, max_length = 32, trim_length = 16}}},				
			},
			section_b = {
        			{type = "string", custom = false, name = "hovered_mime"},
        			{type = "string", custom = false, name = "hovered_size"},
        			{type = "string", custom = false, name = "hovered_ownership"},
			},
			section_c = {
        			{type = "coloreds", custom = false, name = "permissions"},
			}
		}
	},
}

return { setup = function() return yatline_nord_theme end }
