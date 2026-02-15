---@meta
-- OXWM Configuration File

---Load type definitions for LSP
-- @module 'oxwm'
---
-- Modifier key: "Mod4" is the Super/Windows key, "Mod1" is Alt
local modkey = "Mod4"

-- Terminal emulator command (defaults to alacritty)
local terminal = "kitty"

local colors = {
	fg = "#e0dcd4",
	red = "#cdacac",
	bg = "#1a1d21",
	green = "#b8c4b8",
	grey = "#515761",
	blue = "#b0bcc8",
}

-- Workspace tags - can be numbers, names, or icons (requires a Nerd Font)
local tags = { "1", "2", "3", "4", "5", "6", "7" }
-- local tags = { "1", "2", "3", "4", "5" }
-- local tags = { "Ⅰ", "Ⅱ", "Ⅲ", "Ⅳ", "Ⅴ", "Ⅵ", "Ⅶ" }

local bar_font = "JetBrainsMono Nerd Font:style=Regular:size=10"

-- Define your blocks
-- Similar to widgets in qtile, or dwmblocks
local blocks = {
	oxwm.bar.block.shell({
		format = " {} ",
		command = "playerctl metadata --format '{{ title }} - {{ artist }}'",
		interval = 10,
		color = colors.green,
	}),

	oxwm.bar.block.static({
		text = " || ",
		interval = 999999999,
		color = colors.grey,
	}),

	oxwm.bar.block.shell({
		format = " {} ",
		command = "wpctl get-volume @DEFAULT_AUDIO_SINK@",
		interval = 0,
		color = colors.red,
	}),

	oxwm.bar.block.static({
		text = " || ",
		interval = 999999999,
		color = colors.grey,
	}),

	oxwm.bar.block.datetime({
		format = " {} ",
		date_format = "%Y-%m-%d %H:%M:%S W%W",
		interval = 1,
		color = colors.blue,
	}),
}

vol = "$()"

oxwm.set_terminal(terminal)
oxwm.set_modkey(modkey) -- This is for Mod + mouse binds, such as drag/resize
oxwm.set_tags(tags)

-- Set custom symbols for layouts (displayed in the status bar)
-- Available layouts: "tiling", "normie" (floating), "grid", "monocle", "tabbed"
oxwm.set_layout_symbol("tiling", "[T]")
oxwm.set_layout_symbol("normie", "[F]")
oxwm.set_layout_symbol("tabbed", "[=]")

-- Border configuration

-- Width in pixels
oxwm.border.set_width(0)
oxwm.border.set_focused_color(colors.grey)
oxwm.border.set_unfocused_color(colors.bg)

-- Smart Enabled = No border if 1 window
oxwm.gaps.set_smart(disabled)
oxwm.gaps.set_inner(0, 0)
oxwm.gaps.set_outer(0, 0)

-- Rules allow you to automatically configure windows based on their properties
-- You can match windows by class, instance, title, or role
-- Available properties: floating, tag, fullscreen, etc.
--
-- Common use cases:
-- - Force floating for certain applications (dialogs, utilities)
-- - Send specific applications to specific workspaces
-- - Configure window behavior based on title or class

-- Examples (uncomment to use):
-- oxwm.rule.add({ instance = "kitty", floating = true })
-- oxwm.rule.add({ class = "firefox", title = "Library", floating = true })
-- oxwm.rule.add({ instance = "mpv", floating = true })

-- To find window properties, use xprop and click on the window
-- WM_CLASS(STRING) shows both instance and class (instance, class)

oxwm.rule.add({ class = "emacs", tag = 1 })
oxwm.rule.add({ class = "helium-browser", tag = 2 })

-- Font configuration
oxwm.bar.set_font(bar_font)

-- Set your blocks here (defined above)
oxwm.bar.set_blocks(blocks)

-- Bar color schemes (for workspace tag display)
-- Parameters: foreground, background, border

-- Unoccupied tags
oxwm.bar.set_scheme_normal(colors.fg, colors.bg, "#444444")
-- Occupied tags
oxwm.bar.set_scheme_occupied(colors.blue, colors.bg, colors.blue)
-- Currently selected tag
oxwm.bar.set_scheme_selected(colors.blue, colors.bg, colors.blue)

-- Keybindings are defined using oxwm.key.bind(modifiers, key, action)
-- Modifiers: {"Mod4"}, {"Mod1"}, {"Shift"}, {"Control"}, or combinations like {"Mod4", "Shift"}
-- Keys: Use uppercase for letters (e.g., "Return", "H", "J", "K", "L")
-- Actions: Functions that return actions (e.g., oxwm.spawn(), oxwm.client.kill())
--
-- A list of available keysyms can be found in the X11 keysym definitions.
-- Common keys: Return, Space, Tab, Escape, Backspace, Delete, Left, Right, Up, Down

-- Basic window management

oxwm.key.bind({ modkey }, "Grave", oxwm.spawn({ "sh", "-c", "dmenuunicode" }))
oxwm.key.bind({ modkey }, "Q", oxwm.client.kill())
oxwm.key.bind({ modkey, "Shift" }, "Q", oxwm.spawn({ "slock" }))
oxwm.key.bind({ modkey, "Control" }, "Q", oxwm.quit())
oxwm.key.bind({ modkey }, "W", oxwm.spawn({ "helium-browser" }))
oxwm.key.bind({ modkey, "Shift" }, "E", oxwm.spawn({ "emacs" }))
oxwm.key.bind({ modkey, "Shift" }, "R", oxwm.restart())
-- oxwm.key.bind({ modkey, "Control" }, "Return", oxwm.spawn_terminal())
oxwm.key.bind({ modkey }, "D", oxwm.spawn({ "sh", "-c", "dmenu_run" }))
oxwm.key.bind({ modkey, "Control" }, "M", oxwm.spawn({ "kitty", "-e", "profanity" }))
oxwm.key.bind({ modkey, "Shift" }, "M", oxwm.spawn({ "kitty", "-e", "jellyfin-tui" }))
oxwm.key.bind({ modkey, "Shift", "Control" }, "M", oxwm.spawn({ "kitty", "-e", "gurk" }))
oxwm.key.bind({ modkey, "Control", "Shift" }, "M", oxwm.spawn({ "cider" }))
oxwm.key.bind({ modkey }, "F", oxwm.layout.set("normie"))
oxwm.key.bind({ modkey, "Shift" }, "F", oxwm.client.toggle_fullscreen())
oxwm.key.bind({ modkey, "Shift" }, "Space", oxwm.client.toggle_floating())
oxwm.key.bind({ modkey }, "T", oxwm.layout.set("tiling"))
oxwm.key.bind({ modkey, "Shift" }, "T", oxwm.layout.cycle())
oxwm.key.bind({ modkey }, "I", oxwm.inc_num_master(1))
oxwm.key.bind({ modkey }, "P", oxwm.inc_num_master(-1))
oxwm.key.bind({ modkey }, "Backspace", oxwm.spawn({ "sh", "-c", "sysact" }))
oxwm.key.bind({ modkey }, "A", oxwm.toggle_gaps())
oxwm.key.bind({ modkey }, "H", oxwm.set_master_factor(-5))
oxwm.key.bind({ modkey }, "J", oxwm.client.focus_stack(1))
oxwm.key.bind({ modkey, "Shift" }, "J", oxwm.client.move_stack(1))
oxwm.key.bind({ modkey }, "K", oxwm.client.focus_stack(-1))
oxwm.key.bind({ modkey, "Shift" }, "K", oxwm.client.move_stack(-1))
oxwm.key.bind({ modkey }, "L", oxwm.set_master_factor(5))
-- oxwm.key.bind({ modkey, "Control", "Shift" }, "L", oxwm.spawn({ "sh", "-c", "xset dpms toggle" }))
oxwm.key.bind({ modkey, "Control" }, "3", oxwm.spawn({ "sh", "-c", "screenshot full" }))
oxwm.key.bind({ modkey, "Control" }, "4", oxwm.spawn({ "sh", "-c", "screenshot selection" }))
oxwm.key.bind({ modkey, "Control" }, "P", oxwm.spawn({ "sh", "-c", "screenshot colorpicker" }))

oxwm.key.bind({ modkey }, "Comma", oxwm.spawn({ "sh", "-c", "playerctl previous" }))
oxwm.key.bind({ modkey }, "Period", oxwm.spawn({ "sh", "-c", "playerctl next" }))

oxwm.key.bind({ modkey }, "Minus", oxwm.spawn({ "sh", "-c", "pactl set-sink-volume @DEFAULT_SINK@ -5%" }))
oxwm.key.bind({ modkey }, "Equal", oxwm.spawn({ "sh", "-c", "pactl set-sink-volume @DEFAULT_SINK@ +5%" }))

-- Emacs launcher keybinds
local emacs_launcher = "~/.dotfiles/.config/oxwm/emacs-launcher"
oxwm.key.bind({ modkey }, "Return", oxwm.spawn({ "sh", "-c", emacs_launcher .. " '(my-open-vterm-at-point)'" }))
oxwm.key.bind({ modkey, "Control" }, "Return", oxwm.spawn({ "sh", "-c", emacs_launcher .. " '(my/new-frame-with-vterm)'" }))
oxwm.key.bind({ modkey }, "E", oxwm.spawn({ "sh", "-c", emacs_launcher .. " '(my/new-frame-with-vterm)'" }))
oxwm.key.bind(
	{ modkey },
	"R",
	oxwm.spawn({
		"sh",
		"-c",
		"~/.config/oxwm/emacs-launcher '(progn (select-frame-set-input-focus (selected-frame)) (dirvish))'",
	})
)
oxwm.key.bind(
	{ modkey },
	"C",
	oxwm.spawn({
		"sh",
		"-c",
		emacs_launcher .. " '(progn (select-frame-set-input-focus (selected-frame)) (org-capture))'",
	})
)
oxwm.key.bind(
	{ modkey, "Shift" },
	"D",
	oxwm.spawn({ "sh", "-c", emacs_launcher .. " '(progn (select-frame-set-input-focus (selected-frame)) (pass))'" })
)
oxwm.key.bind(
	{ modkey, "Shift" },
	"C",
	oxwm.spawn({
		"sh",
		"-c",
		emacs_launcher .. " '(progn (select-frame-set-input-focus (selected-frame)) (=calendar))'",
	})
)
oxwm.key.bind(
	{ modkey },
	"M",
	oxwm.spawn({ "sh", "-c", emacs_launcher .. " '(progn (select-frame-set-input-focus (selected-frame)) (=mu4e))'" })
)
oxwm.key.bind(
	{ modkey, "Shift" },
	"M",
	oxwm.spawn({ "sh", "-c", emacs_launcher .. " '(progn (select-frame-set-input-focus (selected-frame)) (emms))'" })
)
oxwm.key.bind(
	{ modkey, "Control" },
	"N",
	oxwm.spawn({
		"sh",
		"-c",
		emacs_launcher .. " '(progn (select-frame-set-input-focus (selected-frame)) (find-file \"~/org/notes.org\"))'",
	})
)
oxwm.key.bind(
	{ modkey },
	"N",
	oxwm.spawn({
		"sh",
		"-c",
		emacs_launcher .. " '(progn (select-frame-set-input-focus (selected-frame)) (org-agenda nil \"w\"))'",
	})
)
-- Emacs everywhere in X11 (OXWM)
oxwm.key.bind({ modkey, "Control" }, "E", oxwm.spawn("emacsclient --eval '(thanos/type)'"))
oxwm.rule.add({
	class = "emacs",
	title = "emacs-float",
	floating = true,
})
oxwm.key.bind(
	{ modkey },
	"XF86AudioPlay",
	oxwm.spawn({
		"sh",
		"-c",
		emacs_launcher .. " '(progn (select-frame-set-input-focus (selected-frame)) (bluetooth-list-devices))'",
	})
)
oxwm.key.bind({ modkey, "Shift" }, "XF86AudioPlay", oxwm.spawn({ "kitty", "-e", "pulsemixer" }))

-- Move window to next/previous Monitors
-- Workspace (tag) navigation
-- Switch to workspace N (tags are 0-indexed, so tag "1" is index 0)
oxwm.key.bind({ modkey }, "1", oxwm.tag.view(0))
oxwm.key.bind({ modkey }, "2", oxwm.tag.view(1))
oxwm.key.bind({ modkey }, "3", oxwm.tag.view(2))
oxwm.key.bind({ modkey }, "4", oxwm.tag.view(3))
oxwm.key.bind({ modkey }, "5", oxwm.tag.view(4))
oxwm.key.bind({ modkey }, "6", oxwm.tag.view(5))
oxwm.key.bind({ modkey }, "7", oxwm.tag.view(6))
oxwm.key.bind({ modkey }, "8", oxwm.tag.view(7))
oxwm.key.bind({ modkey }, "9", oxwm.tag.view(8))

-- Move focused window to workspace N
oxwm.key.bind({ modkey, "Shift" }, "1", oxwm.tag.move_to(0))
oxwm.key.bind({ modkey, "Shift" }, "2", oxwm.tag.move_to(1))
oxwm.key.bind({ modkey, "Shift" }, "3", oxwm.tag.move_to(2))
oxwm.key.bind({ modkey, "Shift" }, "4", oxwm.tag.move_to(3))
oxwm.key.bind({ modkey, "Shift" }, "5", oxwm.tag.move_to(4))
oxwm.key.bind({ modkey, "Shift" }, "6", oxwm.tag.move_to(5))
oxwm.key.bind({ modkey, "Shift" }, "7", oxwm.tag.move_to(6))
oxwm.key.bind({ modkey, "Shift" }, "8", oxwm.tag.move_to(7))
oxwm.key.bind({ modkey, "Shift" }, "9", oxwm.tag.move_to(8))

-- brightness keys
oxwm.key.bind({}, "XF86MonBrightnessUp", oxwm.spawn({ "brightnessctl", "set", "+5%" }))
oxwm.key.bind({}, "XF86MonBrightnessDown", oxwm.spawn({ "brightnessctl", "set", "5%-" }))

-- volume keys
oxwm.key.bind({}, "XF86AudioRaiseVolume", oxwm.spawn({ "sh", "-c", "pactl set-sink-volume @DEFAULT_SINK@ +5%" }))
oxwm.key.bind({}, "XF86AudioLowerVolume", oxwm.spawn({ "sh", "-c", "pactl set-sink-volume @DEFAULT_SINK@ -5%" }))
oxwm.key.bind({}, "XF86AudioMute", oxwm.spawn({ "sh", "-c", "pactl set-sink-volume @DEFAULT_SINK@ 0%" }))
oxwm.key.bind({}, "XF86AudioPrev", oxwm.spawn({ "sh", "-c", "playerctl previous" }))
oxwm.key.bind({ "Mod1" }, "Comma", oxwm.spawn({ "sh", "-c", "playerctl previous" }))
oxwm.key.bind({}, "XF86AudioNext", oxwm.spawn({ "sh", "-c", "playerctl next" }))
oxwm.key.bind({ "Mod1" }, "Period", oxwm.spawn({ "sh", "-c", "playerctl next" }))
oxwm.key.bind({}, "XF86AudioPlay", oxwm.spawn({ "sh", "-c", "playerctl play-pause" }))
oxwm.key.bind({ modkey }, "Space", oxwm.spawn({ "sh", "-c", "playerctl play-pause" }))

-- Autostart

oxwm.autostart("picom")
oxwm.autostart("emacs")
oxwm.autostart("helium-browser")
oxwm.autostart("xwallpaper --zoom ~/cloud/photos/wallpapers/der_chiemsee-daniel_fohr.jpg")
oxwm.autostart("dunst")
oxwm.autostart("nm-applet")
