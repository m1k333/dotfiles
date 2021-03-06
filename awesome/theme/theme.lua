---------------------------
-- Default awesome theme --
---------------------------

theme = {}

theme.font          = "DejaVu Sans 10"

theme.bg_normal     = "#000000"
theme.bg_focus      = "#383838"
theme.bg_urgent     = "#000000"
theme.bg_minimize   = "#000000"
theme.bg_systray    = theme.bg_normal

theme.fg_normal     = "#a0a0a0"
theme.fg_focus      = "#f0f0f0"
theme.fg_urgent     = "#ff0000"
theme.fg_minimize   = "#606060"

theme.border_width  = 2
theme.border_normal = "#000000"
theme.border_focus  = "#03a9f4"
theme.border_marked = "#9c27b0"

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- taglist_[bg|fg]_[focus|urgent|occupied|empty]
-- tasklist_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- mouse_finder_[color|timeout|animate_timeout|radius|factor]
-- Example:
--theme.taglist_bg_focus = "#ff0000"

-- Display the taglist squares
theme.taglist_squares_sel   = "~/.config/awesome/theme/taglist/squarefw.png"
theme.taglist_squares_unsel = "~/.config/awesome/theme/taglist/squarew.png"

-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon = "~/.config/awesome/theme/submenu.png"
theme.menu_height = 18
theme.menu_width  = 100

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.bg_widget = "#cc0000"

-- Define the image to load
theme.titlebar_close_button_normal = "~/.config/awesome/theme/titlebar/close_normal.png"
theme.titlebar_close_button_focus  = "~/.config/awesome/theme/titlebar/close_focus.png"

theme.titlebar_ontop_button_normal_inactive = "~/.config/awesome/theme/titlebar/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive  = "~/.config/awesome/theme/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active = "~/.config/awesome/theme/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active  = "~/.config/awesome/theme/titlebar/ontop_focus_active.png"

theme.titlebar_sticky_button_normal_inactive = "~/.config/awesome/theme/titlebar/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive  = "~/.config/awesome/theme/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active = "~/.config/awesome/theme/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active  = "~/.config/awesome/theme/titlebar/sticky_focus_active.png"

theme.titlebar_floating_button_normal_inactive = "~/.config/awesome/theme/titlebar/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive  = "~/.config/awesome/theme/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active = "~/.config/awesome/theme/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_active  = "~/.config/awesome/theme/titlebar/floating_focus_active.png"

theme.titlebar_maximized_button_normal_inactive = "~/.config/awesome/theme/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = "~/.config/awesome/theme/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active = "~/.config/awesome/theme/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active  = "~/.config/awesome/theme/titlebar/maximized_focus_active.png"

theme.wallpaper = "~/.config/awesome/theme/wallpaper.jpg"

-- You can use your own layout icons like this:
theme.layout_fairh = "~/.config/awesome/theme/layouts/fairhw.png"
theme.layout_fairv = "~/.config/awesome/theme/layouts/fairvw.png"
theme.layout_floating  = "~/.config/awesome/theme/layouts/floatingw.png"
theme.layout_magnifier = "~/.config/awesome/theme/layouts/magnifierw.png"
theme.layout_max = "~/.config/awesome/theme/layouts/maxw.png"
theme.layout_fullscreen = "~/.config/awesome/theme/layouts/fullscreenw.png"
theme.layout_tilebottom = "~/.config/awesome/theme/layouts/tilebottomw.png"
theme.layout_tileleft   = "~/.config/awesome/theme/layouts/tileleftw.png"
theme.layout_tile = "~/.config/awesome/theme/layouts/tilew.png"
theme.layout_tiletop = "~/.config/awesome/theme/layouts/tiletopw.png"
theme.layout_spiral  = "~/.config/awesome/theme/layouts/spiralw.png"
theme.layout_dwindle = "~/.config/awesome/theme/layouts/dwindlew.png"

theme.awesome_icon = "/usr/share/awesome/icons/awesome16.png"

-- Define the icon theme for application icons. If not set then the icons 
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
theme.icon_theme = nil

return theme
-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80
