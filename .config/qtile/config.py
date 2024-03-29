import os
import subprocess

from typing import List

from libqtile import bar, layout, widget
from libqtile.config import Click, Drag, Group, Key, Match, Screen, KeyChord
from libqtile import extension
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal
from libqtile import hook

MOD = "mod4"

TERMINAL = guess_terminal()

DMENU_FLAGS = '-l 16 -p run -c -i'

BAR = '#282a36'
LIGHT_BAR = '#393b37'
YELLOW = '#f1fa8c'
RED = '#ff5555'
LIGHT_RED = '#ff9999'
GREEN = '#50fa7b'
CYAN = '#8be9fd'
LIGHT_CYAN = '#abfbff'

@hook.subscribe.startup_once
def autostart():
    home = os.path.expanduser('~/.config/qtile/autorc')
    subprocess.run([home])

keys = [
    Key([MOD], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([MOD], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([MOD], "j", lazy.layout.down(), desc="Move focus down"),
    Key([MOD], "k", lazy.layout.up(), desc="Move focus up"),
    Key([MOD], "s", lazy.next_screen() ),
    Key([MOD], "space", lazy.window.toggle_floating(),
        desc="Move window focus to other window"),

    Key([MOD, "shift"], "h", lazy.layout.shuffle_left(),
        desc="Move window to the left"),
    Key([MOD, "shift"], "l", lazy.layout.shuffle_right(),
        desc="Move window to the right"),
    Key([MOD, "shift"], "j", lazy.layout.shuffle_down(),
        desc="Move window down"),
    Key([MOD, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),

    Key([MOD, "control"], "h", lazy.layout.grow_left(),
        desc="Grow window to the left"),
    Key([MOD, "control"], "l", lazy.layout.grow_right(),
        desc="Grow window to the right"),
    Key([MOD, "control"], "j", lazy.layout.grow_down(),
        desc="Grow window down"),
    Key([MOD, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([MOD], "n", lazy.layout.normalize(), desc="Reset all window sizes"),

    Key([MOD, "shift"], "Return", lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack"),
    Key([MOD], "Return", lazy.spawn(TERMINAL), desc="Launch terminal"),

    Key([MOD], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([MOD], "c", lazy.window.kill(), desc="Kill focused window"),

    Key([MOD, "control"], "r", lazy.restart(), desc="Restart Qtile"),
    Key([MOD, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),

    Key([MOD], "F12", os.system("xbacklight -inc 10"), desc="Inc backlight"),
    Key([MOD], "F11", os.system("xbacklight -dec 10"), desc="Dec backlight"),

    Key([MOD], "r", lazy.spawncmd(),
        desc="Spawn a command using a prompt widget"),

    KeyChord([MOD], "p", [
        Key([], "p", lazy.spawn(f"dmenu_run {DMENU_FLAGS}"), desc="Spawn dmenu run"),
        Key([], "n", lazy.spawn(f"networkmanager_dmenu {DMENU_FLAGS}"), desc="Spawn dmenu for network manager"),
    ]),

    Key([MOD, "mod1"], "q", lazy.spawn("qutebrowser"), desc="Spawn qutebrowser"),
    Key([MOD, "mod1"], "b", lazy.spawn("brave"), desc="Spawn brave"),
    Key([MOD, "mod1"], "f", lazy.spawn("firefox"), desc="Spawn firefox"),

    KeyChord([MOD], "e", [
        Key([], "e", lazy.spawn("emacsclient -c -a 'emacs'"), desc="Spawn Emacsclient"),
        Key([], "d", lazy.spawn("emacsclient -c -a 'emacs' --eval '(dired nil)'"), desc='Emacsclient Dired'),
        Key([], "m", lazy.spawn("emacsclient -c -a 'emacs' --eval '(emms-browser)'"), desc='Emacsclient Dired'),
    ]),

    Key([MOD, "mod1"], "s", lazy.spawn("alacritty -e spt"), desc="Spawn spt (spotify clent)"),
    Key([MOD, "mod1"], "l", lazy.spawn("slock"), desc="lock desktop"),
    Key([MOD, "mod1"], "r", lazy.spawn("alacritty -e ranger"), desc="Spawn ranger (file manager)"),
    Key([MOD, "mod1"], "p", lazy.spawn("alacritty -e pulsemixer"), desc="Spawn pulsemixer"),
]

groups_names = [
    "dev",
    "www",
    "music",
    "virt",
    "sys",
    "other"
]

groups = [Group(i) for i in groups_names]
for i, group in enumerate(groups):
    keys.extend([
        Key([MOD], str(i+1), lazy.group[group.name].toscreen(),
            desc="Switch to group {}".format(group.name)),

        Key([MOD, "shift"], str(i+1), lazy.window.togroup(group.name, switch_group=True),
            desc="Switch to & move focused window to group {}".format(group.name)),
    ])

mouse = [
    Drag([MOD], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([MOD], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([MOD], "Button2", lazy.window.bring_to_front())
]

layout_cfg = {
    'border_width': 2,
    'border_normal': "#bb8888",
    'border_focus': "884444",
    'margin': 8
}

layouts = [
    # layout.Columns(**layout_cfg),
    # layout.Max(),
    layout.Stack(num_stacks=1, **layout_cfg),
    # layout.Bsp(),
    # layout.Matrix(),
    layout.MonadTall(**layout_cfg),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font='sans',
    fontsize=12,
    padding=6,
)

transparent_sep = {
    'foreground': BAR,
    'margin': 2
}

soft_sep = {
    'foreground': '44475a',
    'padding': 2,
    'margin': 4
}

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.Sep(**transparent_sep),
                widget.Image(filename="~/.config/qtile/python.png", margin=4),
                widget.Sep(**transparent_sep),
                widget.CurrentLayout(),
                widget.Sep(**transparent_sep),
                widget.GroupBox(
                    margin_y = 3,
                    margin_x = 0,
                    padding_y = 5,
                    padding_x = 3,
                    borderwidth = 3,
                    rounded = False,
                    highlight_color = LIGHT_BAR,
                    highlight_method = "line",
                    this_current_screen_border = CYAN,
                    this_screen_border = CYAN,
                    other_current_screen_border = LIGHT_BAR,
                    other_screen_border = LIGHT_BAR,
                ),
                widget.Prompt(),
                widget.Sep(**transparent_sep),
                widget.WindowName(),
                widget.Net(format="🌐  ↓{down} ↑{up}", foreground=YELLOW),
                widget.Sep(**soft_sep),
                widget.CPU(format='💻  {load_percent}%', fontsize=12, foreground=RED),
                widget.Sep(**soft_sep),
                widget.Battery(format='🔋  {char} {percent:2.0%} {hour:d}:{min:02d} {watt:.2f} W', foreground=GREEN),
                widget.Sep(**soft_sep),
                widget.Clock(format='📅  %B %-d, %H:%M', foreground=CYAN),
                widget.Sep(**soft_sep),
                widget.CurrentScreen(),
                widget.Sep(**soft_sep),
                widget.Systray()
            ],
            24,
            margin=8,
            background=BAR,
            border_color=CYAN,
            border_width=1
        ),
    ),
]

floating_layout = layout.Floating(float_rules=[
    # Run the utility of `xprop` to see the wm class and name of an X client.
    *layout.Floating.default_float_rules,
    Match(wm_class='confirmreset'),  # gitk
    Match(wm_class='makebranch'),  # gitk
    Match(wm_class='maketag'),  # gitk
    Match(wm_class='ssh-askpass'),  # ssh-askpass
    Match(title='branchdialog'),  # gitk
    Match(title='pinentry'),  # GPG key password entry
])

dgroups_key_binder = None
dgroups_app_rules = []

follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

auto_minimize = True

wmname = "LG3D"
