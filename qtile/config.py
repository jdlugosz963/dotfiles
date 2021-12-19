import os
import subprocess

from typing import List

from libqtile import bar, layout, widget
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile import extension
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal
from libqtile import hook

mod = "mod4"
terminal = guess_terminal()

BAR = '#282a36'
YELLOW = '#f1fa8c'
RED = '#ff5555'
GREEN = '#50fa7b'
CYAN = '#8be9fd'


@hook.subscribe.startup_once
def autostart():
    home = os.path.expanduser('~/.config/qtile/autorc')
    subprocess.run([home])

keys = [
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    Key([mod], "space", lazy.layout.next(),
        desc="Move window focus to other window"),

    Key([mod, "shift"], "h", lazy.layout.shuffle_left(),
        desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(),
        desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(),
        desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),

    Key([mod, "control"], "h", lazy.layout.grow_left(),
        desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(),
        desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(),
        desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),

    Key([mod, "shift"], "Return", lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack"),
    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),

    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "c", lazy.window.kill(), desc="Kill focused window"),

    Key([mod, "control"], "r", lazy.restart(), desc="Restart Qtile"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod], "r", lazy.spawncmd(),
        desc="Spawn a command using a prompt widget"),

    Key([mod], "p", lazy.run_extension(extension.DmenuRun(
        dmenu_prompt="$",
        dmenu_bottom=False,
        dmenu_lines=56,
        background=BAR,
        foreground=CYAN,
    ))),

    Key([mod, "mod1"], "q", lazy.spawn("qutebrowser"), desc="Spawn qutebrowser"),
    Key([mod, "mod1"], "b", lazy.spawn("brave"), desc="Spawn brave"),
    Key([mod, "mod1"], "e", lazy.spawn("emacs"), desc="Spawn emacs"),
    Key([mod, "mod1"], "s", lazy.spawn("alacritty -e spt"), desc="Spawn spt (spotify clent)"),
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
abc = 0
for i in groups:
    abc += 1
    keys.extend([
        Key([mod], str(abc), lazy.group[i.name].toscreen(),
            desc="Switch to group {}".format(i.name)),

        Key([mod, "shift"], str(abc), lazy.window.togroup(i.name, switch_group=True),
            desc="Switch to & move focused window to group {}".format(i.name)),
    ])


layout_cfg = {
    'border_width': 2,
    'border_normal': "#bb8888",
    'border_focus': "884444",
    'margin': 8
}

layouts = [
    layout.Columns(**layout_cfg),
    # layout.Max(),
    layout.Stack(num_stacks=1, **layout_cfg),
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.MonadTall(),
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
extension_defaults = widget_defaults.copy()

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
                widget.GroupBox(),
                widget.Prompt(),
                widget.Sep(**transparent_sep),
                widget.WindowName(),
                widget.Net(format="↓{down} ↑{up}", foreground=YELLOW),
                widget.Sep(**soft_sep),
                widget.CPU(format=' {load_percent}%', fontsize=12, foreground=RED),
                widget.Sep(**soft_sep),
                widget.Battery(foreground=GREEN),
                widget.Sep(**soft_sep),
                widget.Clock(timezone='Europe/Warsaw', format='%B %-d, %H:%M', foreground=CYAN),
            ],
            24,
            margin=8,
            background=BAR,
            border_color=CYAN,
            border_width=1
        ),
    ),
]

mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
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
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

auto_minimize = True

wmname = "LG3D"
