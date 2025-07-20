# xrandr-control

An Emacs plugin for controlling Linux display settings via xrandr.

## Features

Run xrandr Commands: Use xrandr-control-execute to run arbitrary xrandr commands by passing command-line arguments as a string. Returns the command output or signals an error on failure.

Screen Brightness Control: Adjust the primary display's brightness with xrandr-control-set-brightness, an interactive command that accepts a brightness value between 0.0 and 1.0 (e.g., 0.7 for 70%).

## Installation

Copy xrandr-control.el to your Emacs load path.

Add (require 'xrandr-control) to your Emacs configuration (e.g., ~/.emacs or ~/.emacs.d/init.el).

## Usage

Execute xrandr Command: Call (xrandr-control-execute "arguments") with desired xrandr arguments, e.g., (xrandr-control-execute "--output eDP-1 --mode 1920x1080").

Set Brightness: Run M-x xrandr-control-set-brightness and enter a value between 0.0 and 1.0 when prompted.

## Requirements

Emacs 24.3 or later

Linux system with xrandr installed

## License

This project is released under the Unlicense, dedicating it to the public domain. You are free to use, modify, distribute, and sell this software without restrictions. See the LICENSE file for details.
