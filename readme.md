# xrandr-control

An Emacs plugin for controlling Linux display settings via xrandr.

## Installation

Copy xrandr-control.el to your Emacs load path.

Add (require 'xrandr-control) to your Emacs configuration (e.g., ~/.emacs or ~/.emacs.d/init.el).

## Usage

### xrandr-control-set-brightness

- Enter a value between 0.0 and 1.0.

### xrandr-control-list-monitors

### xrandr-control-rotate-monitor

- Select the monitor
- Choose the direction

### xrand-control-create-mode

- Give a name to the mode
- Choose the width
- Choose the height
- Choose the refresh-rate

### Execute raw xrandr command

- Call `(xrandr-control-execute "arguments")`

Example:

```lisp
(xrandr-control-execute "--output eDP-1 --mode 1920x1080")
```

## Requirements

- Emacs 24.3 or later

- Linux system with xrandr installed

## License

This project is released under the Unlicense, dedicating it to the public domain. You are free to use, modify, distribute, and sell this software without restrictions. See the LICENSE file for details.
