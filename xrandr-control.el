;;; xrandr-control.el --- Control xrandr settings from Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Your Name

;; Author: Bruno Dias <dias.h.bruno@gmail.com>
;; Version: 0.1
;; Keywords: tools, hardware, xrandr
;; Homepage: https://github.com/el-sdk/xrandr-control
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides functions to control xrandr settings from within Emacs.
;; It allows users to manage display settings such as resolution, rotation, and
;; output state using the xrandr command-line tool.

;;; Code:

(defun xrandr-control-execute (args)
  "Execute xrandr with the provided ARGS.
ARGS should be a string of command-line arguments to pass to xrandr.
Returns the output of the command as a string, or signals an error if the command fails."
  (let ((command (concat "xrandr " args)))
    (with-temp-buffer
      (let ((exit-code (call-process-shell-command command nil t)))
        (if (zerop exit-code)
            (buffer-string)
          (error "xrandr command failed with exit code %d: %s" exit-code (buffer-string)))))))

(defun xrandr-control-set-brightness (brightness)
  "Set the screen brightness for the primary display using xrandr.
BRIGHTNESS should be a number between 0 and 1 (e.g., 0.7 for 70% brightness)."
  (interactive "nBrightness (0.0 to 1.0): ")
  (unless (and (numberp brightness) (>= brightness 0.0) (<= brightness 1.0))
    (error "Brightness must be a number between 0.0 and 1.0"))
  (let* ((primary-output (with-temp-buffer
                           (call-process-shell-command "xrandr --current | grep primary | awk '{print $1}'" nil t)
                           (string-trim (buffer-string))))
         (args (format "--output %s --brightness %.2f" primary-output brightness)))
    (xrandr-control-execute args)
    (message "Set brightness to %.2f for output %s" brightness primary-output)))

(defun xrandr-control-list-monitors ()
  "List all active monitors configured by xrandr.
Returns an associative list where each key is a monitor name (string)
and each value is a plist of its attributes: width, height, x-offset,
y-offset, and primary status."
  (let ((output (xrandr-control-execute "--listmonitors")))
    (with-temp-buffer
      (insert output)
      (goto-char (point-min))
      (let (monitors)
        (while (re-search-forward "^ *[0-9]+: +\\(?:\\+\\*?\\)?\\([a-zA-Z0-9_-]+\\) \\([0-9]+\\)/[0-9]+x\\([0-9]+\\)/[0-9]+\\([-+0-9]+\\)\\([-+0-9]+\\) \\*?\\(primary\\)?" nil t)
          (let ((name (match-string 1))
                (width (string-to-number (match-string 2)))
                (height (string-to-number (match-string 3)))
                (x-offset (string-to-number (match-string 4)))
                (y-offset (string-to-number (match-string 5)))
                (primary (match-string 6)))
            (push (list name
                        :width width
                        :height height
                        :x-offset x-offset
                        :y-offset y-offset
                        :primary (if primary t nil))
                  monitors)))
        (nreverse monitors)))))

(defun xrandr-control-rotate-monitor (display rotation)
  "Rotate the specified DISPLAY to ROTATION.
DISPLAY is the name of the monitor (e.g., 'eDP-1').
ROTATION is one of 'normal', 'left', 'right', or 'inverted'."
  (interactive
   (let* ((monitors (xrandr-control-list-monitors))
          (display (completing-read "Select monitor: "
                                    (mapcar #'car monitors) nil t))
          (rotation (completing-read "Select rotation: "
                                     '("normal" "left" "right" "inverted") nil t)))
     (list display rotation)))
  (unless (member rotation '("normal" "left" "right" "inverted"))
    (error "Invalid rotation: must be normal, left, right, or inverted"))
  (let ((args (format "--output %s --rotate %s" display rotation)))
    (xrandr-control-execute args)
    (message "Rotated monitor %s to %s" display rotation)))

(defun xrandr-control-create-mode (display-mode-name width height refresh-rate)
  "Create a new mode with DISPLAY-MODE-NAME, WIDTH, HEIGHT, and REFRESH-RATE.
DISPLAY-MODE-NAME is a string (e.g., '1920x1080_60').
WIDTH and HEIGHT are in pixels.
REFRESH-RATE is in Hz (e.g., 60.0)."
  (interactive
   (list
    (read-string "Mode name (e.g., 1920x1080_60): ")
    (read-number "Width (pixels): ")
    (read-number "Height (pixels): ")
    (read-number "Refresh rate (Hz): ")))
  (unless (and (stringp display-mode-name) (> (length display-mode-name) 0))
    (error "Mode name must be a non-empty string"))
  (unless (and (integerp width) (> width 0))
    (error "Width must be a positive integer"))
  (unless (and (integerp height) (> height 0))
    (error "Height must be a positive integer"))
  (unless (and (numberp refresh-rate) (> refresh-rate 0))
    (error "Refresh rate must be a positive number"))
  (let* ((cvt-output (with-temp-buffer
                       (call-process-shell-command
                        (format "cvt %d %d %.2f" width height refresh-rate)
                        nil t)
                       (buffer-string)))
         (modeline (with-temp-buffer
                     (insert cvt-output)
                     (goto-char (point-min))
                     (if (re-search-forward "Modeline \"[^\"]+\" \\(.*\\)$" nil t)
                         (match-string 1)
                       (error "Failed to parse cvt output"))))
         (args (format "--newmode \"%s\" %s" display-mode-name modeline)))
    (xrandr-control-execute args)
    (message "Created new mode %s (%dx%d@%.2fHz)" display-mode-name width height refresh-rate)))

(provide 'xrandr-control)

;;; xrandr-control.el ends here
