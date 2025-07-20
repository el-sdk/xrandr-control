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

(provide 'xrandr-control)

;;; xrandr-control.el ends here
