;;; eshell-toggle.el --- Show/hide eshell under active window

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Dmitry Cherkassov <dcherkassov@gmail.com>
;; Maintainer: Dmitry Cherkassov <dcherkassov@gmail.com>
;; URL: https://github.com/4da/eshell-toggle
;; Version: 0.1.0
;; Package-Requires: ((dash "2.11.0"))
;; Keywords: eshell

;; This file is part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Call `eshell-toggle' to toggle eshell for current buffer.
;; Show eshell at the bottom of current window cd to current buffer's path. 
;; If eshell-toggle'd buffer is already visible in frame for current buffer or current window is (toggled) eshell itself then hide it. 

(require 'dash)
(require 'cl-lib)
(require 'eshell)

;;; Customization

(defgroup eshell-toggle nil
  "Customize group for eshell-toglle.el"
  :group 'emacs
  :prefix "et-")

(defcustom et-eshell-height-fraction
  3
  "Proportion of parent window height and eshell window"
  :group 'eshell-toggle)

(defcustom et-default-directory
  default-directory
  "Default directory to open eshell at if buffer has no associated file."
  :group 'eshell-toggle)

(defcustom et-name-separator
  ":"
  "String to separate directory paths when giving a name to buffer"
  :group 'eshell-toggle)

(defvar eshell-buffer-p nil)

(cl-defun et-visible-for-bufferp (et-buffer-name)
  (-map (lambda (win)
	  (when (string= et-buffer-name
			 (buffer-name (window-buffer win)))
	    (cl-return-from et-visible-for-bufferp win)))
	(window-list))
  nil)

(defun et-make-buffer-name ()
  (let* ((parent (if (buffer-file-name)
		     (file-name-directory (buffer-file-name))
		   et-default-directory))
	 (fname (uniquify-buffer-file-name (current-buffer)))
	 (name (if fname 
		   (string-join (split-string fname "/") et-name-separator)
		 (concat et-name-separator et-default-directory)))
	 (buf-name (concat "*et" name "*")))
    buf-name))

(make-variable-buffer-local 'eshell-buffer-p)

(defun et-show-buffer-split-window (buf-name new-buffer?)
  (let ((height (/ (window-total-height) et-eshell-height-fraction)))  
    (split-window-vertically (- height))
    (other-window 1)

    (if new-buffer?
	;; create a new buffer
	(progn
	  (eshell "new")
	  (rename-buffer buf-name)

	  (setq eshell-buffer-p t)
	  (insert (concat "ls"))
	  (eshell-send-input))
      (progn
	(switch-to-buffer buf-name)))))

(defun eshell-toggle ()
  "Show eshell at the bottom of current window cd to current buffer's path. 
If eshell-toggle'd buffer is already visible in frame for current buffer or current window is (toggled) eshell itself then hide it. "
  (interactive)
  (if (eq eshell-buffer-p t)
      ;; if we are in eshell-toggle buffer just delete its window
      (delete-window)
    
    (let ((buf-name (et-make-buffer-name)))
      (if (get-buffer buf-name)
	  ;; buffer is already created
	  (let ((vis-buf-window (et-visible-for-bufferp buf-name)))
	    (if vis-buf-window
		;; buffer is in visible window, close it
		(delete-window vis-buf-window)

	      ;; buffer is not in visible window, show it
	      (et-show-buffer-split-window buf-name nil)))

	;; buffer is not created, create it
	(et-show-buffer-split-window buf-name t)))))

(provide 'eshell-toggle)

;;; eshell-toggle.el ends here
