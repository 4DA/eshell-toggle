;;; eshell-toggle.el --- Show/hide eshell under active window. -*- lexical-binding: t -*-

;; Copyright (C) 2015, 2016  Dmitry Cherkasov

;; Author: Dmitry Cherkassov <dcherkassov@gmail.com>
;; Maintainer: Dmitry Cherkassov <dcherkassov@gmail.com>
;; URL: https://github.com/4da/eshell-toggle
;; Version: 0.9.0
;; Package-Requires: ((emacs "24.4")(dash "2.11.0"))
;; Keywords: processes

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
(require 'eshell)
(require 'term)
(require 'subr-x)

;;; Customization

;;; Code:

(defgroup eshell-toggle nil
  "Customize group for eshell-toggle.el"
  :group 'emacs)

(defcustom eshell-toggle-height-fraction
  3
  "Proportion of parent window height and eshell window."
  :type 'integer
  :group 'eshell-toggle)

(defcustom eshell-toggle-default-directory
  default-directory
  "Default directory to open eshell at if buffer has no associated file."
  :type 'directory
  :group 'eshell-toggle)

(defcustom eshell-toggle-use-projectile-root
  nil
  "Open eshell at projectile's project root if not nil."
  :type '(choice (const :tag "Disabled" nil)
                 (const :tag "Enabled" t))
  :group 'eshell-toggle)

(defcustom eshell-toggle-name-separator
  ":"
  "String to separate directory paths when giving a name to buffer."
  :type 'string
  :group 'eshell-toggle)

(defcustom eshell-toggle-init-term-char-mode
  nil
  "Switch `ansi-term' buffer to ‘term-char-mode’ after init.  Bind `eshell-toggle' in `term-raw-map'."
  :type 'boolean
  :group 'eshell-toggle)

(defcustom eshell-toggle-init-function
  'eshell-toggle-init-eshell
  "Function to init toggle buffer."
  :type 'function
  :group 'eshell-toggle)

(defvar eshell-toggle--toggle-buffer-p nil)
(make-variable-buffer-local 'eshell-toggle--toggle-buffer-p)

(defun eshell-toggle--visiblep (et-buffer-name)
  "Return if ET-BUFFER-NAME is visible."
  (-some (lambda (win)
           (and (-> win window-buffer buffer-name
                    (string= et-buffer-name))
                win))
	 (window-list)))

(defun eshell-toggle--get-directory ()
  "Return default directory for current buffer."
  (if eshell-toggle-use-projectile-root
      (condition-case nil
          (projectile-project-root)
        (error eshell-toggle-default-directory))
    eshell-toggle-default-directory))

(defun eshell-toggle--make-buffer-name ()
  "Generate toggle buffer name."
  (let* ((dir (eshell-toggle--get-directory))
         (name (string-join (split-string dir "/") eshell-toggle-name-separator))
         (buf-name (concat "*et" name "*")))
    buf-name))

(defun eshell-toggle-init-eshell (dir)
  "Init `eshell' buffer with DIR."
  (eshell "new")
  (insert (concat "cd" " " dir))
  (eshell-send-input)
  (eshell/clear)
  (insert (concat "ls"))
  (eshell-send-input))

;; TODO: move common code to a macro
(defun eshell-toggle-init-ansi-term (dir)
  "Init `ansi-term' buffer with DIR."
  (ansi-term (getenv "SHELL"))
  (term-line-mode)
  (insert (concat "cd" " " dir "; clear; ls"))
  (term-send-input)
  (when eshell-toggle-init-term-char-mode
    (term-char-mode)))

(defun eshell-toggle-init-tmux (dir)
  "Init tmux `ansi-term' buffer with DIR."
  (ansi-term (getenv "SHELL"))
  (term-line-mode)
  (insert (format "tmux new -A -c '%s' -s '%s'" dir dir))
  (term-send-input)
  (when eshell-toggle-init-term-char-mode
    (term-char-mode)))

(defun eshell-toggle--show-buffer-split-window (buf-name new-buffer?)
  "Split window, init BUF-NAME if NEW-BUFFER? is t and activate it."
  (let ((height (/ (window-total-height) eshell-toggle-height-fraction))
        (dir (eshell-toggle--get-directory)))
    (split-window-vertically (- height))
    (other-window 1)

    (if new-buffer?
        (progn
          (funcall eshell-toggle-init-function dir)
          (rename-buffer buf-name)
          (setq eshell-toggle--toggle-buffer-p t))
      (switch-to-buffer buf-name))))

(defun eshell-toggle ()
  "Show eshell at the bottom of current window cd to current buffer's path.
If eshell-toggle'd buffer is already visible in frame for current buffer or current window is (toggled) eshell itself then hide it."
  (interactive)
  (if (eq eshell-toggle--toggle-buffer-p t)
      ;; if we are in eshell-toggle buffer just delete its window
      (delete-window)

    (let ((buf-name (eshell-toggle--make-buffer-name)))
      (if (get-buffer buf-name)
	  ;; buffer is already created
          (or (-some-> buf-name eshell-toggle--visiblep delete-window)
	      (eshell-toggle--show-buffer-split-window buf-name nil))

        ;; buffer is not created, create it
        (eshell-toggle--show-buffer-split-window buf-name t)))))

(when eshell-toggle-init-term-char-mode
  (dolist (kb (where-is-internal 'eshell-toggle))
    (define-key term-raw-map kb 'eshell-toggle)))

(provide 'eshell-toggle)

;;; eshell-toggle.el ends here
