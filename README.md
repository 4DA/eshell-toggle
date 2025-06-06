Simple functionality to show/hide eshell/ansi-term (or almost any other buffer, see `eshell-toggle-init-function` description below) at the bottom of active window with directory of its buffer.

## Installation

    (add-to-list 'load-path "/path/to/eshell-toggle.el")
    (require 'eshell-toggle)
    (global-set-key (kbd "s-`") 'eshell-toggle)

or using use-package and quelpa:

    (use-package eshell-toggle
      :custom
      (eshell-toggle-size-fraction 3)
      (eshell-toggle-find-project-root-package t) ;; for projectile
      ;;(eshell-toggle-find-project-root-package 'projectile) ;; for projectile
      ;; (eshell-toggle-use-projectile-root 'project) ;; for in-built project.el
      (eshell-toggle-run-command nil)
      (eshell-toggle-init-function #'eshell-toggle-init-ansi-term)
      :quelpa
      (eshell-toggle :repo "4DA/eshell-toggle" :fetcher github :version original)
      :bind
      ("s-`" . eshell-toggle))

## Customization

- `eshell-toggle-size-fraction` (NB: `eshell-toggle-height-fraction` is obsolete since 0.10.0) Proportion of parent window size (see `eshell-toggle-window-side` below) and eshell window;
- `eshell-toggle-window-side` - Where to open eshell window, a value can be `'below'` (default), `'above`, `'right` or `'left`;

- `eshell-toggle-default-directory` Default directory to open eshell at if buffer has no associated file;
- `eshell-toggle-name-separator` String to separate directory paths when giving a name to buffer.
- `eshell-toggle-use-projectile-root` is obsolete since 0.10.1, use the following `eshell-toggle-find-project-root-package` instead.
- `eshell-toggle-find-project-root-package`  If set to `'project` it tries to use the built-in `project` to open an eshell at project root, if set to `'projectile` or `t` it uses `projectile`, and if set to nil it detects the current directory.

- `eshell-toggle-run-command` - command to run in a new shell.

- `eshell-toggle-init-function` is a function to initialize toggled buffer, default one is `eshell-toggle-init-eshell`, but can be any function which takes a directory and buffer name as arguments and creates a buffer, ootb there are `eshell-toggle-init-eshell`, `eshell-toggle-init-ansi-term`, `eshell-toggle-init-shell` and `eshell-toggle-init-tmux` and (you have to have tmux binary in your PATH), `dired` and `magit-status` work too.
- `eshell-toggle-init-term-char-mode` If not nil eshell-toggle starts ansi-term based buffers in `term-char-mode`

If you want to change these on a per-directory basis, you can use [.dir-locals.el](https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html) magic:

    ((nil . ((eshell-toggle-init-function . eshell-toggle-init-ansi-term))))
