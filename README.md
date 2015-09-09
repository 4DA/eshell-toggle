Simple functionality to show/hide eshell at the bottom of active window with directory of its buffer.

## Installation
    (add-to-list 'load-path "/path/to/eshell-toggle.el")
    (require 'eshell-toggle)
    (global-set-key (kdb "s-`") 'eshell-toggle)

## Customization
- [et/eshell-height-fraction] Proportion of parent window height and eshell window;
- [et/default-directory] Default directory to open eshell at if buffer has no associated file;
- [et/name-separator] String to separate directory paths when giving a name to buffer.



