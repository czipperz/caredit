# caredit

paredit for c-like languages.

This was made because I declared bankruptcy with cedit.

    (require 'caredit)
    (add-hook 'c++-mode-hook 'caredit-use-default-mappings-this-buffer)

## compilation

You can compile `caredit.el` with `make` (testing the project will do this as well).

## testing

You can test this project using `make check`.

## licensing

caredit is licensed under the GNU GPLv3.

## copyright

I have attributed copyright to the creaters of paredit and cedit
because their work has been directly copied into this project.
