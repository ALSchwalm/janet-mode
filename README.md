janet-mode - A major mode for Janet
=====================================

[![License GPL 3][badge-license]][license]

`janet-mode` provides a major mode for the [Janet][] programming language.

Installation
------------

Ensure that `janet-mode.el` is in your load-path, and then add

    (require 'janet-mode)

to your emacs init file.

Known issues
------------

`font-lock` will get confused editing
[long (multiline) strings](https://janet-lang.org/docs/syntax.html#Long-strings).
The cause is explained in
[EmacsWiki:MultilineRegexp](https://www.emacswiki.org/emacs/MultilineRegexp).
To fix a specific string after editing, select the whole string
including the backticks and run `M-x` `font-lock-fontify-block`.


[Janet]: https://janet-lang.org/
[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg?dummy
[license]: https://github.com/ALSchwalm/janet-mode/blob/master/LICENSE
