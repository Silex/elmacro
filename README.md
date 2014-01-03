# elmacro

Show recorded macro as emacs lisp function.

Set it up like this:

``` scheme
(require 'elmacro)
(elmacro-mode)
```

Then record a macro and `M-x elmacro-show-last-macro` or `M-x elmacro-show-lossage`.

## Examples

### capitalize-last-word

Say you have the following text:

    violets are blue
    roses are red

With the cursor somewhere on the first line. Press the following keys:

`C-x ( C-e M-b M-u M-a C-n C-x )`

Then calling `M-x elmacro-show-last-macro capitalize-line-last-word RET` produces a buffer with:

``` scheme
(defun capitalize-line-last-word ()
  "Change me!"
  (interactive)
  (move-end-of-line 1)
  (backward-word 1)
  (upcase-word 1)
  (move-beginning-of-line 1)
  (next-line 1 1))
```

You can now use `capitalize-line-last-word` in your emacs lisp code :)

## License

Copyright (C) Philippe Vaucher

Author: Philippe Vaucher <philippe.vaucher@gmail.com>
Keywords: macro elisp

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
