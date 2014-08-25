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

`F3 C-e M-b M-u C-a C-n F4`

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
