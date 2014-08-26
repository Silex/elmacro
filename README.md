# elmacro

Show keyboard macros as emacs lisp.

Set it up like this:

``` emacs-lisp
(require 'elmacro)
(elmacro-mode)
```

Then record a keyboard macro and use `M-x elmacro-show-last-macro` or `M-x elmacro-show-lossage`.

## Examples

### upcase-last-word

Say you have the following text:

    violets are blue
    roses are red

With the cursor somewhere on the first line. Press the following keys:

`F3 C-e M-b M-u C-a C-n F4`

Then doing `M-x elmacro-show-last-macro upcase-last-word RET` produces a buffer with:

``` emacs-lisp
(defun upcase-last-word ()
  "Change me!"
  (interactive)
  (move-end-of-line 1)
  (backward-word 1)
  (upcase-word 1)
  (move-beginning-of-line 1)
  (next-line 1 1))
```

You can now do `M-x upcase-last-word` or call it from your elisp code :)

## Contributions are welcome!

Either as suggestions or as pull requests by opening tickets on the
[issue tracker](https://github.com/Silex/elmacro/issues).
