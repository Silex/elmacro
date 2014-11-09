# elmacro

Shows keyboard macros or latest interactive commands as emacs lisp.

## Installation

The recommended way to install elmacro is through MELPA.

Otherwise, simply add `elmacro.el` to your load-path and then `(require 'elmacro)`.

## Usage

To enable elmacro, do <kbd>M-x elmacro-mode</kbd> or enable it from your config file like this:

``` emacs-lisp
(elmacro-mode)
```

## Commands

### elmacro-show-last-commands(count)

<kbd>M-x elmacro-show-last-commands</kbd> shows your latest emacs activity as emacs lisp.
This is more or less a better version of `kmacro-edit-lossage`.

The default number of commands shown is 300. You can change this
number by using a numeric prefix argument or by using the
universal argument, in which case it'll ask for how many in the
minibuffer.

### elmacro-show-last-macro(name)

You can also record a [keyboard macro](https://www.gnu.org/software/emacs/manual/html_node/emacs/Keyboard-Macros.html)
and use <kbd>M-x elmacro-show-last-macro</kbd> to see it as emacs lisp.

It'll ask you which `defun` name you want to give to this macro.

### elmacro-clear-recorded-commands

Clears all the recorded commands.

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

## Options

### elmacro-unwanted-commands-regexp

_Default value: `"^\\(ido\\|smex\\)"`_

Regexp used to filter unwanted commands.

### elmacro-additional-recorded-functions

_Default value: `'(copy-file copy-directory rename-file delete-file make-directory)`_

This is a list of non-interactive functions that you also want to
be recorded.

For example, `dired-copy-file` (`C` key in dired)
doesn't reads its arguments as an interactive specification, and
thus the file name is never stored. Adding `copy-file` to
`elmacro-additional-recorded-functions` solves this.

### elmacro-concatenate-multiple-inserts

_Default value: `t`_

When enabled, will concatenate multiple text insertion together, so instead of:

``` emacs-lisp
(defun abc ()
  (insert "a")
  (insert "b")
  (insert "c"))
```

You get:

``` emacs-lisp
(defun abc ()
  (insert "abc"))
```

### elmacro-objects-to-convert

_Default value: `'(frame window buffer)`_

List of symbols representing which object to convert.

For example, converts `<#window 42>` to `(elmacro-get-window-object 42)`.

## Mouse events support

A nice addition to normal macros is that mouse events (clicks / scroll)
are also recorded and elmacro can figure which emacs window / frame was the target.

For example, by default clicking in a window will generate code like:

``` emacs-lisp
(mouse-set-point '(mouse-1 (#<window 75 on foo.el> 913 (90 . 286) 185432429 nil 913 (10 . 15) nil (90 . 1) (9 . 19))))
```

We see that the `<#window 75 on foo.el>` part is not very useful.
Thanks to the mechanism of `elmacro-objects-to-convert`, the following code is generated
instead (`elmacro-get-window-object` is a helper to return the correct emacs window object):

``` emacs-lisp
(mouse-set-point `(mouse-1 (,(elmacro-get-window-object 75) 913 (90 . 286) 185432429 nil 913 (10 . 15) nil (90 . 1) (9 . 19))))
```

## Contributions welcome!

Either as suggestions or as pull requests by opening tickets on the
[issue tracker](https://github.com/Silex/elmacro/issues).

## Thanks

* [purcell](https://github.com/purcell) for better package description.
* [syohex](https://github.com/syohex) for byte-compilation issues.
* [Youngfrog](https://github.com/YoungFrog) for helping with `elmacro-make-advice-lambda`
