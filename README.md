# q-mode

## Features

q-mode is a major mode for editing q (the language written by [Kx Systems](http://www.kx.com')) in [Emacs](https://www.gnu.org/software/emacs/).

Some of its major features include:
- syntax highlighting (font lock),
- interaction with inferior q[con] instance,
- scans declarations and places them in a menu.

## Installation

To load `q-mode` on-demand, instead of at startup, add this to your
initialization file

```elisp
(autoload 'q-mode "q-mode")
```
The add the following to your initialization file to open all .k
and .q files with q-mode as major mode automatically:

```elisp
(add-to-list 'auto-mode-alist '("\\.[kq]\\'" . q-mode))
```

If you load ess-mode, it will attempt to associate the .q extension
with S-mode.  To stop this, add the following lines to your
initialization file.

```elisp
(defun remove-ess-q-extn ()
 (when (assoc "\\.[qsS]\\'" auto-mode-alist)
  (setq auto-mode-alist
        (remassoc "\\.[qsS]\\'" auto-mode-alist))))
(add-hook 'ess-mode-hook 'remove-ess-q-extn)
(add-hook 'inferior-ess-mode-hook 'remove-ess-q-extn)
```

## Usage

Use `M-x q` to start an inferior q shell. Or use `M-x qcon` to
create an inferior qcon shell to communicate with an existing q
process.  Both can be prefixed with the universal-argument `C-u` to
customize the arguments used to start the processes.

The first q[con] session opened becomes the activated buffer.
To open a new session and send code to the new buffer, it must be
actived.  Switch to the desired buffer and type `C-c M-RET` to
activate it.

The following commands are available to interact with an inferior
q[con] process/buffer. `C-c C-l` sends a single line, `C-c C-f`
sends the surrounding function, `C-c C-r` sends the selected region
and `C-c C-b` sends the whole buffer.  If the source file exists on
the same machine as the q process, `C-c M-l` can be used to load
the file associated with the active buffer.

## Customization

`M-x customize-group` can be used to customize the `q` group.
Specifically, the `inferior-q-program-name` and
`inferior-qcon-program-name` variables can be changed depending on
your environment.

Q-mode indents each level based on `q-indent-step`.  To indent code
based on {}-, ()-, and []-groups instead of equal width tabs, you
can set this value to nil.

The variables `q-msg-prefix` and `q-msg-postfix` can be customized
to prefix and postfix every msg sent to the inferior q[con]
process. This can be used to change directories before evaluating
definitions within the q process and then changing back to the root
directory. To make the variables change values depending on which
file they are sent from, values can be defined in a single line a
the top of each .q file:

```q
/ -*- q-msg-prefix: "system \"d .jnp\";"; q-msg-postfix: ";system \"d .\"";-*-
```

or at the end:

```q
/ Local Variables:
/ q-msg-prefix: "system \"d .jnp\";"
/ q-msg-postfix: ";system \"d .\""
/ End:
```
