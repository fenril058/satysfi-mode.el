<!-- -*- coding: utf-8 -*- -->
# satysfi-mode.el
An Emacs major mode for SATySFi

The mode supports `imenu`, `outline-mode` and `which-function-mode`.

## Configuration

If you using [leaf.el](https://github.com/conao3/leaf.el),
add the following commands to your `init.el`, for example.

```lisp
(leaf satysfi-mode
  :mode
  ("\\.saty$" . satysfi-mode)
  ("\\.satyh$" . satysfi-mode)
  :custom
  (satysfi-typeset-command . "satysfi")     ; default "satysfi -b""
  (satysfi-view-command . "sumatrapdf")     ; default "open""
  (satysfi-enable-electric-pair-mode . 1)   ; close parenthesis automatically when you open it
  (satysfi-enable-which-function-mode . 1)) ; show the chapter or section name
```

## Key Binds

| key bind | effect |
|----------|--------|
| `C-c C-c` | typesets the current buffer file using `compile` command |
| `C-c C-f` | open the generated PDF corresponding to the current buffer file |

## Known Issues
The mode highlits ALL words in `satysfi-reserved-word-list` in the buffer, regardless of the context.
