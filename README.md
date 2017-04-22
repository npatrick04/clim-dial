This is a simple dial gadget for clim, tested with McCLIM.

```lisp
(make-pane 'dial-pane
           :value 0
           :min-value -100
           :max-value 100
           :arc-start 0
           :arc-start pi
           :orientation :clockwise)
```
