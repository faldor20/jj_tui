## Key remapping:

How to hande key remapping:
- Inside nottui:
  - I coud allow users to specify some kind of key mapping function that looks for incoming keys and maps them to other keys.
- Inside my own program.
  - I could make all components take an `keyList` record and let the caller override that.
    - This option is more versitile because it allows one list to use say, arrows, and the other to use something like hjkl

## Handling unusual key combos:
- Aparrently general ansi terminals don't properly distinguish many key combos. But kitty and newer protocols do.
- Maybe I could use this protocol within notty.


## Multithreaded/async support in picos.
Currently if you set the timeout of nottui to 0 it will wait forever for input before it updates the ui.
we need a way to interrupt the ui waiting if the internal state has changed.
Basically the internal nottui loop should be:
Wait on input/a lwd.var change.
If there is a change, re-run
- This allows for way less idle usage
- This allows us to support selection
  - Currently we cannot select becasue the terminal is constatnly re-rendering, I could fix that now probably, but the picos change would also fix it
