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


## Allow single threading support to also behave the same way
I can make this work single threaded as well.
Instead of using async await.
1. use a very small timeout in unix.select/ polling
2. on_invalidate set  the invalidate flag true
3. Re-render when it's been set to true


## locking issues
Currently we have an issue with lwd and locking. 
Basically we want to observe all invalidation that isn't processed already
- Nottui will reprocess the node before rendering if it's already invalid
- Lwd has oninvalidate but it runs event lwd.set that is being delayed because we are during recomputation.
I want to capture all invalidation between the last call to stabalize in nottui and the next event whether that be a invalidation event or a keypress
