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
