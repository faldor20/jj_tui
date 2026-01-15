See the sections below for details about the different ways of
specifying
which revisions to rebase where.

If a working-copy revision gets abandoned, it will be given a new,
empty
revision. This is true in general; it is not specific to this
command.

### Specifying which revisions to rebase

With `--source/-s`, the command rebases the specified revision and
its
descendants to the destination. For example, `jj rebase -s M -o O`
would
transform your history like this (letters followed by an
apostrophe are
post-rebase versions):

```text
O           N'
|           |
| N         M'
| |         |
| M         O
| |    =>   |
| | L       | L
| |/        | |
| K         | K
|/          |/
J           J
```

Each revision passed to `-s` will become a direct child of the
destination,
so if you instead run `jj rebase -s M -s N -o O` (or `jj rebase -s
'M|N' -o
O`) in the example above, then N' would instead be a direct child
of O.

With `--branch/-b`, the command rebases the whole "branch"
containing the
specified revision. A "branch" is the set of revisions that
includes:

* the specified revision and ancestors that are not also ancestors
of the
  destination
* all descendants of those revisions

In other words, `jj rebase -b X -o Y` rebases revisions in the
revset
`(Y..X)::` (which is equivalent to `jj rebase -s 'roots(Y..X)' -o
Y` for a
single root). For example, either `jj rebase -b L -o O` or `jj
rebase -b M
-o O` would transform your history like this (because `L` and `M`
are on the
same "branch", relative to the destination):

```text
O           N'
|           |
| N         M'
| |         |
| M         | L'
| |    =>   |/
| | L       K'
| |/        |
| K         O
|/          |
J           J
```

With `--revisions/-r`, the command rebases only the specified
revisions to
the destination. Any "hole" left behind will be filled by rebasing
descendants onto the specified revisions' parent(s). For example,
`jj rebase -r K -o M` would transform your history like this:

```text
M          K'
|          |
| L        M
| |   =>   |
| K        | L'
|/         |/
J          J
```

Multiple revisions can be specified, and any dependencies (graph
edges)
within the set will be preserved. For example, `jj rebase -r 'K|N'
-o O`
would transform your history like this:

```text
O           N'
|           |
| N         K'
| |         |
| M         O
| |    =>   |
| | L       | M'
| |/        |/
| K         | L'
|/          |/
J           J
```

`jj rebase -s X` is similar to `jj rebase -r X::` and will behave
the same
if X is a single revision. However, if X is a set of multiple
revisions,
or if you passed multiple `-s` arguments, then `jj rebase -s` will
make each
of the specified revisions an immediate child of the destination,
while
`jj rebase -r` will preserve dependencies within the set.

Note that you can create a merge revision by repeating the `-o`
argument.
For example, if you realize that revision L actually depends on
revision M
in order to work (in addition to its current parent K), you can
run `jj
rebase -s L -o K -o M`:

```text
M          L'
|          |\
| L        M |
| |   =>   | |
| K        | K
|/         |/
J          J
```

### Specifying where to rebase the revisions

With `--onto/-o`, the command rebases the selected revisions onto
the
targets. Existing descendants of the targets will not be affected.
See
the section above for examples.

With `--insert-after/-A`, the selected revisions will be inserted
after the
targets. This is similar to `-o`, but if the targets have any
existing
descendants, then those will be rebased onto the rebased selected
revisions.

For example, `jj rebase -r K -A L` will rewrite history like this:
```text
N           N'
|           |
| M         | M'
|/          |/
L      =>   K'
|           |
| K         L
|/          |
J           J
```

The `-A` (and `-B`) argument can also be used for reordering
revisions. For
example, `jj rebase -r M -A J` will rewrite history like this:
```text
M          L'
|          |
L          K'
|     =>   |
K          M'
|          |
J          J
```

With `--insert-before/-B`, the selected revisions will be inserted
before
the targets. This is achieved by rebasing the selected revisions
onto the
target revisions' parents, and then rebasing the target revisions
and their
descendants onto the rebased revisions.

For example, `jj rebase -r K -B L` will rewrite history like this:
```text
N           N'
|           |
| M         | M'
|/          |/
L     =>    L'
|           |
| K         K'
|/          |
J           J
```

The `-A` and `-B` arguments can also be combined, which can be
useful around
merges. For example, you can use `jj rebase -r K -A J -B M` to
create a new
merge (but `jj rebase -r M -o L -o K` might be simpler in this
particular
case):
```text
M           M'
|           |\
L           L |
|     =>    | |
| K         | K'
|/          |/
J           J
```

To insert a commit inside an existing merge with `jj rebase -r O
-A K -B M`:
```text
O           N'
|           |\
N           | M'
|\          | |\
| M         | O'|
| |    =>   |/ /
| L         | L
| |         | |
K |         K |
|/          |/
J           J
```

