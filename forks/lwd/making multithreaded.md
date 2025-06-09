Plans to make it multithreaded


My initial approach was to schedule the chagnes to the lwd varaiables untill a root has finished evaluating. However, this doesn't work if you have two roots that can be evaulated in paralell.

I could:
1. Pervent roots from being evaluated in paralell
2. Could i just prevent any nodes being evaluated in paralell. 
Imagine two roots share a node and are being evaluated in paralell
```mermaid
A1[root]--> B1[shared]
A1-->B2
A2-->B1
A2-->B3
B1--> C1[lwd var]
```
If, while setting the lwd.var we can either lock the node, or lock the root that is holding the node we can safely update $

if we just locked the node, currently the root could be evaluating a parent  node while we lock it... but does that matter?
no.. not really.... if the root holds all locked nodes locked untill the evaluation is finished we are actually safe to set a varaible and it'll still ensure consitency


So, by that logic:
A root should lock and hold locked all nodes it touches while evaluating, this ensures that it will experience consitency.
Infact, we don't need to hold all nodes locked, we could just lock all leaf nodes that still have a path to the parent node, 
but we could unlock any node that has been fully calculated that isn't a leaf, sometimes a leaf could connect to two different mid nodes so we can't neciserily unlock them

if we can lock a node while setting an lwd var we are safe to do set it, otherwise we have to wait until it's finished evaluating.
in a concurrent enviroment this is okay, because we can just wait for the unlock in the background. 
In a non concurrent environment, I think we could locate the root node that locked it, and then add the setting operation to a "post eval" list.
We would have to decide if it should set and then eval while holding the lock and further delay another root evaluation, or let the other root evaluate, or set it, let the root evaluate and then revaluate ourself.


If two graphs are evaluating at the same time and one encounters a locked node from another, the second should move on if possible and then just wait


Could i just used the cached value if a node is locked. no, because it could be recomputing

## How to handle invalidation during re-evaluation:
I guess I could actually just cancel and reschedule rendering, but that could block rendering forever if rendering takes a long time. and a new event comes in

Or I could walk upwards towards each root, locking as i go. however, if there is an evaluation occuring, they will not be able to keep evaluating.

I could introduce a rule that says, if a node is locked because of invalidation, the node should use the cached value.
We can be very sure the node will eventually get recomputed, but the invalidation would have to be in it's own process and constantly trying to work its way up the node graph.


I don't like the idea of

In a concurrent environment i could just drop the recomputation into it's own process and move on, in a single threaded environment the simplest way would be to schedule the update and locking on the root node that currently holds the lock.




- what if we split up invalidation into two parts? currently invalid and next invalid we just set next invalid on all the items without doing any locking. Then when the currently invalidating root node is done, it sets currently invalid to true as it unlocks the node.



## So full explanation:

- to recompute: a root node should walk down the tree of invalidated nodes locking as it goes, then unlock them once done.
- to update a variable a leaf node should lock itself, apply an update and then walk up the tree, marking locking each node and marking it invalid
- if a root node that is currently traversing down the nodes





## invalidate next:
- evaluation starts at the root node, locking as it goes.
- when updating a variable walk up the tree of nodes. When we encounter a locked node (one that is currently being evaluated) we mark it as invalidate-next.
- if we encounter a node that is currently marked invalidated we can just stop
- if we encounter an unlocked node that is marked computed, we mark it invalid
- when a node is finished evaluating it checks to see if it is marked (invalidate next) if it is, we re check the children to find the invalid children and evalute it again.
- this process may need to have some smart locking of this invalidate next status so that the checking and updating of the status doesn't encounter a scenario where we check the node status and then mark it invalidate next and then concurrently the node is marked evaluted by another thread