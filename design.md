# Rendering of current status:
- Should use some kind of mailbox processor
- When it gets triggered, it checks the current status and runs jj to fill the various buffers with the latest graph and such
- Should have a cache that gets invalidated whenever jj says there was a change
  - The cache can store the fully processed data for the view buffers
    iiee


    Status_state 



  - We shouldn't use lwd.var to trigger this updating becasue that will intruduce at least one extra frame of delay:
```
key press   recompute vars and start rendering 
  |------------|---------------/
  render new state
/--|
```
As you can see there always needs to be at least one re-render just to start the rendering of the new command



     
