# Rendering of current status:
- Should use some kind of mailbox processor
- When it gets triggered, it checks the current status and runs jj to fill the various buffers with the latest graph and such
- Should have a cache that gets invalidated whenever jj says there was a change
  - The cache can store the fully processed data for the view buffers
    iiee
