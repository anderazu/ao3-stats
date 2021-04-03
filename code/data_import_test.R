# Short script to check how long it takes to import each data set, save as an 
# .Rda file, and whether that makes a difference to import time. 

# Verdict: .Rda import is faster by about 30% for tags and about 45% for works.
# Hard drive space is also much smaller for the .Rda files, so that's the way to
# go.

library(tidyverse)

# Import tags 
ptm <- proc.time()
tags <- read_csv("data/tags-20210226.csv")
timet1 <- proc.time() - ptm

# Save as .Rda file and delete old object
ptm <- proc.time()
save(tags, file = "data/tags_temp.Rda")
(proc.time() - ptm)   # about 27 seconds

rm(tags)

# Re-import from Rda
ptm <- proc.time()
tags <- load("data/tags_temp.Rda")
timet2 <- proc.time() - ptm


## Repeat for works

# Import original csv
ptm <- proc.time()
works <- read_csv("data/works-20210226.csv")
timew1 <- proc.time() - ptm

# Save as .Rda file and delete old object
ptm <- proc.time()
save(works, file = "data/works_temp.Rda")
(proc.time() - ptm)   # about 80 seconds

rm(works)

# Re-import from Rda
ptm <- proc.time()
works <- load("data/works_temp.Rda")
timew2 <- proc.time() - ptm

## How do the times compare?
cbind(timet1, timet2, timew1, timew2)[c(1:3), ]


# Clean up temp files
file.remove("data/tags_temp.Rda")
file.remove("data/works_temp.Rda")
