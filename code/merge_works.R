# Unfold works into long data frame and do tag merger


library(tidyverse)


# Import data
(load("data/works.Rda"))
(load("data/tags.Rda"))


## Replace duplicate tags in works frame

# Separate out tag rows that need merging
temptags  <- tags %>% 
  filter(!is.na(merger_id))      # non-NA merger_id, need to merge

# Regex to match: Start of line OR +, tag, + OR end of line
makePat <- function(x) {
  pat <- paste0("(^|\\+)", x, "(\\+|$)") 
}

tagpat <- makePat(29)

temp1 <- works %>% 
  filter(grepl(pattern = tagpat, x = tags)) 

temp1 %>% 
  gsub(pattern = tagpat, 
       replacement = "\\1")  # in progress: can I replace just the number part?