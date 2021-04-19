---
title: "Workflow to merge works/tags info for a fandom"
output: 
  html_document: 
    keep_md: yes
---



For most questions that interest me, I need to merge information from the tags and works databases. Here's a workflow for doing that, after a few hours and a short stint in regex purgatory. I'm using the example of pulling all info for a particular fandom, but it could just as easily be all Teen-rated works, everything associated with video games, or whatever.


## Import data

Before this file, I ran:

1. `data_import_save.R`, which does minimal cleaning on the original CSV files and then exports them into `.Rda` format. This includes reordering works to go from oldest to newest and adding an ID number column (`wid`).

2. `merge_works.R`, which replaces any duplicate tag with its "real" (`merger_id`) counterpart.


```r
(load("data/works_merged.Rda"))
```

```
## [1] "works_merged"
```

```r
(load("data/tags.Rda"))
```

```
## [1] "tags"
```

```r
works <- works_merged
rm(works_merged)
```

Peek at the top of each:


```r
works
```

```
## # A tibble: 7,269,693 x 7
## # Groups:   wid [7,269,693]
##    creat_date   wid language restricted complete word_count tags                
##    <date>     <int> <chr>    <lgl>      <lgl>         <dbl> <chr>               
##  1 2008-09-13     1 en       FALSE      TRUE           1836 123+124+12445405+12~
##  2 2008-09-13     2 en       FALSE      TRUE           1338 112+113+13+114+16+7~
##  3 2008-09-13     3 en       FALSE      TRUE           1755 77+78+69+108+109+62~
##  4 2008-09-13     4 en       FALSE      TRUE           1392 78+77+103+66650+23+~
##  5 2008-09-13     5 en       TRUE       TRUE            705 78+77+103+579599+11~
##  6 2008-09-13     6 en       TRUE       TRUE           1583 78+77+103+579599+23~
##  7 2008-09-13     7 en       TRUE       TRUE          30830 13+1589+23+99+16+12~
##  8 2008-09-13     8 en       FALSE      TRUE           2482 87+88+23+11+6856+14~
##  9 2008-09-13     9 en       FALSE      TRUE          52843 103+23+13+16+70+103~
## 10 2008-09-13    10 en       FALSE      TRUE            733 79+5120+23143+10+16~
## # ... with 7,269,683 more rows
```

```r
tags
```

```
## # A tibble: 14,467,138 x 6
##       id type   name                            canonical cached_count merger_id
##    <dbl> <chr>  <chr>                           <lgl>            <dbl>     <dbl>
##  1     1 Media  TV Shows                        TRUE               910        NA
##  2     2 Media  Movies                          TRUE              1164        NA
##  3     3 Media  Books & Literature              TRUE               134        NA
##  4     4 Media  Cartoons & Comics & Graphic No~ TRUE               166        NA
##  5     5 Media  Anime & Manga                   TRUE               501        NA
##  6     6 Media  Music & Bands                   TRUE                19        NA
##  7     7 Media  Celebrities & Real People       TRUE                33        NA
##  8     8 Media  Other Media                     TRUE                11        NA
##  9     9 Rating Not Rated                       TRUE            825385        NA
## 10    10 Rating General Audiences               TRUE           2115153        NA
## # ... with 14,467,128 more rows
```


## Helper functions

Two tasks I'll need: Find the tag ID number associated with a given name in the `tags` frame, and find all rows containing a given tag ID in the `works` frame. 


```r
# Regex to match: Start of line OR non-digit, tag, non-digit OR end of line
makePat <- function(x) {
  pat <- paste0("(^|[^[:digit:]])", x, "([^[:digit:]]|$)") 
}

# Find tag number for a given name
getTagID <- function(x, df = tags) {
  df %>% filter(name == x) %>% pull(var = id)
}

getTagID("RWBY")
```

```
## [1] 767851
```


## Pick a fandom and filter `works` for it

At the end of this process, I'd like three outputs: 

1. The fandom-specific subset of the `works` frame, otherwise in its original format. 

2. A long data frame version of 1, with one tag per row. 

3. The tag list filtered down to contain only tags appearing in 1 and 2.


### Filter the `works` frame

Start by pulling the row(s) for a given tag.


```r
tagpat <- makePat(getTagID("RWBY"))

wred <- works %>% 
  filter(grepl(pattern = tagpat, x = tags))

wred
```

```
## # A tibble: 25,691 x 7
## # Groups:   wid [25,691]
##    creat_date    wid language restricted complete word_count tags               
##    <date>      <int> <chr>    <lgl>      <lgl>         <dbl> <chr>              
##  1 2013-02-23 553012 en       FALSE      TRUE            706 10+116+16+767851+1~
##  2 2013-03-03 560262 en       FALSE      TRUE           8482 12+116+16+767851+9~
##  3 2013-08-17 724110 en       FALSE      TRUE           2853 10+21+14+767851+10~
##  4 2013-08-17 724234 en       FALSE      TRUE           1287 10+21+22+14+767851~
##  5 2013-08-24 730619 en       FALSE      TRUE           2677 11+116+14+767851+1~
##  6 2013-08-26 732987 en       FALSE      TRUE            356 10+21+14+767851+64~
##  7 2013-08-29 735405 en       FALSE      TRUE            235 10+21+14+767851+10~
##  8 2013-09-01 737805 en       FALSE      FALSE          2901 10+116+16+767851+9~
##  9 2013-09-02 739630 en       FALSE      TRUE            838 116+14+767851+1072~
## 10 2013-09-09 745903 en       FALSE      TRUE           2133 13+116+20+767851+1~
## # ... with 25,681 more rows
```

We're now down to about 26K works, a respectable but not overwhelming fraction of the data set. 


### Make a long `works` data frame

Next, unroll the tag column into a long data frame, by way of a list-column.


```r
wlong_fandom <- wred %>% 
  mutate(tag_list = stringr::str_split(.data$tags, 
                                       stringr::fixed("+"))) %>% 
  unnest(tag_list) %>% 
  mutate(tag_list = as.integer(tag_list)) %>% 
  select(-tags)

wlong_fandom
```

```
## # A tibble: 470,820 x 7
## # Groups:   wid [25,691]
##    creat_date    wid language restricted complete word_count tag_list
##    <date>      <int> <chr>    <lgl>      <lgl>         <dbl>    <int>
##  1 2013-02-23 553012 en       FALSE      TRUE            706       10
##  2 2013-02-23 553012 en       FALSE      TRUE            706      116
##  3 2013-02-23 553012 en       FALSE      TRUE            706       16
##  4 2013-02-23 553012 en       FALSE      TRUE            706   767851
##  5 2013-02-23 553012 en       FALSE      TRUE            706   150769
##  6 2013-02-23 553012 en       FALSE      TRUE            706    30924
##  7 2013-02-23 553012 en       FALSE      TRUE            706   994409
##  8 2013-02-23 553012 en       FALSE      TRUE            706  6418184
##  9 2013-02-23 553012 en       FALSE      TRUE            706   813700
## 10 2013-03-03 560262 en       FALSE      TRUE           8482       12
## # ... with 470,810 more rows
```

In the long data frame, I've dropped the original `tags` column, which holds the character string of tag IDs separated by + symbols. It's still there in the `wred` frame, but it's not useful and hurts readability to repeat it every row of `wlong`. 

Finally, for ease of reading I want to attach tag names to my long works frame. Might as well add tag type while I'm in the neighborhood. 


```r
wtagged_fandom <- wlong_fandom %>% 
  left_join(tags %>% select(id, type, name), 
            by = c("tag_list" = "id"))

wtagged_fandom
```

```
## # A tibble: 470,820 x 9
## # Groups:   wid [25,691]
##    creat_date    wid language restricted complete word_count tag_list type      
##    <date>      <int> <chr>    <lgl>      <lgl>         <dbl>    <dbl> <chr>     
##  1 2013-02-23 553012 en       FALSE      TRUE            706       10 Rating    
##  2 2013-02-23 553012 en       FALSE      TRUE            706      116 Category  
##  3 2013-02-23 553012 en       FALSE      TRUE            706       16 ArchiveWa~
##  4 2013-02-23 553012 en       FALSE      TRUE            706   767851 Fandom    
##  5 2013-02-23 553012 en       FALSE      TRUE            706   150769 Freeform  
##  6 2013-02-23 553012 en       FALSE      TRUE            706    30924 Freeform  
##  7 2013-02-23 553012 en       FALSE      TRUE            706   994409 Relations~
##  8 2013-02-23 553012 en       FALSE      TRUE            706  6418184 Character 
##  9 2013-02-23 553012 en       FALSE      TRUE            706   813700 Character 
## 10 2013-03-03 560262 en       FALSE      TRUE           8482       12 Rating    
## # ... with 470,810 more rows, and 1 more variable: name <chr>
```

Check: Are any of these tags missing a match in the `tags` frame?


```r
wtagged_fandom %>% 
  mutate(mismatch = is.na(name)) %>% 
  group_by(mismatch) %>% 
  count() %>% 
  mutate(frac = n / nrow(wtagged_fandom))
```

```
## # A tibble: 2 x 3
## # Groups:   mismatch [2]
##   mismatch      n      frac
##   <lgl>     <int>     <dbl>
## 1 FALSE    470811 1.00     
## 2 TRUE          9 0.0000191
```

```r
# Inspect any rows without a tag match
wtagged_fandom %>% 
  mutate(mismatch = is.na(name)) %>% 
  filter(mismatch) 
```

```
## # A tibble: 9 x 10
## # Groups:   wid [9]
##   creat_date    wid language restricted complete word_count tag_list type  name 
##   <date>      <int> <chr>    <lgl>      <lgl>         <dbl>    <dbl> <chr> <chr>
## 1 2020-02-14 5.49e6 en       FALSE      FALSE         11784 54605328 <NA>  <NA> 
## 2 2020-09-29 6.49e6 en       FALSE      TRUE           6272 47783134 <NA>  <NA> 
## 3 2020-12-01 6.79e6 en       FALSE      TRUE           5149 51007755 <NA>  <NA> 
## 4 2020-12-14 6.85e6 en       FALSE      TRUE           3238 50977323 <NA>  <NA> 
## 5 2020-12-17 6.86e6 en       FALSE      TRUE            791 51128301 <NA>  <NA> 
## 6 2021-01-17 7.04e6 en       FALSE      TRUE            817 53207220 <NA>  <NA> 
## 7 2021-02-22 7.25e6 en       FALSE      TRUE           2941 54637425 <NA>  <NA> 
## 8 2021-02-23 7.25e6 en       FALSE      FALSE          4113 54726570 <NA>  <NA> 
## 9 2021-02-24 7.26e6 en       TRUE       FALSE          5680 54810069 <NA>  <NA> 
## # ... with 1 more variable: mismatch <lgl>
```

Nine of my rows came up with `NA` for a tag name. Looking at the first of them, 54605328:


```r
tags %>% filter(id > 54605320)
```

```
## # A tibble: 197,225 x 6
##          id type      name                      canonical cached_count merger_id
##       <dbl> <chr>     <chr>                     <lgl>            <dbl>     <dbl>
##  1 54605322 Freeform  Redacted                  FALSE                1        NA
##  2 54605325 Freeform  Redacted                  FALSE                1        NA
##  3 54605331 Freeform  Redacted                  FALSE                1        NA
##  4 54605334 Unsorted~ Redacted                  FALSE                1        NA
##  5 54605337 Relation~ Karl Edward Bayerlein/Os~ TRUE                 0        NA
##  6 54605352 Freeform  Redacted                  FALSE                1        NA
##  7 54605355 Freeform  Redacted                  FALSE                1        NA
##  8 54605358 Freeform  Redacted                  FALSE                1        NA
##  9 54605361 Freeform  Redacted                  FALSE                1        NA
## 10 54605364 Unsorted~ Redacted                  FALSE                1        NA
## # ... with 197,215 more rows
```

There are a number of skipped `id` values, and this `NA` from my matched RWBY tags is one of them. To be really careful I'd check them all, but for the moment I'll assume those represent deleted tag entries. 


## Filter the `tags` frame to in-use entries

There are 14.5 million tags and many of them don't apply to this subset of works, so I also want to cut down the `tags` frame to its corresponding subset. 

After everything above, this goes pretty fast. Pull in-use tags from filtered works frame:


```r
tags_used <- wlong_fandom %>% 
  ungroup() %>%   # had a wid group, might want to check back for problems there
  select(tag_list) %>% 
  distinct() %>% 
  arrange(tag_list)

tags_used
```

```
## # A tibble: 61,257 x 1
##    tag_list
##       <int>
##  1        9
##  2       10
##  3       11
##  4       12
##  5       13
##  6       14
##  7       16
##  8       17
##  9       18
## 10       19
## # ... with 61,247 more rows
```

Then join tags frame info to each in-use tag:


```r
tred <- tags_used %>% 
  left_join(tags, by = c("tag_list" = "id")) %>% 
  rename(id = tag_list)

tred
```

```
## # A tibble: 61,257 x 6
##       id type         name                      canonical cached_count merger_id
##    <dbl> <chr>        <chr>                     <lgl>            <dbl>     <dbl>
##  1     9 Rating       Not Rated                 TRUE            825385        NA
##  2    10 Rating       General Audiences         TRUE           2115153        NA
##  3    11 Rating       Teen And Up Audiences     TRUE           2272688        NA
##  4    12 Rating       Mature                    TRUE           1151260        NA
##  5    13 Rating       Explicit                  TRUE           1238331        NA
##  6    14 ArchiveWarn~ Choose Not To Use Archiv~ TRUE           2556570        NA
##  7    16 ArchiveWarn~ No Archive Warnings Apply TRUE           4095298        NA
##  8    17 ArchiveWarn~ Graphic Depictions Of Vi~ TRUE            519931        NA
##  9    18 ArchiveWarn~ Major Character Death     TRUE            379648        NA
## 10    19 ArchiveWarn~ Rape/Non-Con              TRUE            192479        NA
## # ... with 61,247 more rows
```

That's about 61K tags. When I did this the first time, without merging duplicate tags in the works list first, my filtered list of in-use tags was 74,026 entries long. So I've cut off more than 15% of my tags by cleaning up the list first. 

And we're done, unless I find a bug later.


## Save data

Even though the data frames are a much more manageable size now, I'll keep saving the tags and works info in separate files. For works, I'll keep both the subsetted-but-wide-format `wred`, and the one-row-per-tag `wtagged`. 


```r
#save(tred, file = "data/tags_RWBY.Rda")
#save(wred, wtagged_fandom, file = "data/works_RWBY.Rda")
```

