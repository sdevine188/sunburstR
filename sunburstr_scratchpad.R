library(stringr)
library(dplyr)
library(rvest)
library(sunburstR)
library(pitchRx)
library(tidyr)

setwd("C:/Users/Stephen/Desktop/R/sunburstR")

# http://timelyportfolio.github.io/sunburstR/example_baseball.html

# get all data from 2016-08-25
dat <- scrape(start = "2016-08-25", end = "2016-08-25")
glimpse(dat)
glimpse(dat$runner)

# use runner data to get idea of action with a runner on base
#  please note this will not be all action from a game
#  but I think it is an easier dataset to understand
action <- dat$runner %>%
        group_by(event_num) %>%
        filter(row_number() == 1) %>%
        ungroup() %>%
        group_by(gameday_link, inning, inning_side) %>%
        summarize(event = paste(c(event),collapse="-"))
action

sequences <- action %>%
        ungroup() %>%
        group_by(event) %>%
        summarize(count = n())
sequences

# sorry this is messy, but get data in a form
#  so sunburst can build hierarchy
#  which means we will sort in descending order of depth
# note: this will eventually improve
sequences$depth <- unlist(lapply(strsplit(sequences$event,"-"),length))
sequences

# create sunburst
sb <- sequences %>%
        arrange(desc(depth), event) %>%
        sunburst()
sb

# save plot - this doesn't work
pdf("baseball_sunburst.pdf", height = 5, width = 5)
sb
dev.off()



