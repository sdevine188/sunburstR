library(stringr)
library(dplyr)
library(rvest)
library(sunburstR)
library(pitchRx)
library(tidyr)

setwd("C:/Users/Stephen/Desktop/R/sunburstR")

# http://timelyportfolio.github.io/sunburstR/example_baseball.html
# http://www.buildingwidgets.com/blog/2015/7/2/week-26-sunburstr

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
        summarize(event = paste(c(event), collapse="-")) %>% mutate(event = str_c(event, "-end"))
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
        sunburst(count = TRUE)
sb

# save plot - this doesn't work
pdf("baseball_sunburst.pdf", height = 5, width = 5)
sb
dev.off()




######################################

df <- data.frame(event = c(rep("approved", 100), rep("denied", 50), rep("pending", 25),
                           rep("approved-approved", 200), rep("approved-denied", 150), rep("approved-pending", 100),
                           rep("approved-approved-approved", 300), rep("approved-approved-denied", 200),
                           rep("approved-approved-pending", 150), rep("approved-approved-approved-approved", 400),
                           rep("approved-approved-approved-denied", 300), rep("approved-approved-approved-pending", 200),
                           rep("approved-denied-approved-pening", 500), rep("denied-approved-approved-pending-denied", 600),
                           rep("pending-denied-approved-approved-denied-approved-denied", 800)))
df
glimpse(df)


colors <- viridis(4)
colors <- colors %>% map(.x, .f = ~ str_sub(.x, start = 1, end = 7)) %>% unlist()
str(colors)
sund2b(
        df,
        colors = list(range = colors)
)

