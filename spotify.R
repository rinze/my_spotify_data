library(tidyverse)
theme_set(theme_bw(14))
library(rjson)

dd <- do.call(rbind.data.frame, 
              fromJSON(file = "/tmp/001-rinzewind/StreamingHistory.json"))

dd$date <- as.Date(dd$endTime)

# Do the analysis only with songs played for more than 1 minute
dd <- dd[dd$msPlayed > 60000, ]

ddagg1 <- dd %>%
    group_by(date) %>%
    summarise(n_unique_tracks = length(unique(trackName)),
              n_tracks = length(trackName),
              n_unique_artists = length(unique(artistName))) %>%
    complete(date, fill = list(value1 = 0))

plt1 <- ggplot(gather(ddagg1, key, value, -date)) +
    geom_line(aes(x = date, y = value, color = key)) +
    scale_x_date(breaks = "1 week", minor_breaks = NULL) +
    scale_color_brewer(palette = "Set1", 
                       name = NULL, 
                       labels = c("All songs", "Unique artists", "Unique songs")) +
    ylab("# songs / artists") + xlab("Date") +
    ggtitle("Unique artists and songs listened to") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot(plt1)

#### "Special" days ####
ddagg1$ratio1 <- ddagg1$n_tracks / ddagg1$n_unique_artists
ddagg1 <- ddagg1[order(-ddagg1$ratio1), ]
ddagg1$idx1 <- 1:nrow(ddagg1)

plt2 <- ggplot(ddagg1) +
    geom_line(aes(x = idx1, y = ratio1))
plot(plt2)

head(ddagg1, 10)

# Repeat using all tracks / unique tracks
ddagg1$ratio2 <- ddagg1$n_tracks / ddagg1$n_unique_tracks
ddagg1 <- ddagg1[order(-ddagg1$ratio2), ]
ddagg1$idx2 <- 1:nrow(ddagg1)

plt2 <- ggplot(ddagg1) +
    geom_line(aes(x = idx2, y = ratio2))
plot(plt2)

# The first method is better
head(ddagg1, 10)

# What happened those days?
special_days <- sort(head(ddagg1, 5)$date)
print(special_days)

void <- lapply(special_days, function(x) {
    print(x)
    tmpdd <- dd[dd$date == x, ]
    tmpagg <- tmpdd %>%
        group_by(artistName, trackName) %>%
        summarise(n = n()) %>%
        arrange(desc(n)) %>%
        head(5)
    print(tmpagg)
})

# This looks better
dd %>% 
    group_by(artistName, trackName, date) %>% 
    summarise(n = n()) %>% 
    arrange(desc(n)) %>% 
    head(10)
