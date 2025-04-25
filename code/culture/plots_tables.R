library(data.table)
library(epitools)
#library(RColorBrewer)
library(wesanderson)

# setwd("U:/Born In Bradford/Bradford Centre Health Data Science/AoW data chapters/Themes/Culture")
setwd("~/bchds/data_notes/culture")

# .....................
# read and process data
# =====================

# function to process data and add CIs

process <- function (dat, trim = 'Attend: ', drop_event = 'Attend: One selected', drop_group = 'Total') {
  dat <- rbind(dat, 
               dat[Group != 'Total', .(n = sum(n), x = sum(x)), c('Group', 'Event')], 
                  fill = T)
  dat$Level[is.na(dat$Level)] <- 'Total'
  dat <- dat[!(Event %in% drop_event) & !(Group %in% drop_group)]
  dat <- cbind(dat, epitools::binom.exact(x = dat$x, n = dat$n)[, c('proportion', 'lower', 'upper')])
  dat$Event <- gsub(trim, "", dat$Event)
  dat
}

# read and process data

events <- fread("https://raw.githubusercontent.com/danlewer/aow-chapters/refs/heads/main/data/culture/attend_table2.csv", drop = c('perc', 'ci'))
events <- process(events)

activities <- fread("https://raw.githubusercontent.com/danlewer/aow-chapters/refs/heads/main/data/culture/participate_table3.csv", drop = c('perc', 'ci'))
activities <- process(activities, trim = 'Participate: ', drop_event = 'Participate: One selected')

pride <- fread("https://raw.githubusercontent.com/danlewer/aow-chapters/refs/heads/main/data/culture/pride_table4.csv", drop = c('perc', 'ci'))
pride <- process(pride)

# .............
# plot function
# =============

pf <- function(dat, mar = 0.15, cols = brewer.pal(4, 'Spectral'), ymax = 1, lablookup = 'original', show_xax = F) {
  groups <- unique(dat$Group)
  for(i in groups) {
    a <- dat[Group == i]
    levs <- unique(a$Level[a$Level != 'Total'])
    levs <- c('Total', levs)
    a$Level <- factor(a$Level, levs)
    ord <- unique(a[Level == 'Total'])[order(proportion, decreasing = T)]$Event
    a$Event <- factor(a$Event, ord)
    a <- a[order(Event, Level)]
    labels <- unique(a$Event)
    x0 <- seq_along(labels)
    nc <- max(x0) # number of outcomes
    nl <- length(unique(a$Level)) # number of levels
    xl <- mar * (seq_len(nl)-1)
    xl <- c(outer(xl, x0, FUN = '+'))  # x0 positions
    xr <- xl + mar # x1 positions
    yt <- seq(ymax * 0.6, ymax * 0.9, length.out = nl + 1)
    plot(1, type = 'n', xlim = c(1, nc + 1), ylim = c(0, ymax), axes = F, xlab = NA, ylab = NA)
    rect(min(xl) - mar, 0, max(x0) + 1, ymax, col = 'grey97')
    rect(xl, 0, xr, a$proportion, col = rep(cols[1:nl], nc))
    arrows(xl + mar/2, a$lower, y1 = a$upper, code = 3, length = 0.03, angle = 90)
    rect(nc * 0.9, yt[-length(yt)], nc * 0.93, yt[-1], col = cols[nl:1])
    text(nc * 0.94, yt[-length(yt)] + diff(yt)/2, rev(unique(a$Level)), adj = 0)
    axis(2, 0:(ymax*10)/10, paste0(0:(ymax*10) * 10, '%'), pos = min(xl) - mar, las = 2)
    axis(1, c(min(xl) - mar, x0, x0+1), pos = 0, labels = F)
    text(1, ymax * 0.92, i, adj = 0, cex = 1.3)
    xlabs <- if (lablookup[1] == 'original') labels else labs$new[match(labels, labs$old)]
    if (show_xax == T | (show_xax == F & i == tail(groups, 1))) text(x0 + mar/2, -ymax*0.025, xlabs, srt = 80, adj = c(1, 1))
  }
  title(ylab = 'Past-month prevalence', outer = T, line = 3, cex = 1.2)
}

# ..........
# make plots
# ==========

# events

labs <- list(old = c("Religious service", "Party/Nightclub", "Library", "Museum/Gallery", 
                     "Sport", "Youth club", "Performance", "Video game event", "Live music", 
                     "Scouts/Guides", "Political gathering"),
             new = c('Religious service\nor event',
                     'Party, dance, house\nparty or nightclub',
                     'Library',
                     'Museums or galleries,\na historical place,\nor stately home',
                     'Live sport\n(for example at\na stadium)',
                     'Youth clubs',
                     'A performance, such as\na play, pantomime,\nopera, dance, or circus',
                     'Video gaming event\nor festival',
                     'Live music\nconcert\nor gig',
                     'Scounts, Guides,\nExplorer scounts,\nor Rangers',
                     'Political meeting,\nmarch, rally\nor demonstration'))


png('event.png', height = 8, width = 7, units = 'in', res = 300)
par(xpd = NA, mar = c(1, 1, 0, 0), mfrow = c(3, 1), oma = c(10, 3, 0, 2))
pf(events, show_xax = F, lablookup = labs)
dev.off()

# activities

labs <- list(old = c("Digital content", "Art", "Reading", "Charity work", "Sing/music", 
                     "Writing", "Performance", "Poetry/rap"),
             new = c('Make your own graphic\ndesigns, photographs,\nor films/videos',
                     'Make your own drawing,\npainting, sculpture,\nor other art',
                     'Read for\nenjoyment',
                     'Voluntary, charity\nor community work',
                     'Sing, play an\ninstrument, or\nmake music',
                     'Creative writing\n(outside of school or work)',
                     'Take part in theatre,\ndance, circus,\nor other performance',
                     'Participate in a\npoetry/spoken word\nsession'))

png('activities.png', height = 8, width = 7, units = 'in', res = 300)
par(xpd = NA, mar = c(1, 1, 0, 0), mfrow = c(3, 1), oma = c(10, 3, 0, 2))
pf(activities, show_xax = F, lablookup = labs)
dev.off()

# pride

labs <- list(old = c("Pride: Local", "Pride: Culture", "Interest: Local culture"),
             new = c('I am proud to live\nin my local area',
                     'Arts and culture\nin my local area\nmake me feel proud',
                     'Arts and culture\nin my local area\nare of interest to me'))

png('pride.png', height = 8, width = 5, units = 'in', res = 300)
par(xpd = NA, mar = c(1, 1, 0, 0), mfrow = c(3, 1), oma = c(10, 5, 0, 2))
pf(pride, show_xax = F, lablookup = labs)
dev.off()

# ......
# tables
# ======


ftable <- function(file_name = "attend_table2.csv", ..., cell = 'full') {
  a <- fread(file_name, drop = c('perc', 'ci'))
  a <- process(a, drop_event = NA, drop_group = NA, ...)
  ord <- unique(a[Level == 'Total' & Group == 'Total'])[order(proportion, decreasing = T)]$Event
  a[, Event := factor(Event, ord)]
  a$Group <- factor(a$Group, c('Total', 'Year Group', 'Sex', 'Ethnicity'))
  a$Level <- factor(a$Level, c('Total', '8', '9', '10', 'Female', 'Male', 'Pakistani', 'White British', 'Other'))
  a <- a[!(Level == 'Total' & Group != 'Total')]
  f1 <- function (x, dig = 1) format(round(x * 100, dig), nsmall = dig, digits = dig)
  a[, c('proportion', 'lower', 'upper')] <- lapply(a[, c('proportion', 'lower', 'upper')], f1)
  a[, pc := paste0(proportion, ' (', lower, '-', upper, ')')]
  a[, pc := gsub(' ', '', pc)]
  a[, pc := gsub('\\(', ' (', pc)]
  a[, pc := paste0(x, '/', n, ': ', pc)]
  a[, pc2 := f1(x/n)]
  dcast(a, Group + Level ~ Event, value.var = if (cell == 'full') 'pc' else 'pc2')
}

write.csv(ftable('https://raw.githubusercontent.com/danlewer/aow-chapters/refs/heads/main/data/culture/attend_table2.csv', trim = 'Attend: ', cell = 'short'), 'table2.csv')
write.csv(ftable('https://raw.githubusercontent.com/danlewer/aow-chapters/refs/heads/main/data/culture/attend_table2.csv', trim = 'Attend: '), 'table2a.csv')

write.csv(ftable('https://raw.githubusercontent.com/danlewer/aow-chapters/refs/heads/main/data/culture/participate_table3.csv', trim = 'Participate: ', cell = 'short'), 'table3.csv')
write.csv(ftable('https://raw.githubusercontent.com/danlewer/aow-chapters/refs/heads/main/data/culture/participate_table3.csv', trim = 'Participate: '), 'table3a.csv')

write.csv(ftable('https://raw.githubusercontent.com/danlewer/aow-chapters/refs/heads/main/data/culture/pride_table4.csv', cell = 'short'), 'table4.csv')
write.csv(ftable('https://raw.githubusercontent.com/danlewer/aow-chapters/refs/heads/main/data/culture/pride_table4.csv'), 'table4a.csv')

