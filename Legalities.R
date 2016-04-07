
## Legalities
## Having separated out the columns of legalities, fill the data frame
## columns (aka legcats) with the apropriate legalities. I.e. if
## Ancestral recall is banned in commander, legal in freeform, banned
## in legacy, etc, then the column Commander on that card's row should
## read 'Banned'


legalities <- getall("legalities")
head(legalities, n = 200)

library(plyr)
unlistedlegs <- rbind.fill(legalities)
legcats <- unique(unlistedlegs[,1])

library(tidyr)
legcols <- lapply(legalities, function(z) if(!is.null(z))(spread(z, format, legality)))

## In order to keep the rows the same, I need data frames in all list
## entries, no Nulls. Here, I add some data frames to null list
## entries.
nullcols <- lapply(legcols, is.null)
legcols[unlist(nullcols)] <- lapply(legcols[unlist(nullcols)], function(x) x <- data.frame("junker" = "Nope!"))

## So rbind.fill can work as expected.
legalcols <- rbind.fill(legcols)

## This step though is a little troubling. I'm pretty sure the row
## entries are still in order, but not 100% certain. Data appears to
## check out though
row.names(legalcols) <- names(legcols)

## Then I get rid of the junker column
legalcols <- legalcols[,!colnames(legalcols) %in% c("junker")]

df <- cbind(df, legalcols)
