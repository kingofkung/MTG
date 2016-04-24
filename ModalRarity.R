## Get the modal rarity, that is, the most common rarity for each card and turn them into a factor variable.

## str(FullData)
names(FullData)
## str(FullData$LEA$cards)
names(FullData$LEA$cards)


## nom <- "LEA"

DatNames <- names(FullData)

rarities <- lapply(DatNames, function(namer){

    ret <- cbind("cardnames" = FullData[[namer]]$cards$name,
                 "rarities" = FullData[[namer]]$cards$rarity,
                 "set" = namer)
    ret}
                   )

## Why is this so fast, but below requires something obscenely fast like bind_rows?
rars <- as.data.frame(do.call(rbind, rarities))
## Order  cards by  card names.  It's not  strictly necessary,  but it
## seemed like  a good idea.
rars  <- rars[order(rars$cardnames),]
## Initially I was concerned that it might change the date order, but
## when I call it below, it does not.


rarsagg <- aggregate(rarities ~ cardnames, data = rars, function(x)
    names(table(x)[order(-table(x))][1])
                     )

df[,"modalrarity"] <- rarsagg$rarities[ match( df[,"name"], rarsagg$cardnames)]

## Just thought to myself. In addition to modal rarity, maybe we
## should get the latest rarity?


## can we check if printings are listed in order? if they are, maybe
## we can find that out somehow...
rars$date <- SetInfo$releaseDate[match(rars$set, SetInfo$code)]

## split the rars into a list by card names
splrars <- split(rars[, ], rars$cardnames)

## Are all dates in the same order as the sequence along that order?
checkorder <- lapply(unique(rars$cardnames), function(y)
    all(order(splrars[[y]]$date) == seq_along(rownames(splrars[[y]])))
                     )
## No? Then which ones aren't
which(checkorder == FALSE)
## Just this one
splrars[[6347]] <- splrars[[6347]][order(splrars[[6347]]$date),]

## For each card in rars, get the last card rarity
lrar <- lapply(splrars, function(x) tail(x, n = 1))

lrar <- as.data.frame(dplyr::bind_rows(lrar))

head(lrar)
df[,"lastrarity"] <- lrar$rarities[match( df[,"name"], lrar$cardnames)]


## Get number of different rarities in each category
nrarcats <- unlist(lapply(1:length(splrars), function(u) length(unique(splrars[[u]]$rarities))))
## splrars[which(nrarcats == max(nrarcats))]
splrars$`Iron Myr`

lrar <- cbind(lrar, nrarcats)
df[,"nrarcats"] <- lrar$nrarcats[match( df[,"name"], lrar$cardnames)]

 ## write.csv(rars, paste0(writeloc, "rars.csv"), row.names = F)
