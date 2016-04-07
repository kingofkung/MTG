## Get the modal rarity, that is, the most common rarity for each card and turn them into a factor variable.
loc <- "/Users/bjr/Desktop/R_Projects/MTG_Data"

## if(!exists("FullData")) source(paste0(loc,"/OpenMTGJSONSets.R"))

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
rars <- as.data.frame(do.call(rbind, rarities))
## Order cards by card names. It's not strictly necessary, but it seemed
## like a good idea.
rars <- rars[order(rars$cardnames),]


rarsagg <- aggregate(rarities ~ cardnames, data = rars, function(x)
    names(table(x)[order(-table(x))][1])
                     )

df[,"modalrarity"] <- rarsagg$rarities[ match( df[,"name"], rarsagg$cardnames)]
