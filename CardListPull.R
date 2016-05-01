## Here's where I'll keep all pulls for cards.

## This keeps it so that if we have the data already, we don't have to
## read it off the hard drive. Goddammit! df is the command for the F
## distribution's density. We'll need to do something about that.
readloc <- "/Users/bjr/Dropbox/MTGDat/"
if("df" %in% ls()) mtg <- df else mtg <- read.csv(paste0(readloc, "MTGData.csv"))
## Create Numeric versions of power/toughness (for analysis purposes)

## Kill unhinged and unglued from my data. This is supposed to be a serious analysis
mtg <- mtg[mtg$Unhinged ==0 & mtg$Unglued == 0 , ]

mtg$numpow <- as.numeric(as.character(mtg$power))
mtg$numtough <- as.numeric(as.character(mtg$toughness))
names(mtg)

hist(as.numeric(mtg$numtough))

as.character(unique(mtg$modalrarity))


## Quick note: if you want to use subtypes like this, it has to be capitalized
## Or you can use grepl to get the true false while ignoring case
raregoblins <- mtg[grepl("Goblin", mtg$subtypes, ignore.case = TRUE) & mtg$modalrarity == "Rare" ,]

poisons <- mtg[grepl("poison", mtg$text, ignore.case = TRUE), ]

destroylandinfo <- mtg[grepl("destroy\\s+\\w*\\s+land", mtg$text, ignore.case = TRUE)
                       |  grepl("destroy\\s+\\w*\\s+permanent", mtg$text, ignore.case = TRUE)
                       & !grepl("destroy\\s+nonland\\s+permanent", mtg$text, ignore.case = TRUE)
                              ,]
sort(table(destroylandinfo$colors))

writeloc <- "/Users/bjr/Dropbox/MTGDat/"
## write.csv(file = "/Users/bjr/Dropbox/raregoblins.csv", raregoblins)
## write.csv(file = '/Users/bjr/Dropbox/poisonrefs.csv', poisons, row.names = F)
write.csv(file = paste0(writeloc, "landdestroyer.csv"), destroylandinfo, row.names = F)


