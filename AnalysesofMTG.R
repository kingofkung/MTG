## Here's where I'll keep all of my analyses, graphical and statistical

## This looks pretty decent...
mtg <- read.csv("/Users/bjr/Dropbox/MTGData.csv")
mtg$numpow <- as.numeric(as.character(mtg$power))
with(mtg, table(power, numpow))

mtg$numtough <- as.numeric(as.character(mtg$toughness))
names(mtg)

hist(as.numeric(mtg$toughness))

mtg[grep("goblin", mtg$subtypes, ignore.case = TRUE), c("name", "type", "power", "toughness")]

t.test( x = mtg[grep("goblin", mtg$subtypes, ignore.case = T), "numpow"],
       y =  mtg[grep("zombie", mtg$subtypes, ignore.case = T), "numpow"])


t.test( x = mtg[grep("wall", mtg$subtypes, ignore.case = T), "numtough"],
       y =  mtg[grep("beast", mtg$subtypes, ignore.case = T), "numtough"])

t.test(x = mtg[grep("wizard", mtg$type, ignore.case = T), "cmc"],
       y = mtg[grep("beast", mtg$type, ignore.case = T), "cmc"])
