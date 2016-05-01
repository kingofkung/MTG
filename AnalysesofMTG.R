## Here's where I'll keep all of my analyses, graphical and statistical

## This keeps it so that if we have the data already, we don't have to
## read it off the hard drive. Goddammit! df is the command for the F
## distribution's density. We'll need to do something about that.
readloc <- "/Users/bjr/Dropbox/MTGDat/"
if("df" %in% ls()) mtg <- df else mtg <- read.csv(paste0(readloc, "MTGData.csv"))
## Kill unhinged and unglued from my data. This is supposed to be a serious analysis
mtg <- mtg[mtg$Unhinged ==0 & mtg$Unglued == 0 , ]

library(rockchalk)

## Create Numeric versions of power/toughness (for analysis purposes)
mtg$numpow <- as.numeric(as.character(mtg$power))
mtg$numtough <- as.numeric(as.character(mtg$toughness))
names(mtg)

hist(as.numeric(mtg$numtough))

as.character(unique(mtg$modalrarity))


t.test( x = mtg[grep("goblin", mtg$subtypes, ignore.case = T), "numpow"],
       y =  mtg[grep("zombie", mtg$subtypes, ignore.case = T), "numpow"])


t.test( x = mtg[grep("wall", mtg$subtypes, ignore.case = T), "numtough"],
       y =  mtg[grep("beast", mtg$subtypes, ignore.case = T), "numtough"])

t.test(x = mtg[grep("wizard", mtg$subtypes, ignore.case = T), "cmc"],
       y = mtg[grep("beast", mtg$subtypes, ignore.case = T), "cmc"])

t.test(x = mtg[mtg$Blue ==1 & mtg$types %in% "Creature", "cmc"],
       y = mtg[mtg$Red == 1 & mtg$types %in% "Creature", "cmc"]
       )

## Note to self: {U} is actually blue in manaCost
## Also, it's types and subtypes, not type or subtype


## To Do list

## Figure out which color is the strongest color.

## Note. Positivity means the value in 1 is larger. It was negative,
## but I multiplied the t statistics by -1 to make it clearer
ucols <- c("Red", "White", "Blue", "Black", "Green")
pows <- unlist(lapply(ucols, function(x){
    -1 * t.test(numpow ~ get(x), data = mtg)$statistic
}))
names(pows) <- ucols


toughs <- unlist( lapply(ucols, function(x){
   -1 * t.test(numtough ~ get(x), data = mtg)$statistic
}))
names(toughs) <- ucols

rbind("powerst" = pows, "toughnessest" = toughs)

## Looks
## like nocolor isn't artifacts, though, and we'll need to do some
## further manipulation to figure that out...
## artifact creatures, further, are not so easy to separate out
artifacts <- mtg[grep("artifact", mtg$type, ignore.case = T),]


## So that was easy. Simply by virtue of having green in the cost, the
## power/toughness is likely to be higher.
## Note that that doesn't tell us how much higher, only that it's higher.

colbd <- lapply(ucols, function(x){
    typetab <- sort(table(mtg[mtg[, x] == 1 ,"types"]), decreasing = TRUE)
    ## prop.table(typetab)
                })
names(colbd) <- ucols

## They're all creature-centric, but blue's your color for instants
## (aka the fast game, as I see it). Black and Red favor sorcery as
## their second type (Black's got a few more, percentage wise). Green
## and white have the most Enchantments as a percentage though (
paste0( names(sort(table(mtg$types), decreasing = T))[c(1:6, 9)],  collapse = "', '"      )

maincardtypes <- c('Creature', 'Instant', 'Enchantment', 'Sorcery', 'Artifact', 'Land', 'Planeswalker')

for(i in seq_along(maincardtypes[1:5])){
    print(maincardtypes[i])
    y <- lapply(ucols, function(x) table(mtg[ mtg$types %in% maincardtypes[i], c(x, "cmc")]))
    print(y)
    if(i==1) {
        listsav <- list(y)
        names(listsav) <- maincardtypes[i]} else listsav[[maincardtypes[i]]] <- y

}
summary(mtg$types)
mtg$typesrel <- relevel(mtg$types, "Creature")

cmcmod <- lm(cmc ~ Black + Red + Blue + Green + White + typesrel  , data = mtg[mtg$Unhinged ==0 & mtg$Unglued == 0 , ])
summary(cmcmod)

cmcmod2 <- update(cmcmod, .~. + Red * typesrel + Blue * typesrel + Green * typesrel + White * typesrel + Black * typesrel)
summary(cmcmod2)

redvblue <- lm(cmc ~ Red + Blue + Red * typesrel + Blue * typesrel, dat = mtg)
summary(redvblue)

source("/Users/bjr/Desktop/fancyt.R")
fancyt("White", "Green", cmcmod)

powmod <- lm(numpow ~ Green*White*Red*Blue*Black, dat = mtg)
powcoefs <- coef(summary(powmod))

toughmod <- update(powmod, .-numpow + numtough ~ .)
toughcoefs <- coef(summary(toughmod))

powcoefs[powcoefs[,4]<=.05,]

powtypes <- lm(numpow ~ subtypes, mtg)
powtypesum <- coef(summary(powtypes))
summary(powtypesum)
ptslim <- powtypesum[powtypesum[,4] <= .05, 1:2]
ptslim[order(ptslim[,1]),]

toughtypes <- lm(numtough ~ subtypes, mtg)
toughtypesum <- coef(summary(toughtypes))
summary(toughtypesum)
ttslim <- toughtypesum[toughtypesum[,4] <= .05, 1:2]
ttslim[order(ttslim[,1]),]

## Want to figure out if there's a way to make the keywords

keyform <- paste("cmc ~ ", paste(colnames(mtg)[250:415], collapse = " + "))
as.formula(keyform)
keywordlm <- lm(as.formula(keyform), mtg)

keycoefs <- coef(summary(keywordlm))
## The intersect command lets us get the values common to two sets of
## numbers. Ok, so check it out, I just cut out any coefficient whose
## absolute value is less than .5
ssinorder <- intersect(order(keycoefs[,1]), which(keycoefs[,4]<=.05))

ssiocutmid <- intersect(ssinorder, which(abs(keycoefs[,1])>.5))
keycoefs[ssiocutmid ,]

## library(glmnet)
## creatures <- mtg[mtg$types %in% "Creature",]
## creaturecols <- c(3:4, 15:20, 57, 250:415)
## creaturescc <- creatures[complete.cases(creatures[,creaturecols]), creaturecols]
##  model.matrix(as.formula(paste0("cmc ~ ", paste(colnames(creatures[,creaturecols]), collapse = " + "))), creatures)

## glmnet(as.matrix(creaturescc), creaturescc$cmc, family = "gaussian")
