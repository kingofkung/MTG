## Remove everything but the JSON data sets. This should make it run way faster.
rm(list = ls()[!ls() %in% c("AllCards", "SetInfo", "FullData")]
)
loc <- "/Users/bjr/GitHub/MTG"
writeloc <-  "/Users/bjr/Dropbox/MTGDat/"

## the object must be in quotes for exists to work. This is nice, as
## it keeps me able to avoid running the rather long OpenMTGJSONSets.R
## code unless the key data sets don't exist.
source(paste0(loc,"/OpenMTGJSONSets.R"))

## summary(AllCards)

## Note to self (U+2014) is a long dash


cats <- c("name", "layout", "manaCost", "cmc", "type", "types", "text", "loyalty", "power", "toughness", "mciNumber")

## The best way to combine these is by name, making a column out of
## the data and matching the data columns by name before adding them
## to the main df.
df <- as.data.frame( getall(var = "name", wantlist = FALSE))
colnames(df) <- "name"

for(i in 2:length(cats)){
    all <- getall(cats[i], wantlist = FALSE)
    allrows <- match(rownames(df), names(all))
    df[,cats[i]] <- NA
    df[,cats[i]] <- all[allrows]
}


multicats <- c("subtypes", "colors", "colorIdentity")


for(j in 1:length(multicats)){

    datin  <- getall(multicats[j], wantlist = TRUE)
    datin <- lapply(datin, paste, collapse = ", ")
    df[,multicats[j]] <- unlist(datin)
    df[ df[,multicats[j]]== "", multicats[j]] <- NA

}

head(df[,multicats], 50)

## Dummy-fy the red, white, blue, etc...
ucols <-  c("Red", "White", "Blue", "Black", "Green")
df[,ucols] <- NA

for(i in 1:length(ucols)){
    df[ grep("", df$colors), ucols[i]] <- NA
    df[ grep(ucols[i], df$colors), ucols[i]] <- 1
    df[ -grep(ucols[i], df$colors), ucols[i]] <- 0

}



df[, "nocolor"] <- ifelse(rowSums(df[,ucols],na.rm = TRUE) == 0, 1, 0)

## Get/add Legalities to the df
source(paste0(loc,"/Legalities.R"))

## Add Rarity
source(paste0(loc,"/ModalRarity.R"))
## rars[rars$cardnames == "Serra Angel",]
## head(rarsagg)


source(paste0(loc, "/Printings.R"))

source(paste0(loc, "/CardAbilities.R"))

write.csv(df, paste0(writeloc,"MTGData.csv"), row.names = FALSE)
