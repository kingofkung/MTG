## Check out the JSON Data in these magic the gathering datasets

datloc <- "/Users/bjr/GitHub/MTG/Data"

library(jsonlite)

AllCards <- fromJSON(paste0(datloc,"/AllCards-x 2.json"))
SetInfo <- fromJSON(paste0(datloc,"/SetList.json"))
FullData <- fromJSON(paste0(datloc,"/AllSets-x 2.json"))

getall <- function( var, dat = AllCards, wantlist = TRUE) {
    out <- lapply(dat, function(x) x[[var]])
    out

    if(!wantlist) return(unlist(out)) else return(out)

}

## Make it easier to look at cards with all of these na's
card <- function(cardname, dat = df) dat[cardname, !is.na(dat[cardname,])]
