## Check out the JSON Data in these magic the gathering datasets

datloc <- "/Users/bjr/GitHub/MTG/Data"

library(jsonlite)

## If the data hasn't already been read in, read it in
if(!exists("AllCards")) AllCards <- fromJSON(paste0(datloc,"/AllCards-x 2.json"))
if(!exists("SetInfo")) SetInfo <- fromJSON(paste0(datloc,"/SetList.json"))
if(!exists("FullData")) FullData <- fromJSON(paste0(datloc,"/AllSets-x 2.json"))

getall <- function( var, dat = AllCards, wantlist = TRUE) {
    out <- lapply(dat, function(x) x[[var]])
    out

    if(!wantlist) return(unlist(out)) else return(out)

}

## Make it easier to look at cards with all of these na's and symbols we'd like hidden
##'
##'
##' @title card
##' @param cardname The card's name, as a text string.
##' @param dat Usually the data frame df, but we could use whatever we want!
##' @return a one row data frame with the card's information
##' @author Benjamin Rogers
card <- function(cardname, dat = df){
    dat[cardname,
        !is.na(dat[cardname,])
        & dat[cardname,] != 0
        & dat[cardname,] != "No"
        & !colnames(dat) %in% "printsPaste"]
}

##' I decided I wanted a function that allows me to look at a card's
##' list and data frame info at the same time. If only there were a way
##' to get the info more elegantly
##'
##' returns card output from the data frame and cards list
##' @title compare
##' @param cardsname
##' @return void
##' @author Benjamin Rogers
compare <- function(cardsname){
    print(card(cardsname))
    print(AllCards[cardsname])


}
