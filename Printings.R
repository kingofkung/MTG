

## Get printings
allPrintings <- getall("printings", wantlist = TRUE)
uniquePrints <- sort(unique(unlist(allprintings)))

head(allPrintings)

printsPasted <- unlist(lapply(allPrintings, paste, collapse = ", "))
df$printsPaste <- printsPasted

df[,uniquePrints] <- NA


for(i in 1:length(uniquePrints)){
    ## df[ grep("", df$printsPaste), uniquePrints[i]] <- NA
    df[ grep(uniquePrints[i], df$printsPaste), uniquePrints[i]] <- 1
    df[ -grep(uniquePrints[i], df$printsPaste), uniquePrints[i]] <- NA


}

df$printsPaste <- NULL

## Note: set information is kept under SetInfo
