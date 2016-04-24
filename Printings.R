

## Get printings
allPrintings <- getall("printings", wantlist = TRUE)
uniquePrints <- sort(unique(unlist(allPrintings)))

head(allPrintings)

printsPasted <- unlist(lapply(allPrintings, paste, collapse = ", "))
df$printsPaste <- printsPasted

df[,uniquePrints] <- NA


for(i in 1:length(uniquePrints)){
    ## df[ grep("", df$printsPaste), uniquePrints[i]] <- NA
    df[ grep(uniquePrints[i], df$printsPaste), uniquePrints[i]] <- 1
    df[ -grep(uniquePrints[i], df$printsPaste), uniquePrints[i]] <- 0


}

df$printsPaste <- NULL

colnames(SetInfo)

head(data.frame( colnames(df[,uniquePrints]),SetInfo$code, SetInfo$name))

codematches <- match(colnames(df[,uniquePrints]), SetInfo$code)

## View(SetInfo[order(SetInfo$code),])

colnames(df)[colnames(df) %in% uniquePrints] <- SetInfo$name[codematches]
## colnames(df)

## Note: set information is kept under SetInfo
