## So I thought I'd put together something to extract the abilities of
## cards from their text. The goal of this script is to take the text,
## look for prenamed magic abilities, and have the same kind of dummy
## that we do for the set information, with 1 if they have the
## ability, and NA if they don't. We could also do yes and no, if we
## wanted, but that would make it so that when we called a card, it
## would have too much information. Frankly, I'm already starting to
## think we have too much information every time we call up a card...




abilities <- read.csv("/Users/bjr/GitHub/MTG/Data/keywords.csv", stringsAsFactors = T)
abilities <- apply(abilities, 1, function(x) as.character(x))

## Kill the spaces at the ends of some of these keywords. Get only the
## ones whose last character is a space.
lastIsSpace <- substr(abilities, nchar(abilities), nchar(abilities) ) == " "
## Then, take those observations, and, get rid of the space at the end using substr
abilities[lastIsSpace] <- substr(abilities[lastIsSpace],1, nchar(abilities[lastIsSpace])-1)

## Deal with tap/untap in 25
## tapuntap <- unlist(strsplit(abilities[25], "[/]"))
regexes <- c(tapAbility = "\\{T\\}",
             greenAbility = "\\{G\\}", plainsAbility = "\\{W\\}", mtAbility = "\\{R\\}", swampAbility = "\\{B\\}", blueAbility = "\\{U\\}",
             artifAbility = "\\{C\\}", digitAbility =  "\\{\\d+\\}")
abilities <- c(abilities[1:24], abilities[26:length(abilities)])
abilities <- c(abilities, regexes

               )
## turns out that there are both colorles {C} and {n} where n could be
## any digit. I've go the first one captured. We'll see about the
## second later


## Noticed a duplicate. Get rid of them
abilities <- unique(abilities)


## Find any ability where we'd like to remove land from and replace with a land name
landabilities <- abilities[grep('land', abilities, ignore.case= T)][c(1,3)]

basiclands <- c("Plains", "Island", "Mountain", "Swamp", "Forest")

## Remove land from the land abilities, and add each of the above
landabilities <- gsub("land", "", landabilities, T)

landabilities <- unlist(lapply(basiclands, function(x) paste0(x, landabilities)))

abilities <- c(abilities, landabilities)


## Now that we've got our abilities listed, we need to figure out how to
## df$text[ grepl("\\{T\\}", df$text, T)][1:10]


abilcols <- sapply(seq_along(abilities), function(z){
    ifelse(grepl(abilities[z], df$text, T), "Yes", NA)
}
       )

colnames(abilcols) <- abilities

colnames(abilcols)[colnames(abilcols) %in% regexes]
names(regexes)

df[,colnames(abilcols)] <- as.data.frame(abilcols)
