## So I thought I'd put together something to extract the abilities of
## cards from their text. The goal of this script is to take the text,
## look for prenamed magic abilities, and have the same kind of dummy
## that we do for the set information, with 1 if they have the
## ability, and NA if they don't. We could also do yes and no, if we
## wanted, but that would make it so that when we called a card, it
## would have too much information. Frankly, I'm already starting to
## think we have too much information every time we call up a card...

head(df[,"text"])

abilities <- read.csv("/Users/bjr/GitHub/MTG/Data/keywords.csv", stringsAsFactors = T)
abilities <- apply(abilities, 1, function(x) as.character(x))
## Get only the ones whose last character is a space.
lastIsSpace <- substr(abilities, nchar(abilities), nchar(abilities) ) == " "
## Then, take those observations, and, get rid of the space at the end using substr
abilities[lastIsSpace] <- substr(abilities[lastIsSpace],1, nchar(abilities[lastIsSpace])-1)

## Noticed a duplicate. Get rid of them
abilities <- unique(abilities)

## Kill the spaces at the ends of some of these keywords


landabilities <- abilities[grep('land', abilities, ignore.case= T)]

basiclands <- c("Plains", "Island", "Mountain", "Swamp", "Forest")

## Remove land from the land abilities, and add each of the above
landabilities <- gsub("land", "", landabilities, T)

landabilities <- unlist(lapply(basiclands, function(x) paste0(x, landabilities)))

df$text[grepl("banding", df$text, T)]

