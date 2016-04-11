## Get printings
allprintings <- getall("printings", wantlist = TRUE)
uniquePrints <- sort(unique(unlist(allprintings)))
print(uniquePrints)
