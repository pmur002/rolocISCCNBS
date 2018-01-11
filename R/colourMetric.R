
## Mapping of sRGB to ISCC-NBS colour names
## NOTE the implicit ordering of names in "block.rds" is
## expand.grid(0:255, 0:255, 0:255)
ISCCNBSnames <- readRDS(system.file("extdata", "block.rds",
                                    package="rolocISCCNBS"))

LUVcoords <- function(sRGB) {
    luv <- as(sRGB, "LUV")
    coords <- coords(luv)
    ## Special case "black" to avoid NAs (set U and V to 0)
    coords[coords[,"L"] == 0, ] <- 0
    coords
}

ISCCNBSblock <- function(colour, colourList) {
    ## Convert colours to block name
    ## This gives us which block the colour is within
    RGB <- coords(colour)
    blockNames <- ISCCNBSnames[round(RGB[,1]*255) +
                               round(RGB[,2]*255)*256 +
                               round(RGB[,3]*255)*256*256 + 1]
    if (identical(colourList, ISCCNBScolours$colours)) {
        ## 0 for correct block, Inf for everything else
        t(outer(ISCCNBScolours$names, blockNames,
                function(x, y) ifelse(x == y, 0, Inf)))
    } else {
        ## Convert colour list to block name
        ## This gives us which block each colour list name is within
        listRGB <- coords(colourList)
        listBlockNames <- ISCCNBSnames[round(listRGB[,1]*255) +
                                       round(listRGB[,2]*255)*256 +
                                       round(listRGB[,3]*255)*256*256 + 1]
        ## For each colour, if in same block as list colour,
        ## compute distance between colour and list colour
        ## Inf for everything else
        specCoords <- LUVcoords(colour)
        listCoords <- LUVcoords(colourList)
        distances <- t(apply(specCoords, 1, euclideanDistance, listCoords))
        t(outer(listBlockNames, blockNames,
                function(x, y) ifelse(x == y, distances, Inf)))
    }
}
