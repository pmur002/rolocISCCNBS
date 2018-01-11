
## Mapping of sRGB to ISCC-NBS colour names
## NOTE the implicit ordering of names in "block.rds"
## expand.grid(0:255, 0:255, 0:255)
ISCCNBSnames <- readRDS(system.file("extdata", "block.rds",
                                    package="rolocISCCNBS"))

ISCCNBSblock <- function(colour, colourList) {
    if (identical(colourList, ISCCNBScolours$colours)) {
        RGB <- coords(colour)
        blockNames <- ISCCNBSnames[round(RGB[,1]*255) +
                                   round(RGB[,2]*255)*256 +
                                   round(RGB[,3]*255)*256*256 + 1]
        t(outer(ISCCNBScolours$names, blockNames,
                function(x, y) 1 - (x == y)))
    } else {
        stop("Sorry, non-ISCCNBS colour lists are not currently supported")
    }
}
