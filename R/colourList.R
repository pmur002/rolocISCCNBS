
## sRGB centroids for ISCC-NBS colour name blocks
NBScentroids <- read.table(system.file("extdata", "NBS-ISCC-rgb.txt",
                                       package="rolocISCCNBS"),
                           comment.char="!")
## Separate out components of colour names to match names in 'ISCCNBSnames'
## (so screen readers will have an easier time)
NBScentroidNames <-
    gsub("(very|pale|ish|brilliant|strong|vivid|deep|moderate|light|dark|medium)",
         "\\1 ",
         gsub("yellowgreen", "yellow green",
              gsub("orangeyellow", "orange yellow",
                   gsub("olive(.)", "olive \\1",
                        NBScentroids$V4))))
ISCCNBScolours <- colourList(NBScentroidNames,
                             sRGB(as.matrix(NBScentroids[1:3])/255))

ISCCNBScolors <- ISCCNBScolours

