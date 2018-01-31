\name{ISCCNBSblock}
\alias{ISCCNBSblock}
\title{
  An ISCC-NBS Colour Metric
}
\description{
  A colour metric based on the ISCC-NBS System of Colour Designation.
  
}
\usage{
ISCCNBSblock(colour, colourList, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{colour}{
    An \code{"sRGB"} colour object representing colours to find a match for.
  }
  \item{colourList}{
    An \code{"sRGB"} colour object representing a list of colours to
    find a match within.
  }
  \item{\dots}{
    Other arguments passed by the \code{colourMatch} function from the
    \pkg{roloc} package.
  }
}
\details{
  All \code{colours} and all if \code{colourList} are converted to
  an ISCCNBS block.  The distance between a colour specification and
  a colour name is 0 if both are within the same block or \code{Inf}
  if they are not in the same block.  It is also possible that either
  the colour specification or the colour name does not correspond to
  any block (could not be converted), in which case the distance is
  \code{NA}.
}
\value{
  A matrix of distances, with one row per colour specification and one
  column per colour name.  
}
\author{
  Paul Murrell
}
\keyword{ dplot }% use one of  RShowDoc("KEYWORDS")
