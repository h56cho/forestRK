\name{get.tree.forestRK}
\alias{get.tree.forestRK}
\title{
 Extracts the structure of one or more trees in a forestRK object
}
\usage{
 get.tree.forestRK(forestRK.object = forestRK(), tree.index=c())
}
\arguments{
 \item{forestRK.object}{
 	a \code{forestRK} object.
 }

 \item{tree.index}{
 	a vector of indices of the trees that we want to extract from
 	the \code{forestRK} object.
  }
}
\value{
 A list containing \code{forestRK} trees that have their indices
 specified in the function argument \code{tree.index}.
}
\description{
 Extracts structure of one or more trees from a \code{forestRK} object.

 Each tree in the list are named by the exact indices of the tree;
 for example, if the code
 \code{obj <- get.tree.forestRK(forestRK.1, tree.index=c(4,5,6))} was used to
 extract the structure of the 4th, 5th, and 6th trees in the forest, the user
 can retrieve the information pertains explicitly to the 4th tree in the forest
 by doing \code{obj["4"]]}.
}
\examples{
  ## example: iris dataset
  ## load the forestRK package
  library(forestRK)

  x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),]
  y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new

  # random forest
  # min.num.obs.end.node.tree is set to 5 by default;
  # entropy is set to TRUE by default
  # normally nbags and samp.size have to be much larger than 30 and 50
  forestRK.1 <- forestRK(x.train, y.train, nbags = 30, samp.size = 50)

  # get tree
  tree.index.ex <- c(1,3,8)
  get.tree <- get.tree.forestRK(forestRK.1, tree.index = tree.index.ex)
  get.tree[["8"]] # display the 8th tree of the random forest
}
\author{
 Hyunjin Cho, \email{h56cho@uwaterloo.ca}
 Rebecca Su, \email{y57su@uwaterloo.ca}
}
\seealso{
\code{\link{forestRK}}
}
\keyword{get.tree}
\keyword{forestRK}
