\name{var.used.forestRK}
\alias{var.used.forestRK}
\title{
 Extract the list of covariates used to perform the splits to
 generate a particular tree(s) in a \code{forestRK} object
}
\usage{
 var.used.forestRK(forestRK.object = forestRK(), tree.index = c())
}
\arguments{
 \item{forestRK.object}{
 	a \code{forestRK} object.
 }

 \item{tree.index}{
 	a vector storing the indices of the trees that we are interested to examine.
 }
}
\value{
 A list of vectors that stores the names of covariates on which each split was
 performed to construct the specific tree(s) in a \code{forestRK} model that the
 user provided.
}
\description{
 Spits out the list of covariates used to perform the splits to generate
 a particular tree(s) in a \code{forestRK} object that the user provided.

 The function extracts the list of names of covariates used in splits to
 construct a single or a multiple numbers of trees from a \code{forestRK}
 object. The \code{var.used.forestRK} displays the actual name of the covariate
 used for each split (not their numericized ones), consistent to the exact
 order of the split; for instance, the 1st element of the vector
 \code{covariate.used.for.split.tree[["6"]]} from the example below is the
 covariate on which the 1st split had occured while the 6th tree in the
 \code{forestRK.1} object was built.

 Each vector in the list are named by the exact indices of the tree;
 for example, if the code
 \code{obj <- var.used.forestRK(forestRK.1, tree.index=c(4,5,6))} is used to
 extract the list of covariates used for splitting to construct 4th, 5th, and
 6th trees in the forest, and the user can retrieve the information pertains
 explicitly to the 6th tree in the forest by doing \code{obj[["6"]]}.
}
\examples{
  library(forestRK)

  x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),]
  y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new

  # random forest
  # min.num.obs.end.node.tree is set to 5 by default;
  # entropy is set to TRUE by default
  # normally nbags and samp.size have to be much larger than 30 and 50
  forestRK.1 <- forestRK(x.train, y.train, nbags = 30, samp.size = 50)

  # prediction from a random forest RK
  covariate.used.for.split.tree <- var.used.forestRK(forestRK.1,
                                                     tree.index=c(4,5,6))

  # retrieve the list of covariates used for splitting for the 'tree #6'
  covariate.used.for.split.tree[["6"]]
}
\author{
 Hyunjin Cho, \email{h56cho@uwaterloo.ca}
 Rebecca Su, \email{y57su@uwaterloo.ca}
}
\seealso{
\code{\link{forestRK}}
}
\keyword{covariates}
\keyword{forestRK}
\keyword{var.used}
