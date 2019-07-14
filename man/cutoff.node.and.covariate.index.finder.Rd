\name{cutoff.node.and.covariate.index.finder}
\alias{cutoff.node.and.covariate.index.finder}
\title{
 Identifies optimal cutoff point of an impure node for splitting after applying
 the \code{rk} (Random K) algorithm.
}
\usage{
 cutoff.node.and.covariate.index.finder(x.node = data.frame(),
                                        y.new.node = c(), entropy = TRUE)
}
\arguments{
 \item{x.node}{
 	a numericized data frame of covariates of the observations from a particular
 	node prior to the split (can be obtained after applying \code{x.organizer()});
 	\code{x.node} should contain no \code{NA} or \code{NaN}'s.
 }

 \item{y.new.node}{
 	a vector storing numericized class type of the observations from a particular
 	node before the split (can be obtained after applying \code{y.organizer()});
 	\code{y.new.node} should contain no \code{NA} or \code{NaN}'s.
 }

 \item{entropy}{
 	\code{TRUE} if Entropy is used as the splitting criteria;
 	\code{FALSE} if Gini Index is used as the splitting criteria.
 	Default is set to \code{TRUE}.
 }
}
\value{
 A list containing the following items:

 \item{cutoff.value}{
 	the value at which the optimal split should take place.
 }

 \item{cutoff.node}{
 	the index of the observation (observation number) at which optimal split
 	should occur.
 }

 \item{covariate.ind}{
 	numeric index of the covariate at which the optimal split should occur.
 }

 \item{split.record.optimal}{
 	the \code{kidid_split} output of the optimal split.
 }
}
\description{
 Identifies optimal cutoff point of an impure dataset for splitting after
 applying the \code{rk} (Random K) algoritm, in terms of Entropy or Gini Index.

 To give an example, if the function gives \code{cutoff.value} of 2.5,
 \code{covariate.ind} of 4, and \code{cutoff.node} of 23, this would inform the
 user that if a split is to be performed on the particular node that the user is
 considering, the split should occur on the 4th covariate (the actual name of
 this covariate would be the name of the 4th column from the original dataset),
 at the value of 2.5 (left child node in this case would be the group of
 observations that have their 4th covariate value less than or equal to 2.5, and
 for the right child node would be the group of observations that have their 4th
 covariate value greater than 2.5), and that this splitting point corresponds to
 the 23rd observation point of the node.

 This function internally loads the packages \code{partykit} and
 \code{rapportools}; the package \code{partykit} is internally loaded to
 generate the object \code{split.record.optimal}, and the package
 \code{rapportools} is loaded to allow the validation of one of the
 stopping criteria that uses \code{is.boolean} method.

 This function is ran internally in the \code{construct.treeRK} function.
}
\examples{
  ## example: iris dataset
  ## load the forestRK package
  library(forestRK)

  ## numericize the data
  x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),]
  y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new

  # implementation of cutoff.node.and.covariate.index.finder()
  res <- cutoff.node.and.covariate.index.finder(x.train, y.train,
                                               entropy=FALSE)
  res$cutoff.value
  res$cutoff.node
  res$covariate.ind
  res$split.record.optimal
}
\author{
 Hyunjin Cho, \email{h56cho@uwaterloo.ca}
 Rebecca Su, \email{y57su@uwaterloo.ca}
}
\seealso{
\code{\link{construct.treeRK}}
}
\keyword{cutoff.node}
\keyword{covariate.index}
\keyword{treeRK}
