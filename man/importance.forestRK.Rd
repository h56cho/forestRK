\name{importance.forestRK}
\alias{importance.forestRK}
\title{
 Calculates Gini Importance or Mean Decrease Impurity (same algorithm is used in
 'scikit-learn') of each covariate that we consider in the \code{forestRK} model
}
\usage{
 importance.forestRK(forestRK.object = forestRK())
}
\arguments{
 \item{
 	forestRK.object}{a \code{forestRK} object.
 }
}
\value{
 A list containing the following items:

 \item{importance.covariate.names}{
 	a vector of names of the covariates of our dataset ordered from the most
 	important to the least important.
 }

 \item{average.decrease.in.criteria.vec}{
 	a vector storing the average decrease in the weighted splitting criteria
 	by each covariate that was calculated across all trees in the forestRK
 	object; the numbers are ordered from the highest average decrease in
 	criteria (importance) to the lowest, so the i-th importance number from
 	this vector pertains to the i-th covariate listed in the vector output
 	\code{importance.covariate.names}.
 }

 \item{ent.status}{
 	status of the parameter \code{entropy}; \code{TRUE} if Entropy is used for
 	splitting critera, \code{FALSE} if Gini Index is used instead.
 }

 \item{x.original}{
 	a numericized data frame storing covariates of each observation from the given
 	(training) dataset that was used to construct the \code{forestRK} object in
 	the beginning of the \code{forestRK} function call.
 }
}
\description{
 Calculates Gini Importance of each covariate considered in the forestRK model,
 and list them in the order of most important to the least important.

 The Gini Importance or Mean Decrease in Impurity algorithm is also used in
 'scikit-learn'. Gini Importance is defined as the total decrease in node
 impurity averaged over all trees of the ensemble, where the decrease in node
 impurity is obtained after weighting by the probability for an observation to
 reach that node (which is approximated by the proportion of samples reaching
 that node).
}
\examples{
  ## example: iris dataset
  ## load the forestRK package
  library(forestRK)

  ## numericize the data
  x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),]
  y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new

  # random forest
  # min.num.obs.end.node.tree is set to 5 by default;
  # entropy is set to TRUE by default
  # typically the nbags and samp.size has to be much larger than 30 and 50
  forestRK.1 <- forestRK(x.train, y.train, nbags=30, samp.size=50)
  # execute importance.forestRK function
  imp <- importance.forestRK(forestRK.1)
}
\author{
 Hyunjin Cho, \email{h56cho@uwaterloo.ca}
 Rebecca Su, \email{y57su@uwaterloo.ca}
}
\seealso{
\code{\link{forestRK}}
}
\keyword{importance}
\keyword{covariates}
\keyword{forestRK}
