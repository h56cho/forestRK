\name{forestRK}
\alias{forestRK}
\title{
 Builds up a random forest RK model based on the given (training) dataset
}
\usage{
 forestRK(X = data.frame(), Y.new = c(),
          min.num.obs.end.node.tree = 5, nbags, samp.size, entropy = TRUE)
}
\arguments{
 \item{X}{
 	a numericized data frame storing covariates of each observation
 	contained in the given (training) dataset (obtained via \code{x.organizer()});
 	\code{X} should contain no \code{NA} or \code{NaN}'s.
 }

 \item{Y.new}{
 	a vector storing the numericized class types of each observation contained in
 	the given (training) dataset \code{X}; \code{Y.new} should contain no
 	\code{NA} or \code{NaN}'s.
 }

 \item{min.num.obs.end.node.tree}{
 	the minimum number of observations that we want each end node of our
 	\code{rktree} to contain. Default is set to 5.
 }

 \item{nbags}{
 	number of bootstrap samples that we want to generate to generate a forest.
 }

 \item{samp.size}{
 	number of observations that we want each of our bootstrap samples to contain.
 }

 \item{entropy}{
 	\code{TRUE} if we use Entropy as the splitting criteria;
 	\code{FALSE} if we use the Gini Index for the splitting criteria.
 	Default is set to \code{TRUE}.
 }
}
\value{
 A list containing the following items:

 \item{X}{
  The original (training) dataset that was used to construct the
  random forest RK model.
 }

 \item{forest.rk.tree.list}{
  A list of trees (\code{construct.treeRK} objects) contained in the
  \code{forestRK} model.
 }

 \item{bootsamp.list}{
  A list containing data frames of bootstrap samples that were generated from
  the given (training) dataset \code{X}.
 }

 \item{ent.status}{
  The value of the parameter \code{entropy}.
 }
}
\description{
 Builds up a random forest RK model onto the given (training) dataset.

 The functions \code{bstrap} and \code{construct.treeRK} are used inside
 this function. Once the call for \code{bstrap} generates bootstrap samples of
 the training dataset, then the function \code{construct.treeRK} is called
 in order to build a tree on each of those bootstrap dataset, to form a bigger
 forest.

 Calling of this function internally loads the package \code{rapportools}; this
 is to allow the use of \code{is.boolean} method to check one of the stopping
 criteria.
}
\examples{
  ## example: iris dataset
  ## load the forestRK package
  library(forestRK)

  # covariates of training data set
  x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),]
  y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new

  # Implement forestRK function
  # min.num.obs.end.node.tree is set to 5 by default;
  # entropy is set to TRUE by default
  # normally nbags and samp.size has to be much larger than 30 and 50
  forestRK.1 <- forestRK(x.train, y.train, nbags = 30, samp.size = 50)

  # extract the first tree in the forestRK.1 model
  forestRK.1$forest.rk.tree.list[[1]]
}
\author{
 Hyunjin Cho, \email{h56cho@uwaterloo.ca}
 Rebecca Su, \email{y57su@uwaterloo.ca}
}
\seealso{
\code{\link{bstrap}}
\code{\link{construct.treeRK}}
}
\keyword{forestRK}
