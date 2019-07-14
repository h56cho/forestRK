\name{bstrap}
\alias{bstrap}
\title{
 Performs bootstrap sampling of the (training) dataset
}
\usage{
 bstrap(dat = data.frame(), nbags, samp.size)
}
\arguments{
 \item{dat}{
 	a numericized data frame that stores both the covariates of the observations
 	and their numericized class types \code{y};
 	\code{dat} should contain no \code{NA} or \code{NaN}'s.
 }

 \item{nbags}{
 	the number of bags or the number of bootstrap samples that we want to
 	generate.
 }

 \item{samp.size}{
 	the number of samples that each bag (individual bootstrap sample) should
 	contain.
 }
}
\value{
 A list containing a data frames of bootstrap samples generated from \code{dat}.
}
\description{
 Performs bootstrap sampling of our (training) dataset; this function is used
 inside of the \code{forestRK} function.
}
\examples{
  ## example: iris dataset
  ## load the forestRK package
  library(forestRK)

  # covariates of training data set
  x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),]
  y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new
  # combine the covariates x with class types y
  b <- data.frame(cbind(x.train, y.train))

  ## bstrp function example
  bootstrap.sample <- bstrap(dat = b, nbags = 20, samp.size = 30)
}
\author{
 Hyunjin Cho, \email{h56cho@uwaterloo.ca}
 Rebecca Su, \email{y57su@uwaterloo.ca}
}
\seealso{
\code{\link{forestRK}}
}
\keyword{bootstrap}
\keyword{forestRK}
