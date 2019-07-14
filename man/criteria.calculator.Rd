\name{criteria.calculator}
\alias{criteria.calculator}
\title{
 Calculates Entropy or Gini Index of a particular node before (or without) a
 split
}
\usage{
 criteria.calculator(x.node = data.frame(), y.new.node = c(),
                     entropy = TRUE)
}
\arguments{
  \item{x.node}{
  	numericized data frame of covariates of a particular node
  	(can be obtained by applying \code{x.organizer}) before or without a split;
  	\code{x.node} should contain no \code{NA} or \code{NaN}'s.
 }

  \item{y.new.node}{
  	numericized vector of class type (\code{y}) of a particular node
  	(can be obtained by applying \code{y.organizer}) before or without split;
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

  \item{criteria}{
  	the value of the Entropy or the Gini Index of a particular node.
 }

  \item{ent.status}{
  	logical value (\code{TRUE} or \code{FALSE}) of the parameter \code{entropy}.
 }
}
\description{
   Calculates the Entropy or Gini Index of a particular node before (or without)
   a split. This function is used inside the
   \code{criteria.after.split.calculator} method.
}
\examples{
 ## example: iris dataset
 library(forestRK) # load the package forestRK

 # covariates of training data set
 x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),]
 # numericized class types of observations of training dataset
 y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new

 ## criteria.calculator() example
 ## calculate the Entropy of the original training dataset
 criteria.calculator(x.node = x.train, y.new.node = y.train)
 ## calculate the Gini Index of the original training dataset
 criteria.calculator(x.node = x.train, y.new.node = y.train, entropy = FALSE)
}
\author{
 Hyunjin Cho, \email{h56cho@uwaterloo.ca}
 Rebecca Su, \email{y57su@uwaterloo.ca}
}
\seealso{
 \code{\link{criteria.after.split.calculator}}
}
\keyword{calculator}
\keyword{entropy}
\keyword{gini.index}
\keyword{treeRK}
