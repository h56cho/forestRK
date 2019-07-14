\name{y.organizer}
\alias{y.organizer}
\title{
 Numericize the vector containing categorical class type(\code{y})
 of the original data
}
\usage{
 y.organizer(y = c())
}
\arguments{
  \item{y}{
  	a vector containing the class type of each observation
    from the dataset on which we want to build our
    rktree models (the training dataset); \code{y} should contain
    no \code{NA} or \code{NaN}.
  }
}
\value{
  A list containing the following items:

  \item{y.new}{
  	a vector containing numericized class type of each observation from
  	the dataset from which our rktree models are generated from.
  	(these are typically the observations from the training set)
  }

  \item{y.factor.levels}{
  	a vector storing original names of the numericized class types.
  }
}
\description{
  Numericizes a vector of categorical class type of each (training)
  data point.

  NOTE: In order to use other functions within the forestRK package,
  you must ensure that the original vector of class type \code{y}
  contains no missing record (\code{NA}, \code{NaN}), that is,
  you have to remove any record containing \code{NA} or \code{NaN}
  prior to applying the \code{y.organizer} function.

  Following is the summary of the data cleaning process with
  \code{y.organizer()}:
  1. remove all \code{NA} or \code{NaN}'s from the dataset
  in hand.
  2. split the training dataset into a data frame that contains
  covariates of ALL observations (BOTH training and test observations),
  and a vector that contains class types of the training observations;
  3. apply the \code{y.organizer} to the vector that contains class type
  of each training observation.

 PROPER DATA CLEANING IS NECESSARY FOR THE forestRK FUNCTIONS TO WORK!
}
\examples{
  ## example: iris dataset
  ## load the package forestRK
  library(forestRK)

  ## Basic Procedures:
  ## 1. Extract the portion of the data that stores class type of each
  ##    TRAINING observation, and make it as a vector
  ## 2. apply y.organizer function to the vector obtained from 1

  y.train <- y.organizer(as.vector(iris[c(1:25,51:75,101:125),5]))
  ## retrieves the original names of each class type, if the class names
  ## were originally non-numeric
  y.train$y.factor.levels
  ## retrieves the numericized vector that stores classification category
  y.train$y.new
}
\author{
  Hyunjin Cho, \email{h56cho@uwaterloo.ca}
  Rebecca Su, \email{y57su@uwaterloo.ca}
}

\seealso{
  \code{\link{x.organizer}}
}
\keyword{organizer}
\keyword{y}
\keyword{Data Cleaning}
