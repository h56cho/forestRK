\name{x.organizer}
\alias{x.organizer}
\title{
 Numericizing a data frame of covariates from the original dataset
 via Binary or Numeric Encoding
}
\usage{
 x.organizer(x.dat = data.frame(), encoding = c("num","bin"))
}
\arguments{
 \item{x.dat}{
 	a data frame storing covariates of each observation
 	(can be either numeric or non-numeric) from the original data;
 	\code{x.dat} should contain no \code{NA} or \code{NaN}.
 	The rownames of \code{x.dat} should be numerical index for each observations.
 }
  \item{encoding}{
 	type of encoding done for the categorical features;
 	"\code{num}" stands for Numeric Encoding, and
 	"\code{bin}" stands for Binary Encoding.
 	When the data in question only has numeric features, then
 	the user can select either one of "\code{num}" or "\code{bin}",
 	and the \code{x.organizer} function will just return the original
 	numeric dataset.
 }
}
\value{
 A numericized data frame of the covariates from the original data
 obtained via either Numeric or Binary Encoding.
}
\description{
 Takes the original data frame of covariates as an input (which may or
 may not be numeric), and converts it into a numericized data frame by
 applying either Binary or Numeric Encoding.

 Binary Encoding for categorical features are recommended for
 tree ensembles when the cardinality of categorical feature is >= 1000;
 Numeric Encoding for categorical features are recommended for
 tree ensembles when the cardinality of categorical features is < 1000.

 For more information about the Binary and Numeric Encoding and their
 effectiveness under different cardinality, please visit:
 https://medium.com/data-design/
 visiting-categorical-features-and-encoding-in-decision-trees-53400fa65931

 NOTE: In order to use other functions within the forestRK package,
 you must ensure that the numericized data frame of covariates (the
 \code{x.organizer} object) contains no missing record,
 that is, you have to remove any record containing \code{NA} or \code{NaN}
 prior to applying the \code{x.organizer} function.

  Following is the summary of the data cleaning process with
  \code{x.organizer()}:

  1. remove all \code{NA} or \code{NaN}'s from the data in hand.
  2. split the data into a data frame that contains covariates of
   ALL data points, (BOTH training and test observations), and a
   vector that contains class types of the training observations;
  3. apply the \code{x.organizer} to the big data frame of covariates
  of all observations.
  4. split the \code{x.organizer} output into a training and
  a test set, as needed.

 PROPER DATA CLEANING IS ABSOLUTELY NECESSARY FOR forestRK FUNCTIONS TO WORK!
}
\examples{
  ## example: iris dataset
  library(forestRK) # load the package forestRK

  ## Basic Procedures
  ## 1. Apply x.organizer to a data frame that stores covariates of
  ## ALL observations (BOTH training and test observations)
  ## 2. Split the output from 1 into a training and a test set, as needed

  # note: iris[,1:4] are the columns of the iris dataset that stores
  # covariate values

  # covariates of training data set
  x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),]
}
\author{
  Hyunjin Cho, \email{h56cho@uwaterloo.ca}
  Rebecca Su, \email{y57su@uwaterloo.ca}
}
\seealso{
\code{\link{y.organizer}}
}
\keyword{organizer}
\keyword{x}
\keyword{Data Cleaning}
