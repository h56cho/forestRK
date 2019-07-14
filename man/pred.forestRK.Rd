\name{pred.forestRK}
\alias{pred.forestRK}
\title{
 Make predictions on the test data based on the forestRK model
 constructed from the training data
}
\usage{
  pred.forestRK(x.test = data.frame(), x.training = data.frame(),
                y.training = c(), y.factor.levels,
                min.num.obs.end.node.tree = 5,
                nbags, samp.size, entropy = TRUE)
}
\arguments{
 \item{x.test}{
 	 a numericized data frame of covariates of the data points on which we want
 	 to make our predictions (typically the test observations); \code{x.test} can
 	 be obtained by applying the \code{x.organizer()} function. \code{x.test}
 	 should contain no \code{NA} or \code{NaN}'s.
 }

 \item{x.training}{
 	a numericized data frame of covariates of data points from which we build our
 	\code{forestRK} model (typically the training observations); \code{x.training}
 	can be obtained by applying the \code{x.organizer()} function.
 	\code{x.training}should contain no \code{NA} or \code{NaN}'s.
 }

 \item{y.training}{
 	a vector that stores numericized class types of the training
 	data points; \code{y.training} should contain no \code{NA} or \code{NaN}'s.
 }

 \item{min.num.obs.end.node.tree}{
 	the minimum number of observations that we want each end node of
 	our \code{rktree} to contain. Default is set to 5.
 }

 \item{nbags}{
 	the number of bootstrap samples that we want to generate to form a
 	\code{forest-RK}.
 }

 \item{samp.size}{
 	the number of data points that we want each of our bootstrap sample to
 	contain.
 }

 \item{y.factor.levels}{
 	a vector of original names of all class types that the user considers in his
 	or her study (can be obtained via \code{y.organizer()$y.factor.levels})
 }

 \item{entropy}{
 	\code{TRUE} if we use Entropy as the splitting criteria;
 	\code{FALSE} if we use the Gini Index for the splitting criteria.
 	Default is set to \code{TRUE}.
 }
}
\value{
 A list containing the following items:

 \item{x.test}{
  	the original test dataset that we used to make predictions.
 }

 \item{df.of.predictions.for.all.observations}{
  	a data frame storing predicted class types for all test observations from
  	each tree in the forest; each row of this data frame pertains to individual
  	test observation, and each column pertain to a specific tree from the
  	\code{forestRK} model.
  	This data frame stores predicted (numericized) class type of each test
  	observation from each tree in the \code{forestRK} model.
 }

 \item{forest.rk}{
 	  a \code{forestRK} object that was generated in the beginning of the function
 	  call.
 }

 \item{test.prediction.df.list}{
 	  a list of data frames storing the \code{prediction.df}'s
 	  (the data frame that can be obtained via \code{pred.treeRK()$prediction.df})
 	  of the test observations that were generated from each tree in the
 	  \code{forestRK} model. Note that the test data points in
 	  \code{test.prediction.df.list} are re-ordered by the increasing original
 	  observation index number.
 }

 \item{pred.for.obs.forest.rk}{
 	  a vector that stores the actual predicted class labels of the
 	  test observations instead of their numericized (integer) class types.
 	  Note that the test data points in \code{pred.for.obs.forest.rk} are
 	  re-ordered by the increasing original observation index number.
 }

 \item{num.pred.for.obs.forest.rk}{
 	  the numericized version of \code{pred.for.obs.forest.rk}.
 	  Note that the test data points in \code{num.pred.for.obs.forest.rk} are
 	  re-ordered by the increasing original observation index number.

 }
}
\description{
 Makes predictions on the test dataset based on the \code{forestRK} model
 constructed from the training dataset.

 Please be aware that, the test data points in \code{test.prediction.df.list}
 , \code{pred.for.obs.forest.rk}, and \code{num.pred.for.obs.forest.rk} are
 re-ordered by the increasing original index number (the original rownames) of
 those test observations. So if you shuffled the data before seperating them
 into a training and a test set, the order of the data points in which they are
 presented under the attribute \code{test.prediction.df.list},
 \code{pred.for.obs.forest.rk}, and \code{num.pred.for.obs.forest.rk} may not be
 same as the shuffled order of your original test set.

 Calling of this function internally loads the package \code{rapportools}; this
 is to allow the use of \code{is.boolean} method to check one of the stopping
 criteria in the beginning.

 The basic mechanism behind \code{pred.forestRK} function is the following:

 When the function is called, it calls \code{forestRK} function after passing
 the user-specified training data as an argument, in order to first generate the
 \code{forestRK} object. After that, the function uses \code{pred.treeRK}
 function to make predictions on the test observations based on each individual
 tree in the \code{forestRK} object. Once the individual prediction from each
 tree are obtained for all of the test observations, the function stores those
 individual predictions under a big dataframe. Once that data frame is complete,
 then the function collapses the results by the rule of the majority votes.
 For example, for the m-th observation from the test set, if the most frequently
 predicted class type for that m-th test observation by all of the rkTrees in
 the forest is class type 'A', then by the rule of the majority votes, the
 \code{pred.forestRK} function will assign class 'A' as the predicted class type
 for that m-th test observation based on the \code{forestRK} model.
}

\examples{
  ## example: iris dataset
  ## load the forestRK package
  library(forestRK)

  ## numericize the data
  x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),]
  x.test <- x.organizer(iris[,1:4], encoding = "num")[c(26:50,76:100,126:150),]
  y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new

  y.factor.levels <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.factor.levels

  ## make prediction from a random forest RK model
  ## typically the nbags and samp.size has to be much larger than 30 and 50
  pred.forest.rk <- pred.forestRK(x.test = x.test, x.training = x.train,
                                  y.training = y.train,
                                  y.factor.levels,
                                  min.num.obs.end.node.tree = 6,
                                  nbags = 30, samp.size = 50, entropy = FALSE)
  pred.forest.rk$test.prediction.df.list[[10]]
  pred.forest.rk$pred.for.obs.forest.rk # etc....
}
\author{
 Hyunjin Cho, \email{h56cho@uwaterloo.ca}
 Rebecca Su, \email{y57su@uwaterloo.ca}
}
\seealso{
\code{\link{pred.treeRK}}
\code{\link{forestRK}}
}
\keyword{prediction}
\keyword{forestRK}
