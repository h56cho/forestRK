\name{criteria.after.split.calculator}
\alias{criteria.after.split.calculator}
\title{
 Calculates Entropy or Gini Index of a node after a given split
}
\usage{
 criteria.after.split.calculator(x.node = data.frame(), y.new.node = c(),
                                 split.record = kidids_split(),
                                 entropy = TRUE)
}
\arguments{
 \item{x.node}{
 	numericized data frame of covariates (obtained via \code{x.organizer()})
 	from a particular node that is to be split; \code{x.node} should contain no
 	\code{NA} or \code{NaN}'s.
 }

 \item{y.new.node}{
 	numericized class type of each observation from a particular node that is to
 	be split; \code{y.new.node} should contain no \code{NA} or\code{NaN}'s.
 }

 \item{split.record}{
 	output of the \code{kidids_split} function from the \code{partykit} package
 	that describes a particular split.
 }

 \item{entropy}{
 	\code{TRUE} if Entropy is used as the splitting criteria;
 	\code{FALSE} if Gini Index is used instead.
 	Default is set to \code{TRUE}.
 }
}
\value{
 The value of Entropy or Gini Index of a particular node after a particular
 split.
}
\description{
 Calculates Entropy or Gini Index of a particular node after a particular split;
 this function is called within \code{construct.treeRK} function.

 The argument \code{split.record} is a \code{kidids_split} object
 from the package \code{partykit}; the method \code{kidids_split} splits the
 data according to the criteria specified by an user ahead of time, and returns
 a vector storing the index of the split group (group "1" or "2") that each
 observation from the original data in question belongs to after the split has
 occurred.

 For more information about the function, please see the \code{partykit}
 documentation.
}
\examples{
  ## example: iris dataset
  library(forestRK) # load the package forestRK
  library(partykit)

  # covariates of training data set
  x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),]
  # numericized class types of observations of training dataset
  y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new
  ## criteria.after.split.calculator() example in the implementation
  ## of the forestRK algorithm

  ent.status <- TRUE

  # number.of.columns.of.x.node
  # = total number of covariates that we consider
  number.of.columns.of.x.node <- dim(x.train)[2]
  # m.try = the randomly chosen number of covariates that we consider
  # at the time of split
  m.try <- sample(1:(number.of.columns.of.x.node),1)
  ## sample m.try number of covariates from the list of all covariates
  K <- sample(1:(number.of.columns.of.x.node), m.try)

  # split the data
  # (the choice of the type of split used here is only arbitrary)
  # for more information about kidids_split,
  # please refer to the documentation for the package 'partykit'
  sp <- partysplit(varid=K[1], breaks = x.train[1,K[1]], index = NULL,
                   right = TRUE, prob = NULL, info = NULL)
  split.record <- kidids_split(sp, data=x.train)

  # implement critera.after.split function based on kidids_split object
  criteria.after.split <- criteria.after.split.calculator(x.train,
                                    y.train, split.record, ent.status)
  criteria.after.split
}
\author{
 Hyunjin Cho, \email{h56cho@uwaterloo.ca}
 Rebecca Su, \email{y57su@uwaterloo.ca}
}
\seealso{
\code{\link{criteria.calculator}}
}
\keyword{calculator}
\keyword{after.split}
\keyword{treeRK}
