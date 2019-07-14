\name{construct.treeRK}
\alias{construct.treeRK}
\title{
 Constructs a classification tree on the (training) dataset, by implementing
 the RK (Random 'K') algorithm
}
\usage{
 construct.treeRK(x.train = data.frame(), y.new.train = c(),
                  min.num.obs.end.node.tree = 5, entropy = TRUE)
}
\arguments{
 \item{x.train}{
 	a numericized data frame of covariates of the data on which we want to
 	build our rktree models (typically the training data); this data frame can be
 	obtained by applying the \code{x.organizer} function. \code{x.train} should
 	contain no \code{NA} or \code{NaN}'s.
 }

 \item{y.new.train}{
 	a numericized class types of the observations from the dataset on which we
 	want to build our rktree models (typically the training data).
 	\code{y.new.train} should contain no \code{NA} or \code{NaN}'s.
 }

 \item{min.num.obs.end.node.tree}{
 	the minimum number of observations that we want each end node of our
 	rktree to contain. Default is set to '5'.
 }

 \item{entropy}{
 	\code{TRUE} if Entropy is used as the splitting criteria;
 	\code{FALSE} if Gini Index is used as the splitting criteria.
 	Default is set to \code{TRUE}.
 }
}
\value{
 A list containing the following items:

 \item{covariate.names}{
 	a vector of the names of all covariates that we consider in our model.
 }

 \item{l}{
 	length of the hierarchical flag.
 }

 \item{x.node.list}{
 	a list containing a series of children nodes produced from the numericized
 	data frame \code{x.train} as the \code{rktree} model was building up.
 }

 \item{y.new.node.list}{
 	a list containing a series of children nodes produced from the numericized
 	vector of class type \code{y.new.train} as the \code{rktree} model was
 	building up.
 }

 \item{flag}{
 	hierchical flag that characterizes each split in the \code{rktree}.
 }

 \item{covariate.split}{
 	a matrix that lists numericized covariates used for each split as the
 	\code{rktree} was built.
 }

 \item{value.at.split}{
 	a vector that lists the values at which each node of the \code{rktree}
 	was split.
 }

 \item{amt.decrease.criteria}{
 	a matrix that lists the amount of decrease in splitting criteria after
 	each split as the \code{rktree} was built.
 }
 \item{num.obs}{
  a matrix that stores the number of observations contained in each parent node
  right before each split.
 }
}
\description{
 Constructs a classification tree based on the dataset of interest by
 implementing the RK (Random 'K') algorithm.

 The package \code{rapportools} is loaded internally when this function is
 called; this is to use the method \code{is.boolean} to check one of the
 stopping criteria in the beginning of the function. The functions specifically
 from the \code{forestRK} package that are being used inside
 \code{construct.treeRK} are \code{criteria.calculator} and
 \code{cutoff.node.and.covariate.index.finder}.

 The \code{construct.treeRK} output is one of the arguments that is used to call
 the \code{pred.treeRK} function.


 DESCRIPTIONS OF THE RETURNED VALUES:

 The hirarchical flag of a rktree (\code{construct.treeRK()$flag}) is
 constructed in the following way:

 (1) the first entry of the flag, "r" denotes for "root";
 (2) the subsequent strings of the flag is constructed in the way that last "x"
  denotes for the left child node of the node represented by the series of
  characters that are before the last "x", and the last "y" denotes for the
  right child node of the node represented by the series of characters that are
  before the last "y".

  For example, the flag "rxyx" is the left child node of the node represented
  by "rxy".

 \code{x.node.list} and \code{y.node.list} are the lists of children nodes
 (for \code{x} and \code{y}, respectively) of the \code{rktree},
 listed in the order consistent to the order of the nodes represented in the
 \code{rktree}'s hirarchical flag.

 \code{covariate.split} is a matrix that lists the numericized covariate names
 that were used for the splits to construct the rktree. The first entry of
 \code{covariate.split} is \code{NA}, which stands for the condition at the
 root. The number immediately underneath \code{NA} is the numericized covariate
 name that was used for the first split in the \code{rktree}, and the number
 below that is the numericized covariate name that was used for the
 second split, etc. If the numericized covariate name listed under
 \code{covariate.split} is the number "n", this corresponds to the "n"-th
 covariate or the name of the "n"-th column of the data frame \code{x.train}.

 \code{value.at.split} is a vector that lists the actual values of the
 covariates at which the split had occured while constructing the rktree.
 The first entry of \code{value.at.split} is \code{NA}, which denotes for the
 root prior to any splits. To give an example of how to interpret the
 \code{value.at.split}, if the second entry appear in the \code{covariate.split}
 is 4, and the second entry appear under \code{value.at.split} is 0.5, this
 indicates that the first split of the rktree had occured on the covariate
 corresponds to the 4th column of the data frame \code{x.train}, and the exact
 criteria for that first split was (4th covariate value) <= 0.5 vs.
 (4th covariate value) > 0.5.

 \code{amount.decrease.criteria} is a matrix that lists the amount of decrease
 in splitting criteria (Entropy or Gini Index) after each split had occurred.
 The first entry of \code{amount.decrease.criteria} is \code{NA},
 which denotes for the condition at the root (no split). To give an example,
 if the second entry appear in the \code{amount.decrease.criteria} is 0.91,
 and if \code{entropy} was set to \code{TRUE}, this means that after the first
 split, the Entropy of the original node had decreased by 0.91.

 \code{num.obs} is a matrix that stores the number of observations contained
 within a parent node prior to the split; the matrix starts with the entry "NA",
 in order to reflect the condition at "root". The 2nd entry of \code{num.obs}
 would inform us on the number of observations contained within the parent node
 on which the 1st split had took place while the \code{rktree} was built; the
 3rd entry of the \code{num.obs} would inform us on the number of observations
 contained within the parent node on which the 2nd split had took place,
 and so on.
}

\examples{
  ## example: iris dataset
  ## load the forestRK package
  library(forestRK)

  ## numericize the data
  x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),]
  y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new

  # Construct a tree
  # min.num.obs.end.node.tree is set to 5 by default;
  # entropy is set to TRUE by default
  tree.entropy <- construct.treeRK(x.train, y.train)
  tree.gini <- construct.treeRK(x.train, y.train,
                                min.num.obs.end.node.tree = 6, entropy = FALSE)
  tree.entropy$covariate.names
  tree.gini$flag # ...etc...
}
\author{
 Hyunjin Cho, \email{h56cho@uwaterloo.ca}
 Rebecca Su, \email{y57su@uwaterloo.ca}
}
\seealso{
\code{\link{pred.treeRK}}
}
\keyword{construct.tree}
\keyword{treeRK}
