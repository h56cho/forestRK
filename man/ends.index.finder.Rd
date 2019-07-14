\name{ends.index.finder}
\alias{ends.index.finder}
\title{
 Identifies numerical indices of the end nodes of a \code{rktree} from the
 matrix of hierarchical flags.
}
\usage{
 ends.index.finder(tr.flag = matrix())
}
\arguments{
 \item{tr.flag}{
 	a \code{construct.treeRK()$flag} object or a similar flag matrix.
 }
}
\value{
  A vector that lists the indices of the end nodes of a given \code{rktree}
  (indices that are consistent to the indices in \code{x.node.list},
  \code{y.new.node.list}, and \code{flag} that are returned by the
  \code{construct.treeRK} function).
}
\description{
 Identifies numerical indices of the end nodes of a \code{rktree} by
 closely examining the structure of the \code{rktree} flag (obtained via
 \code{construct.treeRK()$flag}); the precise algorithm used is the following:

 if m-th string in the list of \code{rktree} flag is a substring of one or
 more of (m + 1),...,n-th strings in the list of flag, then the node
 represented by the m-th string of the flag is not an end node; otherwise,
 the node represented by the m-th string of the flag is the end node.
}
\examples{
  ## example: iris dataset
  ## load the forestRK package
  library(forestRK)

  # covariates of training data set
  x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),]
  y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new

  # Construct a tree
  # min.num.obs.end.node.tree is set to 5 by default;
  # entropy is set to TRUE by default
  tree.entropy <- construct.treeRK(x.train, y.train)

  # Find indices of end nodes of tree.entropy
  end.node.index <- ends.index.finder(tree.entropy$flag)
}
\author{
 Hyunjin Cho, \email{h56cho@uwaterloo.ca}
 Rebecca Su, \email{y57su@uwaterloo.ca}
}
\seealso{
\code{\link{construct.treeRK}}
}
\keyword{end.nodes}
\keyword{treeRK}
