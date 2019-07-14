\name{importance.plot.forestRK}
\alias{importance.plot.forestRK}
\title{
 Generates importance \code{ggplot} of the covariates considered in the
 \code{forestRK} model
}
\usage{
 importance.plot.forestRK(importance.forestRK.object = importance.forestRK(),
                          colour.used = "dark green", fill.colour = "dark green",
                          label.size = 10)
}
\arguments{
 \item{importance.forestRK.object}{
 	an \code{importance.forestRK} object.
 }

 \item{colour.used}{
 	colour used for the border of the importance plot; default is "dark green".
 }

 \item{fill.colour}{
 	colour used to fill the bars of the importance plot; default is "dark green"
 	(yes, I like dark green).
 }

 \item{label.size}{
 	size of the labels; default is set to 10.
 }
}
\value{
 An importance plot of the covariates considered in the \code{forestRK} model,
 ordered from the most important covariate to the least important covariate.
}
\description{
 Generates importance \code{ggplot} of the covariates considered in the
 \code{forestRK} model.

 When the number of covariates under consideration is huge, it can be pretty
 difficult to read the covariate name from this plot. In this case, user can
 identify the name of the covariate that he or she is interested in by
 extracting \code{importance.covariate.names} from the
 \code{importance.forestRK.object} that was used in the function call.
 \code{importance.covariate.names} lists the original names of the covariates
 after ordering them from the most important to the least important. So for
 example, the exact name of the covariate that has the second highest importance
 would be the second element of the vector \code{importance.covariate.names},
 and so on.
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
  forestRK.1 <- forestRK(x.train, y.train, nbags = 30, samp.size = 50)
  # execute forestRK.importance function
  imp <- importance.forestRK(forestRK.1)

  # generate importance plot
  importance.plot.forestRK(imp)
}
\author{
 Hyunjin Cho, \email{h56cho@uwaterloo.ca}
 Rebecca Su, \email{y57su@uwaterloo.ca}
}
\seealso{
\code{\link{forestRK}}
}
\keyword{importance.plot}
\keyword{covariates}
\keyword{forestRK}
\keyword{importance}
\keyword{plot}
