\name{mds.plot.forestRK}
\alias{mds.plot.forestRK}
\title{
 Makes 2D MDS (multidimensional scaling) \code{ggplot} of the test observations
 based on the predictions from a \code{forestRK} model.
}
\usage{
mds.plot.forestRK(pred.forestRK.object = pred.forestRK(),
 plot.title ="MDS Plot of Test Data Colour Coded by Forest RK Model Predictions",
 xlab ="First Coordinate", ylab = "Second Coordinate",
 colour.lab = "Predictions By The Random Forest RK Model")
}
\arguments{
 \item{pred.forestRK.object}{
 	a \code{pred.forestRK()} object.
 }

 \item{plot.title}{
 	an user specified title for the mds plot; the default is
 	"MDS Plot of Test Data Colour Coded by Forest RK Model Predictions".
 }

 \item{xlab}{
 	label for the x-axis of the plot; the default is "First Coordinate".
 }

 \item{ylab}{
 	label for the y-axis of the plot; the default is "Second Coordinate".
 }

 \item{colour.lab}{
 	label title for the legend that specifies categories for each colour;
 	the default is "Predictions By The Random Forest RK Model".
 }
}
\value{
 A multidimensional scaling ggplot (2D) of the test observations, colour coded
 by their predicted class types.
}
\description{
 Plots 2D MDS (Multi-Dimensional Scaling) \code{ggplot} of the test observations
 based on the provided \code{forestRK} model, and each test observation is
 colour coded by their predicted class types.

 The plot also has legends that tells user which colour pertains to which
 predicted class type.

 The existing R functions \code{dist} and \code{cmdscale} were used in this
 function to compute the Multi-Dimensional Scales of the test data.
}
\examples{
  ## example: iris dataset
  ## load the forestRK package
  library(forestRK)

  x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),]
  x.test <- x.organizer(iris[,1:4], encoding = "num")[c(26:50,76:100,126:150),]
  y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new
  y.factor.levels <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.factor.levels

  # min.num.obs.end.node.tree is set to 5 by default;
  # entropy is set to TRUE by default
  # typically the nbags and samp.size has to be much larger than 30 and 50
  pred.forest.rk <- pred.forestRK(x.test = x.test,
                                  x.training = x.train, y.training = y.train,
                                  nbags = 30, samp.size = 50,
                                  y.factor.levels = y.factor.levels)

  # generate a classical mds plot of test observations
  # and colour code them by the predicted class
  mds.plot.forestRK(pred.forest.rk)
}
\author{
 Hyunjin Cho, \email{h56cho@uwaterloo.ca}
 Rebecca Su, \email{y57su@uwaterloo.ca}
}
\seealso{
\code{\link{forestRK}}
}
\keyword{mds.plot}
\keyword{forestRK}
\keyword{plot}
