##' A mds.plot.forestRK function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Makes 2D mds (multidimensional scaling) ggplot of the test observations
##'        based on the predictions from a forestRK model.
##' @param pred.forestRK.object a pred.forestRK() object.
##' @param plot.title an user specified title for the mds plot;
##'                   the default is
##'                   "MDS Plot of Test Data Colour Coded by Forest RK Model Predictions".
##' @param xlab label for the x-axis of the plot; the default is "First Coordinate".
##' @param ylab label for the y-axis of the plot; the default is "Second Coordinate".
##' @param colour.lab label title for the legend that specifies categories for
##'                   each colour; the default is
##'                   "Predictions By The Random Forest RK Model".
##' @return a multidimensional scaling ggplot (2D) of the test observations,
##'         colour coded by their predicted class types.
##' @examples
##' ## example: iris dataset
##' ## load the forestRK package
##' library(forestRK)
##'
##' # covariates of training data set
##' x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),]
##' # covariates of test dataset
##' x.test <- x.organizer(iris[,1:4], encoding = "num")[c(26:50,76:100,126:150),]
##' y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new
##' y.factor.levels <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.factor.levels
##'
##' # min.num.obs.end.node.tree is set to 5 by default; entropy is set to TRUE by default
##' pred.forest.rk <- pred.forestRK(x.test = x.test, x.training = x.train, y.training = y.train,
##'                                 nbags = 100, samp.size = 50, y.factor.levels = y.factor.levels)
##'
##' #generate a classical mds plot and colour code by the predicted class
##' mds.plot.forestRK(pred.forest.rk)
mds.plot.forestRK <- function(pred.forestRK.object = pred.forestRK(), plot.title = "MDS Plot of Test Data Colour Coded by Forest RK Model Predictions", xlab ="First Coordinate", ylab = "Second Coordinate", colour.lab = "Predictions By The Random Forest RK Model"){

    ## Extract the x.test dataset from the pred.forest.rk object
    x.test <- pred.forestRK.object$x.test

    ## 1) Sanity check
    if(!(dim(x.test)[1] > 2) || !(dim(x.test)[2] >= 1) || is.null(x.test)){
        stop("Invalid dimension for the matrix x.test (to draw a 2D mds plot, we need at least 3 data points)")
    }

    if(is.null(pred.forestRK.object)){
        stop("'pred.forestRK.object' needs to be provided in the function call")
    }

    ## 2) Load the ggplot2 package
    ## library(ggplot2)

    ## 3) Generate mds plot of the test observations colour coded by the predicted class type
    x.test <- x.test[order(as.numeric(row.names(x.test))),] # Reorder the x.test data frame by increasing observation index
    pred.for.obs.forest.rk <- as.vector(pred.forestRK.object$pred.for.obs.forest.rk) # Retrive the data frame of predictions made by the forestRK model
    x.test <-cbind(x.test, pred.for.obs.forest.rk) # Last column (which is named "pred.for.obs.forest.rk") of the data frame x.test now stores predicted class from the forestRK model

    # calculate the cmd scale of the test observations
    x.dist <- dist(x.test[ ,1:(dim(x.test)[2]-1)])
    cmd.scale <- cmdscale(x.dist, eig = TRUE, k = 2)

    # plot the mds plot
    g <- ggplot(x.test, aes(x = cmd.scale$point[,1], y = cmd.scale$point[,2], color = pred.for.obs.forest.rk)) + geom_point()
    g2 <- g + labs(title = plot.title, x = xlab, y= ylab, colour = colour.lab)

    ## 4) Return the mds ggplot
    g2
}
