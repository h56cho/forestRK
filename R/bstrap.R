##' A bstrap function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Performs bootstrap sampling of our (training) dataset.
##' @param dat a numericized data frame that stores covariate of the observations
##'            as well as their numericized class types y; dat should contain no
##'            NA or NaN's.
##' @param nbags the number of bags or number of bootstrap samples that we want
##'              to generate.
##' @param samp.size the number of samples that each bag (individual bootstrap sample)
##'                  should contain.
##' @return A list containing a data frames of bootstrap samples.
##' @examples
##' ## example: iris dataset
##' ## load the forestRK package
##' library(forestRK)
##'
##' # covariates of training data set
##' x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),]
##' y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new
##' # combine the covariates x with class types y
##' b <- data.frame(cbind(x.train, y.train))
##'
##' ## bstrp function example
##' bootstrap.sample <- bstrap(dat = b, nbags = 20, samp.size = 30)
bstrap <- function(dat = data.frame(), nbags, samp.size){

    ## Sanity check
    if(!(dim(dat)[1] > 1) || !(dim(dat)[2] >= 1) || is.null(dat)){
        stop("Invalid dimension for the dataset dat")
    }

    if(is.null(nbags) || !(nbags > 1)){
        stop("'nbags' (number of bags to be generated) needs to be greater than or equal to 2")
    }

    if(is.null(samp.size) || !(samp.size > 1)){
        stop("'samp.size' (sample size for each bag) needs to be greater than or equal to 2")
    }

    ## 1) Setup
    n.obs <- dim(dat)[1] # number of observations in the data set 'dat'
    samp.index <- data.frame(matrix(nrow = samp.size, ncol = nbags, 0))
    for (i in 1:nbags){
        samp.index[,i] <- c(sample(1:n.obs, samp.size, replace = TRUE))
    }

    ## 2) Make a list of bootstrap sample
    bootsamp.list <- list()

    for (j in 1:nbags){
        bootsamp.list[[j]] <- dat[samp.index[,j],]
    }

    ## bootsamp.list is a list of data frames containing all of the covariates as well as the numericized class type 'y.new' for all observations
    ## 3) Return the bootsamp.list
    bootsamp.list
}
