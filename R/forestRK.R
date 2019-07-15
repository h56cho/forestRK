##' A forestRK function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Builds up a random forest RK onto the given (training) dataset.
##' @param X a numericized data frame storing covariates of each observation
##'          from the given (training) dataset (obtained via x.organizer());
##'          X should contain no NA or NaN's.
##' @param Y.new a vector storing the numericized class types of each observation
##'              from the given (training) dataset X; Y.new should contain no NA
##'              or NaN's.
##' @param min.num.obs.end.node.tree the minimum number of observations that we
##'                                  want each of our end node of our tree to contain.
##'                                  Default is set to 5.
##' @param nbags number of bootstrap samples that we want to generate.
##' @param samp.size number of samples that we want each of our bootstrap samples
##'                  to contain.
##' @param entropy TRUE if we use entropy as the splitting criteria;
##'                FALSE if we use the gini index for the splitting criteria.
##'                Default is set to TRUE.
##' @return The original dataset used to construct the random forest RK (X).
##' @return A list of trees in the generated random forest RK (forest.rk.tree.list).
##' @return A list containing data frames of bootstrap samples that were generated
##'         from the dataset X (bootsamp.list).
##' @return The value of the parameter 'entropy' (ent.status).
##' @examples
##' ## example: iris dataset
##' ## load the forestRK package
##' library(forestRK)
##'
##' # covariates of training data set
##' x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),]
##' y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new
##'
##' # Implement forestRK function
##' # min.num.obs.end.node.tree is set to 5 by default;
##' # entropy is set to TRUE by default
##' forestRK.1 <- forestRK(x.train, y.train, nbags = 100, samp.size = 100)
##' forestRK.1.tree <- forestRK(x.train, y.train, min.num.obs.end.node.tree = 6,
##'                             nbags = 100, samp.size = 100, entropy = FALSE)$forest.rk.tree.list[[1]]
forestRK <- function(X = data.frame(), Y.new = c(), min.num.obs.end.node.tree = 5, nbags, samp.size, entropy = TRUE){

    ## Import the rapportools package
    ## library(rapportools)

    ## Sanity check
    if(!(dim(X)[1] > 1) || !(dim(X)[2] >= 1) || is.null(X)){
        stop("Invalid dimension for the dataset X")
    }

    if(!(length(Y.new) > 1) || is.null(Y.new)){
        stop("Invalid length for the vector Y.new")
    }

    if(!(dim(X)[1] == length(Y.new))){
        stop("The number of observations in the dataset X and the vector Y.new do not match")
    }

    if(!(min.num.obs.end.node.tree > 1)){
        stop("'min.num.obs.end.node.tree' (minimum number of data points contained in each end node) has to be greater than 1")
    }

    if(is.null(nbags) || !(nbags > 1)){
        stop("'nbags' (number of bags to be generated) needs to be greater than or equal to 2")
    }

    if(is.null(samp.size) || !(samp.size > 1)){
        stop("'samp.size' (sample size for each bag) needs to be greater than or equal to 2")
    }

    if(!(is.boolean(entropy))){
        stop("The parameter 'entropy' has to be either TRUE or FALSE")
    }

    ## 1) Setup
    ent.status <- entropy
    n.col.X <- dim(X)[2] # total number of covariates in the given (training) dataset that we are dealing with
    dat <- data.frame(X, Y.new) # make a data frame that we will pass as an argument in the bstrap function
    forest.rk.tree.list <- list() # make a list to store trees in the forest rk model
    bootsamp.list <- bstrap(dat, nbags, samp.size) # bootsamp.list is a list containing data frames of bootstrap samples

    ## 2) Generate trees for a random forest RK model
    for (z in 1:(length(bootsamp.list))){
        forest.rk.tree.list[[z]] <- construct.treeRK(bootsamp.list[[z]][, 1:n.col.X], as.vector(bootsamp.list[[z]][ , (n.col.X + 1)]), min.num.obs.end.node.tree, ent.status)
    }

    ## 3) Return the results
    results <- list(X, forest.rk.tree.list, bootsamp.list, ent.status)
    names(results) <- c("X", "forest.rk.tree.list", "bootsamp.list", "ent.status") #forest.rk.tree.list = list of trees in the forest

    # Return the results
    results
}
