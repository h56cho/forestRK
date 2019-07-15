##' A var.used.forestRK function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Spits out the list of covariates used to perform the splits to
##'        generate a particular tree(s) of a forestRK object.
##' @param forestRK.object a forestRK object.
##' @param tree.index a vector storing the indices of the trees that we are
##'                   interested to examine.
##' @return a list of vectors of names of covariates that was used for the splits
##'         to construct the given tree(s) in a forestRK model.
##' @examples
##' ## example: iris dataset
##' ## load the forestRK package
##' library(forestRK)
##'
##' # covariates of training data set
##' x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),]
##' y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new
##'
##' #random forest
##' # min.num.obs.end.node.tree is set to 5 by default;
##' # entropy is set to TRUE by default
##' forestRK.1 <- forestRK(x.train, y.train, nbags=100, samp.size=100)
##'
##' # prediction from a random forest RK
##' covariate.used.for.split.tree <- var.used.forestRK(forestRK.1, tree.index=c(4,5,6))
##' covariate.used.for.split.tree[["6"]] # retrieve the list of covariates used for splitting for the 'tree #6'
var.used.forestRK <- function(forestRK.object = forestRK(), tree.index = c()){

    ## Extract the original matrix X from the forestRK.object
    x.original <- forestRK.object$X

    ## Sanity checks
    if(is.null(forestRK.object)){
        stop("'forestRK.object' has to be provided in the function call")
    }

    if(!(dim(x.original)[1] > 1) || !(dim(x.original)[2] >= 1) || is.null(x.original)){
        stop("Invalid dimension for the dataset 'x.original'")
    }

    if(!(length(tree.index) >= 1) || is.null(tree.index)){
        stop("Invalid length for the vector 'tree.index'")
    }

    ## 1) Setup
    forest.rk.tree.list <- forestRK.object$forest.rk.tree.list
    covariate.split.vec <- c()
    covariates.used.list <- list()
    length(covariates.used.list) <- length(tree.index)
    names(covariates.used.list) <- as.character(levels(as.factor(tree.index)))

    ## 2) For each tree in a forestRK model, get the list of covariates that were used for splitting
    for(i in 1:length(tree.index)){
        covariate.split.vec <- c(covariate.split.vec, ((forest.rk.tree.list)[[tree.index[i]]])$covariate.split[-1,1])
        covariates.used.in.bag <- c((colnames(x.original))[covariate.split.vec])

        ## Now the vector covariates.used.in.bag is a character vector of original names of the covariates that were used for constructing a tree
        covariates.used.list[[i]] <- covariates.used.in.bag
    }

    ## 3) Return the list of covariate names used for splitting
    covariates.used.list
}
