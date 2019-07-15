##' A get.tree.forestRK function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Extracts an individual tree structure from a forestRK object.
##' @param forestRK.object a forestRK() object.
##' @param tree.index a vector of indices of the trees that we want to extract
##'                   from the forestRK() object.
##' @return A list containing forestRK trees that have their indices specified
##'         in the function argument 'tree.index'.
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
##' #get tree
##' tree.index.ex <- c(1,3,8)
##' get.tree <- get.tree.forestRK(forestRK.1, tree.index = tree.index.ex)
##' get.tree[["8"]] # display the 8th tree of the random forest
get.tree.forestRK <- function(forestRK.object = forestRK(), tree.index=c()){

    ## Sanity checks
    if(is.null(forestRK.object)){
        stop("A 'forestRK.object' needs to be provided in the function call")
    }

    if(!(length(tree.index) >= 1)){
        stop("Invalid length for the vector 'tree.index'")
    }

    if(is.null(tree.index)){
        stop("'tree.index' needs to be provided in the function call")
    }

    ## 1) Setup
    tree.rk <- list()
    length(tree.rk) <- length(tree.index)
    # assign names into the list 'tree.rk'
    names(tree.rk) <- as.character(levels(as.factor(tree.index)))

    ## 2) Get Tree
    for(i in 1:length(tree.index)){
        tree.rk[i]<-(forestRK.object$forest.rk.tree.list)[tree.index[i]]
    }

    ## 3) Return the result
    # Return tree.rk
    tree.rk
}
