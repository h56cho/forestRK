##' A criteria.calculator function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Calculates Entropy or Gini Index of a particular node before (or without)
##'        a split.
##' @param x.node numericized data frame (obtained via x.organizer()) of
##'               covariates of a particular node before (or without) a split;
##'               x.node should contain no NA or NaN's.
##' @param y.new.node numericized vector of class type (y) of a particular node
##'                   before (or without) splitting it; y.new.node should contain
##'                   no NA or NaN's.
##' @param entropy TRUE if entropy is used as the splitting criteria; FALSE if
##'                gini index is used as the splitting criteria. Default is set
##'                to TRUE.
##' @return the Entropy or the Gini Index of a particular node (criteria).
##' @return logical value (TRUE or FALSE) of the parameter 'entropy' (ent.status).
##' @examples
##' ## example: iris dataset
##' library(forestRK) # load the package forestRK
##'
##' # covariates of training data set
##' x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),]
##' # numericized class types of observations of training dataset
##' y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new
##'
##' ## criteria.calculator() example
##' ## calculate the Entropy of the original training dataset
##' criteria.calculator(x.node = x.train, y.new.node = y.train)
##' ## calculate the Gini Index of the original training dataset
##' criteria.calculator(x.node = x.train, y.new.node = y.train, entropy = FALSE)
criteria.calculator <- function(x.node = data.frame(), y.new.node = c(), entropy = TRUE){

    ## Load the library rapportools
    ## library(rapportools)

    ## Sanity check
    if(!(dim(x.node)[1] > 1) || !(dim(x.node)[2] >= 1) || is.null(dim(x.node))){
        stop("Invalid dimension for x.node")
    }

    if(!(length(y.new.node) > 1) || is.null(y.new.node)){
        stop("Invalid length of y.new for this node")
    }

    if(!(dim(x.node)[1] == length(y.new.node))){
        stop("Number of observations in the x matrix and the vector of y's do not match")
    }

    if(!(is.boolean(entropy))){
        stop("'The parameter 'entropy' has to be either TRUE or FALSE")
    }

    ## Setup
    vec.levels <- unique(y.new.node)
    ent.status <- entropy # TRUE if entropy is used, FALSE if Gini Index is used
    n <- dim(x.node)[1] # extract number of observations in the data frame x.node
    sum.vec <- rep(0, length(vec.levels)) # sum.vec is an empty vector that stores how many observations fall under each level of y.new (numericized class type)

    ## Place appropriate values into sum.vec
    for (t in 1:(length(vec.levels))){
        sum.vec[t] <- sum(y.new.node %in% vec.levels[t])
    }

    prop.vec <- sum.vec / n # prop.vec is a vector containing the proportions of the observations in a particular node that have their y.new value equal to 't'

    ## 1) Calculate the value of entropy of this particular node if ent.status == TRUE
    if (ent.status == TRUE){
        criteria <- 0 # initial value of the entropy of the particular node

        # calculate entropy of the particular node before any split
        for(k in 1:(length(prop.vec))){
            criteria2 <- - prop.vec[k] * ifelse(prop.vec[k] > 0, log2(prop.vec[k]), 0)
            criteria <- criteria + criteria2
        }
    }

    ## 2) Calculate the value of gini index of this particular node if ent.status == FALSE
    else {criteria <- 1 - sum((prop.vec)^2)}

    results <- list(criteria, ent.status)
    names(results) <- c("criteria", "ent.status")

    ## Return the entropy or gini index (criteria) of the particular node that we are dealing with, as well as the entropy status (ent.status)
    results
}


