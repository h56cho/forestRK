##' A y.organizer function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Numericizing the vector containing categorical class type(y) of the
##'        original (training) data.
##' @param y a vector containing the class type of each observation contained in
##'          the original (training) dataset; y should contain no NA or NaN's.
##' @return a numeric vector representing class type of each observation from the
##'         original (training) dataset (y.new).
##' @return a vector storing original names of the numericized class types
##'         (y.factor.levels).
##'
##' ***please do read the y.organizer section of the forestRK documentation for
##'  detailed explanation of the proper data cleaning steps using y.organizer
##' (apply y.organizer to training set only)***
##'
##' @examples
##' ## example: iris dataset
##' # load the package forestRK
##' library(forestRK)
##'
##' Basic Procedures:
##'  1. Extract the portion of the data that stores class type of each TRAINING
##'     observation, and make it as a vector
##'  2. apply y.organizer function to the vector obtained from 1
##'
##' y.train <- y.organizer(as.vector(iris[c(1:25,51:75,101:125),5]))
##' # retrieves the original names of each class type,
##' # if the class names were originally non-numeric
##' y.train$y.factor.levels
##' #retrieves the numericized vector that stores classification category
##' y.train$y.new
y.organizer <- function(y = c()){

    ## Sanity check
    if(!(length(y) > 1) || is.null(length(y))){
        stop("The length of the vector that stores class type is invalid (has to be greater than 1)")
    }

    ## Compute length of the vector y
    y.length <- length(y)

    ## Setup
    y.factor <- as.character(as.factor(y))
    y.factor.levels <- levels(as.factor(y)) # different levels of class types from the training dataset
    length.all.levels <- length(y.factor.levels)

    vec <- c(1:(length.all.levels))
    y.new.vec <- c()

    ## Numericize the original y vector
    for (i in 1:y.length){
        for (q in 1:length.all.levels){
            if(y.factor[i] == y.factor.levels[q]){y.factor[i] <- vec[q]}
        }
    }

    y.new <- as.vector(as.numeric(y.factor)) # y.new is a numericized y vector

    results <- list(y.factor.levels, y.new)
    names(results) <- c("y.factor.levels", "y.new")

    ## Return the results
    results
}
