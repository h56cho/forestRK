##' An importance.forestRK function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Calculates importance of each covariate considered in the forestRK model.
##' @param forestRK.object a forestRK object.
##' @return a vector containing the list of covariate names ordered from the most
##'         important to the least important (importance.covariate.names).
##' @return a vector that contains average decrease in splitting criteria across
##'         all trees of the random forest by each covariate
##'         (average.decrease.in.criteria.vec).
##' @return a boolean variable that is TRUE is entropy is used, and is FALSE if
##'         Gini Index is used (ent.status).
##' @return the dataset containing covariate values that was used to construct
##'         forestRK.object (x.original).
##' @examples
##' ## example: iris dataset
##' ## load the forestRK package
##' library(forestRK)
##'
##' # covariates of training data set
##' x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),]
##' y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new
##'
##' # random forest
##' # min.num.obs.end.node.tree is set to 5 by default;
##' # entropy is set to TRUE by default
##' forestRK.1 <- forestRK(x.train, y.train, nbags=100, samp.size=100)
##' # execute importance.forestRK function
##' imp <- importance.forestRK(forestRK.1)
importance.forestRK <- function(forestRK.object = forestRK()){

    ## Sanity check
    if(is.null(forestRK.object)){
        stop("'forestRK.object' needs to be provided in the function call")
    }

    ## 1) Set up
    x.ori <- forestRK.object$X
    ent.status <- forestRK.object$ent.status
    tree.list  <- forestRK.object$forest.rk.tree.list

    # reminder: a tree object contains: ("a", "x.node.list","y.new.node.list","flag","covariate.split","value.at.split","amount.decrease.entropy")
    length.forest.rk.tree.list <- length(tree.list) #number of trees in the random forest
    n.covariates <- dim(x.ori)[2] #total number of covariates from our original dataset
    total.decrease.in.criteria.mat <- matrix(nrow = length.forest.rk.tree.list, ncol = n.covariates, 0)
    average.decrease.in.criteria.vec <- rep(0, n.covariates) #a vector storing average decrease in entropy of each covariate

    ## 2) For each tree, calculate the total amount of decrease in splitting criteria (entropy or Gini Index) by each covariate considered.
    for(i in 1:length.forest.rk.tree.list){
        for(d in 1:n.covariates){
            amount.decrease.criteria <- as.vector((forestRK.object$forest.rk.tree.list[[i]])[["amount.decrease.criteria"]])
            covariate.split <- as.vector((forestRK.object$forest.rk.tree.list[[i]])[["covariate.split"]])

            ## Note: length(amount.decrease.criteria) == length(covariate.split)
            total.decrease.in.criteria <- 0
            len <- length(covariate.split)

            if(len >= 2){ # if at least one split had occurred while constructing the tree... (note: first entry of covariate.split is just NA)
                for (j in 2:len){ # the first index is skipped since the first entry of the amount.decrease.criteria, covariate.split, and num.obs are just NA
                    if(covariate.split[j] == d){total.decrease.in.criteria <- total.decrease.in.criteria + ((amount.decrease.criteria[j]) * ((tree.list[[i]])$num.obs)[j,1] / (dim(x.ori)[1]))}
                }
            }

            else{total.decrease.in.criteria <- 0} # when no split had occurred (e.g. due to smaller sample size)

            total.decrease.in.criteria.mat[i,d] <- total.decrease.in.criteria
            # the matrix total.decrease.in.criteria.mat has rows of trees and columns of covariates;
            # the matrix entries are the total (weighted) amount of decrease in entropy for each column of covariates
        } # end of for(d in 1:n.covariates)
    } # end of for(i in 1:length.forest.rk.tree.list)


    ## 3) Calculate the average decrease in the (weighted) splitting criteria for each covariate across all trees in the random forest
    for (j in 1:n.covariates){
        average.decrease.in.criteria.vec[j] <- (sum(total.decrease.in.criteria.mat[,j]))/(length.forest.rk.tree.list)
    }
    average.decrease.in.criteria.vec <- average.decrease.in.criteria.vec[order(average.decrease.in.criteria.vec, decreasing = TRUE)]

    ## 4) Change the entries of vec.important.covariate by their actual covariate names, from the most important to the least important
    importance.covariate.names <- colnames((x.ori))[order(average.decrease.in.criteria.vec, decreasing = TRUE)]

    ## 5) Make a list of results
    results <- list(importance.covariate.names, average.decrease.in.criteria.vec, ent.status, x.ori)
    names(results) <- c("importance.covariate.names","average.decrease.in.criteria.vec", "ent.status", "x.original")

    ## 6) Return the results
    results
}
