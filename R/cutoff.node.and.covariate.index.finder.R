##' A cutoff.node.and.covariate.index.finder function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Identifies optimal cutoff point of an impure dataset for splitting,
##'        in terms of Entropy or Gini Index.
##' @param x.node a numericized data frame (obtained after applying x.organizer)
##'               of covariates of the observations from a particular node prior
##'               to the split; x.node should contain no NA or NaN's.
##' @param y.new.node  a vector storing numericized class type y of the observations
##'                    from a particular node before the split; y.new.node should
##'                    contain no NA or NaN's.
##' @param entropy TRUE if Entropy is used as the splitting criteria;
##'                FALSE if Gini Index is used as the splitting criteria.
##'                Default is set to TRUE.
##' @return the value at which the split occurs (cutoff.value).
##' @return the index of the observation on which split should occur (cutoff.node).
##' @return numeric index of the covariate at which the split should occur (covariate.ind).
##' @return the kididsplit output of the optimal split (split.record.optimal).
##' @examples
##' ## example: iris dataset
##' ## load the forestRK package
##' library(forestRK)
##'
##' # covariates of training data set
##' x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),]
##' y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new
##'
##' # implementation of cutoff.node.and.covariate.index.finder()
##' res <- cutoff.node.and.covariate.index.finder(x.train, y.train, entropy = FALSE)
##' res$cutoff.value
##' res$cutoff.node
##' res$covariate.ind
##' res$split.record.optimal
cutoff.node.and.covariate.index.finder <- function(x.node = data.frame(), y.new.node = c(), entropy = TRUE){

    ## Import the partykit and rapportools package
    ## library(partykit)
    ## library(rapportools)

    ## Sanity check
    if(!(dim(x.node)[1] > 1) || !(dim(x.node)[2] >= 1) || is.null(dim(x.node))){
        stop("Invalid dimension for x.node")
    }

    ## Setup
    ent.status <- entropy # TRUE if entropy is used for the splitting criteria; FALSE is Gini Index is used for the splitting criteria
    number.of.columns.of.x.node <- dim(x.node)[2] # number.of.columns.of.x.node == total number of covariates that we consider
    m.try <- sample(1:(number.of.columns.of.x.node), 1) # m.try = the randomly chosen number of covariates that we should consider for splitting
    # essential step for implementing the forestRK algorithm
    K <- sample(1:(number.of.columns.of.x.node), m.try, replace = FALSE) # once m.try is determined, sample "m.try covariates" from the given list of all covariates
    # essential step for implementing the forestRK algorithm

    ## Another rounds of sanity checks
    if(!(length(y.new.node) > 1) || is.null(y.new.node)){
        stop("Invalid length of y.new.node")
    }

    if(!(dim(x.node)[1] == length(y.new.node))){
        stop("The number of observations in x.node and y.new.node do not match")
    }

    if(!is.boolean(ent.status)){
        stop("'ent.status' has to be either TRUE or FALSE")
    }

    if(!(m.try >= 1)){
        stop("Invalid value for m.try (has to be greater than or equal to 1)")
    }

    if(!(length(K) >= 1)){
        stop("The number of covariates under consideration (K) is not greater than or equal to 1")
    }

    ## Find cutoff node and covariate index that gives optimal split in terms of the chosen splitting criteria
    ## 1) First fix the covariate to be the first covariate of the vector K, and consider only the first observation from the dataset x.node
    covariate.ind <- 1 # initial index of K chosen for splitting
    cutoff.node <- 1 # initial index of observation chosen for splitting (cutoff.node=1 means we are choosing the first observation in the particular node to be the point of split)
    sp.min <- partysplit(varid = K[covariate.ind], breaks = x.node[cutoff.node, K[covariate.ind]],  right = TRUE) # initial split that gives the minimum entropy (only for now)
    split.record.min <- kidids_split(sp.min, data = x.node) # initial split record that minimizes the splitting criteria (only for now)
    criteria.min <- criteria.after.split.calculator(x.node, y.new.node, split.record.min, ent.status) # minimum value of the entropy or Gini Index acheived by the optimal split (only for now)

    ## 2) Continue to fix the covariate to be the first covariate of the vector K, but now consider 2nd, 3rd, 4th, .... observations from the dataset x.node
    if (dim(x.node)[1] >= 2){
        # dim(x.node)[1] = number of observations in a particular node before the split

        # the index m here stands for the value of cutoff.node
        for (m in 2:(dim(x.node)[1])){
            sp <- partysplit(varid = K[covariate.ind], breaks = x.node[m, K[covariate.ind]], right = TRUE)
            split.record <- kidids_split(sp, data = x.node)
            criteria.new <- criteria.after.split.calculator(x.node, y.new.node, split.record, ent.status)

            # update criteria.min and cutoff.node (note: we did not alter the covariate.ind at this stage; covariate.ind == 1)
            if(criteria.new < criteria.min){
                criteria.min <- criteria.new
                cutoff.node <- m
            }
        }
    }

    ## 3) If the length of the vector K is greater than or equal to 2, continue the same kind of updating process for criteria.min, cutoff.node, and
    ## covariate.index for the subsequent covariates stored in K, over all observations stored in the data frame x.node
    if ((length(K)) >= 2){

        # the index i here stands for the value of covariate.ind
        # the index j here stands for the value of cutoff.node
        for (i in 2:(length(K))){
            for (j in 1:((dim(x.node))[1])){
                sp <- partysplit(varid = K[i], breaks = x.node[j,K[i]], right = TRUE)
                split.record <- kidids_split(sp, data = x.node)
                criteria.new <- criteria.after.split.calculator(x.node, y.new.node, split.record, ent.status)

                # update the variables criteria.min, cutoff.node, and covariate.ind as the loop iterates
                if(criteria.new < criteria.min){
                    criteria.min <- criteria.new
                    cutoff.node <- j
                    covariate.ind <- i
                } # end of if(criteria.new < criteria.min)
            } # end of for(j in 1:((dim(x.node))[1]))
        } # end of for(i in 2:(length(K)))
    } # end of if ((length(K)) >= 2) statement


    cutoff.value <- x.node[cutoff.node, K[covariate.ind]] # cutoff.node = the value n of n-th observation in the data frame x.node where split should be based on.
    # cutoff.value = the value of the covariate K[covariate.ind] at which the split should take place.
    covariate.ind <- K[covariate.ind] # now the variable covariate.ind stands for the actual numerical index of the covariate (out of 1:(number.of.columns.of.x.node) covariates) that should be used for a split.

    ## Generate the record of the optimal split
    sp.optimal <- partysplit(varid = covariate.ind, breaks = x.node[cutoff.node, covariate.ind], right = TRUE)
    split.record.optimal <- kidids_split(sp.optimal, data = x.node)

    results <- list(cutoff.value, cutoff.node, covariate.ind, split.record.optimal)
    names(results) <- c("cutoff.value", "cutoff.node", "covariate.ind", "split.record.optimal")

    ## Return the results of the function
    results
}
