##' A construct.treeRK function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Constructs a classification tree based on the dataset of interest by
##'        implementing the RK (Random 'K') algorithm.
##' @param x.train a numericized data frame (obtained after applying x.organizer)
##'                of covariates of the dataset of interest (typically the training set)
##'                that we had before any splitting took place. x.train should contain
##'                no NA or NaN's.
##' @param y.new.train a numericized class types of the observations of the
##'                    dataset of interest (typically the training set) that we
##'                    had before any splitting took place. y.new.train should
##'                    contain no NA or NaN's.
##' @param min.num.obs.end.node.tree the minimum number of observations that we
##'                                  want each of the end nodes of our tree to
##'                                  contain. Default is set to '5'.
##' @param entropy TRUE if Entropy is used as the splitting criteria;
##'                FALSE if Gini Index is used as the splitting criteria.
##'                Default is set to TRUE.
##' @return vector of names of all covariate (covariate.names).
##' @return length of the flag (l).
##' @return a list containing children nodes of the numericized data frame x.train
##'         (x.node.list).
##' @return a list containing children nodes of the numericized vector of class
##'         type y.new.train (y.new.node.list).
##' @return hierchical flag (flag).
##' @return a matrix that lists the covariates used for splitting (covariate.split).
##' @return a vector that lists the values at which the node was split (value.at.split).
##' @return a matrix that lists the amount of decrease in splitting criteria
##'         after each split (amt.decrease.criteria).
##' @return a matrix that lists the number of observation in each parent node
##'         right before each split (num.obs)
##' @examples
##' ## example: iris dataset
##' ## load the forestRK package
##' library(forestRK)
##'
##' # covariates of training data set
##' x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),]
##' y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new
##'
##' # Construct a tree
##' # min.num.obs.end.node.tree is set to 5 by default;
##' # entropy is set to TRUE by default
##' tree.entropy <- construct.treeRK(x.train, y.train)
##' tree.gini <- construct.treeRK(x.train, y.train, min.num.obs.end.node.tree = 6, entropy = FALSE)
##' tree.entropy$covariate.names
##' tree.gini$flag # ...etc...
construct.treeRK <- function(x.train = data.frame(), y.new.train = c(), min.num.obs.end.node.tree = 5, entropy = TRUE){

    ## Import the rapportools package
    ## library(rapportools)

    ## Extract covariate names from the original dataset
    covariate.names <- colnames(x.train)
    ## Extract the value of the parameter 'entropy'
    ent.status <- entropy

    ## Sanity check
    if(!(dim(x.train)[1] > 1) || !(dim(x.train)[2] >= 1) || is.null(dim(x.train))){
        stop("Invalid dimension for x.train")
    }

    if(!(length(y.new.train) > 1) || is.null(y.new.train)){
        stop("Invalid lengths for y.new.train")
    }

    if(!(dim(x.train)[1] == length(y.new.train))){
        stop("The number of observations in x.train and y.new.train do not match")
    }

    if(!(min.num.obs.end.node.tree > 1)){
        stop("Minimum number of observations allowed in each end node has to be greater than 1")
    }

    if(!is.boolean(entropy)){
        stop("The parameter 'entropy' has to be either TRUE or FALSE")
    }

    ## Construct tree
    ## 1) Setup
    x.node.list <- list(x.train) # x.node.list is a list that stores data frames of x.node that are created by each split(parents and their children)
    y.new.node.list <- list(y.new.train) # y.new.node.list is a list that stores numericized class types for the nodes created by each split
    covariate.split <- rbind(NA) # the covariate from the current data set (current node) on which the next split should take place
    value.at.split <- rbind(NA) # the cutoff.value of the current node at which the next split should take place
    amt.decrease.criteria <- rbind(NA) # amount of decrease in entropy after the split
    num.obs <- rbind(NA) # number of observations contained in each parent node right before the split
    flag <- rbind("r") # hierarchical flag

    l <- 1 # set initial value for 'l'

    ## 2) Tree construction
    while(l <= length(x.node.list)){ # stopping condition

        # calculate the value of the splitting criteria of the node of our interest prior to the split
        criteria.x.node <- criteria.calculator(x.node.list[[l]], y.new.node.list[[l]], entropy = ent.status)$criteria

        # if the current node is not pure, and if the number of observations contained in the current node is greater than or equal to the value of min.num.obs.end.node.tree.....
        if(dim(x.node.list[[l]])[1] >= min.num.obs.end.node.tree && criteria.x.node != 0){
            obj <- cutoff.node.and.covariate.index.finder(x.node.list[[l]], y.new.node.list[[l]], ent.status)
            # K.split.x.node = the numerical index K of the covariate on which the next split should take place
            K.split.x.node <- obj$covariate.ind
            # cutoff.node.x.node = observation index of the cutoff node on which the the next split should take place
            cutoff.node.x.node <- obj$cutoff.node
            # obtain the split record of the optimal split on the current node
            split.record.optimal.x.node <- obj$split.record.optimal

            # split the current node
            pos <- which(split.record.optimal.x.node == 1)
            x1 <- x.node.list[[l]][pos,] # a child node 1
            x2 <- x.node.list[[l]][-pos,] # a child node 2
            y.new.1 <- y.new.node.list[[l]][pos]
            y.new.2 <- y.new.node.list[[l]][-pos]
            # calculate the value of the splitting criteria of our choice after the split
            criteria.x.node.after.split <- criteria.after.split.calculator(x.node.list[[l]], y.new.node.list[[l]], split.record.optimal.x.node, entropy = ent.status)

            # retain the split only if the number of observations contained in the children nodes x1 and x2 are greater than or equal to the value of min.num.obs.end.node.tree,
            # and only if the splitting criteria of the node has decreased after the split found from executing cutoff.node.and.covariate.index.finder() above
            if (dim(x1)[1] >= min.num.obs.end.node.tree && dim(x2)[1] >= min.num.obs.end.node.tree && criteria.x.node > criteria.x.node.after.split){
                # calculate the amount of decrease in entropy after split (= (ent. before split) - (ent. after split))
                amt.decrease.criteria.ind <- criteria.x.node - criteria.x.node.after.split
                # update amt.decrease.ent after the split takes place
                amt.decrease.criteria <- rbind(amt.decrease.criteria, amt.decrease.criteria.ind)
                # update the list of number of observations contained in the parent node right before the time of split
                number.of.data.point <- dim(x.node.list[[l]])[1]
                num.obs <- rbind(num.obs, number.of.data.point)

                # update the x.node.list and y.new.node.list
                x.node.list[length(x.node.list)+1] <- list(x1)
                x.node.list[length(x.node.list)+1] <- list(x2)
                y.new.node.list[length(y.new.node.list)+1] <- list(y.new.1)
                y.new.node.list[length(y.new.node.list)+1] <- list(y.new.2)

                # save information by making flags
                flag <- rbind(flag, paste0(flag[l],"x"))
                flag <- rbind(flag, paste0(flag[l],"y"))

                # update covariate.split and value.at.split after the split takes place
                # covariate.split is a vector that stores the numeric indices of covariates that were used for the series of split as we construct our RK tree
                covariate.split <- rbind(covariate.split, K.split.x.node)
                value.at.split <- c(value.at.split, round(x.node.list[[l]][cutoff.node.x.node, K.split.x.node],3))

                l <- l + 1 #update the value of 'a'
            } # end of if(dim(x1)[1] >= min.num.obs.end.node.tree && dim(x2)[1] >= min.num.obs.end.node.tree && criteria.x.node > criteria.x.node.after.split) statement

            # if the number of observations contained in children nodes x1 and/or x2 are less than the value of min.num.obs.end.node.tree,
            # or if criteria.x.node <= criteria.x.node.after.split do not retain the split;
            # just update the index 'l' so that we can jump to the next node
            else{ l <- l + 1 }

        } # end of if(dim(x.node.list[[l]])[1] >= min.num.obs.end.node.tree && criteria.x.node != 0) statement

        # if the number of observations in the current node is less than min.num.obs.end.node.tree, or if the current node is already pure, do not perform any splits
        # just update the index 'l' so that we can jump to the next node
        else { l <- l + 1 }

    } # closure of the while loop

    ## 3) Gather all of the information about the tree that we just constructed
    tr <- list(covariate.names, l, x.node.list, y.new.node.list, flag ,covariate.split, value.at.split, amt.decrease.criteria, num.obs) #information about tree
    names(tr) <- c("covariate.names", "l", "x.node.list", "y.new.node.list", "flag", "covariate.split", "value.at.split", "amount.decrease.criteria", "num.obs")

    ## 4) Return the tree that we just constructed
    tr
}

