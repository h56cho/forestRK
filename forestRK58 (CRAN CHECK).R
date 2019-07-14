# Based on the paper:
# "Forest-RK: A New Random Forest Induction Method" by Simon Bernard, Laurent Heutte, Sebastien Adam,
# 4th International Conference on Intelligent Computing (ICIC), Sep 2008, Shanghai, China, pp.430-437
#
# also based on the discussion: https://stats.stackexchange.com/questions/168964/building-a-regression-tree-with-r-from-scratch/168967#168967
#
# Author: Hyunjin-Dominique Cho, Rebecca Su
# Date: June 22, 2019
# Name: forestRK
# Version: v58 -- version 'passed CRAN checks' 
#                 (added BostonHousing Dataset Example at the end of the file)
#
# Comments:    1. since K is random, there is no point in making the cv function;
#                 let's not do stuffs that are unnecessary.
#
#


##' A y.organizer function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Numericizing the vector containing categorical class type(y) of the original (training) data.
##' @param y a vector containing the class type of each observation contained in the original (training) dataset; y should contain no NA or NaN's.
##' @return a numeric vector representing class type of each observation from the original (training) dataset (y.new).
##' @return a vector storing original names of the numericized class types (y.factor.levels).
##'
##' ***please do read the y.organizer section of the forestRK documentation for detailed explanation of the proper data cleaning steps using y.organizer
##' (apply y.organizer to training set only)***
##'
##' @examples
##' ## example: iris dataset
##' # load the package forestRK
##' library(forestRK)
##'
##' Basic Procedures:
##'  1. Extract the portion of the data that stores class type of each TRAINING observation, and make it as a vector
##'  2. apply y.organizer function to the vector obtained from 1
##'
##' y.train <- y.organizer(as.vector(iris[c(1:25,51:75,101:125),5]))
##' # retrieves the original names of each class type, if the class names were originally non-numeric
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


##' A x.organizer function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Numericizing a data frame of covariates from the original dataset via
##'        Binary Encoding (for categorical features with cardinality >= 1000) or Numeric Encoding (for categorical features with cardinality < 1000)
##' *** For more information about Binary and Numeric Encoding and their effectiveness, please visit:
##'  https://medium.com/data-design/visiting-categorical-features-and-encoding-in-decision-trees-53400fa65931***
##'
##' @param x.dat a data frame storing covariates of each observation (can be either numeric or non-numeric) from the original dataset; x.dat should contain no NA or NaN's.
##' @param encoding type of encoding that the user prefers to perform on the categorical feature(s); "num" for Numeric Encoding, or "bin" for Binary Encoding.
##' @return A data frame storing the numericized version of the covariates from the original dataset.
##'
##' ***please do read the x.organizer section of the forestRK documentation for detailed explanation of the proper data cleaning steps using x.organizer
##' (apply x.organizer to covariates of all observations after combining the training and the test set)***
##'
##' @examples
##' ## example: iris dataset
##' library(forestRK) # load the package forestRK
##'
##' Basic Procedures
##' 1. Apply x.organizer to a data frame that stores covariates of ALL observations
##'    (BOTH training and test observations)
##' 2. Split the output from 1 into a training and a test set, as needed
##'
##' # note: iris[,1:4] are the columns of the iris dataset that stores covariate values
##' x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),] # covariates of training data set
x.organizer <- function(x.dat = data.frame(), encoding = c("num","bin")){

    ## Sanity checks
    if(!(dim(x.dat)[1] > 1) || !(dim(x.dat)[2] >= 1) || is.null(dim(x.dat))){
        stop("Invalid dimension for the data matrix x.dat")
    }

    if(encoding == "bin"){
        ## Numercize the data frame of covariates via Binary Encoding
        x.new <- rep(0, dim(x.dat)[1])
        n.cov <- dim(x.dat)[2]
        for(j in 1:n.cov){
    		    if(!(is.factor(x.dat[,j]) || is.character(x.dat[,j]))){
    		        x.new <- data.frame(x.new, x.dat[,j])
    	          	colnames(x.new)[dim(x.new)[2]] <- colnames(x.dat)[j]
    		    }

    		    else if(is.factor(x.dat[,j]) || is.character(x.dat[,j])){
    			      x.bin  <- data.frame(matrix(as.integer(intToBits(as.integer(as.factor(x.dat[,j])))), ncol = 32,
    			                           nrow = length(x.dat[,j]),byrow = TRUE)[ ,1:ceiling(log(length(unique(x.dat[,j])) + 1)/log(2))])
    			      colnames(x.bin) <- paste(colnames(x.dat)[j], c(1:(dim(x.bin)[2])))
    			      x.new <- data.frame(x.new, x.bin)
    		    } # end of else if(is.factor(x.dat[,j]) || is.character(x.dat[,j]))
        } # end of for(j in 1:n.cov)

        x.new <- x.new[,-1]

    } # end of if(encoding == "bin")

    else if(encoding == "num"){
        ## Convert the original data frame into a numericized one via Numeric Encoding
        x.new <- data.frame(data.matrix(x.dat))
    }

    ## Return x.new, the numericized data frame of covariates of training and test observations
    data.frame(x.new)
}


##' A criteria.calculator function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Calculates Entropy or Gini Index of a particular node before (or without) a split.
##' @param x.node numericized data frame (obtained via x.organizer()) of covariates of a particular node before (or without) a split; x.node should contain no NA or NaN's.
##' @param y.new.node numericized vector of class type (y) of a particular node before (or without) splitting it; y.new.node should contain no NA or NaN's.
##' @param entropy TRUE if entropy is used as the splitting criteria; FALSE if gini index is used as the splitting criteria. Default is set to TRUE.
##' @return the Entropy or the Gini Index of a particular node (criteria).
##' @return logical value (TRUE or FALSE) of the parameter 'entropy' (ent.status).
##' @examples
##' ## example: iris dataset
##' library(forestRK) # load the package forestRK
##'
##' x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),] # covariates of training data set
##' y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new # numericized class types of observations of training dataset
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


##' A criteria.after.split.calculator function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Calculates Entropy or Gini Index of a node after a given split.
##' @param x.node numericized data frame (obtained via x.organizer()) of covariates of a particular node that is to be split; x.node should contain no NA or NaN's.
##' @param y.new.node numericized class type of each observation of a particular node that is to be split; y.new.node should contain no NA or NaN's.
##' @param split.record output of the kidids_split function from the partykit package for a given split.
##' @param entropy TRUE if Entropy is used as the splitting criteria; FALSE if Gini Index is used as the splitting criteria. Default is set to TRUE.
##' @return The value of Entropy or Gini Index of a particular node after a given split.
##' @examples
##' ## example: iris dataset
##' library(forestRK) # load the package forestRK
##' library(partykit)
##'
##' x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),] # covariates of training data set
##' y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new # numericized class types of observations of training dataset
##'
##' ## criteria.after.split.calculator() example in the implementation of the forestRK algorithm
##'
##' ent.status <- TRUE
##'
##' # number.of.columns.of.x.node = total number of covariates that we consider
##' number.of.columns.of.x.node <- dim(x.train)[2]
##' # m.try = the randomly chosen number of covariates that we consider at the time of split
##' m.try <- sample(1:(number.of.columns.of.x.node),1)
##' ## sample m.try number of covariates from the list of all covariates
##' K <- sample(1:(number.of.columns.of.x.node), m.try)
##'
##' # split the data (the choice of the type of split used here is only arbitrary)
##' # for more information about kidids_split, please refer to the documentation for the package 'partykit'
##' sp <- partysplit(varid=K[1], breaks = x.train[1,K[1]], index = NULL, right = TRUE, prob = NULL, info = NULL)
##' split.record <- kidids_split(sp, data=x.train)
##'
##' # implement critera.after.split function based on kidids_split object
##' criteria.after.split <- criteria.after.split.calculator(x.train, y.train, split.record, ent.status)
##' criteria.after.split
criteria.after.split.calculator <- function(x.node = data.frame(), y.new.node = c(), split.record = kidids_split(), entropy = TRUE){

    ## Load the library rapportools and partykit
    ## library(rapportools)
    ## library(partykit)

    ## Setup
    ent.status <- entropy # ent.status is TRUE if entropy is used; it is FALSE if Gini Index is used
    criteria.x.node <- criteria.calculator(x.node, y.new.node, ent.status)$criteria # extract the entropy or Gini Index of a particular node before splitting it
    unique.length.split.record <- length(unique(split.record))

    ## Sanity check
    if(!(dim(x.node)[1] > 1) || !(dim(x.node)[2] >= 1) || is.null(dim(x.node))){
        stop("Invalid dimension for x.node")
    }

    if(!(length(y.new.node) > 1) || is.null(y.new.node)){
        stop("Invalid length for the y.new.node")
    }

    if(!(dim(x.node)[1] == length(y.new.node))){
        stop("The number of observations in x.node and y.new.node do not match")
    }

    if(is.null(split.record)){
        stop("'split.record' (kikids_split object) has to be provided as a parameter")
    }

    if(!is.boolean(entropy)){
        stop("The parameter 'entropy' has to be either TRUE or FALSE")
    }

    if(!(unique.length.split.record == 1 || unique.length.split.record == 2)){
        stop("Error in 'split.record': the number of splits is not 1 or 2")
    }

    ## Calculate entropy or Gini Index of a particular node after a split
    ## First case: unique.length.split.record == 2
    if(unique.length.split.record == 2){
        length.split.record <- length(split.record) # total number of observations in the object split.record
        split.record.level <- levels(as.factor(split.record)) # split.record.level == c("1","2")
        length.split.record.level <- length(split.record.level) # length.split.record.level == 2
        criteria.vec <- rep(0, length.split.record.level) # make an empty vector of length 2 to store entropy or gini index of each split group
        n.vec <- rep(0, length.split.record.level) # make an empty vector of length 2 to store samples sizes for each split group

        for (z in 1:length.split.record.level){
            n.vec[z] <- length(which(split.record %in% as.numeric(c(split.record.level[z])))) # calculate sample sizes for each split group
            y.new.split.record <- y.new.node[which(split.record %in% as.numeric(c(split.record.level[z])))] # split the vector y.new.node by the split.record.level (1 or 2)
            vec.levels <- unique(y.new.split.record)
            vec2 <- rep(0, length(unique(y.new.split.record)))

            # 'vec2' is a vector that stores the number of observations for each class type 'p' for the split group that we are dealing with
            for (p in 1:(length(unique(y.new.split.record)))){
                vec2[p] <- sum(y.new.split.record %in% vec.levels[p])
            }

            # 'prop' is a vector that stores the proportion of observations for each class type 'p' for the split group that we are dealing with
            prop <- vec2/n.vec[z]

            # 1) if ent.status == TRUE, calculate entropy of the splitted node
            if (ent.status == TRUE){
                criteria <- 0 # initial value of the entropy

                for (y in 1:(length(prop))){
                    criteria2<- -prop[y] * ifelse(prop[y] > 0, log2(prop[y]), 0)
                    criteria <- criteria + criteria2
                }

            # criteria.vec in this case stores entropy of each of the two splits
            criteria.vec[z] <- criteria
            } #end of if(ent.status == TRUE) statement

            # 2) if entropy==FALSE, calculate Gini Index of the splitted node
            else {criteria.vec[z] <- 1-sum((prop)^2)} # criteria.vec in this case stores Gini Index of each of the two splits.
        } # end of for(z in 1:length.split.record.level) loop

        ## Calculate the entropy or Gini Index of the node after the split
        criteria.after.split <- criteria.vec[1] * n.vec[1] / (sum(n.vec)) + criteria.vec[2] * n.vec[2] / (sum(n.vec))

    } # end of the if (unique.length.split.record == 2) statement

    ## Second case: unique.length.split.record == 1
    else {criteria.after.split <- criteria.x.node}

    ## Return criteria.after.split
    criteria.after.split
}


##' A cutoff.node.and.covariate.index.finder function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Identifies optimal cutoff point of an impure dataset for splitting, in terms of Entropy or Gini Index.
##' @param x.node a numericized data frame (obtained after applying x.organizer) of covariates of the observations from a particular node prior to the split; x.node should contain no NA or NaN's.
##' @param y.new.node  a vector storing numericized class type y of the observations from a particular node before the split; y.new.node should contain no NA or NaN's.
##' @param entropy TRUE if Entropy is used as the splitting criteria; FALSE if Gini Index is used as the splitting criteria. Default is set to TRUE.
##' @return the value at which the split occurs (cutoff.value).
##' @return the index of the observation on which split should occur (cutoff.node).
##' @return numeric index of the covariate at which the split should occur (covariate.ind).
##' @return the kididsplit output of the optimal split (split.record.optimal).
##' @examples
##' ## example: iris dataset
##' ## load the forestRK package
##' library(forestRK)
##'
##' x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),] # covariates of training data set
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


##' A construct.treeRK function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Constructs a classification tree based on the dataset of interest by implementing the RK (Random 'K') algorithm.
##' @param x.train a numericized data frame (obtained after applying x.organizer) of covariates of the dataset of interest (typically the training set) that we had before any splitting took place. x.train should contain no NA or NaN's.
##' @param y.new.train a numericized class types of the observations of the dataset of interest (typically the training set) that we had before any splitting took place. y.new.train should contain no NA or NaN's.
##' @param min.num.obs.end.node.tree the minimum number of observations that we want each of the end nodes of our tree to contain. Default is set to '5'.
##' @param entropy TRUE if Entropy is used as the splitting criteria; FALSE if Gini Index is used as the splitting criteria. Default is set to TRUE.
##' @return vector of names of all covariate (covariate.names).
##' @return length of the flag (l).
##' @return a list containing children nodes of the numericized data frame x.train (x.node.list).
##' @return a list containing children nodes of the numericized vector of class type y.new.train (y.new.node.list).
##' @return hierchical flag (flag).
##' @return a matrix that lists the covariates used for splitting (covariate.split).
##' @return a vector that lists the values at which the node was split (value.at.split).
##' @return a matrix that lists the amount of decrease in splitting criteria after each split (amt.decrease.criteria).
##' @return a matrix that displays the number of observations of each parent node before each split(num.obs)
##' @examples
##' ## example: iris dataset
##' ## load the forestRK package
##' library(forestRK)
##'
##' x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),] # covariates of training data set
##' y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new
##'
##' # Construct a tree
##' # min.num.obs.end.node.tree is set to 5 by default; entropy is set to TRUE by default
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


##' An ends.index.finder function
##'
##' @author Hyunjin-Dominique Cho, Rebecca Su
##' @title Identifies numerical indices of the end nodes of a rktree.
##' @param tr.flag a construct.treeRK()$flag object or a similar flag matrix.
##' @return A vector that lists the indices of the end nodes of a given rktree (indices that corresponds to the indices in x.node.list and y.new.node.list that is returned by the construct.treeRK function).
##' @examples
##' ## example: iris dataset
##' ## load the forestRK package
##' library(forestRK)
##'
##' x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),] # covariates of training data set
##' y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new
##'
##' # Construct a tree
##' # min.num.obs.end.node.tree is set to 5 by default; entropy is set to TRUE by default
##' tree.entropy <- construct.treeRK(x.train, y.train)
##'
##' # Find indices of end nodes of tree.entropy
##' end.node.index <- ends.index.finder(tree.entropy$flag)
ends.index.finder <- function(tr.flag = matrix()){

    ## 1) Sanity check
    if(!(dim(tr.flag)[1] >= 1) || !(dim(tr.flag)[2] == 1) || is.null(tr.flag)){
        stop("The format of the tree flag provided is invalid")
    }

    ## 2) Setup
    end.node.index <- c() # a vector storing end nodes
    split.list <- list() # a list of vectors that store individual character component of a flag
    tf.vec <- c() # a vector storing TRUE or FALSE

    ## 3) Determine the end nodes of a tree
    ## (i) Split the flag into a series of individual characters
    for(i in 1:dim(tr.flag)[1]){
        split.list[i] <- strsplit(tr.flag[i,], "", fixed = FALSE)
    }

    ## (ii) If there were at least one split while constructing the tree (if(dim(tr.flag)[1] > 1)),
    ## try to match the string of v-th flag with the strings of (v + 1)-th, (v + 2)-th....., dim(tr.flag)[1] - th flags.
    ## if the string of v-th flag and the substring of a subsequent flag match, then the node depicted by the v-th flag is NOT an end node.
    ## otherwise, the node depicted by the v-th flag is an end node of the given tree.
    if(dim(tr.flag)[1] > 1){
        for (v in 1:((dim(tr.flag)[1]) - 1)){
            for(j in c((v+1):(dim(tr.flag)[1]))){
                tf.vec  <- c(tf.vec, (!(tr.flag[v,1] == substr(tr.flag[j,1], 1, length(split.list[[v]])))))
            } # end of for(j in c((v+1):(dim(tr.flag)[1]))) statement

            if(length(which(tf.vec == FALSE)) == 0){end.node.index <- c(end.node.index, v)}
            tf.vec <- c()
        } # end of for(v in 1:((dim(tr.flag)[1]) - 1)) statement

        end.node.index <- c(end.node.index, dim(tr.flag)[1]) #note: the node depicted by the very last flag is always an end node
    } # end of if(dim(tr.flag)[1] > 1) statement

    ## (iii) If no split was taken place to construct the tree, then the end.node.index is simply '1'
    else{end.node.index <- c(1)}

    ## 4) Return the end node indices
    end.node.index
}


##' A pred.treeRK function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Makes predictions on the test dataset based on the treeRK model provided.
##' @param X a numericized data frame (obtained via x.organizer()) storing covariates of the test dataset (or the dataset that we want to make predictions on); X should contain no NA or NaN's.
##' @param rktree a construct.treeRK() object.
##' @return a data frame of test observations with the predicted class types indicated under the very last column (prediction.df).
##' @return the flag generated from applying the rktree model to the test set (flag.pred).
##' @examples
##' ## example: iris dataset
##' ## load the forestRK package
##' library(forestRK)
##'
##' x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),] # covariates of training data set
##' x.test <- x.organizer(iris[,1:4], encoding = "num")[c(26:50,76:100,126:150),] # covariates of test dataset
##' y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new
##'
##' ## Construct a tree
##' # min.num.obs.end.node.tree is set to 5 by default; entropy is set to TRUE by default
##' tree.entropy <- construct.treeRK(x.train, y.train)
##' tree.gini <- construct.treeRK(x.train, y.train, min.num.obs.end.node.tree = 6, entropy = FALSE)
##'
##' ## Make predictions on the test set based on the constructed rktree model
##' # last column of prediction.df stores predicted class on the test observations based on a given rktree
##' prediction.df <- pred.treeRK(X = x.test, tree.entropy)$prediction.df
##' flag.pred <- pred.treeRK(X = x.test, tree.entropy)$flag.pred
pred.treeRK <- function(X = data.frame(), rktree = construct.treeRK()){

    ## load the library partykit
    ## library(partykit)

    ## 1) Setup
    ## Sanity check
    if(!(dim(X)[1] > 1) || !(dim(X)[2] >= 1) || is.null(X)){
        stop("Invalid dimension for the test dataset X")
    }

    if(is.null(rktree)){
        stop("A rktree (construct.tree() object) needs to be provided as one of the parameters")
    }

    if(!(dim(X)[2] == dim(rktree$x.node.list[[1]])[2])){
        stop("The number of covariates in the matrix X does not match with that of the dataset that was used to construct the given tree")
    }

    ## Get total number of covariates (from the test dataset)
    n.cov <- dim(X)[2]

    ## Extract bunch of information from the rktree objects
    value.at.split <- rktree$value.at.split
    covariate.split <- rktree$covariate.split
    y.new.node.list <- rktree$y.new.node.list
    x.node.list <- rktree$x.node.list # node: x.node.list stores nodes from the TRAINING dataset, NOT TEST dataset
    split.length <- length(value.at.split)
    end.node.index <- ends.index.finder(rktree$flag)
    flag.pred <- rbind("r")
    # reminder: X = the dataset on which we want to make predictions based on the model that we fitted in our training dataset
    X.node.list <- list(X)

    y.new.end.node.list <- y.new.node.list[end.node.index]
    # make an empty vector to store predictions from the training data for each end node
    training.pred <- rep(0, length(y.new.end.node.list))

    # get predictions from the training data
    for (i in 1:(length(y.new.end.node.list))){
        # making predictions for each end node by the majority vote
        training.pred[i] <- as.numeric(names(which.max(table(y.new.end.node.list[[i]]))))
    }

    ## 2) If split.length >= 2, compute classes from the training dataset
    ## If at least one split had occurred while constructing the rktree (if(split.length >= 2))...
    if(split.length >= 2){

        ## 3) Predict class types for the observations in test dataset based on the model fit from the training dataset
        ## NOTE: This part of the code can be seen as being unnecessarilary complicate,
        ##       but I chose to retain the code below in order to ensure the robustness of the function.

        # set initial values for l2, i, and a
        l2 <- 1 # index for x.node.list
        i <- 2 # index for covariate.split and value.at.split
        a <- 2 # index for flag.pred
        # create an empty vector that stores TRUE or FALSE
        tf.vec <- c()

        # build the flag.pred
        while(l2 <= length(x.node.list) && l2 <= length(X.node.list)){ # stopping condition
            for (j in 1:length(end.node.index)){
            # does the current value of l2 match with one of the indices of the end node of the rktree?
                tf.vec <- c(tf.vec, l2 == end.node.index[j])
            }

            ## If the value of l2 is not one of the indices of the end nodes of the rktree that we base our classifications on,
            ## (if(length(which(tf.vec == TRUE)) == 0)),
            ## split the test set at the covariate and the value specified in the matrices covariate.split and value.at.split.
            ## (recall: covariate.split and value.at.split are determined from fitting a tree on the training dataset)
            if(length(which(tf.vec == TRUE)) == 0){
            	sp <- partysplit(varid = covariate.split[i], breaks = value.at.split[i], right = TRUE)
            	split.record <- kidids_split(sp, data = X.node.list[[l2]])
            	pos <- which(split.record == 1)

                ## case (i): if the split given by covariate.split and value.at.split do not make one of the children nodes to be empty...
                if(length(pos) != 0 && length(pos) != dim(X.node.list[[l2]])[1]){
                    # split the current X.node (recall: 'X.node' == node of the test set; 'x.node' == node of the training set)
                    x1 <- X.node.list[[l2]][pos,]
                    x2 <- X.node.list[[l2]][-pos,]
                    X.node.list[length(X.node.list)+1] <- list(x1)
                    X.node.list[length(X.node.list)+1] <- list(x2)

                    # save information by updating flags
                    flag.pred <- rbind(flag.pred, paste0(flag.pred[a-1],"x"))
                    flag.pred <- rbind(flag.pred, paste0(flag.pred[a-1],"y"))

                    # update the values of 'l2', 'i', and 'a' after splitting the test set
                    l2 <- l2 + 1
                    i <- i + 1
                    a <- a + 1
                } # end of if(length(pos) != 0 && length(pos) != dim(X.node.list[[l2]])[1])

                ## case (ii): if the split given by covariate.split and value.at.split make the left child nodes to be empty (else if(length(pos) == 0))...
                else if(length(pos) == 0){
                    x1 <- NULL # assign NULL as the left child node (x1)
                    x2 <- X.node.list[[l2]] # assign the entire parent node as the right child node
                    # update X.node.list
                    X.node.list[length(X.node.list)+1] <- list(x1)
                    X.node.list[length(X.node.list)+1] <- list(x2)

                    # save information by updating flags
                    flag.pred <- rbind(flag.pred, paste0(flag.pred[a-1],"x"))
                    flag.pred <- rbind(flag.pred, paste0(flag.pred[a-1],"y"))

                    # update the values of 'l2', 'i', and 'a'
                    l2 <- l2 + 1
                    i <- i + 1
                    a <- a + 1
                } # end of else if(length(pos) == 0)

                ## case (iii): if the split given by covariate.split and value.at.split make the right child nodes to be empty
                ## (else if(length(pos) == dim(X.node.list[[l2]])[1]))...
                else if(length(pos) == dim(X.node.list[[l2]])[1]){
                    x1 <- X.node.list[[l2]] # assign the entire parent node as the left child node
                    x2 <- NULL # assign NULL as the right child node (x1)
                    # Update the X.node.list
                    X.node.list[length(X.node.list)+1] <- list(x1)
                    X.node.list[length(X.node.list)+1] <- list(x2)

                    # save information by updating flags
                    flag.pred <- rbind(flag.pred, paste0(flag.pred[a-1],"x"))
                    flag.pred <- rbind(flag.pred, paste0(flag.pred[a-1],"y"))

                    # update the values of 'l2', 'i', and 'a'
                    l2 <- l2 + 1
                    i <- i + 1
                    a <- a + 1
                } # end of else if(length(pos) == dim(X.node.list[[l2]])[1])
            } # end of if(length(which(tf.vec == TRUE)) == 0)

            ## If the value of l2 is one of the indices of the end nodes of the rktree that we base our classifications on,
            ## do not perform any kind of split, and keep the end node intact (and do not update the index 'i', since we are not performing any split).
            else { l2 <- l2 + 1; a <- a + 1 }
            tf.vec <- c()
        } # closure of the while(l2 <= length(x.node.list) && l2 <= length(X.node.list))
    } # closure of if(split.length >= 2)

    ## 4) If split.length < 2 (if we had performed no split to construct the rktree),
    ## keep the X.node.list as is.
    else{ X.node.list <- X.node.list}

    ## Sanity check (if the resulting flag.pred is not identical to rktree$flag, stop the entire process)
    stopifnot(length(which((flag.pred[,1]  == rktree$flag[,1]) == FALSE)) == 0)

    ## 5) Based on the flag, identify the end nodes that occurs after applying the rktree model to the test set
    end.node.index.test <- ends.index.finder(flag.pred)
    # get end nodes after fitting rktree model onto the test set
    X.end.node.list.test <- X.node.list[end.node.index.test]
    # make an empty vector to store predictions on the test set
    test.pred <- rep(0, length(X.end.node.list.test))
    # extra column in the end to store the predicted classes for each observation in the test set
    prediction.df <- data.frame(matrix(0, nrow = 0, ncol = (n.cov + 1)))
    colnames(prediction.df)[length(colnames(prediction.df))] <- "rktree.prediction"

    ## 6) Make predictions on the ends node of the test set
    for (j in 1:length(end.node.index.test)){

        ## The flag pattern for the j-th end node of the test set should match with one of the flag patterns of the end nodes of the rktree.
        ## So take the matching end node index from the training set,
        ## and assign the classification of that training node as the predicted class for the observations in the corresponding test end node.
        k <- which((rktree$flag)[end.node.index, 1] %in% flag.pred[end.node.index.test[j], 1])
        test.pred[j] <- training.pred[k]
    }

    ## 7) Make prediction.df data frame for the test set
    for (i in 1:(length(X.end.node.list.test))){
        n.obs <- dim(X.end.node.list.test[[i]])[1] # number of TEST observations contained in a rktree end node

        if(n.obs >= 1 && !is.null(n.obs)){
            predictions <- rep(test.pred[i], n.obs)
            prediction.df <- as.data.frame(rbind(prediction.df, data.frame(X.end.node.list.test[[i]], predictions)))
        }
    }

    prediction.df <- prediction.df[order(as.numeric(row.names(prediction.df))),] # re-order by row names (which corresponds to the observation number)!

    ## 8) Return the results
    results <- list(prediction.df, flag.pred)
    names(results) <- c("prediction.df", "flag.pred")

    results
    ## NOTE: structure of prediction.df:
    ## (row names) = observation number from our original test dataset
    ## (column i, i = 1,2,...,n.cov) = values pertaining to the i-th covariate
    ## (column n.cov+1) = prediction based on the rktree obtained from the training data set.
}


##' A draw.treeRK function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Creates a plot of a rktree.
##' @param tr a construct.treeRK() object (a tree).
##' @param y.factor.levels a y.organizer()$y.factor.levels output.
##' @param font font type used in the rktree plot; default is "Times".
##' @param node.colour colour of the node used in the rktree plot; default is "White".
##' @param text.colour colour of the text used in the rktree plot; default is "Dark Blue".
##' @param text.size size of the text in the rktree plot; default is 0.67.
##' @param tree.vertex.size size of the rktree plot vertices; default is 75.
##' @param tree.title title of the rktree plot; default title is "Diagram of a Tree".
##' @param title.colour colour of the title of the rktree plot; default title colour is "Dark Blue".
##' # side note: (YES, the author likes dark blue)
##' @return A rktree igraph plot.
##' @examples
##' ## example: iris dataset
##' ## load the forestRK package
##' library(forestRK)
##'
##' x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),] # covariates of training data set
##' y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new
##'
##' y.factor.levels <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.factor.levels
##'
##' ## Construct a tree
##' # min.num.obs.end.node.tree is set to 5 by default; entropy is set to TRUE by default
##' tree.entropy <- construct.treeRK(x.train, y.train)
##'
##' # Plot the tree
##' draw.treeRK(tree.entropy, y.factor.levels, font="Times", node.colour = "black", text.colour = "white", text.size = 0.7, tree.vertex.size = 100, tree.title = "Decision Tree", title.colour = "dark green")
draw.treeRK <- function(tr = construct.treeRK(), y.factor.levels, font = "Times", node.colour = "white", text.colour = "dark blue", text.size = 0.67, tree.vertex.size = 75, tree.title = "Diagram of a Tree", title.colour = "dark blue"){

    ## Sanity check
    if(is.null(tr)){
        stop("A rktree (construct.tree() object) needs to be provided in the function call")
    }

    if(is.null(y.factor.levels) || !(length(y.factor.levels) > 1)){
        stop("The vector 'y.factor.levels' has to be at least length of 2")
    }

    if(!(dim(tr$covariate.split)[1] > 1)){
        stop("No decision tree can be plotted since no split had occurred while constructing the tree")
    }

    ## 1) Import the igraph library
    ## library(igraph)

    ## 2) Extract some information from the rk tree object
    covariate.names <- tr$covariate.names
    covariate.split <- tr$covariate.split
    y.new.node.list <- tr$y.new.node.list
    end.node.index <- ends.index.finder(tr$flag)
    y.new.end.node.list <- y.new.node.list[end.node.index]

    ## 3) Predict class types on the training observations
    ## Make an empty vector to store predictions from the training data for each end node
    training.pred <- rep(0, length(y.new.end.node.list))

    ## Predict class types on the training observations
    for (i in 1:(length(y.new.end.node.list))){
        training.pred[i] <- as.numeric(names(which.max(table(y.new.end.node.list[[i]])))) #making predictions by the majority vote
    }

    ## 4) Make edges of the tree graph
    ##
    f <- as.character(unlist(tr$flag))  # e.g. f = c("r","rx","ry","rxx","rxy","ryx","ryy","rxxx","rxxy","rxyx","rxyy",....)
    v <- as.numeric(as.vector(unlist(tr$value.at.split))[-1]) # tr$value.at.split[1] == NA

    ed <- c() # a vector storing edges

    ## Make edges
    for(i in 1:(length(f)-1)){
        branch <- f[i]

        for (j in (i+1):(length(f))){
            ch <- f[j]
            if ((substr(ch,1,nchar(branch))==branch) && (nchar(ch)==(nchar(branch)+1))){ed <- c(ed,i,j)}
        }
    }

    i.graph <- make_empty_graph(length(unlist(tr$flag)), directed = T) # creating empty graph (i.e. a graph with no edges)
    i.graph <- add_edges(i.graph, ed) # adding edges onto the empty graph

    # Generate names
    nom <- rep(0, gsize(i.graph)+1) # an empty vector storing names of the edges
    for (j in 1:length(end.node.index)){ # note: length(end.node.index) == length(training.pred)
        nom[c(end.node.index[j])] <- y.factor.levels[training.pred[j]]
    }

    v2 <- c()
    for (h in 1:length(v)){
        v2[h] <-  paste0(covariate.names[covariate.split[h + 1,]]," =< ", v[h])
    }

    v2[1] <- paste0("(ROOT)", v2[1])
    nom <- as.character(replace(nom, c(which(nom == 0)), v2))

    V(i.graph)$name <- nom

    ## Regulate the text size of the node
    V(i.graph)$label.cex <- text.size
    ## Regulate the text colour of the node
    V(i.graph)$label.color <- text.colour
    ## Regulate the color of node
    V(i.graph)$color <- node.colour
    ## Regulate the font family of the node
    V(i.graph)$label.family <- font


    ## 5) Plot the graph
    par(mar = c(0,0,0,0), ps=14,cex=1)
    V(i.graph)$class <- names(V(i.graph))

    g <- {
            plot(i.graph, layout = layout.reingold.tilford(i.graph, root = 1, flip.y = T, circular = F),
            vertex.size = tree.vertex.size, vertex.shape = "rectangle")

            title(tree.title, cex.main=2,col.main = title.colour)
         }

    ## 6) Return the graph
    g
}

##' A bstrap function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Performs bootstrap sampling of our (training) dataset.
##' @param dat a numericized data frame that stores covariate of the observations as well as their numericized class types y; dat should contain no NA or NaN's.
##' @param nbags the number of bags or number of bootstrap samples that we want to generate.
##' @param samp.size the number of samples that each bag (individual bootstrap sample) should contain.
##' @return A list containing a data frames of bootstrap samples.
##' @examples
##' ## example: iris dataset
##' ## load the forestRK package
##' library(forestRK)
##'
##' x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),] # covariates of training data set
##' y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new
##' b <- data.frame(cbind(x.train, y.train)) # combine the covariates x with class types y
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




##' A forestRK function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Builds up a random forest RK onto the given (training) dataset.
##' @param X a numericized data frame storing covariates of each observation from the given (training) dataset (obtained via x.organizer()); X should contain no NA or NaN's.
##' @param Y.new a vector storing the numericized class types of each observation from the given (training) dataset X; Y.new should contain no NA or NaN's.
##' @param min.num.obs.end.node.tree the minimum number of observations that we want each of our end node of our tree to contain. Default is set to 5.
##' @param nbags number of bootstrap samples that we want to generate.
##' @param samp.size number of samples that we want each of our bootstrap samples to contain.
##' @param entropy TRUE if we use entropy as the splitting criteria; FALSE if we use the gini index for the splitting criteria. Default is set to TRUE.
##' @return The original dataset used to construct the random forest RK (X).
##' @return A list of trees in the generated random forest RK (forest.rk.tree.list).
##' @return A list containing data frames of bootstrap samples that were generated from the dataset X (bootsamp.list).
##' @return The value of the parameter 'entropy' (ent.status).
##' @examples
##' ## example: iris dataset
##' ## load the forestRK package
##' library(forestRK)
##'
##' x.ex <- x.organizer(iris[,1:4])
##' x.train <- x.organizer(iris[,1:4])[c(1:25,51:75,101:125),] # covariates of training data set
##' y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new
##'
##' # Implement forestRK function
##' # min.num.obs.end.node.tree is set to 5 by default; entropy is set to TRUE by default
##' forestRK.1 <- forestRK(x.train, y.train, nbags = 100, samp.size = 100)
##' forestRK.1.tree <- forestRK(x.train, y.train, min.num.obs.end.node.tree = 6, nbags = 100, samp.size = 100, entropy = FALSE)$forest.rk.tree.list[[1]]
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

##' A pred.forestRK function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Makes predictions on the test dataset based on the forestRK model constructed from the training dataset.
##' @param x.test a numericized data frame (obtained via x.organizer()) that stores covariates of the data points on which we want to make our predictions; x.test should contain no NA or NaN's.
##' @param x.training a numericized data frame that stores covariates of the training data points (or the dataset from which the forestRK model will be generated); x.training should contain no NA or NaN's.
##' @param y.training a vector that stores numericized class types y of the training observations; y.training should contain no NA or NaN's.
##' @param min.num.obs.end.node.tree the minimum number of observations that we want each end node of each tree to contain. Default is set to 5.
##' @param nbags the number of bootstrap samples that we want to generate.
##' @param samp.size the number of samples that each bootstrap sample should contain.
##' @param y.factor.levels output of y.organizer()$y.factor.levels.
##' @param entropy TRUE if we use entropy as the splitting criteria; FALSE if we use the gini index for the splitting criteria. Default is set to TRUE.
##' @return x.test (x.test).
##' @return a data frame storing predicted classes for all test observations from each tree in the forest (df.of.predictions.for.all.observations).
##' @return a forestRK object (forest.rk).
##' @return a list of data frames that store covariates of the test observations as well as their predicted class from each tree in the random forest (test.prediction.df.list).
##' @return a data frame displaying names of the predicted class by the forestRK model for each test observation (predictions.forest.rk).
##' @return numericized version of predictions.forest.rk (num.pred.for.obs.forest.rk).
##' @return status of the parameter 'entropy'; TRUE if Entropy is used, FALSE if Gini Index is used (entropy.status).
##' @examples
##' ## example: iris dataset
##' ## load the forestRK package
##' library(forestRK)
##'
##' x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),] # covariates of training data set
##' x.test <- x.organizer(iris[,1:4], encoding = "num")[c(26:50,76:100,126:150),] # covariates of test dataset
##' y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new
##'
##' y.factor.levels<- y.organizer(iris[c(1:25,51:75,101:125),5])$y.factor.levels
##'
##' ## make prediction from a random forest RK model
##' pred.forest.rk <- pred.forestRK(x.test = x.test, x.training = x.train, y.training = y.train, y.factor.levels, min.num.obs.end.node.tree = 6, nbags = 100, samp.size = 50, entropy = FALSE)
##' pred.forest.rk$test.prediction.df.list[[10]]
##' pred.forest.rk$pred.for.obs.forest.rk # etc....
pred.forestRK <- function(x.test = data.frame(), x.training = data.frame(), y.training = c(),
                          y.factor.levels, min.num.obs.end.node.tree = 5, nbags, samp.size, entropy = TRUE){
    ## Import the rapportools package
    ## library(rapportools)

    ## Sanity check
    if(!(dim(x.test)[1] > 1) || !(dim(x.test)[2] >= 1)){
        stop("Invalid dimensions for the test dataset x.test")
    }

    if(!(dim(x.training)[1] > 1) || !(dim(x.training)[2] >= 1)){
        stop("Invalid dimensions for the training dataset x.training")
    }

    if(!(length(y.training) > 1) || is.null(y.training)){
        stop("Invalid length for the vector y.training")
    }

    if(!(dim(x.training)[1] == length(y.training))){
        stop("The number of observations in the dataset x.training and the vector y.training do not match")
    }

    if(is.null(y.factor.levels) || !(length(y.factor.levels) > 1)){
        stop("The vector 'y.factor.levels' has to be at least length of 2")
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

    if(!(dim(x.test)[2] == dim(x.training)[2])){
        stop("The number of covariates in the matrix x.test does not match with that of the dataset x.training")
    }

    ## 1) Setup
    ent.status <- entropy
    forest.rk <- forestRK(x.training, y.training, min.num.obs.end.node.tree, nbags, samp.size, ent.status)
    forest.rk.tree.list.training <- forest.rk$forest.rk.tree.list
    bootsamp.list.training <- forest.rk$bootsamp.list
    # list of data frames storing the prediction.df's of the test observations generated from each tree in the random forest RK
    test.prediction.df.list <- list()

    # length(bootsamp.list.training) == (number of trees in the forest) == length(forest.rk.tree.list.training)
    for (s in 1:(length(bootsamp.list.training))){
        test.prediction.df.list[[s]] <- pred.treeRK(X = x.test, rktree = forest.rk.tree.list.training[[s]])$prediction.df
    }

    ## 2) Predict class type via forestRK model
    ## df.of.predictions.for.all.observations has rows of test observations and each of its column pertain to a specific tree in the random forest RK
    ## df.of.predictions.for.all.observations stores predicted class type of each test observation from each tree in the random forest RK
    df.of.predictions.for.all.observations <- data.frame(matrix(0, nrow = dim(test.prediction.df.list[[1]])[1], ncol = length(test.prediction.df.list)))
    rownames(df.of.predictions.for.all.observations) <- rownames(test.prediction.df.list[[1]])

    for (i in 1:(length(test.prediction.df.list))){
        df <- test.prediction.df.list[[i]]
        df.of.predictions.for.all.observations[,i] <- df$predictions
    }

    ## pred.for.obs.forest.rk is a vector that stores the class type of each test observation that is predicted by the forestRK model
    pred.for.obs.forest.rk <- c()
    for (j in 1:dim(df.of.predictions.for.all.observations)[1]){
        mode <- names(sort(summary(as.factor(unlist(c(df.of.predictions.for.all.observations[j,])))), decreasing = TRUE))[1]
        mode <- as.numeric(mode)
        pred.for.obs.forest.rk <- c(pred.for.obs.forest.rk, mode)
    }

    ## Make alias of pred.for.obs.forest.rk
    num.pred.for.obs.forest.rk <- pred.for.obs.forest.rk

    for (z in 1:length(pred.for.obs.forest.rk)){
        # the vector pred.for.obs.forest.rk now stores the actual prediction class labels instead of the numericized version of the class labels
        pred.for.obs.forest.rk[z] <- y.factor.levels[c(num.pred.for.obs.forest.rk[z])]
    }

    pred.for.obs.forest.rk <- as.data.frame(pred.for.obs.forest.rk)# a data frame that has its column name as "pred.for.obs.forest.rk"
    rownames(pred.for.obs.forest.rk) <- rownames(test.prediction.df.list[[1]])

    ## 3) Return the results
    results <- list(x.test, df.of.predictions.for.all.observations, forest.rk, test.prediction.df.list, pred.for.obs.forest.rk, num.pred.for.obs.forest.rk, ent.status)
    names(results) <- c("x.test", "df.of.predictions.for.all.observations", "forest.rk", "test.prediction.df.list", "pred.for.obs.forest.rk", "num.pred.for.obs.forest.rk", "entropy.status")

    ## Return the results
    results
}

##' A get.tree.forestRK function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Extracts an individual tree structure from a forestRK object.
##' @param forestRK.object a forestRK() object.
##' @param tree.index a vector of indices of the trees that we want to extract from the forestRK() object.
##' @return A list containing forestRK trees that have their indices specified in the function argument 'tree.index'.
##' @examples
##' ## example: iris dataset
##' ## load the forestRK package
##' library(forestRK)
##'
##' x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),] # covariates of training data set
##' y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new
##'
##' #random forest
##' # min.num.obs.end.node.tree is set to 5 by default; entropy is set to TRUE by default
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

##' A var.used.forestRK function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Spits out the list of covariates used to perform the splits to generate a particular tree(s) of a forestRK object.
##' @param forestRK.object a forestRK object.
##' @param tree.index a vector storing the indices of the trees that we are interested to examine.
##' @return a list of vectors of names of covariates that was used for the splits to construct the given tree(s) in a forestRK model.
##' @examples
##' ## example: iris dataset
##' ## load the forestRK package
##' library(forestRK)
##'
##' x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),] # covariates of training data set
##' y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new
##'
##' #random forest
##' # min.num.obs.end.node.tree is set to 5 by default; entropy is set to TRUE by default
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



##' A mds.plot.forestRK function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Makes 2D mds (multidimensional scaling) ggplot of the test observations based on the predictions from a forestRK model.
##' @param pred.forestRK.object a pred.forestRK() object.
##' @param plot.title an user specified title for the mds plot; the default is "MDS Plot of Dataset Colour Coded by Forest RK Model Predictions".
##' @param xlab label for the x-axis of the plot; the default is "First Coordinate".
##' @param ylab label for the y-axis of the plot; the default is "Second Coordinate".
##' @param colour.lab label title for the legend that specifies categories for each colour; the default is "Predictions By The Random Forest RK Model".
##' @return a multidimensional scaling ggplot (2D) of the test observations, colour coded by their predicted class types.
##' @examples
##' ## example: iris dataset
##' ## load the forestRK package
##' library(forestRK)
##'
##' x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),] # covariates of training data set
##' x.test <- x.organizer(iris[,1:4], encoding = "num")[c(26:50,76:100,126:150),] # covariates of test dataset
##' y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new
##' y.factor.levels <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.factor.levels
##'
##' # min.num.obs.end.node.tree is set to 5 by default; entropy is set to TRUE by default
##' pred.forest.rk <- pred.forestRK(x.test = x.test, x.training = x.train, y.training = y.train, nbags = 100, samp.size = 50, y.factor.levels = y.factor.levels)
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

##' An importance.forestRK function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Calculates importance of each covariate considered in the forestRK model.
##' @param forestRK.object a forestRK object.
##' @return a vector containing the list of covariate names ordered from the most important to the least important (importance.covariate.names).
##' @return a vector that contains average decrease in splitting criteria across all trees of the random forest by each covariate (average.decrease.in.criteria.vec).
##' @return a boolean variable that is TRUE is entropy is used, and is FALSE if Gini Index is used (ent.status).
##' @return the dataset containing covariate values that was used to construct forestRK.object (x.original).
##' @examples
##' ## example: iris dataset
##' ## load the forestRK package
##' library(forestRK)
##'
##' x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),] # covariates of training data set
##' y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new
##'
##' # random forest
##' # min.num.obs.end.node.tree is set to 5 by default; entropy is set to TRUE by default
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

##' An importance.plot.forestRK function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Generates importance plot of the covariates considered in the forestRK model.
##' @param importance.forestRK.object an importance.forestRK() object.
##' @param colour.used colour used for the border of the importance plot; default is "dark green".
##' @param fill.colour colour used to fill the bars of the importance plot; default is "dark green".
##' @param label.size size of the labels; default is set to 10.
##' @return an importance plot of the covariates considered in the forestRK model.
##' @examples
##' ## example: iris dataset
##' ## load the forestRK package
##' library(forestRK)
##'
##' x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),] # covariates of training data set
##' y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new
##'
##' # random forest
##' # min.num.obs.end.node.tree is set to 5 by default; entropy is set to TRUE by default
##' forestRK.1 <- forestRK(x.train, y.train,nbags=100, samp.size=100)
##' # execute forestRK.importance function
##' imp <- importance.forestRK(forestRK.1)
##' # generate importance plot
##' importance.plot.forestRK(imp)
importance.plot.forestRK <- function(importance.forestRK.object = importance.forestRK(), colour.used = "dark green", fill.colour = "dark green", label.size = 10){

    ## load the ggplot2 package
    ## library(ggplot2)

    # Sanity checks
    if(is.null(importance.forestRK.object)){
        stop("'importance.forestRK.object' needs to be provided in the function call")
    }

    ent.status <- importance.forestRK.object$ent.status

    ## 1) Set up
    # for the geom_title label
    if(ent.status == TRUE){ent.label <- "Entropy"}
    else{ent.label <- "Gini Index"}

    ## 2) Make the importance plot
    average.decrease.in.criteria.vec <- importance.forestRK.object$average.decrease.in.criteria.vec
    importance.covariate.names.vec <- importance.forestRK.object$importance.covariate.names
    average.decrease.in.criteria.df <- as.data.frame(average.decrease.in.criteria.vec, row.names = importance.covariate.names.vec)
    g <- ggplot(average.decrease.in.criteria.df, aes(x=reorder(row.names(average.decrease.in.criteria.df),average.decrease.in.criteria.vec),y=average.decrease.in.criteria.vec)) + theme_grey(base_size = label.size) + coord_flip() 
    g2 <- g + geom_bar(stat = "identity", color=colour.used, fill = fill.colour) + theme(legend.position = "top") + labs(x="Covariate Names", y="Average Decrease in Splitting Criteria", title = paste("Importance Plot Based On The Splitting Criteria", ent.label))

    ## 3) Return the importance plot
    g2
}



##  MAIN PROGRAM (1) - (11)
##  NOTE: The numbers in bracket (e.g. '(1)') refers to the respective page # on the presentation slide.
## Example: iris dataset
###############################################################################################################
###############################################################################################################


set.seed(7829) #set seed
#Variables from Gaussian distribution with different parameters
#import package
library(forestRK)
library(partykit)

## load the forestRK package
library(forestRK)
###############################################################################################################


### (1)
## always apply x.original and y.original functions first before applying any other functions
x.train <- (x.organizer(iris[,1:4], encoding = "num"))[c(1:25,51:75,101:125),]
x.test <- (x.organizer(iris[,1:4], encoding = "num"))[c(26:50,76:100,126:150),]
y.train <- (y.organizer((iris)[c(1:25,51:75,101:125),5]))$y.new
y.factor.levels <- (y.organizer((iris)[c(1:25,51:75,101:125),5]))$y.factor.levels # original names of the classes

x.train
x.test
y.train
#################################################################################################################


### (2)
# split.criteria.calculator() example
criteria.calculator(x.node = x.train, y.new.node = y.train)
criteria.calculator(x.node = x.train, y.new.node = y.train, entropy = FALSE)
#################################################################################################################


### (3)
# criteria.after.split.calculator() example
ent.status <- FALSE
number.of.columns.of.x.node <- dim(x.train)[2] #number.of.columns.of.x.node = total number of covariates that we consider
m.try <- sample(1:(number.of.columns.of.x.node),1) # m.try = the randomly chosen number of covariates that we should consider for splitting
K <- sample(1:(number.of.columns.of.x.node), m.try)
sp <- partysplit(varid=K[1], breaks = x.train[1,K[1]], index = NULL, right = TRUE, prob = NULL, info = NULL)
split.record <- kidids_split(sp, data=x.train)
criteria.after.split <- criteria.after.split.calculator(x.train, y.train, split.record, ent.status)

criteria.after.split
#################################################################################################################


### (4)
# cutoff.node.and.covariate.index.finder() example
cutoff.node.and.covariate.index.finder(x.train, y.train)
cutoff.node.and.covariate.index.finder(x.train, y.train, entropy = FALSE)
#################################################################################################################


### (5)
# Create a tree
# min.num.obs.end.node.tree is set to 5 by default; entropy is set to TRUE by default
tree.entropy <- construct.treeRK(x.train, y.train)
tree.entropy

tree.gini <- construct.treeRK(x.train, y.train, min.num.obs.end.node.tree = 6, entropy = FALSE)
tree.gini

# find indices of the end nodes of the tree that we constructed above
end.node.index <- ends.index.finder(tree.entropy$flag)
end.node.index
#################################################################################################################


### (6)
# prediction of a single rktree
prediction.df <- pred.treeRK(X=x.test, tree.entropy)$prediction.df
prediction.df

flag.pred <- pred.treeRK(X=x.test, tree.entropy)$flag.pred
flag.pred
#################################################################################################################


### (7)
#plot the tree
draw.treeRK(tree.entropy, y.factor.levels) #plot under the default setting
draw.treeRK(tree.entropy, y.factor.levels, font="Times", node.colour = "black", text.colour = "white", text.size = 0.7, tree.vertex.size = 95, tree.title = "Decision Tree", title.colour = "dark green")
#################################################################################################################


### (8)
#bstrp function example
bootstrap.sample <- bstrap(dat = data.frame(cbind(x.train,y.train)), nbags = 20, samp.size = 30)

# random forest
# min.num.obs.end.node.tree is set to 5 by default; entropy is set to TRUE by default
forestRK.1 <- forestRK(x.train, y.train, nbags=100, samp.size=100)
forestRK.2 <- forestRK(x.train, y.train, min.num.obs.end.node.tree = 6, nbags=100, samp.size=100, entropy = FALSE)
forestRK.2
#################################################################################################################


### (9)
# prediction from a random forest RK
pred.forest.rk <- pred.forestRK(x.test = x.test, x.training = x.train, y.training = y.train, nbags = 100, samp.size = 100, y.factor.levels = y.factor.levels, entropy = TRUE)
pred.forest.rk$test.prediction.df.list[[27]]
pred.forest.rk$pred.for.obs.forest.rk

### check the accuracy of the prediction made by the forestRK package
y.test <- iris[c(26:50,76:100,126:150),5]
sum(as.vector(pred.forest.rk$pred.for.obs.forest.rk) == as.vector(y.test)) / length(y.test) # 71/75 for iris dataset with numeric encoding!

### for Soybean dataset
# y.test <- Soybean[-vec,1]
# sum(as.vector(pred.forest.rk$pred.for.obs.forest.rk) == as.vector(y.test)) / length(y.test)
# 263/374 for Soybean dataset with numeric encoding
# 288/374 for Soybean dataset with binary encoding (performs better than the numeric encoding)!

### for BostonHousing2 dataset
### because the test observations in pred.for.obs.forest.rk are re-ordered by increasing index number,
### to match them properly with the observations in y.test, we need to re-order the observations in y.test
### by the increasing index number (index number == original row number of each test observation)
# y.test <- BostonHousing[-vec,1]
# names(y.test) <- rownames(x.test)
# sum(as.vector(pred.forest.rk$pred.for.obs.forest.rk) == as.vector(y.test[order(as.numeric(names(y.test)))]))
### relatively poor performance though!

#################################################################################################################


### (10)
# get tree
tree.index.ex <- c(1,3,8)
get.tree <- get.tree.forestRK(forestRK.1, tree.index = tree.index.ex)
# display 8th tree in the forest (which is the third element of the vector 'tree.index.ex')
get.tree[["8"]]

# get the list of variable used for splitting
covariate.used.for.split.tree <- var.used.forestRK(forestRK.1, tree.index = c(4,5,6))
# get the list of names of covariates used for splitting to construct tree #6 in the forestRK.1
covariate.used.for.split.tree[["6"]]

# generate a classical mds plot and colour code by the predicted class
mds.plot.forestRK(pred.forest.rk)
#################################################################################################################


### (11)
#generate the plot of feature importance
imp <- importance.forestRK(forestRK.1)
imp

importance.plot.forestRK(imp, colour.used="dark green", fill.colour="dark green", label.size=10)
#################################################################################################################
#################################################################################################################

### also could test with the soybean data
# Soybean Database
library(mlbench)
data(Soybean)
dim(Soybean)
levels(Soybean$Class)
head(Soybean)
Soybean <- na.omit(Soybean)
vec <- seq.int(1,562, by=3)
x <- x.organizer(Soybean[,2:36], encoding="bin")
x.train <- x[vec,]
x.test <- x[-vec,]
y.train <- y.organizer(Soybean[vec,1])$y.new
y.factor.levels <- y.organizer(Soybean[vec,1])$y.factor.levels
# ... and the rest is the same as iris dataset
#...and everything worked for Soybean dataset!
##########################################################

### Boston Housing Dataset Example
library(mlbench)
data(BostonHousing2)
head(BostonHousing2)
summary(BostonHousing2)
BostonHousing <- na.omit(BostonHousing2)
BostonHousing <- BostonHousing[sample(nrow(BostonHousing)),] #shuffle the data!
vec <- c(1:(as.integer(2/3*(dim(BostonHousing)[1]))))
x <- x.organizer(BostonHousing[,2:19], encoding="num")
x.train <- x[vec,]
x.test <- x[-vec,]
y.train <- y.organizer(BostonHousing[vec,1])$y.new
y.factor.levels <- y.organizer(BostonHousing[vec,1])$y.factor.levels
# ... and the rest is same as iris dataset
#... and everything worked for the BostonHousing2 dataset!


