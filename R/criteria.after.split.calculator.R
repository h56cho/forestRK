##' A criteria.after.split.calculator function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Calculates Entropy or Gini Index of a node after a given split.
##' @param x.node numericized data frame (obtained via x.organizer()) of
##'               covariates of a particular node that is to be split;
##'               x.node should contain no NA or NaN's.
##' @param y.new.node numericized class type of each observation of a particular
##'                   node that is to be split; y.new.node should contain no NA
##'                   or NaN's.
##' @param split.record output of the kidids_split function from the partykit
##'                     package for a given split.
##' @param entropy TRUE if Entropy is used as the splitting criteria; FALSE if
##'                Gini Index is used as the splitting criteria. Default is set
##'                to TRUE.
##' @return The value of Entropy or Gini Index of a particular node after a given
##'         split.
##' @examples
##' ## example: iris dataset
##' library(forestRK) # load the package forestRK
##' library(partykit)
##'
##' # covariates of training data set
##' x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),]
##' # numericized class types of observations of training dataset
##' y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new
##'
##' ## criteria.after.split.calculator() example in the implementation of the
##' ## forestRK algorithm
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
