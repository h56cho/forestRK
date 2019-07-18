##' A pred.treeRK function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Makes predictions on the test dataset based on the treeRK model provided.
##' @param X a numericized data frame (obtained via x.organizer()) storing
##'          covariates of the test dataset (or the dataset that we want to make
##'          predictions on); X should contain no NA or NaN's.
##' @param rktree a construct.treeRK() object.
##' @return a data frame of test observations with the predicted class types
##'         indicated under the very last column (prediction.df).
##' @return the flag generated from applying the rktree model to the test set
##'         (flag.pred).
##' @examples
##' ## example: iris dataset
##' ## load the forestRK package
##' library(forestRK)
##'
##' # covariates of training data set
##' x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),]
##' # covariates of test dataset
##' x.test <- x.organizer(iris[,1:4], encoding = "num")[c(26:50,76:100,126:150),]
##' y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new
##'
##' ## Construct a tree
##' # min.num.obs.end.node.tree is set to 5 by default; entropy is set to TRUE by default
##' tree.entropy <- construct.treeRK(x.train, y.train)
##' tree.gini <- construct.treeRK(x.train, y.train, min.num.obs.end.node.tree = 6,
##'                               entropy = FALSE)
##'
##' ## Make predictions on the test set based on the constructed rktree model
##' # last column of prediction.df stores predicted class on the test observations
##' # based on a given rktree
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
                ## If the current node is not specified as NULL...
                if(!is.null(X.node.list[[l2]])){
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
                } # end of if(!is.null(X.node.list[[l2]]))

				## If the current node IS specified as NULL...
				## simply assign its children nodes as NULL as well.
                else{
                    x1 <- NULL
                    x2 <- NULL
                    X.node.list[length(X.node.list)+1] <- list(x1)
                    X.node.list[length(X.node.list)+1] <- list(x2)

                    # save information by updating flags
                    flag.pred <- rbind(flag.pred, paste0(flag.pred[a-1],"x"))
                    flag.pred <- rbind(flag.pred, paste0(flag.pred[a-1],"y"))

                    # update the values of 'l2', 'i', and 'a'
                    l2 <- l2 + 1
                    i <- i + 1
                    a <- a + 1
                }
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

