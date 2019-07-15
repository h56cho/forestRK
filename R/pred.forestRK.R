##' A pred.forestRK function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Makes predictions on the test dataset based on the forestRK model
##'        constructed from the training dataset.
##' @param x.test a numericized data frame (obtained via x.organizer()) that
##'               stores covariates of the data points on which we want to make
##'               our predictions; x.test should contain no NA or NaN's.
##' @param x.training a numericized data frame that stores covariates of the
##'                   training data points (or the dataset from which the
##'                   forestRK model will be generated); x.training should
##'                   contain no NA or NaN's.
##' @param y.training a vector that stores numericized class types y of the
##'                   training observations; y.training should contain no NA or
##'                   NaN's.
##' @param min.num.obs.end.node.tree the minimum number of observations that we
##'                                  want each end node of each tree to contain.
##'                                  Default is set to 5.
##' @param nbags the number of bootstrap samples that we want to generate.
##' @param samp.size the number of samples that each bootstrap sample should contain.
##' @param y.factor.levels output of y.organizer()$y.factor.levels.
##' @param entropy TRUE if we use entropy as the splitting criteria;
##'                FALSE if we use the gini index for the splitting criteria.
##'                Default is set to TRUE.
##' @return x.test (x.test).
##' @return a data frame storing predicted classes for all test observations
##'         from each tree in the forest (df.of.predictions.for.all.observations).
##' @return a forestRK object (forest.rk).
##' @return a list of data frames that store covariates of the test observations
##'         as well as their predicted class from each tree in the random forest
##'         (test.prediction.df.list).
##' @return a data frame displaying names of the predicted class by the forestRK
##'         model for each test observation (predictions.forest.rk).
##' @return numericized version of predictions.forest.rk (num.pred.for.obs.forest.rk).
##' @return status of the parameter 'entropy';
##'         TRUE if Entropy is used,
##'         FALSE if Gini Index is used (entropy.status).
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
##' y.factor.levels<- y.organizer(iris[c(1:25,51:75,101:125),5])$y.factor.levels
##'
##' ## make prediction from a random forest RK model
##' pred.forest.rk <- pred.forestRK(x.test = x.test, x.training = x.train,
##'                                 y.training = y.train, y.factor.levels,
##'                                 min.num.obs.end.node.tree = 6, nbags = 100,
##'                                 samp.size = 50, entropy = FALSE)
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
