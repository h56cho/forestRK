##' An importance.plot.forestRK function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Generates importance plot of the covariates considered in the forestRK
##'        model.
##' @param importance.forestRK.object an importance.forestRK() object.
##' @param colour.used colour used for the border of the importance plot;
##'                    default is "dark green".
##' @param fill.colour colour used to fill the bars of the importance plot;
##'                    default is "dark green".
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
##' # min.num.obs.end.node.tree is set to 5 by default;
##' # entropy is set to TRUE by default
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
