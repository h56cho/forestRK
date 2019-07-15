##' An ends.index.finder function
##'
##' @author Hyunjin-Dominique Cho, Rebecca Su
##' @title Identifies numerical indices of the end nodes of a rktree.
##' @param tr.flag a construct.treeRK()$flag object or a similar flag matrix.
##' @return A vector that lists the indices of the end nodes of a given rktree
##'         (indices that corresponds to the indices in x.node.list and
##'         y.new.node.list that is returned by the construct.treeRK function).
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

