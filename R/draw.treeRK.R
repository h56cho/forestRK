##' A draw.treeRK function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Creates a plot of a rktree.
##' @param tr a construct.treeRK() object (a tree).
##' @param y.factor.levels a y.organizer()$y.factor.levels output.
##' @param font font type used in the rktree plot; default is "Times".
##' @param node.colour colour of the node used in the rktree plot;
##'                    default is "White".
##' @param text.colour colour of the text used in the rktree plot;
##'                    default is "Dark Blue".
##' @param text.size size of the text in the rktree plot;
##'                  default is 0.67.
##' @param tree.vertex.size size of the rktree plot vertices;
##'                         default is 75.
##' @param tree.title title of the rktree plot;
##'                   default title is "Diagram of a Tree".
##' @param title.colour colour of the title of the rktree plot;
##'                     default title colour is "Dark Blue".
##' # side note: (YES, the author likes dark blue)
##' @return A rktree igraph plot.
##' @examples
##' ## example: iris dataset
##' ## load the forestRK package
##' library(forestRK)
##'
##' # covariates of training data set
##' x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),]
##' y.train <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.new
##'
##' y.factor.levels <- y.organizer(iris[c(1:25,51:75,101:125),5])$y.factor.levels
##'
##' ## Construct a tree
##' # min.num.obs.end.node.tree is set to 5 by default;
##' # entropy is set to TRUE by default
##' tree.entropy <- construct.treeRK(x.train, y.train)
##'
##' # Plot the tree
##' draw.treeRK(tree.entropy, y.factor.levels, font="Times", node.colour = "black",
##'             text.colour = "white", text.size = 0.7, tree.vertex.size = 100,
##'             tree.title = "Decision Tree", title.colour = "dark green")
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
    opar <- par(mar = c(0,0,0,0), ps=14,cex=1)
    on.exit(par(opar))

    V(i.graph)$class <- names(V(i.graph))

    g <- {
            plot(i.graph, layout = layout.reingold.tilford(i.graph, root = 1, flip.y = T, circular = F),
            vertex.size = tree.vertex.size, vertex.shape = "rectangle")

            title(tree.title, cex.main=2,col.main = title.colour)
         }

    ## 6) Return the graph
    g
}


