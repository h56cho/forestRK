##' A x.organizer function
##'
##' @author Hyunjin Cho, Rebecca Su
##' @title Numericizing a data frame of covariates from the original dataset via
##'        Binary Encoding (for categorical features with cardinality >= 1000)
##'        or Numeric Encoding (for categorical features with cardinality < 1000)
##' *** For more information about Binary Encoding and its effectiveness, please visit:
##'  https://medium.com/data-design/visiting-categorical-features-and-encoding-in-decision-trees-53400fa65931***
##'
##' @param x.dat a data frame storing covariates of each observation
##'              (can be either numeric or non-numeric) from the original dataset;
##'              x.dat should contain no NA or NaN's.
##' @param encoding type of encoding that the user prefers to perform on the
##'                 categorical feature(s); "num" for Numeric Encoding, or "bin"
##'                 for Binary Encoding.
##' @return A data frame storing the numericized version of the covariates from
##'         the original dataset.
##'
##' ***please do read the x.organizer section of the forestRK documentation for
##' detailed explanation of the proper data cleaning steps using x.organizer
##' (apply x.organizer to covariates of all observations after combining the
##' training and the test set)***
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
##' # covariates of training data set
##' x.train <- x.organizer(iris[,1:4], encoding = "num")[c(1:25,51:75,101:125),]
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
