###################################################################################
#' Execute learned function for input data 
#' 
#' After completion of the learning phase (step="sfa") this function can be used
#' to apply the learned function to the input data. \cr
#'    The execution is completed in 4 steps:\cr
#'     1. projection on the input principal components (dimensionality
#'     reduction)\cr
#'     2. expansion (if necessary)\cr
#'     3. projection on the whitened (expanded) space\cr
#'     4. projection on the slow functions
#'
#' @param sfaList 			A list that contains all information about the handled sfa-structure
#' @param DATA				Input data, each column a different variable
#' @param prj				If not NULL, the preprocessing step 1 is skipped for SFA2				
#' @param ncomp				number of learned functions to be used
#'
#' @return matrix \code{DATA} containing the calculated output \cr
#'
#' @references  \code{\link{sfa2}} \code{\link{sfa1}} \code{\link{sfaStep}}
#' @export
###################################################################################
sfaExecute <- function (sfaList, DATA, prj=NULL, ncomp=NULL){
	if(is.vector(DATA)){DATA=t(as.matrix(DATA))}
	else{DATA=as.matrix(DATA)};
	if (sfaList$deg>=2){
		if(is.null(prj)){#TODO: why is prj not used anywhere else here in this function? whats the use of it?			
			DATA=(DATA-customRep(sfaList$avg0,customSize(DATA,1)))%*%t(sfaList$W0); #check if this can be simplified
		}
		DATA=sfaList$sfaExpandFun(sfaList, DATA);
		DATA=DATA-customRep(sfaList$avg1,customSize(DATA,1));
		if(is.null(ncomp)){
			DATA=DATA%*%t(sfaList$SF);
		}
		else{			
			DATA=DATA%*%t(sfaList$SF[1:ncomp,]);
		}
	}
	else{   #deg==1
		DATA=DATA-customRep(sfaList$avg0,customSize(DATA,1));  #t() here is a fix, since a single vector here is a different default dimension than matlab, also used above
		if (!is.null(sfaList$SFWt)){
			DATA=DATA%*%sfaList$SFWt;
		}
		else{
			if(is.null(ncomp)){
				DATA=DATA%*%t(sfaList$SF);
			}
			else{			
				DATA=DATA%*%t(sfaList$SF[1:ncomp,]);
			}
		}
	}
	return(DATA)
}



