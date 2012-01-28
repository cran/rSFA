###################################################################################
#' Create structured list for linear SFA
#'
#' @param sfaRange 			number of slowly-varying functions to be kept
#' @param axType			is the type of derivative approximation to be used, see: \code{\link{sfaTimediff}}
#' @param regCt				regularization constant is not used at the moment		
#'
#' @return list \code{sfaList} contains all settings for sfa1\cr
#'
#' @references \code{\link{sfa1}} \code{\link{sfaStep}} \code{\link{sfa2Create}}
#' @export
###################################################################################
sfa1Create <- function (sfaRange, axType="ORD1", regCt=0){
	sfaList=list()
	sfaList$axType=axType
	if (!(sfaList$axType=="ORD1" | sfaList$axType=="ORD3a" )){
		sfaList$axType="ORD1";
	}
	sfaList$regCt=regCt;
	sfaList$sfaRange=sfaRange;
	sfaList$step="init";
	sfaList$deg=1;
	return(sfaList);
}

###################################################################################
#' Create structured list for expanded SFA
#'
#' @param ppRange 			number of dimensions to be kept after preprocessing step
#' @param sfaRange 			number of slowly-varying functions to be kept
#' @param ppType			preprocessing type: ="PCA" (principal component analysis) or ="SFA1" (linear sfa)
#' @param axType			is the type of derivative approximation to be used, see: \code{\link{sfaTimediff}}
#' @param regCt				regularization constant is not used at the moment
#' @param opts				optional list of additional options
#' @param xpDimFun			Function to calculate dimension of expanded data
#' @param sfaExpandFun		Function to expand data 
#'
#' @return list \code{sfaList} contains all settings for sfa2\cr
#'
#' @references  \code{\link{sfa2}} \code{\link{sfaStep}} \code{\link{sfa1Create}}
#' @export
###################################################################################
sfa2Create <- function (ppRange, sfaRange, ppType="SFA1", axType="ORD1", regCt=0, opts=NULL, xpDimFun=xpDim, sfaExpandFun=sfaExpand){
	sfaList=list()
	sfaList$ppRange=ppRange;	
	sfaList$xpDimFun=xpDimFun;
	sfaList$sfaExpandFun=sfaExpandFun;
	if(length(ppRange)==2){  
		ppDim=ppRange[2]-ppRange[1]+1;
	}
	else{
		ppDim=ppRange;
	}
	sfaList$xpRange=xpDimFun(ppDim);
	sfaList$sfaRange=sfaRange;
	sfaList$ppType=ppType;
	sfaList$axType=axType;
	sfaList$regCt=regCt;
	sfaList$opts=opts;		
	sfaList$step="init";
	sfaList$deg=2;
	return(sfaList);
}