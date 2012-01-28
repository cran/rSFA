###################################################################################
#' Perform non-linear regression 
#' 
#' Given the data in arg, expand them nonlinearly in the same way as it was
#' done in the SFA-object sfaList (expanded dimension M) and search the vector
#' RCOEF of M constant coefficients, such that the sum of squared residuals 
#' between a given function in time FUNC and the function\cr
#'       R(t) = (v(t) - v0)' * RCOEF,        t=1,...,T,\cr
#' is minimal  
#'
#' @param sfaList 			A list that contains all information about the handled sfa-structure
#' @param arg				Input data, each column a different variable
#' @param func				(T x 1) the function to be fitted nonlinearly 
#'
#' @return returns a list \code{res} with\cr
#'	 \code{res$R} (T x 1) the function fitted by NL-regression 
#'	 \code{res$rcoef} (M x 1) the coefficients for the NL-expanded dimensions 
#' @export
###################################################################################
sfaNlRegress <- function (sfaList, arg, func){
	#to matrix
	arg=as.matrix(arg)
	# project data and reduce dimensionality
	arg=arg-customRep(sfaList$avg0,customSize(arg,1));
	# expand data
	arg=sfaExpand(sfaList, arg%*%t(sfaList$W0));
	arg=arg-customRep(sfaList$avg1,customSize(arg,1));
	#now ARG contains (v(t)-v0)' (rows: t, columns: expanded space dim M)
	fmat = arg * customRepmat (t(func),1,customSize(arg,2)); #no matrix mult
	df1 =  colMeans(fmat);  #apply(fmat,2,mean);        # column mean
	rcoef = ginv(sfaList$myB)%*%df1;
	# the above line is for regular B (=myB) equivalent to 
	#    rcoef = SFA_STRUCTS{hdl}.myB \ d_f;
	# But pinv() (ginv() in R) works better, if B is singular
	R= arg %*% rcoef
	res=list()
	res$R<-R
	res$rcoef<-rcoef
	return(res)
}