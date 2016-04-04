#' barplot.wts  
#' barplot.wts.efront
#'
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param x %% ~~Describe \code{x} here~~
#' @param legend.text %% ~~Describe \code{legend.text} here~~
#' @param col %% ~~Describe \code{col} here~~
#' @param ylab %% ~~Describe \code{ylab} here~~
#' @param xlab %% ~~Describe \code{xlab} here~~
#' @param bar.ylim %% ~~Describe \code{bar.ylim} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (x, legend.text = NULL, col = NULL, ylab = NULL, xlab = NULL, 
#'     bar.ylim = NULL) 
#' {
#'     n = ncol(x)
#'     p = nrow(x)
#'     xpos = (abs(x) + x)/2
#'     xneg = (x - abs(x))/2
#'     if (is.null(bar.ylim)) {
#'         ymax <- max(colSums(xpos, na.rm = T))
#'         ymin <- min(colSums(xneg, na.rm = T))
#'         ylim = c(ymin, ymax)
#'     }
#'     else {
#'         ylim = bar.ylim
#'     }
#'     barplot(xpos, legend.text = legend.text, col = col, ylab = ylab, 
#'         xlab = xlab, args.legend = list(x = 1.2, y = bar.ylim[2], 
#'             cex = 0.5), ylim = bar.ylim)
#'     barplot(xneg, add = T, col = col)
#'     abline(h = 0)
#'   }
#' 


barplot.wts = function(x,legend.text = NULL,col = NULL,ylab = NULL ,xlab = NULL,bar.ylim = NULL)
{
	n = ncol(x); p = nrow(x)
	xpos = (abs(x)+x)/2
	xneg = (x-abs(x))/2
	if(is.null(bar.ylim))
	{ymax <- max(colSums(xpos,na.rm=T))
		ymin <- min(colSums(xneg,na.rm=T))
		ylim = c(ymin,ymax)}   else {ylim = bar.ylim}
	barplot(xpos,legend.text = legend.text,col = col,ylab = ylab,xlab = xlab,
			ylim = bar.ylim, las=2)
	axis(1,labels=colnames(xpos),las=2)
	barplot(xneg,add = T,col = col,axisnames=FALSE)
	abline(h=0)
}


barplot.wts.efront = function(wts.efront,legend.text = NULL,col = NULL,ylab = NULL ,xlab = c("MU","VOL"),bar.ylim = NULL)
{
	xlab.choose <- match.arg(xlab)
#   cat(xlab.choose,"\n")
	xlab <- wts.efront[xlab.choose,]
	xlab <- round(xlab,4)
	xlab <- sprintf("%.4f", xlab)
	x <- wts.efront[-c(1,2),]
	n = ncol(x); p = nrow(x)
	xpos = (abs(x)+x)/2
	xneg = (x-abs(x))/2
	if(is.null(bar.ylim))
	{ymax <- max(colSums(xpos,na.rm=T))
		ymin <- min(colSums(xneg,na.rm=T))
		ylim = c(ymin,ymax)}   else {ylim = bar.ylim}
	colnames(xpos) <- xlab
	barplot(xpos,legend.text = legend.text,col = col,ylab = ylab,xlab = xlab.choose,
			ylim = bar.ylim, las=2, cex.names=0.8, bty="n")
	barplot(xneg,add = T,col = col,axisnames=FALSE,axes=FALSE)
	abline(h=0)
}





