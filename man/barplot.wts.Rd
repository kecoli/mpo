\name{barplot.wts}
\alias{barplot.wts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
barplot.wts
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
barplot.wts(x, legend.text = NULL, col = NULL, ylab = NULL, xlab = NULL, bar.ylim = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{legend.text}{
%%     ~~Describe \code{legend.text} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{bar.ylim}{
%%     ~~Describe \code{bar.ylim} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, legend.text = NULL, col = NULL, ylab = NULL, xlab = NULL, 
    bar.ylim = NULL) 
{
    n = ncol(x)
    p = nrow(x)
    xpos = (abs(x) + x)/2
    xneg = (x - abs(x))/2
    if (is.null(bar.ylim)) {
        ymax <- max(colSums(xpos, na.rm = T))
        ymin <- min(colSums(xneg, na.rm = T))
        ylim = c(ymin, ymax)
    }
    else {
        ylim = bar.ylim
    }
    barplot(xpos, legend.text = legend.text, col = col, ylab = ylab, 
        xlab = xlab, args.legend = list(x = 1.2, y = bar.ylim[2], 
            cex = 0.5), ylim = bar.ylim)
    barplot(xneg, add = T, col = col)
    abline(h = 0)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
