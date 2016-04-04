\name{mathWtsEfrontRisky}
\alias{mathWtsEfrontRisky}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mathWtsEfrontRisky
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mathWtsEfrontRisky(returns, mu.efront, bar.ylim = c(0, 1), display.wts = T, digits = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{returns}{
%%     ~~Describe \code{returns} here~~
}
  \item{mu.efront}{
%%     ~~Describe \code{mu.efront} here~~
}
  \item{bar.ylim}{
%%     ~~Describe \code{bar.ylim} here~~
}
  \item{display.wts}{
%%     ~~Describe \code{display.wts} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
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
function (returns, mu.efront, bar.ylim = c(0, 1), display.wts = T, 
    digits = NULL) 
{
    V = var(returns)
    mu = apply(returns, 2, mean)
    one = rep(1, nrow(V))
    z1 = solve(V, one)
    a = as.numeric(t(mu) \%*\% z1)
    cc = as.numeric(t(one) \%*\% z1)
    z2 = solve(V, mu)
    b = as.numeric(t(mu) \%*\% z2)
    d = b * cc - a^2
    g1 = as.matrix((b * z1 - a * z2)/d, ncol = 1)
    g2 = as.matrix((cc * z2 - a * z1)/d, ncol = 1)
    n = length(mu.efront)
    a1 = matrix(rep(g1, n), ncol = n)
    a2 = g2 \%*\% mu.efront
    wts.efront = a1 + a2
    MU.EFRONT = mu.efront
    VOL.EFRONT = (1/cc + (cc/d) * (mu.efront - a/cc)^2)^0.5
    if (display.wts) {
        barplot.wts(wts.efront, legend.text = T, col = topo.colors(10), 
            ylab = "WEIGHTS", xlab = "VOL", bar.ylim = c(-2, 
                6))
    }
    if (is.null(digits)) {
        out = list(mu = MU.EFRONT, vol = VOL.EFRONT, wts = wts.efront)
    }
    else {
        out = rbind(MU.EFRONT, VOL.EFRONT, wts.efront)
        out = as.data.frame(round(out, digits = digits))
        names(out) = paste("PORT", 1:n, sep = "")
    }
    out
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
