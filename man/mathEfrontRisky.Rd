\name{mathEfrontRisky}
\alias{mathEfrontRisky}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mathEfrontRisky
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mathEfrontRisky(returns, npoints = 100, efront.only = T, display = T, digits = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{returns}{
%%     ~~Describe \code{returns} here~~
}
  \item{npoints}{
%%     ~~Describe \code{npoints} here~~
}
  \item{efront.only}{
%%     ~~Describe \code{efront.only} here~~
}
  \item{display}{
%%     ~~Describe \code{display} here~~
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
function (returns, npoints = 100, efront.only = T, display = T, 
    digits = NULL) 
{
    V = var(returns)
    mu = apply(returns, 2, mean)
    one = rep(1, nrow(V))
    z = solve(V, one)
    a = as.numeric(t(mu) \%*\% z)
    cc = as.numeric(t(one) \%*\% z)
    z = solve(V, mu)
    b = as.numeric(t(mu) \%*\% z)
    d = b * cc - a^2
    gmv = mathGmv(returns)
    sigma.gmv = gmv$vol
    mu.stocks = apply(returns, 2, mean)
    sigma.stocks = apply(returns, 2, var)^0.5
    mu.max = 2 * max(mu.stocks)
    sigma.max = (1/cc + (cc/d) * (mu.max - a/cc)^2)^0.5
    sigma.max = 1.2 * sigma.max
    sigma = seq(sigma.gmv, sigma.max, length = npoints)
    mu.efront = a/cc + ((d * sigma^2)/cc - d/cc^2)^0.5
    if (!efront.only) {
        mu.front = a/cc - ((d * sigma^2)/cc - d/cc^2)^0.5
    }
    mu[1] = a/cc
    xlim = c(0, max(sigma))
    if (efront.only) {
        ylim = range(mu.efront, mu.stocks)
    }
    else {
        ylim = range(mu.efront, mu.front)
    }
    if (display) {
        plot(sigma, mu.efront, type = "l", lwd = 3, xlim = xlim, 
            ylim = ylim, xlab = "VOL", ylab = "MEAN RETURN", 
            main = "PORTFOLIO FRONTIER")
        if (!efront.only) {
            lines(sigma, mu.front)
        }
        points(gmv$vol, gmv$mu, pch = 19)
        text(gmv$vol, gmv$mu, "GMV", cex = 1.2, pos = 2)
        points(sigma.stocks, mu.stocks, pch = 20)
        text(sigma.stocks, mu.stocks, names(returns), cex = 0.5, 
            pos = 4)
        text(0.07, 0.06, "EFFICIENT FRONTIER")
        arrows(0.07, 0.056, 0.09, 0.038, length = 0.1)
    }
    if (is.null(digits)) {
        out = list(mu.efront = mu.efront, vol.efront = sigma)
    }
    else {
        vol.efront = sigma
        out = rbind(mu.efront, vol.efront)
        out = round(out, digits = digits)
    }
    out
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
