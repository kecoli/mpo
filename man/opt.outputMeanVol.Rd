\name{opt.outputMeanVol}
\alias{opt.outputMeanVol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
opt.outputMeanVol
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
opt.outputMeanVol(opt, returns, digits = NULL, names = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{opt}{
%%     ~~Describe \code{opt} here~~
}
  \item{returns}{
%%     ~~Describe \code{returns} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
}
  \item{names}{
%%     ~~Describe \code{names} here~~
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
function (opt, returns, digits = NULL, names = NULL) 
{
    wts = opt$weights
    sigmasq = as.numeric(t(wts) \%*\% var(returns) \%*\% wts)
    sigma = sqrt(sigmasq)
    mu.ret = apply(returns, 2, mean)
    mu = as.numeric(t(wts) \%*\% mu.ret)
    if (is.null(digits)) {
        names(sigma) = "sigma"
        names(mu) = "mu"
        output = c(wts, mu, sigma)
    }
    else {
        if (is.null(names)) {
            output = list(wts = wts, mean = mu, vol = sigma)
            output = lapply(output, round, digits)
        }
        else {
            output = list(wts, mu, sigma)
            names(output) = names
            output = lapply(output, round, digits)
        }
    }
    output
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
