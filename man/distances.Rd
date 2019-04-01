\name{distances}
\alias{GoldenbergDistance}
\alias{VarDistance}
\title{
Special distances for \command{\link{adoption}}
}
\description{
  Distance function given here are defined for the sole use within the model
  definition for \command{\link{adoption}}.
  These functions increase the processing speed in
  \command{\link{adoption}}.
  
  THESE FUNCTIONS SHOULD NEVER BE USED OUTSIDE THE MODEL DEFINITION FOR
  \command{\link{adoption}}. THEY SHOULD BE USED ONLY IN THE SAME WAY AS
  GIVEN IN THE EXAMPLES OF \command{\link{adoption}}, WITHOUT ANY MODIFICATIONS!
}
\note{
  Since these distance function modify the values of the argument
  \code{W} by reference, the use of these distance functions will nearly
  always lead to a system crash if these functions are used wrongly.
  However, it is save to use them in the model definition for defining
  the weight function, e.g.,
  
  \code{weight =  function(param, dist, W)}\cr
  \code{    GoldenbergDistance(param, dist, W, Goldenberg_C)}\cr
  
  which equivalent to (but much faster than)
  
  \code{weight = function(param, dist) \{ }\cr
  \code{    neighbour <- dist <= param[1] }\cr
  \code{    diag(neighbour) <- 0 }\cr
  \code{    neighbour / Goldenberg_C }\cr
  \code{\}}

  Note that the weight is, in the first piece of code, defined with
  an additional argument \code{W} more, which refers to a matrix of
  correct size in \command{\link{adoption}}.
}
 
\usage{
  GoldenbergDistance(param, dist, W, Goldenberg_C)
  VarDistance(param, dist, W)
}
\arguments{
  \item{param}{the weight parameter. For the Goldenberg distance it has
    (at least) two parameters; for the VAR distance it has one parameter.
  }
  \item{dist}{the matrix for Euclidean distances between
    the coordinates that are given by the function
    \code{coord} in the model definition.
  }
  \item{W}{A square matrix of size \code{m}, where \code{m} is the
    market size. Because of this argument, (nearly) any arbitrary
    use of the distance function will crash
    the whole system! Within adoption the correct size of the matrix
    will be passed.
  }
  \item{Goldenberg_C}{Some large constant, e.g. \eqn{1e6}}
}

\details{
  DO NOT USE THESE FUNCTIONS OUTSIDE THE MODEL DEFINITIONS FOR
  \command{\link{adoption}}.
}
\value{
  \code{NULL}
}

\adoption
\me
  
\examples{%  library(adoption);  library(RandomFieldsUtils)

Goldenberg <- list( ## model by Goldenberg, Libai, Muller (2010)
    m = 1000L,
    repetitions=10L,
    dt = 1,
    relative.instance = 0.2,
    SOCIAL = c(1, 5, 5),
    PRIVATE = c(5, 1, 5),
    Ic.start = function(param, m, rep, ...) {
      m * rnorm(m * rep, param[1], prod(param[1:2]))
    },
    Ic = function(param, Nt, m, start) {
      Inf * (2 * (Nt > start) - 1) ## start has size m * rep, i.e. Nt is
      ##                                   recycled
    },
    Ic.param = c("mean h" = 0.02,
		 "sigma" = 0.4),
    Ic.param.min = c(0.005, 0.08),
    Ic.param.max = c(0.1, 1.5),
    
    coord = function(param, m) {
      if (param[1] == 1) as.matrix(1:m)
      else {
	m2 <- ceiling(sqrt(m))
	m3 <- ceiling(m / m2)
	as.matrix(expand.grid(1:m2, 1:m3))[1:m, ]
      }},
    coord.param = c(dim = 2),
    weight =  function(param, dist, W) {
      GoldenbergDistance(param, dist, W, 1e6)
    },
 
    weight.param.min = 1.5,
    weight.param.max = 1.5,
    weight.param = c("max distance d"=1.5),
 
    Utrafo = function(U, threshold, ...) 1e6 * as.double(U>=threshold),
    
    Uthreshold = 0, ## here: constant for any people; we might
    Uthreshold.min = 0,
    Uthreshold.max = 0,
    Up.start = function(param, m, rep) rep(-1, m * rep),
    Up = function(param, m, nT, rep, ...) {
      pmax(-1e6 + 1,
	   -log(runif(nT * rep * m)/(1-param[1])) / log(1-param[2]))
      },
    Up.param = c(prob_a=0.1, prob_b=0.1),
    Up.param.min = c(0.005, 0.05),
    Up.param.max = c(0.99, 0.99),
    
    "MAX/PLUS OPERATORS" = rep(5, 3),
    alpha = c("alpha_1"=0, "alpha_2"=1),
    alpha.min = c(0, 1),
    alpha.max = c(0, 1),
    beta = c("beta_1"=1, "beta_2"=1),
    beta.min = c(1, 1),
    beta.max = c(1, 1),
    gamma = c("gamma_1"=0.5, "gamma_2"=0.5),
    gamma.min = c(0.5, 0.5),
    gamma.max = c(0.5, 0.5)
 )

RFoptions(cores=2)  ## see package RandomFieldsUtils
print(adoption(Goldenberg, join_models=FALSE, buttons2right=TRUE,
               gui=interactive()))
}

\keyword{dynamic}
\keyword{models}
