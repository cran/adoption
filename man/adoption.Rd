\name{adoption}
\alias{adoption}
\alias{Bass model}
\title{
Modelling Utility Diffusion (GUI)
}
\description{
  This GUI allows real-time simulations of utility diffusion processes
  with various paramaters to better understand the dynamics of first
  trials.
}
\usage{
adoption(user = NULL, Tend=25, quantiles = c(0.25, 0.75),
         included.models = c("Bass (1969)",
	     		     "Modified Bass",
			     "Goldenberg et al. (2010)",
			     "Generalized Goldenberg",
			     "Rand & Rust (2011)",
			     "Autoregressive (VAR)"),
         dt = NULL, data = NULL, cumdata = NULL, ...)

}
\arguments{
  \item{user}{
    string or list of model specifications.
    If \code{model} is a string, the gui expects a name of file that has
    been created by the button \sQuote{pdf & save} in a preceding
    session.

    Otherwise \code{user} can be the return value of a preceding 
  }
  \item{Tend}{
    \code{Tstart} and \code{Tend} give start and end point,
    respectively, of the simulation period. The time step is given
    in the gui.
  }
   \item{quantiles}{
    if given, also the quantiles of the (cumulative) number of adopters
    is shown. Note that \code{quantiles} leads to a slower performance
    of the gui.
  }
  \item{included.models}{
    character vector of standard models that should be included in the
    gui. Default is all. 
  }
  \item{data, dt, cumdata}{
    A vector of number of trials \code{data} (or cummulative trials)
    and its corresponding time lag \code{dt}.
  }
  \item{...}{
    additional, secondary options, see \link{RFoptions}.
  }
}
\details{
  The package allows for parallel computing when the number of
  repetitions is larger than one. See the example below.
  HOWEVER THIS FUNCTION MAY NOT BE USED TOGETHER WITH THE R PACKAGE
  PARALLEL, OR SIMILAR PACKAGES.
  

  Comments on the model definition and the gui are given in file
  adoption_gui.pdf.
 
}
\value{
  A list of models with the latest parameters chosen.
}
\seealso{  See the file adoption_gui.pdf for an introduction.}
\adoption
\me
  
\examples{   %  library(adoption);  library(RandomFieldsUtils)
  RFoptions(cores=2)  ## see package RandomFieldsUtils
  adoption(gui=interactive())
}

\keyword{dynamic}
\keyword{models}
