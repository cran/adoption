\name{Output of adoption}
\alias{print.adoption}
\alias{str.adoption}


\title{\code{print} and \code{str} for the output of the function \command{adoption}}

\description{\code{"adoption"} is an S3 class which indicates the
  output of the function \command{\link{adoption}}.
  The commands \command{print} and \command{str} give some nice output.
  The command \command{print} additionally returns the output within a list.
}


\usage{
\method{print}{adoption}(x,..., level=0)
\method{str}{adoption}(object, ..., give.attr=FALSE)
}


\arguments{
  \item{x,object}{objects of S3 class  \code{"adoption"}}
  \item{level}{integer. If \code{level<=0} only a verbal description is
    given.
    If \code{level=1} the important parameters of the models are
    returned.
    If \code{level>1} simulation results for the models are also
    returned where they are available.
  }
  \item{...}{optional arguments that are ignored}
  \item{give.attr}{logical. If \code{TRUE} the attributes are also printed.}
}
  

\value{
  \describe{
   \item{print}{The higher the \code{level}, the more details are
      printed. The print out is also returned within an invisible list.
      So, \code{z <- print(adoption(), level=1)} might make sense.
    }
    \item{str}{It uses essentially the basic \link[utils]{str} function.
      \command{str} returns \code{NULL}
    }
  }
}

\me

\examples{
RFoptions(cores=2)  ## see package RandomFieldsUtils

## only the model definitions and the simulation result for the
## first model (Bass, '_set_') are returned; no pictures are drawn
str(adoption(gui=FALSE, printlevel=0))

## pictures are drawn and the result returned by 'adoption' is
## reduced to most revelant information (level=1). 'print' also
## returns this information in a list
z <- print(adoption(gui=FALSE), level=1)
str(z, give.attr=FALSE)
}

\keyword{classes}
