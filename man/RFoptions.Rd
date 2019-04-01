\name{RFoptions}
\alias{RFoptions}
\title{Setting control arguments}
\description{
 \command{\link{RFoptions}} sets and returns control arguments for the analysis
 and the simulation of random fields.
 See \code{\link[RandomFieldsUtils]{RFoptions}} for the
 definition of \code{RFoptions(..., no.readonly = TRUE)}.
}
%\usage{
% RFoptions(..., no.readonly = TRUE)
%}
%\arguments{
% \item{...}{arguments in \code{tag = value} form, or a list of tagged
% values.}
% \item{no.readonly}{If \command{\link{RFoptions}} is called without
%   argument then all arguments are returned in a list. If
%   \code{no.readonly=TRUE} then only rewritable arguments are returned.
%   Currenlty all arguments are rewritable. So the list is empty.
% }
%}
\details{
  Note that this function can only be used after
  \code{library(RandomFieldsUtils)}
  has been made available explicitely.
  
  The parameter for \pkg{adoption} are
  
  \describe{
    \item{buttons2right}{logical.
      The gui has many buttons and sliders. If
      \code{buttons2right=TRUE} 
      all buttons are placed on the right of the graphics, else about half
      of the stuff is placed below the graphics which gives a more compact view.
      if \code{buttons2right} is not given, it is \code{TRUE} for Windows
      sytems, else \code{FALSE}. For Windows systems \code{buttons2right=FALSE}
      does not work (The author is happy to get any hint why).

      Default: \code{NA}
    }
   
    \item{\code{cores}}{
      Number of cores for multicore algorithms;
      currently only used for the Cholesky decomposition.
      
      Default : 1
    }

    \item{\code{cumfit}}{Logical variable that directs the model fitting
      by least squares.
      If \code{TRUE} the model is fitted to the cummulative distribution
      function of the data, if \code{FALSE} to the density function.
      
      Default : \code{TRUE}
    }
    
    
     \item{extension}{string. The extension name R data files.
       Default: \code{"rda"}
     }
     
    \item{factr}{control parameter of \command{\link[stats]{optim}}
      Default: 1e14
    }

    \item{filename}{base name of the pdf files when
      the images are stored by mean of the button \sQuote{Images -> pdf}.
      The user may change the name in the gui.
      
      Only the basic name should be given, no extensio.
      Then a bundle of pictures is created
      when the button \sQuote{Images -> pdf} is pressed. Also an rda file
      is created that contains the whole session information.

      Default: \code{"adoption"}
    }

  
    \item{\code{fit_m}}{logical.
      if \code{TRUE} the market size \code{m} is also
      estimated. In the case, the estimation time is massively
      increased.

      Default: \code{FALSE}
    }

    \item{\code{fit_operators}}{
      If \code{FALSE}, what is the default, the
      parameters \code{alpha}, \code{beta}, \code{gamma}
      and \code{Uthreshold} are not suggested to be fitted.
      }
    
    \item{\code{fit_repetitions}}{
      Positive integer for the precision of the fitting.
      The actual number of repetitions used in the model fitting is the
      maximum of \code{fit_repetitions} and the value of the slider
      \sQuote{repetitions}.
     
      Default : 10
    }

 
    \item{fontsize}{integer between 6 and 12 or NA.      
      If \code{font.size} is \code{NA}, it chosen 7 for Windows sytems, else
      8.

      Default: \code{NA}
    }

     \item{gui}{
      logical. If \code{TRUE} the gui is opened. If \code{FALSE}
      the calculations for the first model are performed.
      The results are returned in model form.      
      Additionally if \code{\link[RandomFieldsUtils:RFoptions]{printlevel}}$\eqn{>0}
      (what is the default) all the graphics are drawn on the default
      graphical device.
      
      Default: TRUE
    }

    \item{join_models}{logical. If \code{TRUE} the model(s) passed by
      \code{user} are joined with the list of standard models. If it is
      \code{FALSE} 
      the list of models in the gui are replaced by the \code{user}
      models.  If it is \code{NA} and 
      \code{user} contains a single model then
      it behaves as \code{join_models = TRUE} 
 
      
      Default: \code{NA}
    }

    \item{max_increasing}{integer. The fitting of the 
      complete fitting procedure is stopped
      if no improvement is observed in \code{max_increasing}
      subsequent global iteration steps.
      
      Default:  \code{3}
    }
    

     \item{numberSteps}{integer.
      Each slider is divided into \code{numberSteps} parts.
      When one of the borders is crossed a new simulation is calculated.
      If \code{numberSteps} is large enough, this leaves the impression
      that the figures are continuously updated.
      
      Default: \code{25}
    }
    
    \item{pgtol}{control parameter of \command{\link[stats]{optim}}

      Default: 1e-1
    }

 
    \item{ratio}{Logical.
      Graphical parameter. If \code{TRUE} then density functions and
      cumulative distribution functions are shown, multiplied by 100.
      If \code{FALSE} then frequencies and 
      cumulative frequencies are shown.
      If \code{NA} then actual value of \code{ratio}
      is \code{TRUE} iff no data are given.
      
      \code{ratio} does not have
      an influence how parameters are fitted to data; the fitting is
      always based on frequency data.

      Default: \code{TRUE}
    }
    
    \item{screen.shot}{string giving the system command to create a
      screen shot.

      Default: \code{"xfce4-screenshooter -w -s ."}
    }
    
    \item{showNindiv}{
      integer scalar or vector.
      If it is a vector, it specifies the individuals, for which the
      utility functions should be shown. If it is a single number,
      it plots the utilities of the individuals \code{1:show.n.indiv}.

      Default: 10      
    }

    \item{simuOnTheFly}{
      logical or integer. This option controls the behavior of entry boxes.
      If it has \code{1} the simulations are updated by the tcltk
      binding "<FocusOut>", i.e. whenever another entry box is entered.
      values greater than 1 then the binding is "<KeyRelease>",
      i.e. simulations are updated whenever a key is pressed.
      If \code{FALSE} or \code{0} no binding happens; so an update does
      not happen and updates must be foreced through the button
      \sQuote{new simulation}
    }
    
    \item{sliderColumn}{
      Relative position of the sliders on the right handside of the
      graphics.

      Default: Windows:50' Linux:38
    }
    
    \item{startwith}{integer.
      the \eqn{n}th model within the list \code{included.model}
      to start with. This option is only considered if \code{user}
      is not a filename.

      Default: 1
    }

    \item{tracefit}{internal logical or integer value.     
%      This parameter determines
%      whether intermediate results are shown in the gui during
%      optimization. This parameter can large increase the time for
 %     fitting.
%      If \code{FALSE} or \code{0} then no intermediate results are
%      shown.
%      Otherwise, the results of after each optimization are shown.
%      If the value is at least \code{2} then each step within each
%      optimization are shown.
%
%     Default: \code{FALSE}
    }
    \item{Tstart}{
      \code{Tstart} and \code{Tend} give start and end point,
      respectively, of the simulation period. The time step is given
      in the gui.
      
      Default : 1
    }

     
    \item{wait}{
      if \code{wait} is negative, the prompt reappears after the gui is
      launched. The current session model is stored in
      \code{.adoption.exit}
      in the \code{.GlobalEnv} when leaving the gui.
      
      If \code{wait} is non-negative,
      the system checks every \code{wait} milli seconds
      whether the gui has finished and returns the current session
      model. 

      Default: 5000
    }
  
    
    \item{\code{windows}}{logical.
      If \code{TRUE} the fitting progress is shown in a separate window.
      If \code{NA} the value is set to \code{TRUE} iff the operation
      system is Windows.

      Default: \code{NA}.
    }

    \item{ymax}{Vector of 5 numbers that determines the rescaling of the
      graphs for \eqn{dN}, so that the same scale is kept as long as the
      values of  \eqn{dN} are not too small and too large relative to
      the current scale.

      Let \eqn{c} be the current upper limit of the graph and \eqn{n}
      the maximum value of \eqn{dN} for the next graph to be plotted. Then
      the following rules are applied.
      \enumerate{
	\item
	While \eqn{n > c * }\code{ymax[1]} do
	\eqn{c \leftarrow c *}\code{ymax[2]}
	\item
	If \eqn{n < c /}\code{ymax[3]} and
	\eqn{c \ge } \code{ymax[4] * m / 100}
	then \eqn{c \leftarrow c / } \code{ymax[5]}.
      }

      Default: \code{c(1.3, 3, 6.7, 0.02, 6)}      
    } 
  }
}

 
\value{
 \code{NULL} if any argument is given, and the full list of
 arguments, otherwise.
}

\adoption
\me

\seealso{
\code{\link[RandomFieldsUtils]{RFoptions}},
}


\examples{% library(adoption)
library(RandomFieldsUtils)
RFoptions(GETOPTIONS="adoption")
RFoptions(ymax=c(1.2, 2, 0.25, 0.02, 4))
adoption(gui=interactive())
}

\keyword{models}

