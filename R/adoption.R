## Authors 
## Martin Schlather, schlather@math.uni-mannheim.de
##
## Copyright (C) 2018 -- 2019 Martin Schlather
##
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License
## as published by the Free Software Foundation; either version 3
## of the License, or (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOCSE.  Sentryvaee the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.  

printWithNames <- function(yy, level=0) {
  N <- names(yy)
  ncharN <- nchar(N)
  maxN <- max(ncharN)
  for (j in 1:length(yy)) {
    cat("\n ")
    if (level > 0) cat(rep(" ..", level), sep="")
    cat("$ ", N[j], formatC("", width=maxN-ncharN[j]), ": ", sep="")
    len <- length(yy[[j]])
    ynames <- names(yy[[j]])
    if (!is.null(ynames))
      ynames <- sapply(strsplit(ynames, ' \\['), function(x) x[[1]])
    if (is.integer(yy[[j]])) cat("int ")
    else if (is.numeric(yy[[j]])) cat("num ")
    else if (is.list(yy[[j]])) {
      cat("List of", length(yy[[j]]))
      return(printWithNames(yy[[j]], level=level+1))
    }
    
    if (len < 1) cat("(0)")
    else {
      if (len > 1) {
        if (is.vector(yy[[j]])) cat("[1:", length(yy[[j]]), "]", sep="")
        else cat("[", paste0("1:", dim(yy[[j]]), collapse=", "), "]", sep="")
      }
      if (is.null(ynames)) {
        if (length(yy[[j]]) <= 6) cat(yy[[j]])
        else cat("", round(digits=2, yy[[j]][1:6]), "...")
      } else cat(paste0(" '", ynames, "'=", yy[[j]], collapse=","))
    }
  }
  cat("\n\n")
}

print.adoption <- function(x,..., level=0) {
  if (!is.list(x[[1]])) stop("not an adoption model (list of lists expected)")
  if (level < 1) {
    if (!hasArg("level"))
      cat("The object returned by `adoption' contains ", length(x),
          " definition", if (length(x) > 1) "s", # ":\n",
          " for models and sets.\n",
          "Use argument level=1 (or 2) in 'print' to see more details.\n",
          "To see the complete content use 'str'.\n\n", sep="")
    return(invisible(names(x)))
  } else {
    if (level == 1) x[["_set_"]] <- NULL
    else {
      info <- list()
      while(length(x[["_set_"]]) > 0) {
        info[[length(info) + 1]] <- x[["_set_"]]$setinfo 
        x[["_set_"]] <- NULL
      }
      if (length(info) > 0) {
        for (i in 1:length(info)) 
          x[[info[[i]]$class]]$simuvalues <- info[[i]]$simuvalue
      }
    }
    y <- lapply(x, function(z) {
      z$classinfo <- z$coord.param <- z$Ic <- z$Ic.start <- 
          z$coord <- z$weight <- z$Utrafo <- z$Up.start <- z$CrossReferences <-
            z$Up <- z$theor.dN <- NULL
      z })
    names <- names(y)
    for (ix in 1:length(y)) {
      yy <- y[[ix]]
      if (!is.null(names[ix]))
        cat("'", names[ix], "'",
            #"is a list of ", length(yy), " elements:",
            sep="")
      printWithNames(yy)
    }
    return(invisible(y))
  }
}

str.adoption <- function(object, ..., give.attr=FALSE) {
  if (!is.list(object[[1]]))
    stop("not an adoption model (list of lists expected)")
  names <- names(object)
  for (ix in 1:length(object)) {
    if (!is.null(names[ix])) cat("'", names[ix], "' is a ", sep="")
      utils::str(object[[ix]],..., give.attr=give.attr) ##
  }
  invisible(NULL)
}


adoption <- function(user = NULL,
		     Tend=25,
		     quantiles = c(0.25, 0.75),
		     included.models = c("Bass (1969)",
					 "Modified Bass",
					 "Goldenberg et al. (2010)",
					 "Generalized Goldenberg",
					 "Rand & Rust (2011)",
					 "Autoregressive (VAR)"),
                     dt = NULL,
                     data = NULL,
                     cumdata = NULL,
		     ...
		     ) {
  if (length(list(...)) > 0) {
    RFoptOld <- RFoptions(SAVEOPTIONS="adoption", ...)
    on.exit(RFoptions(LIST=RFoptOld))
  }
  RFopt <- RFoptions(GETOPTIONS=c("basic", "adoption"))
  basic <- RFopt$basic
  optAdo <- RFopt$adoption

  sysname <- Sys.info()["sysname"]
  if (sysname == "maxOS" && optAdo$screen.shot == "xfce4-screenshooter -w -s .")
    optAdo$screen.shot <- "screencapture -w -W -i ~/Desktop/capture$(date +%Y%m%d-%H%M%S).png"
 
  filename <- optAdo$filename  
  join_models <- optAdo$join_models
  wait <- optAdo$wait
  ext <- paste0(".", optAdo$extension)
	     
  included.models <- match.arg(included.models, several.ok = TRUE)
  all.included.models <- eval(as.list(args(adoption))$included.models)
  if (length(user) > 0) {
    if (is.na(join_models))
        join_models <- !is.character(user) && is.list(user) &&
            (!is.list(user[[1]]) || length(user) == 1)
    if (is.character(user)) {
      if (substring(user, nchar(user), nchar(user) - 3) != ext) {
	filename <- user
        user <- paste0(user, ext)
      } else filename <- substring(user, 1, nchar(user) - 3)
      load (user)
    }
  }


#### see https://www.tcl.tk/man/tcl/TkCmd/label.htm
  
#  if (!interactive()) {
#    warning("'adoption' can be used only in an interactive mode")
#    #return(NULL)
#  }
  wait <- as.integer(wait)
  Env <- if (wait >= 0) environment() else .GlobalEnv
  if (exists(".adoption.exit", .GlobalEnv))
    rm(".adoption.exit", envir=.GlobalEnv)

  order.show <- list("m", ## market (number of potential consumers) MUSS 1. sein
		     "repetitions", ## how often is simulation study repeated?
		     "dt", ## time increment
		     "relative.instance", ## only when showing spatial pattern.
		     ##     Then it gives the time instance at which it is shown
		     "SOCIAL: U_c" = c(1),
		     "Ic.param",
		     "U_r" = c(1,3),
		     "weight.param",
		     "coord.param" = FALSE,
		     "PRIVATE: U_p" = c(1,3,1),
		     "Up.param",
		     "U_p (start)" = 1,
		     "Up.start.param",
		     "OTHERS" = 3,
		     "Uthreshold",
		     "MAX/PLUS OPERATORS" = c(2, 2, 2),
		     "alpha",
		     "beta",
		     "gamma" 	      
		     )
  
  
  constraints <- list(
                   m.min = 4L,
                   m.max = 2000L,
		   repetitions.min = 1L,
		   repetitions.max = 20L,
		   dt.min = 0.01,
		   dt.max = 1,
		   relative.instance.min = 0,
		   relative.instance.max = 1,
		   Ic.param.min = c(-10, -10, 0),
		   Ic.param.max = c(10, 10, 1),		   
		   weight.param.min = c(-3, 0),
		   weight.param.max = c(3, 1),
		   coord.param.min = 1,
		   coord.param.max = 10,
		   coord.param.integer = TRUE,
		   Uthreshold.min = -10,
		   Uthreshold.max = 10,
		   Up.start.param.min = c(-50, -50),
		   Up.start.param.max = c(10, 10),
		   Up.param.min = c(0, 0),
		   Up.param.max = c(1, 1), 
		   alpha.min = c(0, 0),
		   alpha.max = c(1, 1),
		   beta.min = c(0, 0),
		   beta.max = c(1, 1),
		   gamma.min = c(0, 0),
		   gamma.max = c(1, 1)
  )

  Bass69 <- list(
    m = 1000,    
    repetitions=1L,
    repetitions.max=1,
    dt = 0.1,
    relative.instance = 0,
    max.relative.instance = 0,
    SOCIAL = c(1, 3, 3),
    Ic.start = NULL,
    Ic =  function(param, Nt, m, ...) {
      rep(- 4 * Nt * (param[1] - param[2] + param[2] * Nt / m), each=m)
    },
    Ic.param = c("innovation p" =  0.02,
		 "imitation q" = 0.4),
    Ic.param.min = c(1e-4, 1e-4),
    Ic.param.max = c(0.3, 1),
    
    ##    coord = function(param, m) as.matrix(1:m),
    ##   coord.param = c(dim = 2),
    ##   weight = function(param, dist) dist * 0,
 
    Utrafo = function(U, ...) 4 * U,
    
    PRIVATE = rep(5, 3),
    Up.start = function(param, m, rep) base::rep(-(1:m), rep),
    CrossReferences = function() TRUE, ## kann noch ausgebaut werden; besagt
    ##                             hier, dass Ic.param auch Up beeinflusst
    ##           da environment neu gesetzt wird, ist kein zugriff der Funktion
    ##           auf die definierende Umgebung mehr moeglich.
    ##		 Goldenberg_C <- 1e6 geht z.B. nicht mehr
    Up = function(param, dt, m, nT, rep)
      rep(2 * dt * m * Value("Ic.param", 1), nT * rep * m),      
    Uthreshold = 0,
    Uthreshold.min = 0,
    Uthreshold.max = 0,
 
    "MAX/PLUS OPERATORS" = rep(5, 3),
    alpha = c("alpha_1"=1, "alpha_2"=1),
    alpha.min = c(1, 1),
    alpha.max = c(1, 1),    
    beta = c("beta_1"=0.5, "beta_2"=0.5),
    beta.min = c(0.5, 0.5),
    beta.max = c(0.5, 0.5),
    gamma = c("gamma_1"=0.5, "gamma_2"=0.5),
    gamma.min = c(0.5, 0.5),
    gamma.max = c(0.5, 0.5),

    theor.dN = function(Ic.param, t, m) {
      p <- Ic.param[1]
      q <- Ic.param[2]
      E <- exp((q+p) * (t + log(p / m)/(p+q)))
      Nt <- (m * E - p) / (E + q/ m)
      (m - Nt) * (p + q * Nt / m)
    }
  )
  
  Bass69_M_p <- 0.02
  Bass69_M <-  list(
    m = 1000L,
    repetitions=1L,
    dt = 0.1,
    relative.instance = 0,
    SOCIAL = c(1, 1, 3),
    Ic.start = NULL,
    Ic = function(param, Nt, m, ...)
      rep(- 4 * Nt * (param[1] - param[2] + param[2] * Nt / m), each=m),
    Ic.param = c("innovation p" = Bass69_M_p,
		 "imitation q" = 0.4),
    Ic.param.min = c(1e-4, 1e-4),
    Ic.param.max = c(0.3, 1),
 

    #   coord = function(param, m) as.matrix(1:m),
    #   coord.param = c(dim = 2),
    #  weight = function(param, dist) dist * 0,
 

    Utrafo = function(U, ...) 4 * U,

    Up.start.param = c("maximal Utility"=0,    ##
			"scattering rel. to m"=1),
    Up.start.param.min = c(-1000, 0.001),
    Up.start.param.max = c(0.1, 10),
    Up.start = function(param, m, rep) {
      p <- c(param[1], param[1] - param[2] * m)
      runif(rep * m, min(p), max(p)) 
    },
    Up = function(param, dt, m, nT, rep) {
      ## for stochastic part:
      ## Factor 2 : from variogram to variance
      ## another Factor 4 : to balance out twice the max|plus operator
      factor <- 2 * dt * m * param[1] ## factor because of max|plus operator
      rep(factor, nT * rep * m)
      ## RMfbm(var=8 * param[2]) + factor * R.p(1)
    },
    Up.param = c("Up slope factor s"= Bass69_M_p   ##
					#"Up variance v"=0
					   ), # p1 oben
    Up.param.min = c(1e-4),
    Up.param.max = c(0.3),
    Uthreshold = 0,
 
    "MAX/PLUS OPERATORS" = c(5, 2, 2),
    alpha = c("alpha_1"=1, "alpha_2"=1),
    alpha.min = c(1, 1),
    alpha.max = c(1, 1),    
    beta = c("beta_1 [e.g. max=(1,0)]"=0.5, "beta_2 [e.g. x=(1,1)]"=0.5),
    gamma = c("gamma_1"=0.5, "gamma_2"=0.5),
    
    theor.dN = function(Ic.param, t, m) {
      p <- Ic.param[1]
      q <- Ic.param[2]
      E <- exp((q+p) * (t + log(p / m)/(p+q)))
      Nt <- (m * E - p) / (E + q/ m)
      (m - Nt) * (p + q * Nt / m)
    }  
  )

  Goldenberg_C <- 1e6
  Goldenberg_G <- list( ## from Goldenberg, Libai, Muller. 2010
    ## missing: Internal factors: Some probability b exists such that during a
    ## giventime period, an individual will be affected by an interaction (e.g.,
    ## word of mouth) with exactly one other individual who has already
    ##adopted the product.
    m = 1000L,
    repetitions=10L,
    dt = 1,
    relative.instance = 0.2,
    PRIVATE = c(5, 1, 3),
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
      GoldenbergDistance(param, dist, W, Goldenberg_C)
    },
    ## NEVER DELETE THIS INFORMATION
    ## above funciton is equivalent to
    ## Goldenberg $ weight <- function(param, dist) {
    ##   neighbour <- dist <= param[1]
    ##   diag(neighbour) <- 0
    ##   neighbour / Goldenberg_C
    ##  }

    weight.param.min = c(0, 0),
    weight.param.max = c(5, 1),
    weight.param = c("max distance d"=1.5, # > sqrt(2) + resolution error
		      "prob of aversion"=0),
 
    Utrafo = function(U, threshold, ...) Goldenberg_C * as.double(U>=threshold),
    
    Uthreshold = 0, ## here: constant for any people; we might
    Up.start = function(param, m, rep) rep(-1, m * rep),
    Up = function(param, m, nT, rep, ...) {
      pmax(-Goldenberg_C + 1,
	   -log(runif(nT * rep * m)/(1-param[1])) / log(1-param[2]))
      },
    Up.param = c(prob_a=0.1, prob_b=0.1),
    Up.param.min = c(0.005, 0.05),
    Up.param.max = c(0.99, 0.99),
    
    "MAX/PLUS OPERATORS" = c(5, 2, 2),
    alpha = c("alpha_1"=0, "alpha_2"=1),
    alpha.min = c(0, 1),
    alpha.max = c(0, 1),
    beta = c("beta_1 [e.g. max=(1,0)]"=1, "beta_2 [e.g. x=(1,1)]"=1),
    gamma = c("gamma_1"=0.5, "gamma_2"=0.5)
  )


  Goldenberg <- Goldenberg_G
  Goldenberg $ SOCIAL <- c(1, 5, 5)
  Goldenberg $ PRIVATE <- c(5, 1, 5)
  Goldenberg $ "MAX/PLUS OPERATORS" <- rep(5, 3)
  Goldenberg $ weight.param.min <- 1.5 # > sqrt(2) + resolution error
  Goldenberg $ weight.param.max <- 1.5 # > sqrt(2) + resolution error
  Goldenberg $ weight.param <- c("max distance d"=1.5) # > sqrt(2) + resol error
  Goldenberg $ Uthreshold.min <- 0
  Goldenberg $ Uthreshold.max <- 0
  Goldenberg $ alpha.min <- c(0, 1)
  Goldenberg $ alpha.max <- c(0, 1)
  Goldenberg $ beta <- c("beta_1"=1, "beta_2"=1)

  Goldenberg $ beta.min <- c(1, 1)
  Goldenberg $ beta.max <- c(1, 1)
  Goldenberg $ gamma.min <- c(0.5, 0.5)
  Goldenberg $ gamma.max <- c(0.5, 0.5)

  
  randrust <- Goldenberg
  randrust $ Up <- function(param, m, nT, rep, ...) {
    param[2] * (runif(nT * rep * m) / (1-param[1]) - 1)
  }
  
  randrust $ Up.param = c(z_1=0.025, z_2=0.1)
  randrust $ Up.param.min <- c(0.001, 0.001)
  randrust $ Up.param.max <- c(0.99, 0.99)
  

  var <- list(
    m = 1000L,
    repetitions=10L,
    dt = 1,
    relative.instance = 0.2,

    SOCIAL = c(1, 1, 3),
    Ic.start = NULL,
    Ic = NULL,

    coord = function(param, m) {
      if (param[1] == 1) as.matrix(1:m)
      else {
   	m2 <- ceiling(sqrt(m))
	m3 <- ceiling(m / m2)
	as.matrix(expand.grid(1:m2, 1:m3))[1:m, ]
    }},
    coord.param = c(dim = 2),  	      
    weight = function(param, dist, W) VarDistance(param, dist, W),
    ## NEVER DELETE THIS INFORMATION
    ## above funciton is for the generalized Goldenberg model equivalent to
    ## weight =  function(param, dist) {
    ##   neighbour <- dist <= param[1]
    ##   diag(neighbour) <- FALSE
    ##   n <- sum(neighbour)
    ##   w <- neighbour / Goldenberg_C
    ##   if (param[2] > 0) {
    ##     idx <- which(neighbour)[rbinom(n, 1, prob=param[2]) == 1]
    ##     w[idx] <- -1 / Goldenberg_C
    ##   }
    ##   w
    ## },
    ##  The above funciton is for the original Goldenberg model equivalent to
    ##  weight = function(param, dist) {
    ##  neighbour <- exp(-dist * param[1])
    ##  diag(neighbour) <- 0
    ##  neighbour
    ## },
    
    weight.param = c("spatial scale k"=1),

    Utrafo = function(U, ...) 2 * U,
     
    Uthreshold = 0,
    Up.start = function(param, m, rep) runif(m * rep, min(param), max(param)),
    Up.start.param = c("min. start value"=-12,
		       "max. start value"=-6),
    Up = function(param, ...) {
      if (TRUE || !exists("RMfbm")) {
        ##	message("package 'RandomFields' is not installed -- the gui only works for Up driven by Wiener process.")
	return(NULL)
      }
      ## RMfbm(alpha=1, var=4 * param[1]^2)
    },
    Up.param = c(Up.sigma=0.5),

    "MAX/PLUS OPERATORS" = c(5, 2, 2),
    alpha = c("alpha_1"=1, "alpha_2"=0),
    alpha.min = c(1, 0),
    alpha.max = c(1, 0),
    beta = c("beta_1 [e.g. max=(1,0)]"=1, "beta_2 [e.g. x=(1,1)]"=1),
    gamma = c("gamma_1"=0.5, "gamma_2"=0.5)
  )

  
  modelclasses <-list()
  for (i in included.models)
    modelclasses[[i]] <- switch(pmatch(i, all.included.models),
			   "1" = Bass69,
			   "2" = Bass69_M,
			   "3" = Goldenberg,
			   "4" = Goldenberg_G,
			   "5" = randrust,
			   "6" = var
			   )
  if (length(user) > 0) {
    if (!is.list(user[[1]])) user <- list(user = user)
    modelclasses <- if (join_models) c(user, modelclasses) else user
  }
 
  ##  par.old <- par(no.readonly = TRUE); par.old$new <- NULL;  on.exit(par(par.old))
  Value <- function() {} ## This is just to avoid a warning by the CRAN checker

  Ugui.intern(currentClass = optAdo$startwith,
	      Tstart = optAdo$Tstart,
	      Tend = Tend,
	      screen.shot =  optAdo$screenshot,
	      show.n.indiv = optAdo$showNindiv,
	      quantiles = quantiles,
	      order.show = order.show,
	      constraints = constraints,
              parent.ev = Env,
	      modelclasses = modelclasses,
	      filename = filename,
	      gui = optAdo$gui,
	      fontsize = optAdo$fontsize,
	      buttons2right = optAdo$buttons2right,
	      p_ymax = optAdo$ymax,
	      numberSteps = optAdo$numberSteps,
	      sliderColumn = optAdo$sliderColumn,
              data = data,
              cumdata = cumdata,
              dt = dt,
              cumfit = optAdo$cumfit,
              fit_repetitions = optAdo$fit_repetitions,
              fit_operators = optAdo$fit_operators,
              ratio = if (is.na(optAdo$ratio)) length(data)+length(cumdata) == 0
                      else optAdo$ratio,
              tracefit = optAdo$trace,
              pgtol = optAdo$pgtol,
              factr = optAdo$factr,
              fit_m = optAdo$fit_m,
              simuOnTheFly = optAdo$simuOnTheFly,
              windows = optAdo$windows,
              max_increasing = optAdo$max_increasing,
              PL = basic$printlevel,
              cores = basic$cores
	      )

  if (wait >= 0) {
    while (!exists(".adoption.exit", envir=Env))
      RandomFieldsUtils::sleep.micro(wait)
    res <- get(".adoption.exit", envir=Env)
    rm(".adoption.exit", envir=Env)
    if (length(res) == 0) return(res)
    for (i in 1:length(res)) {
      if (is.function(res[[i]])) environment(res[[i]]) <- .GlobalEnv
    }
    class(res) <- "adoption"
    return(res)
  } else return(invisible(NULL))
}

Ugui.intern <- function(currentClass,
			Tstart, Tend,
			screen.shot = "xfce4-screenshooter -w ",
			show.n.indiv,
			quantiles,
			order.show,
			constraints,
			parent.ev=NULL,		
			quantile.type = 5, ## 7 is standard
			colour=c("black",  "darkred", "darkblue",
				      "orange", "forestgreen"),
			subcol=c("grey",  "red", "lightblue",
				     "yellow", "lightgreen"),
			fgcol=c("white", "white", "white", "white", "white"),
                        other.col= # c("green", "red", "yellow"),
                           c("pink", ## data
                             "orange", ## theoretical 
                             "purple"), ## fitted
			modelclasses = NULL,
			Spatial = FALSE,
			filename = "adoption",
			gui,
			fontsize,
			buttons2right,
			p_ymax,
			numberSteps,
			sliderColumn,
                        data = NULL,
                        cumdata = NULL,
                        dt = NULL,
                        cumfit = TRUE,
                        fit_repetitions,
                        fit_operators,
                        ratio,
                        tracefit,
                        pgtol,
                        factr,
                        fit_m,
                        simuOnTheFly = TRUE,
                        windows = NA,
                        max_increasing,
                         PL = 1,
                        cores=1
                        ) {
  if (length(data) > 0) {
    if (any(data != as.integer(data)) || any(data < 0))
      stop("data must consist of non-negative integer")
    n.data <- sum(data)
    if (n.data < 10)
      stop("With less than 10 total trials this looks an unsound data set.")
    if (n.data > 1000)
      message("NOTE: with more than 1000 trials the gui can be (very) slow.")
    if (is.null(dt)) stop("'dt' must be given explicite when data are given.")
    if (length(cumdata) > 0) stop("either 'data' or 'cumdata' might be given.")
    cumdata <- cumsum(data)
    Tend <- Tstart + (length(data) - 1) * dt
 } else if (length(cumdata) > 0) {
    idx <- !is.na(cumdata)
    cd <- cumdata[idx]
    if (any(cd != as.integer(cd)) || any(cd < 0))
      stop("cumdata must consist of non-negative integer")
    n.data <- max(cd, na.rm = TRUE)
    if (n.data < 10)
      stop("With less than 10 total trials this looks an unsound cumdata set.")
    if (n.data > 1000)
      message("WARNING: with more than 1000 trials the gui can be very slow.")
    if (is.null(dt)) stop("'dt' must be given explicite when cumdata is given.")
    data <- c(cumdata[1], diff(cumdata))   
    Tend <- Tstart + (length(data) - 1) * dt
 } else {
   n.data <- 0
   if (!is.null(dt)) stop("'dt' may be given only when data are given.")
 }

  sysname <- Sys.info()["sysname"]
  win.sys <- sysname == "Windows"

  Windows <- if (is.na(windows)) win.sys else windows
  ColumnNumber <- Idx.Ic.param <- Idx.Up.param <- Idx.Up.start.param <-
    Idx.Uthreshold <- Idx.alpha <- Idx.beta <- Idx.coord.param <- Idx.dt <-
      Idx.gamma <- Idx.m <- Idx.relative.instance <- Idx.repetitions <-
        Idx.weight.param <- W.by.reference <- plotting <- t.W.list <-  NULL 
  
  if (Windows) {
    LineNumber <- 0
    MaxColumns <- 30
    progress.clr <- c("purple", "orange")
    fit.dev <- -1
    Message <- function(...) {
      message(...)
      LineNumber <- get("LineNumber", envir=ENVIR)
      if (!fit.dev %in% dev.list()) LineNumber <- 0
      if (LineNumber == 0) {
        if (fit.dev %in% dev.list()) dev.off(fit.dev)
        dev.new(width=4, height=8, title= "Messages")
        par(mar=rep(0, 4))
        LineNumber <- 999
        assign("fit.dev", dev.cur(), envir=ENVIR)
      } else dev.set(fit.dev)
      
      if (LineNumber >= 38) {
        plot(Inf, Inf, xlim=c(0,MaxColumns), ylim=c(-40, -2)) #, axes=FALSE)
        LineNumber <- 1
      }

      txt <- paste0(...)
      text(x=0, y=-LineNumber, adj=c(0, 1), cex=0.8, labels=txt)
      assign("LineNumber", LineNumber + length(strsplit(txt, "\n")[[1]]),
             envir=ENVIR)
      assign("ColumnNumber", 0, envir=ENVIR)
    }
           
    Progress <- function(char) {
      cat(char)
      points(ColumnNumber %% MaxColumns, -LineNumber, pch="*",
             col=progress.clr[(as.integer(ColumnNumber / MaxColumns) %% 2) + 1])
      assign("ColumnNumber", ColumnNumber + 1, envir=ENVIR)
    }
  } else {
      Message <- function(...) message(...)
      Progress <- function(char) cat(char)
  }


  
  if (is.na(buttons2right)) buttons2right <- Windows
  if (is.na(fontsize)) fontsize <- if (Windows) 7 else 8
  
  if (fontsize < 6 || fontsize > 12) stop("'fontsize' must be within [6,12]")
  if (win.sys && fontsize > 8)
    stop("for Windows systems 'fontsize' must be within [6,8]")
  setname <- "_set_"
  names_classes <- names(modelclasses)
  idx <- names_classes == setname #substr(names_classes, 1, nchar(setname)) == setname
  setinfo <- modelclasses[idx]
  modelclasses <- modelclasses[!idx]
  classinfo <- list()
  models <- length(modelclasses)
  for (i in 1:models) {
    classinfo[[i]] <- modelclasses[[i]]$classinfo
    modelclasses[[i]]$classinfo <- NULL
  }
  
  stopifnot(models > 0)
  ENVIR <- environment()
   
  Titel <- c("Adoption Gui: ", " model")
  set.mixcol <- c(colour[1], subcol[-1])
  max.sets <- length(colour)
  if (length(show.n.indiv) == 1) show.n.indiv <- 1:show.n.indiv
  show.n.indiv <- show.n.indiv[show.n.indiv >= 1]
  stopifnot(length(show.n.indiv) > 0)
  fst.col <- 1L
  snd.col <- 3L
  trd.col <- 4L
  forth.col <- 5L
  fifth.col <- 8L
  fst.row <- 1L
  snd.row <- 32L
  forth.row <- 17L
  row.sl <- 1L # giving the position of the first label
  width.entry <- 4L
  wide.width.entry <- if (Windows) 5L else 6L
  width.slider <- if (Windows) 10L else 6L + fontsize ## 8:12
  length.slider <- 130L
  button.pady <- (fontsize >= 10) + (fontsize >= 12)  ## pixel
  Edigits <- 5L
  Sdigits <- 10L

  data.col <- 1
  theor.col <- 2
  datafit.col <- 3

  linear <- function(y, y0, x=c(10, 6)) {
    a <- (diff(y) / diff(x))
    b <- y[1] - a * x[1]
    ans <- a * y0 + b
    ans
  }
  
  ## fontsize 10 and 6
  image.rowspan <- round(#(if (Windows) 1 else 1) * ## 2
			 if (buttons2right) 10 else linear(c(15, 20), fontsize))
  image.colspan <-  round(#(if (Windows) 1 else 1) *
                          linear(c(15, 20), fontsize))
  line.compression <- if (win.sys) 8 else if (buttons2right) 3 else 1.3  # 8:1; <8:2
  plothscale <- if (win.sys) c(2, 2, 2, 2, 2, 1, 1)[fontsize-5] ## 8:0.9
                else linear(c(1.4, 0.95), fontsize) 
  cex.default <- linear(c(0.8, 0.05), fontsize) 
  cex <- linear(c(1.4, 0.8), fontsize) 
  cex.txt <- cex * 0.82
  cex.pch <- if (win.sys) 5 else 2
  pch <- if (win.sys) 1 else 16

  plotvscale <- plothscale

  below.plots <- round(if (Windows) 1.7 * 2 * image.rowspan + 2
		       else if (buttons2right) 2 * image.rowspan + 2L
    #                  else c(55, 52, 50, 45, 39, 36, 32)[fontsize - 5])
                     else c(45, 45, 45, 45, 43, 40, 37, 32)[fontsize - 4])
  row.last <- image.rowspan + 4L
  xline <- 0.06 * fontsize + 1.5
  yline <- 0.12 * fontsize + 1
  mar <- rep(c(linear(c(4.2, 3.2), fontsize), 0.2), each=2)
  
  if (buttons2right) {
    col.plot <- fst.col
    row.plot <- fst.row
    col.row <- 1L
    row.return <- col.row + 1L
    extrarow <- 1L
    row.graphics <- row.return + 6L
    row.models <- row.graphics + 11L
    row.globals <- row.models + models + 2L
    if (is.na(sliderColumn)) {      
      sliderColumn <- if (Windows) 60 else 38
     cat("NOTE: It may happen that the columns of sliders on the right hand side\nare far away from or within the graphics. You should change (drastically)\nthe value of 'sliderColumn' by 'RFoptions(sliderColumn=###)' then.\nDefault value is ",
	  sliderColumn, ".\n\n", sep="")
    }
    col.fit <- sliderColumn - 1 + (n.data > 0)
    if (win.sys) col.fit <- col.fit - 2 * fontsize
    col.sl <- col.fit + 1
    delta.row.graphics <- 1L
    delta.row.title <- 1L
    sticky <- "e"
    line.row.compression <- line.compression
    line.col.compression <- 1 
    sticky.copy <- "e"
 } else {
    col.plot <- fst.col
    row.plot <- fst.row
    col.return <- fst.col
    row.return <- below.plots + 1L
    extrarow <- 2L
    col.graphics <- snd.col
    row.graphics <- below.plots
    col.models <- forth.col
    row.models <- below.plots
    col.globals <- fifth.col
    row.globals <- below.plots
    col.fit <- round(fst.col + (if (Windows) 1 else (-10 + 2 * fontsize)) *
                     image.colspan + 1) - 1 + (n.data > 0)
    col.sl <- col.fit + 1
    delta.row.graphics <- 0.5
    delta.row.title <- 2L
    sticky <- "w"
    line.row.compression <- line.col.compression <- line.compression
    col.copy <- col.sl
    col.row <- 1L
    sticky.copy <- "w"
  }
   
  importance <- rep(2, 10)
  clr.general <- "light goldenrod yellow"
  clr.graph <- "blue"
  clr.title <-"darkgreen"
  clr.global <- clr.title
  clr.alert <- "darkred"
  clr.bg <- NULL ##"ghost white"
  clr.slider <- "gainsboro"
  clr.slider.frame <- "gray"
  clr.empty <- "ghost white"
  clr.unused <- "lightgray"
  clr.setentry <-  "grey40"
  fg <- c("black", "gray25", "gray50", "gray70", clr.unused)

  
  nT <- DeltaUp <- t.W <- dummy <- Tseq <- Tseq.show <- imgLU <- tt <-
    copyFctn <-
  buttonApplyAllOver <- buttonNewSimu <- buttonScreenShot <- buttonStepFitting <-
    buttonSaveimages <- buttonReturn <- classTitle <- pictureTitle <-
      globalTitle <-  buttonCompleteFitting <-NULL
  
  sets <- rep(FALSE, max.sets)
  col_currentClass <- rep(NA, max.sets)
  set_colClass <- function(set, class) {
    sets[set] <- TRUE
    col_currentClass[set] <- class
    for (a in c("sets", "col_currentClass")) assign(a, get(a), envir = ENVIR)
  }
  set_colClass(1, currentClass)
 
  non.show.loc <- which(names(order.show) != "")
  titles.val <- order.show[non.show.loc]
  idx <- sapply(titles.val, is.logical)
  do.not.show <- names(titles.val[idx])
  fixed.coord <- "coord.param" %in% do.not.show
  titles.loc <- non.show.loc[!idx]

  globalParams <- titles.loc[1] - 1
  stopifnot(length(globalParams) > 1 || globalParams >= 2)
  titles.val <- order.show[titles.loc]
  titles <- names(titles.val)
  titles.guiloc <- c(titles.loc, 9999)
  if (length(titles.loc) == 0) stop("no titles found")
  for (i in 1:length(titles.loc))
    titles.guiloc[i] <- titles.guiloc[i] - sum(non.show.loc < titles.loc[i])
  
  Names <- unlist(order.show[-non.show.loc])
  
  stopifnot(length(Names) > globalParams)
  all.names <- c(Names, do.not.show)
  min.names <- paste(all.names, "min", sep=".")
  max.names <- paste(all.names, "max", sep=".")
  integer.names <- paste(all.names, "integer", sep=".")

  Laengen <- t(sapply(modelclasses,
		      function(x) sapply(x[all.names], length)
		      ))
 
  totLaengen <- rowSums(Laengen)
  colnames(Laengen) <- all.names
  maxLen <- apply(Laengen, 2, max)
  Len <- rep(list(maxLen), models)
  summaxlen <- sum(maxLen)

  for (i in 1:models) {
    if (nchar(names_classes[i]) == 0) names_classes[i] <- paste("Model", i)
    modelclasses[[i]]$name <- names_classes[i]
  }
  names_classes <- substr(names(modelclasses), 1, 24)
  abbr.class <- function(names, maxlength=4) {
    stopifnot(length(names) > 0)
    ok <- c(LETTERS, 0:9, "#")
    ans <- character(length(names))    
    for (iS in 1:length(names)) {
      S <- names[iS]
      i <- 1
      while(i <= nchar(S)) {
	if ((c <- substr(S, i, i)) %in% ok) i <- i + 1
	else if (c == "(" && substr(S, i+1, i+2) %in% c("19", "20"))
	  S <- paste0(substr(S, 1, i-1),  substr(S, i+3, nchar(S)))
	  else S <- paste0(substr(S, 1, i-1),  substr(S, i+1, nchar(S)))
      }
      ans[iS] <- S
    }
    substr(ans, 1,  7)
  }
  #abbr.class <- function(name) base::abbreviate(name, minlength=8, strict=TRUE)
  shortnames_classes <- abbr.class(names(modelclasses))

  user_variables <- c("minsets", "maxsets", "col_currentClass",
		      "L", "minclasses", "maxclasses", "currentClass")
  minsets <- maxsets <-vector("list", max.sets)
  titles.val <- L <- minclasses <- maxclasses <- isinteger <- strictpos <-
    Fitting <- vector("list", models)
  names(Fitting) <- names_classes
  default.false <- c("alpha", "beta", "gamma", "Uthreshold")

  for (class in 1:models) {
    if (n.data > 0) {
      modelclasses[[class]]$dt <- modelclasses[[class]]$dt.min <-
        modelclasses[[class]]$dt.max <- dt
      modelclasses[[class]]$m <- modelclasses[[class]]$m.min <- n.data
      modelclasses[[class]]$m.max <- 2 * modelclasses[[class]]$m
    }
    
    if (length(modelclasses[[class]]$CrossReferences) != 0) {
      if (!is.function(modelclasses[[class]]$CrossReferences) ||
          !is.logical(cr <- modelclasses[[class]]$CrossReferences()) || !cr)
        stop("'CrossReferences' must be a function that returns TRUE")
      for (j in 1:length(modelclasses[[class]]))
        if (is.function(modelclasses[[class]][[j]]))
          environment(modelclasses[[class]][[j]]) <- ENVIR
    }
     
    Lorig <- modelclasses[[class]]
    L[[class]] <- rep(list(numeric(0)), length(all.names))
    names(L[[class]]) <- all.names
    idx <- sapply(Lorig[all.names], length) > 0
    L[[class]][all.names[idx]] <- Lorig[all.names[idx]]
     
    is_fctn <- sapply(Lorig, is.function)
    fctnNames <- names(Lorig)[is_fctn]
    L[[class]][fctnNames] <- Lorig[fctnNames]

    maxcl <- constraints[max.names]
    tmp <- Lorig[max.names]
    tmp <- tmp[sapply(tmp, length) > 0]
    for (n in names(tmp)) { ## wegen strictpos
      x <- c(tmp[[n]], rep(-1, length(maxcl[[n]]) - length(tmp[[n]])))      
      maxcl[[n]] <- x
    }
   
    ## maxcl[names(tmp)] <- tmp
    maxclasses[[class]] <- maxcl

    val <- order.show[titles]
    tmp <- Lorig[titles]
    tmp <- tmp[sapply(tmp, length) > 0]
    val[names(tmp)] <- tmp
    titles.val[[class]] <- val
  
    mincl <- constraints[min.names]
    isinteger[[class]] <-
      lapply(mincl, function(x) rep(is.integer(x), length(x)))
    tmp <- Lorig[min.names]
    tmp <- tmp[sapply(tmp, length) > 0]
    for (n in names(tmp)) { ## wegen strictpos
      x <- c(tmp[[n]], rep(-1, length(mincl[[n]]) - length(tmp[[n]])))      
      mincl[[n]] <- x
    }
    minclasses[[class]] <- mincl

    names(isinteger[[class]]) <- integer.names
    for (cond in list(constraints, Lorig)) {
      tmp <- cond[integer.names]
      names(tmp) <- integer.names
      tmp.idx <- sapply(tmp, length) > 0
      isinteger[[class]][tmp.idx] <- tmp[tmp.idx]
    }

    strictpos[[class]] <- lapply(mincl, function(m) m > 0)
    names(strictpos[[class]]) <- Names

    if (n.data > 0) {
      Fitting[[class]] <- vector("list", length(all.names))
      names(Fitting[[class]]) <- all.names
      for (i in 1:length(all.names)) {
        Fitting[[class]][[i]] <- rep(NA, maxLen[i])
        if (i <= globalParams || all.names[i] %in% do.not.show ||
            Laengen[class, i] == 0) next
        value <- fit_operators || all(all.names[i] != default.false)
        j <- 1:Laengen[class, i]
        Fitting[[class]][[i]][j] <- !isinteger[[class]][[i]][j] &
          (minclasses[[class]][[i]][j] < maxclasses[[class]][[i]][j]) & value
      }
    }
  } ## for class
  ymax <- 0.25 * L[[currentClass]][["m"]]
 
  minsets[[1]] <- minclasses[[currentClass]]
  maxsets[[1]] <- maxclasses[[currentClass]]

  for(i in 1:length(all.names)) assign(paste0("Idx.", all.names[i]), i)
  ## Idx.m <- pmatch("m", all.names)

  OnReturn <- function(...) {
    L <- GetL()
    if (is.null(L)) return()
    assign(".adoption.exit", L, envir=parent.ev)
    tkDestroy(tt)
  }
  
  plotvoid <- function()
    plot(Inf, Inf, xlim=c(0,1), ylim=c(0,1), axes=FALSE, xlab="", ylab="",
	 frame=TRUE)

  
 plotSimulation <- function(do.plot=TRUE) {
    ##
    LL <- L[[currentClass]]
    if (!exists("all.rs", envir=ENVIR)) {
      plotvoid()
      return()
    }

    ##    cat("simu ... ")
    repetitions <- Wert(Idx.repetitions)
    m <- Wert(Idx.m)
    dt <- Wert(Idx.dt)
    threshold <- as.double(Wert(Idx.Uthreshold))
    repm <- repetitions * m
    show.n.indiv <- show.n.indiv[show.n.indiv <= m]

    ## folgende Zeilen eigentlich nur einmal notwendig
    dNt <- matrix(0L, nrow=nT, ncol=repetitions)
    ## achtung: ungewolltes pointer matching vermeiden! Deshalb ReSe wiederholt.
    M.show <- matrix(as.double(NA), nrow=nT, ncol=length(show.n.indiv))
    U.show <- matrix(as.double(NA), nrow=nT, ncol=length(show.n.indiv))
    Up.show <- matrix(as.double(NA), nrow=nT, ncol=length(show.n.indiv))
    Ic.show <-matrix(as.double(NA), nrow=nT, ncol=length(show.n.indiv))
    Ir.show <- matrix(as.double(NA), nrow=nT, ncol=length(show.n.indiv))
    DeltaUp.show <- matrix(as.double(NA), nrow=nT, ncol=length(show.n.indiv))
    Never.Tried <- matrix(1L, nrow=nT, ncol=m)
 
 
    ## jetzt alle Zeilen immer notwendig
    U <- get("UpStart", envir=ENVIR) + 0 ## + 0 ultrawichtig
    Ir <- rep(as.double(NA), length(U))

    never.tried <- U < threshold

   dim(never.tried) <- c(m, repetitions)

    if (!all(never.tried))
      Message(round(sum(!never.tried) / repm * 100, 2),
              "% have tried already at the beginning")
    it <- 1    
    dNt[it, ] <- Nt <- as.integer(colSums(!never.tried))
    
    LIc <- LL$Ic
    if (!is.function(LIc)) LIc<-function(param, Nt, m, start) rep(0, repm)
    alpha <- AlleWerte(Idx.alpha)
    beta <- AlleWerte(Idx.beta)    
    gamma <- AlleWerte(Idx.gamma)
    Icstart <- get("Ic.start", envir=ENVIR)
    
    IcParam <- AlleWerte(Idx.Ic.param)
   
   ## bis hierher 0 sec
   ## Print(length(t.W), m^2)
    stopifnot(length(U) == repm, length(U) == repm,
	      length(never.tried) == repm,
	      length(t.W) == m^2 || length(t.W) == 0,
	      is.logical(t.W.list) ||(length(t.W.list)==2 && is.list(t.W.list)),
	      length(DeltaUp) == repm * (nT - 1), length(dummy) == repm)
    
    Ic <- LIc(param=IcParam, Nt=Nt, m=m, start=Icstart)
    stopifnot(length(Ic) == repetitions * m)
    stopifnot(is.double(Ic))


    U.show[it, ] <- Up.show[it, ] <- U[show.n.indiv]
    Ic.show[it, ] <- Ic[show.n.indiv]
    Ir.show[it, ] <- 0
    
    ## avx lohnt erst, wenn quantile beschleunigt ist
    ## 1/4 der Zeit auf R; 3/4 der Zeit nur xA() auf cc; rest
    ## for it: 0.03; Per image: 0.04 (0.01 ellapsed);  0.08:quants + assign
    ## vernachlaessigbar; .show - Zuweisungen brauchen 1/10 der GesamtZeit


    for (it in 2:nT) {
      if (!is.null(LL$Utrafo)) U <- LL$Utrafo(U, threshold, m)      
      .Call(C_adoption, U, Ic,
            if (is.logical(t.W.list)) { if (t.W.list) NULL else t.W }
            else t.W.list,
            Ir, dt, DeltaUp, it-1L,
	    threshold, never.tried, alpha, beta, gamma, dummy,
	    FALSE, dNt, nT, Nt, show.n.indiv, M.show,
	    U.show, Ic.show, Ir.show, DeltaUp.show, Up.show, Never.Tried, cores)
      
      Ic <- LIc(param=IcParam, Nt=Nt, m=m, start=Icstart)
    }
    ##   Print(do.plot)

    rM_N <- rowMeans(dNt)
    quants <- NULL
    if (length(quantiles) > 0)# sehr teuer: Befehl 30% Gesamtzeit bei Bass
      quants <- t(apply(dNt, 1, quantile, quantiles, type=quantile.type))

    cum_N <- apply(dNt, 2, cumsum)
    ## dim(cum_N) <- dim(dNt) ## fast version of next lines
    if (any(dim(dNt) == 1)) dim(cum_N) <- dim(dNt)
    else stopifnot(all(dim(cum_N) == dim(dNt)))
    cumquants <- NULL
    if (length(quantiles) > 0)# sehr teuer: Befehl 30% Gesamtzeit bei Bass
      cumquants <- t(apply(cum_N, 1, quantile,quantiles,type=quantile.type))
    cum_N <- rowMeans(cum_N) 


    rM_N <- rM_N / dt
    ymax.cur <- max(rM_N)
    while (ymax.cur > p_ymax[1] * get("ymax", envir=ENVIR))
	   assign("ymax", ymax * p_ymax[2], envir=ENVIR)
    if (ymax.cur < ymax / p_ymax[3] &&
        ymax >= p_ymax[4] * m / 100)
      assign("ymax", ymax / p_ymax[5], envir=ENVIR)
  
    assign(simuValue(1), ## no measurable time needed
	   list(U.show = U.show,
		Ic.show = Ic.show,
		Ir.show = Ir.show,
		M.show = M.show,
		DeltaUp.show = DeltaUp.show,
  		Up.show = Up.show,
                Never.Tried = Never.Tried,
		Tseq.show = Tseq.show,
		dt = dt,
		m = m,
		quants = quants / dt,
		rM_N = rM_N,
		cumquants = cumquants,
		cum_N = cum_N,
		theor = if (!is.null(LL$theor.dN))
			  LL$theor.dN(Ic.param=IcParam, t=Tseq, m=m) / m * 100
		),
	   envir=ENVIR)

    if (!do.plot) return()
  
    n.sets <- sum(sets)     
    par(mfrow=c(2, 2), mar=mar, cex=cex.default)

    p <- 4
    if (pictures[1]) {
      n.sets <- min(4, n.sets)
      plotSpatial(which(sets)[1:n.sets])
      if ((p <- p - n.sets) <= 0) return()
    }

    if (pictures[3]) {
      plotTrials()
      if ((p <- p - 1) <= 0) return()
    } 

    if (pictures[5]) {
         plotCumulTrials() 
     if ((p <- p - 1) <= 0) return()
    }

     if (pictures[8]) {
      plotU()
      if ((p <- p - 1) <= 0) return()
    }

    if (pictures[2]) {
      plotIc() 
      if ((p <- p - 1) <= 0) return()
    }
 
    if (pictures[4]) {
      plotIr()   
      if ((p <- p - 1) <= 0) return()
    }

     if (pictures[6]) {	
	plotM()
      if ((p <- p - 1) <= 0) return()
    }
   
    if (pictures[7]) {
      plotUp()
      if ((p <- p - 1) <= 0) return()
    }
  
    if (pictures[9]) {
      plotDeltaUp()
      if ((p <- p - 1) <= 0) return()
    }

    if (p == 4) {
      plot(Inf, Inf, xlim=c(0,1), ylim=c(0,1),axes=FALSE, xlab="", ylab="")
      text(0.5, 0.5, adj=c(0.5,0.5), cex=cex.txt,
	   labels="Please tick one of the blue boxes to see a graphic")
    }
    
  }
 
  if (gui) {
    tt <- try(tcltk::tktoplevel(), silent=TRUE)
    gui <- !is(tt, "try-error")
    if (!gui)
      message("The packages 'tcltk' and 'tkrplot' are not correctly installed.")
    if (gui) {
      imgLU <- try(tkrplot::tkrplot(tt, fun = plotSimulation,
                                    hscale=plothscale, vscale=plotvscale),
                   silent = TRUE)
      gui <- !is(imgLU, "try-error")
      if (!gui)
        message("The package 'tkrplot' is not correctly installed.")
    }
  }

  if (gui) {
    tkValue <- tcltk::tclvalue
    "tkValue<-" <- do.call("::", list("tcltk", "tclvalue<-"))
    tkfont <- tcltk::tkfont.create(size=fontsize)
    tkDestroy <- tcltk::tkdestroy
    tkLabel <- tcltk::tklabel
    tkEntry <- tcltk::tkentry
    tkScale <- tcltk::tkscale
    tkBind <- tcltk::tkbind
    tkGridConf <- tcltk::tkgrid.configure
    tkVar <- tcltk::tclVar
    tkGrid <- tcltk::tkgrid    
    tkCheckbutton <- tcltk::tkcheckbutton
    tkRadiobutton <- tcltk::tkradiobutton
    tkConfigure <- tcltk::tkconfigure
    Tcl <- tcltk::tcl
    tkRreplot <- tkrplot::tkrreplot
    tkButton <- tcltk::tkbutton
    tkDelete <- tcltk::tkgrid.forget
    tkSaveFile <- tcltk::tkgetSaveFile
    "tkTitle<-" <- do.call("::", list("tcltk", "tktitle<-"))
    tkGet <- tcltk::tkcget

    tcltk::tkwm.protocol(tt, "WM_DELETE_WINDOW", OnReturn)
    tkGrid(tkLabel(tt, text="", width=1), column=fst.col + image.colspan, row=1)
    tkGrid(tkLabel(tt, text="", width=1), column=forth.col + image.colspan,
	   row=1)
    tkGrid(tkLabel(tt, text="", width=1), column=col.sl+image.colspan, row=1)
    clr.bg <- tkValue(tkGet(tt, "-bg"))
    ##  col_fg <- tkValue(tcltk::tkGet(tt, "-fg"))
  } else {
    tkValue <- function(name) get(name, envir=ENVIR)    
    "tkValue<-" <- function(name, value) {
      assign(name, value, envir=ENVIR)
      name
    }
    tkfont <- tt <- NULL
    tkDestroy <- tkLabel <- tkEntry <- tkScale <- tkBind <- tkGridConf <-
      tkGrid <-  tkCheckbutton <- tkRadiobutton <-
        tkConfigure <- Tcl <- tkButton <- tkDelete <-tkSaveFile <-
          "tkTitle<-" <- tkTopLevel <- tkGet <-
            function(...) return(NULL)
    tkVarNumber <- 1
    tkVar <- function(init) {
      name <- paste0("_tk_", tkVarNumber)
      assign("tkVarNumber", tkVarNumber + 1, envir=ENVIR)
      tkValue(name) <- init
      name
    }
    tkRreplot <- function(...) plotSimulation(do.plot=FALSE)
    clr.bg <- "#d9d9d9"
  } 
  
  Wert <- function(i, j=1) {
    ## delete next line
    stopifnot(hasArg("j") || Laengen[col_currentClass[1], i] == 1)
    ans <- try(as.numeric(tkValue(get(entryValue(i, j=j), envir=ENVIR))),
               silent = TRUE)
    if (is(ans, "try-error")) {
      Message("'", labName(i,j), "' is not numeric.")
      return(NA)
    } else return(ans)      
  }
  
  AlleWerte <- function(i, set=1) {
    len <- Laengen[col_currentClass[set], i]
    if (len == 0) return(0)
    ans <- numeric(len)
    for (j in 1:len) ans[j] <- Wert(i, j=j)
    ans
  }
            
  ERROR <- function(txt) {
    cat(txt, "\n")
    OnReturn()
  }

  unused <- "(unused)"  
  labName <- function(i, j, class=currentClass) {
    LL <- L[[class]]
    n0 <- names(LL[[i]])
    if (length(LL[[Names[i]]]) < j || !is.finite(LL[[Names[i]]][j]))
      return(unused)
    if (length(n0) > 0) n0[j]
    else paste0(Names[i], if (Laengen[currentClass,i] != 1) paste0("[", j, "]"))
  }
  labObj <- function(i, j, set=1) paste0(Names[i], "_", j, "_", set)
  basename <- function(name, j, set=1)
     paste0(name, set, if (!missing(j) && length(j)>0) j else 1)

  BaseName <- function(i, j, set=1) basename(all.names[i], j, set)
  slValue <- function(i, j, set=1)
    paste0("sl_", BaseName(i, j, set))
  entryValue <- function(i, j, set=1) 
    paste0("entry_", 
	  if (is.numeric(i)) BaseName(i, j, set) else basename(i, j, set))
  slWidget <- function(i, j, set=1)
    paste0("wiS_", BaseName(i, j, set))
  entryWidget <- function(i, j, set=1)
    paste0("wiE_", BaseName(i, j, set))
  setValue <- function(name, value, set=1) {
    len <- length(value)
    class <- col_currentClass[set]
    prefix <- "entry_"
    variab <- paste0(prefix, basename(name, 1:len, set))
    for (j in 1:len) {
       Y <- get(variab[j], envir=ENVIR)
       tkValue(Y) <- value[j]
    }
  }
  Value <- function(name, j, set=1) { # sehr langsam
    class <- col_currentClass[set]
    prefix <- "entry_"
    if (missing(j)) {
      len <- Laengen[class, name]
      if (len == 0) return(0)
      ans <- numeric(len)
      variab <- paste0(prefix, basename(name, 1:len, set))
      for (j in 1:len) {
	ans[j] <- tkValue(get(variab[j], envir=ENVIR))
      }
    } else {
      variab <- paste0(prefix, basename(name, j, set))
      ans <- tkValue(get(variab, envir=ENVIR))
    }
    ans <- try(as.numeric(ans), silent=TRUE)
    if (is(ans, "try-error")) {
      Message("'", name, "' is not numeric.")
      return(NA)
    } else return(ans)      
  }
  simuValue <- function(set) paste0("simu_", set)
  delColumnButton <- function(set) paste0("delete_", set)
  firstButton <- function(set) paste0("first_", set)
  classButton <- function(s) paste0("buttonClass", s)
  pictureButton <- function(s) paste0("buttonPicture", s)
  pictureValue <- function(s) paste0("valuePicture", s)
  fitButton <- function(i, j) paste0("buttonFit", i, "_", j)
  fitValue <- function(i, j) paste0("valueFit", i, "_", j)
  shortname <- function(s) paste0("shortname", s)

  
  entryAssign <- function(i, j, set=1, genuinevalue) { # loest EntryChanges aus
    entry <- get(entryValue(i, j, set=set), envir=ENVIR)
    class <- col_currentClass[set]
    if (isinteger[[class]] [[i]] [j])
      genuinevalue <- as.integer(round(genuinevalue))
    tkValue(entry) <- as.character(genuinevalue)
    ## assign(entryValue(i, j, set=set), entry, envir=ENVIR)
  }
  

  first <- function(fromset, gui=TRUE) {
    if (gui) assign("plotting", FALSE, envir=ENVIR)
    variables <- c("minsets", "maxsets", "L", "currentClass")
    for (v in variables) get(v, envir=ENVIR) 
   
    set <- 1
    col_class <- currentClass
    LL <- getexactL(set)
    
    if (any(is.na(unlist(LL)))){
      Message("class or column cannot be changed as there are\nincorrect or unfilled fields")
      return(TRUE)
    }

    
    mini <- minsets[[set]]
    maxi <- maxsets[[set]]

    fromClass <- col_currentClass[fromset]
    

    ## naechster Befehlt laedt minclasses etc
    if (classButtons(fromClass, fromset=fromset, simu=FALSE)) return(TRUE)

  
    simu <- get(simuValue(set), envir=ENVIR)
    assign(simuValue(set), get(simuValue(fromset), envir=ENVIR), envir=ENVIR)
    assign(simuValue(fromset), simu, envir=ENVIR)

    L[[col_class]] <- LL ## muss nach classButtons kommen
    set_colClass(fromset, col_class) ## muss zwingend vor putexactL kommen
    minsets[[fromset]] <- mini ## muss zwingend vor putexactL kommen
    maxsets[[fromset]] <- maxi ## muss zwingend vor putexactL kommen
    for (v in variables) assign(v, get(v), envir=ENVIR) ## dito
    putexactL(LL, fromset)
       
    if (gui) {
      labels(fromset)
      labels()
      position()
      start_simu(update="m")
      assign("plotting", c(first=TRUE), envir=ENVIR) 
      tkRreplot(imgLU)
    }
    return(FALSE)
  }
  

  GetL <- function() {
    set <- which(sets)
  
    LL <- vector("list", length(set))
    names(LL) <- rep(setname, length(set)) # paste0(setname, 1:length(set))
    
    S <- which(sets)
    for (is in 1:length(S)) {
      set <- S[is]
      LL[[is]] <- getexactL(set)
      if (any(is.na(unlist(LL[[is]])))){
	Message("class or column cannot be saved as there are\nincorrect or unfilled fields")
	return(NULL)
      }
      LL[[is]]$setinfo <- list(setnr = is,
			       class = col_currentClass[set],
			       set = set,
			       name = names_classes[class],
			       minset = minsets[[set]],
			       maxset = maxsets[[set]],
			       simuvalue = get(simuValue(set), envir=ENVIR))
    }

    for (ic in 1:models) {      
     L[[ic]]$classinfo <- list(minclasses = minclasses[[ic]],
                               maxclasses = maxclasses[[ic]],
                               titles.val = titles.val[[ic]])
    }
    names(L) <- names(modelclasses)
    return(c(LL, L))
  }

  
  applyAllover <- function(...) {
    n <- sum(sets)
    if (n < 2) Message("Buttom has no effect as long as\nonly one set of parameters is defined.")
    else {
      ## get
      assign("plotting", FALSE, envir=ENVIR)
      val <- vector("list", globalParams)
      for (i in 1:globalParams) {
        if (Laengen[class, i] == 0) next
        val[[i]] <- numeric(Laengen[class, i])
        for (j in 1:Laengen[class, i]) {
          ans <- try(as.numeric(tkValue(get(entryValue(i, j), envir=ENVIR))),
                   silent = TRUE)
          if (is(ans, "try-error")) {
            Message("'", labName(i,j), "' is not numeric.")
            return(NULL)
          }
          val[[i]][j] <- ans
        }
      }

      ## set
      variables <- c("minsets", "maxsets")
      for (a in variables) assign(a, get(a, envir = ENVIR))
      for (set in 2:n) {
	class <- col_currentClass[set]
	for (i in 1:globalParams) {
	  if (Laengen[class, i] == 0) next
	  for (j in 1:Laengen[class, i]) {
            entryAssign(i, 1, set, val[[i]][j])
            minsets[[set]][[i]][j] <- minsets[[1]][[i]][j]
            maxsets[[set]][[i]][j] <- maxsets[[1]][[i]][j]
          }
	}
      }
      for (a in variables) assign(a, get(a), envir = ENVIR)

      ## update simulations
      for (set in 2:n) {
	class <- col_currentClass[set]
        if (first(set, gui=FALSE))
          Message("'apply allover' did not work for one of the columns")
        start_simu(update="m")
        plotSimulation(do.plot=FALSE)
        if (first(set, gui=FALSE)) {
          Message("Current set has some major problem.\nCheck and/or inform author of the package")
          return(NULL)
	}
      }
      tkRreplot(imgLU)        
    }
  }

  
  copy <- function(...) {
    sets <- get("sets", envir=ENVIR)
    set <- which(!sets)
    if (length(set) == 0) return()
    set <- set[1]
    set_colClass(set, currentClass)

    for (i in 1:length(Names)) {
      if (Laengen[currentClass, i] == 0) next      
      for (j in 1:Laengen[currentClass, i]) {
        ans <- try(as.numeric(tkValue(get(entryValue(i, j), envir=ENVIR))),
                   silent = TRUE)        
	entryAssign(i, j, set, if (is(ans, "try-error")) 1 else ans)
      }
    }
    variables <- c("minsets", "maxsets")
    for (a in variables) get(a, envir = ENVIR)
    minsets[[set]] <- minsets[[1]]
    maxsets[[set]] <- maxsets[[1]]
    for (a in variables) assign(a, get(a), envir = ENVIR)
    assign(simuValue(set), get(simuValue(1), envir=ENVIR), envir=ENVIR)
    
    tkConfigure(get(shortname(set)), text=shortnames_classes[currentClass],
		bd=0, pady=0, font=tkfont)
    labels(set)
    position()
  }

  
  deleteColumn <- function(set) {
    stopifnot(set > 1)
    sets <- get("sets", envir=ENVIR)
    sets[set] <- FALSE  
    assign("sets", sets, envir=ENVIR)

    class <- col_currentClass[set]
  
    tkDelete(get(delColumnButton(set), envir=ENVIR))
    tkDelete(get(firstButton(set), envir=ENVIR))

    for (i in 1:length(Names)) {
      for (j in 1:maxLen[i]) {
	tkDelete(get(entryWidget(i, j, set), envir=ENVIR))
	tkDelete(get(slWidget(i, j, set), envir=ENVIR))
	tkDelete(get(labObj(i, j, set), envir=ENVIR))
      }
    }
    tkDelete(get(shortname(set), envir=ENVIR))
    
    position()
    tkRreplot(imgLU)
   }
 
 
  EmptyEntry <- function(i, j, set=1) {
    X <- get(entryValue(i,j,set), envir=ENVIR)
    tkConfigure(get(entryWidget(i, j, set), envir=ENVIR),
		fg = clr.empty, bg=clr.empty,
		textvariable=X, bd =0,
		width=if (i <= globalParams) wide.width.entry else width.entry,
                borderwidth=1, selectborderwidth=1
                )
    tkValue(X) <- "1"
  }
  
 
  SetEntry <- function(i, j, set=1, genuinevalue) {
    class <- col_currentClass[set]   
    X <- get(entryValue(i, j, set), envir=ENVIR)
    if (isinteger[[class]] [[i]][j])
      genuinevalue <- as.integer(round(genuinevalue))
    tkConfigure(get(entryWidget(i, j, set), envir=ENVIR),
                fg = clr.setentry, bg=clr.bg, bd =0,
                textvariable=X,
                width=if (i <= globalParams) wide.width.entry else width.entry,
                borderwidth=1, selectborderwidth=1
                )
    tkValue(X) <- as.character(genuinevalue)
  }
  
  fromTo <- function(from, genuinevalue, to = genuinevalue+(genuinevalue-from),
		     mini = NULL, maxi=NULL,
		     pos=FALSE, int=FALSE) {
    ## genuinevalue, from, to muessen als k/numberSteps geschrieben werden
    ## koennen (das ist das interne Verhalten von tcltk), um das
    ## Verhalten von tcltk von aussen zu kontrollieren. Auch muss
    ## die Zahl der signifikanten Stellen deutlich hoeher beim Slider
    ## sein als bei der entry box wegen Rundungsfehler
     
    if (to == from) {
      if (int) to <- base::round(to)
      orig <- to
      if (pos) to <- sqrt(to)     
      return(list(from=orig, to=orig, genuinevalue=orig,
                  sliderFrom = to, sliderTo = to, SliderValue=to,
                  reso=0))
    }
   
    if (int) genuinevalue <- base::round(genuinevalue)
    if (pos) {
 #     Print("pos", from, to, genuinevalue)
      from <- sqrt(from)
      to <- sqrt(to)
      if (genuinevalue <= 0) genuinevalue <- from
      Slidervalue <- sqrt(genuinevalue)
    } else Slidervalue <- genuinevalue

#    Print("x", from, to, Slidervalue)
    
    SliderValue <- signif(Slidervalue, Sdigits) ## slider value
  
    if (length(mini)>0 && SliderValue >= mini)
      from <- min(genuinevalue, max(from, mini))
    if (length(maxi)>0 && SliderValue <= maxi)
      to <- max(genuinevalue, min(to, maxi))
    N <- round(numberSteps * SliderValue / (to-from))
    if (N == 0) {
      if (SliderValue != to && SliderValue != from) {
	from <- min(0, from, SliderValue * 2)
	to <- max(0, to, SliderValue * 2)
      }
      step <- (from - to) / numberSteps
      if (int && !pos) step <- max(1, round(step))
    } else {
      step <- SliderValue / N
#      Print(SliderValue, to, from)
      step <- if (int && !pos) round(step) else signif(step, Sdigits)
      k <- round((SliderValue - from) / step)
      from <- SliderValue - k * step
      to <- from + numberSteps * step

#      Print(from, to, k, numberSteps, step, N)
      
    }
       
    list(from= if (pos) from^2 else from,
         to= if (pos) to^2 else to,
         SliderValue=SliderValue, sliderFrom = from, sliderTo = to,
	 reso=step, # if (isinteger[[class]][[i]][j]) -1 else
	 genuinevalue=signif(genuinevalue, Edigits))
  }

  oldEntry <- "_x_"
  EntryChanges <- function(i, j, set=1, factor=3, genuinevalue = tkValue(ev)) {
    name <- Names[i]
    ev <- get(entryValue(i, j, set=set), envir=ENVIR)
                                        #   
    if (oldEntry != genuinevalue) { # wait until non-character
      ## key is pressed e.g. return
      oldEntry <<- genuinevalue
      return()
    }
    oldEntry <<- "_x_"

    genuinevalue <- try(as.numeric(genuinevalue), silent=TRUE)
    if (is(genuinevalue, "try-error")) {
      Message("Non-numeric entry. Please correct it immediately.")
      return()
    }
    ##  if (!is.na(old) && genuinevalue == old) return()

    class <- col_currentClass[set]  
    if (strictpos[[class]] [[i]][j] && genuinevalue <= 0) {
      Message("Only positive values are allowed for '", 
                labName(i, j, class), "'.", sep="")
      return(NULL)
    }

    int <- isinteger[[class]] [[i]] [j]
    if (int && genuinevalue != as.integer(genuinevalue)) {
      Message("Only integers are allowed for '", 
                labName(i, j, class), "'.", sep="")
      return(NULL)
    }
  
    if (set == 1) {
      mini <- signif(minclasses[[class]][[i]][j], Sdigits)
      maxi <- signif(maxclasses[[class]][[i]][j], Sdigits)
      if (strictpos[[class]] [[i]][j]) {
        min <- min(max(genuinevalue / factor, mini), genuinevalue) 
        V <- fromTo(from=min, genuinevalue = genuinevalue,
                    to = if (genuinevalue == min) genuinevalue * 5
                         else genuinevalue + (genuinevalue - min),
                    pos=TRUE, int=int)
      } else {
	 V <- if (genuinevalue == 0) 
		fromTo(from = min(0, mini), genuinevalue = genuinevalue,
                       to = max(0, maxi), int=int)
	      else fromTo(from = genuinevalue - abs(genuinevalue) * factor,
                          genuinevalue = genuinevalue,
			  mini=mini, maxi=maxi, int=int)
      }
      ms <- minsets
      ms[[set]][[i]][j] <- V$from
      minsets <<- ms
      ms <- maxsets
      ms[[set]][[i]][j] <- V$to
      maxsets <<- ms
     					      
      sl <- get(slValue(i, j), envir=ENVIR)
      tkConfigure(get(slWidget(i, j, set=set), envir=ENVIR),
      		  to=V$sliderTo, from=V$sliderFrom, resolution=V$reso)

      tkValue(sl) <- V$SliderValue ## zwingend nach tkconfigure!
      tkValue(ev) <- as.character(V$genuinevalue, Edigits)
      start_simu(update=name)
      tkRreplot(imgLU)
    } else {
       assign("plotting", FALSE, envir=ENVIR) 
       for (i in 1:2) {
  	if (first(set, gui=FALSE)) {
	  Message("'apply allover' did not work for one of the columns")
	  break
	}
 	start_simu(update="m") ## NOT name, since globals change with set>1 too!
	assign("plotting", c("entry"=TRUE), envir=ENVIR) 
	if (i==1) plotSimulation(do.plot=FALSE)
        else tkRreplot(imgLU)
      }
    }

    return(NULL)
  }

  
 
  SliderChanges <- function(i, j, set=1) {
    name <- Names[i]
    class <- col_currentClass[set]

    Evalue <- as.numeric(tkValue(get(slValue(i,j,set=set), envir=ENVIR)))
    
    if (strictpos[[class]] [[i]][j]) Evalue <- Evalue^2
    Evalue <- if (isinteger[[class]] [[i]][j]) as.integer(base::round(Evalue))
              else  signif(Evalue, digits=Edigits)
    entryAssign(i, j, set=set, genuinevalue=Evalue)

    if (plotting) {
      start_simu(update=name)
      tkRreplot(imgLU)
    }
    ##return(Evalue)
  }

  
  t_W <- function() {
    LL <- L[[currentClass]]
    m <- Wert(Idx.m)
    if (!fixed.coord) {      
      coord <-as.matrix(LL$coord(param=AlleWerte(Idx.coord.param), m=m))
      assign("Coordinates", coord, envir=ENVIR)
      dist.matrix <- as.matrix(dist(coord))
    }

    if (W.by.reference)
      LL$weight(param=AlleWerte(Idx.weight.param), dist=dist.matrix, t.W)
    else t.W <- LL$weight(param=AlleWerte(Idx.weight.param), dist=dist.matrix)
    
    Sparse <- .Call(C_isSparse, t.W, 0.2 * m^2)# sum(W.matrix != 0) < 0.2 * m^2)

    if (is.logical(Sparse)) {
      assign("t.W.list", Sparse, envir=ENVIR)
      return(if (Sparse) m else m^2)
    } 

    assign("t.W.list", envir=ENVIR, .Call(C_filled, t.W, Sparse))
    return(sum(Sparse))
    


##    print(t.W)
##     Print(Sparse)
##    L <- apply(t.W != 0, 2, function(x) {w <- which(x); names(w)<-NULL; w-1L})
##    L2 <- if (sum(t.W != 0) >= 0.2 * m^2 || length(L) == 0) length(L) == 0
##          else list(L, t.W[which(t.W != 0)])
##    Print(Sparse, sum(t.W != 0) < 0.2 * m^2, L2,  t.W.list)
##    stopifnot(all.equal(t.W.list, L2))
##    xxxxx
  }
  

  get.RS <- function() .Random.seed[length(.Random.seed)]


  old.size <- -1
  start_simu <- function(update, all=FALSE, fit.param = FALSE) {
    LL <- L[[currentClass]]
   
    repetitions <-  Wert(Idx.repetitions)
    m <- Wert(Idx.m)
    dt <- Wert(Idx.dt)
    
    assign("Tseq.show", seq(Tstart, Tend, by=dt), envir=ENVIR)
    assign("Tseq", Tseq.show - Tstart, envir=ENVIR)
    
    assign("nT", length(Tseq), envir=ENVIR)
    stopifnot(nT > 1)
    repm <- repetitions * m

    assign("dummy", rep(0, repm), envir=ENVIR)
 
    if (!exists("all.rs", envir=ENVIR)) {
      runif(1)
      all <- TRUE
    }
    
    if (fixCoord <- g <- Up.param <- Up.start <- Ic.start <- all) {
      assign("all.rs",
             if (exists("all.rs", envir=ENVIR)) get("all.rs", envir=ENVIR)+100
	     else base::round(runif(1,1,100000)), envir=ENVIR)
    } else if (fit.param) {
      Up.start <- g <- Ic.start <- TRUE
    } else  {
      if (missing(update)) ERROR("update missing")
      switch(update,
         "m" = fixCoord <- g <- Up.param <- Up.start <- Ic.start <- TRUE,
         "repetitions" = g <- Up.param <- Up.start  <- Ic.start <- TRUE,
	 "dt" =  Up.param <- Up.start <- g <- TRUE,
	 "relative.instance" = NULL,
	 "Ic.param" = Ic.start <- TRUE,
	 "coord.param" = fixCoord <- g <- !fixed.coord,
         "weight.param" = g <- TRUE,
 	 "Uthreshold" = NULL,
	 "Up.start.param" = Up.start <- TRUE,
	 "Up.param" = Up.param <- TRUE,
	 "alpha" = NULL,
	 "beta" = NULL,
	 "gamma" = NULL,
         stop("unknown parameter")
         )
    }
    if (length(LL$CrossReferences)>0)
      Up.param <- Up.start <- g <- Ic.start <- TRUE
  
    if (Up.param) {
      Up.param <- AlleWerte(Idx.Up.param)
      if (all) assign("DeltaUp.rs", get.RS(), envir=ENVIR)
      set.seed(get("DeltaUp.rs", envir=ENVIR))

      Up <- LL$Up(param=Up.param, dt=dt, m=m, nT = nT-1, rep = repetitions)
      if (length(Up) == 0) {
	assign("DeltaUp", envir=ENVIR,
	       rnorm((nT - 1) * repm, sd=sqrt(dt) * Wert(Idx.Up.param, j=1)))
      } else if (is(Up, "RMmodel")) {
	stop("BUG")
	Z <- NULL
	#Z <- RFsimulate(Up, Tseq, n=repm, seed = get("DeltaUp.rs",envir=ENVIR))
	assign("DeltaUp", apply(as.matrix(Z), 2, diff), envir=ENVIR)
      } else assign("DeltaUp", Up, envir=ENVIR)
    }
    
    if (all) assign("Up.rs", get.RS(), envir=ENVIR)
    if (Up.start) {
      set.seed(get("Up.rs", envir=ENVIR))
      assign("UpStart", LL$Up.start(param=AlleWerte(Idx.Up.start.param),
				     m=m, rep=repetitions),
             envir=ENVIR)
    }
    
    if (all) assign("Ic.rs", get.RS(), envir=ENVIR)
    if (is.null(LL$Ic.start)) assign("Ic.start", NULL, envir=ENVIR)
    else {
      if (Ic.start) {
	set.seed(get("Ic.rs", envir=ENVIR))
	assign("Ic.start", LL$Ic.start(param=AlleWerte(Idx.Ic.param),
				      m=m, rep=repetitions),
	       envir=ENVIR)
      }
    }

 
    if (all) assign("g.rs", get.RS(), envir=ENVIR)

   # Print(g, if (missing(update)) "no update" else update, all, fit.param, fixCoord)

    if (g) set.seed(get("g.rs", envir=ENVIR)) ## NOTE: fixCoord => g !!!    
    if (fixCoord) {
      if (xor(length(LL$weight) > 0, length(LL$coord) > 0))
        stop("either both or none of 'weight' and 'coord' should be given.")
      if (length(LL$coord) > 0) {
        assign("W.by.reference", envir=ENVIR,
               length(as.list(args(LL$weight))) == 4)
        M <- if (W.by.reference) matrix(0.0, ncol=m, nrow=m) else NULL
        assign("t.W", M, envir=ENVIR)
        M <- NULL
        if (m > 20000) gc()
        if (fixed.coord) {
          coord <- LL$coord(param=AlleWerte(Idx.coord.param), m=m)
          coord <-as.matrix(coord)
          assign("Coordinates", coord, envir=ENVIR)
          assign("dist.matrix", as.matrix(dist(coord)), envir=ENVIR)
        } else assign("Coordinates", matrix(ncol=0, nrow=0), envir=ENVIR)
        gc()
     } else {
        assign("t.W", NULL, envir=ENVIR) ## default. might be overwritten
        assign("t.W.list", TRUE, envir=ENVIR) ## default. might be overwritten
      }
    }

    if (g) {
      matrix_calc <- if (length(LL$coord) > 0) t_W() else 1
      size <- nT * ceiling(1 + repetitions / cores) * (10 * m + matrix_calc)
      if (old.size != size) {
        ##Print(matrix_calc, size)
        if (size > 3e7) 
          Message("Note that your choice of parameters leads\nto ",
                  if (size > 3e8) "very ",  "long computing times.")
        assign("old.size", size, envir=ENVIR)
      }
    }
  }

  
  PlotMessage <- function(txt) {
    plotvoid()
    y <- seq(0.5 - 0.02 * length(txt), by=0.04, length.out=length(txt))
    text(0.5, y, adj=c(0.5, 0), txt, cex=cex.txt * 1.2)
  }

  plotSpatial <- function(sets) {     
    if (ncol(Coordinates) <= 1) {
      PlotMessage("spatial plot not available")      
    } else {	
      if (ncol(Coordinates) > 2) {
	Coordinates <- Coordinates[, 1:2]
      }
      for (set in sets) {
	simu <- get(simuValue(set), envir=ENVIR)
	t <- 1 + as.integer(Wert(Idx.relative.instance) * (Tend - Tstart) /
			    simu$dt)
	lab <- paste("t = ",round(Tseq.show[t],2))
	plot(Coordinates[, 1], Coordinates[, 2], cex.lab=cex, cex.axis=cex,
	     col=c(colour[set], "white")[1 + simu$Never.Tried[t, ]],
	       pch=15, cex=60 / sqrt(simu$m), ylab=lab, xlab=lab)
      }
    }
  }

  LeftMargin <- function(expr, within=TRUE)
    if (within) title(main=expr, line=-1, cex.main=1.4 * cex.txt)
    else mtext(expr, 2, line=yline, cex=cex.txt)
   
  plotTrials <- function(within=TRUE) {
    m <- Wert(Idx.m)
    s.sets <- rev(which(sets))
    m.lty <- 1 + (1 : length(quantiles))
    ylim <- c(0, get("ymax", envir=ENVIR))
    if (ratio) ylim <- ylim * (100 / m)
    plot(Inf, Inf, xlim = c(Tstart, Tend), ylim=ylim, xlab="", ylab="",
	 cex.axis=cex)
    mtext("time", 1, line=xline, cex=cex.txt)
    LeftMargin("percentage of trials [%] / dt", within=within)

  
    if (n.data > 0) {
      set <- 1
      simu <- get(simuValue(set), envir=ENVIR)
      class <- col_currentClass[set]
      M <- min(length(simu$Tseq.show), length(data))
      d <- data[1:M] / dt
      points(simu$Tseq.show[1:M], if (ratio) d * (100 / m) else d,
             col = other.col[1], pch=pch, cex=cex.pch) #lty = 1, lwd = 3) 
      if (length(fitted[[currentClass]]) > 0) {
        lines(simu$Tseq.show,
              if (ratio) fitted[[currentClass]]$rM_N * (100 / m)
              else fitted[[currentClass]]$rM_N,
              col = other.col[3], lty = 1, lwd = 3)
        y <- max(data, simu$rM_N)
        if (!cumfit) 
          text(x=simu$Tseq.show[1], y=if (ratio) y * (100 / m) else y,
               cex=2,
               col=other.col[3], adj=c(0, 0),
               labels=paste0("rss=", round(fitted[[currentClass]]$rss_dN)))
      }
    }
     
    for (set in s.sets) {
      simu <- get(simuValue(set), envir=ENVIR)
       
      if (length(quantiles) > 0) {
	matlines(simu$Tseq.show,
                 if (ratio) simu$quants * (100 / m) else simu$quants,
                 col=subcol[set], lty=m.lty, lwd=1)
      }
      lines(simu$Tseq.show,
            if (ratio) simu$rM_N * (100 / m) else simu$rM_N,
            col=colour[set], lty=1, lwd=3);
    }

    if (length(simu$theor) > 0) {
      lines(simu$Tseq.show,  if (ratio) simu$theor else simu$theor * (m / 100),
            col = other.col[2], lty = 2, lwd = 3)
    }

    for (set in which(sets)) {
      simu <- get(simuValue(set), envir=ENVIR)
    }    
  }
  
  plotCumulTrials <- function(within=TRUE) {
    s.sets <- rev(which(sets))
    m.lty <- 1 + (1 : length(quantiles))
    m <- Wert(Idx.m)
    ylim <- c(0, if (ratio) 100 else m)
    plot(Inf, Inf, xlim = c(Tstart, Tend), ylim=ylim,
	 cex.lab=cex, cex.axis=cex, xlab="", ylab="")
    mtext("time", 1, line=xline, cex=cex.txt)
    LeftMargin("cumulative percentage of trials [%]", within=within)
    
    if (n.data > 0) {
      set <- 1
      simu <- get(simuValue(set), envir=ENVIR)
      M <- min(nT, length(data))
      points(simu$Tseq.show[1:M],
             if (ratio) cumdata[1:M] * (100 / m) else cumdata[1:M],
             col = other.col[1], pch=pch, cex=cex.pch) #, lty = 1, lwd = 2)
      if (length(fitted[[currentClass]]) > 0) {
        lines(simu$Tseq.show,
              if (ratio) fitted[[currentClass]]$cum_N * (100 / m)
              else fitted[[currentClass]]$cum_N,
              col = other.col[3], lty = 1, lwd = 3)
        if (cumfit) {
          rss <- round(fitted[[currentClass]]$rss_N)
          text(x=Tend, y=0, cex=2, col=other.col[3], adj=c(1, 0),
               labels=paste0("rss=", rss))
          idx <- !is.na(cumdata[1:M])
          text(x=Tend, y=0, cex=2, col=colour[set], adj=c(1, -2),
               labels=paste0("rss=", round(sum((cumdata[1:M] - simu$cum_N)^2,
                                               na.rm=TRUE))))
        }
      }
    }
    
    for (set in s.sets) {      
      simu <- get(simuValue(set), envir=ENVIR)
      if (length(quantiles) > 0)
	matlines(simu$Tseq.show,
                 if (ratio) simu$cumquants * (100 / m) else simu$cumquants,
                 col=subcol[set], lty=m.lty, lwd=1,
		 ylim=ylim);
      lines(simu$Tseq.show,
            if (ratio) simu$cum_N * (100 / m) else simu$cum_N,
            col=colour[set], lty=1, lwd=3);
    }
    
    if (length(simu$theor) > 0) {
      th <- cumsum(simu$theor) * simu$dt
      lines(simu$Tseq.show, if (ratio) th else th * (m / 100),
	    col = other.col[2], lty = 2, lwd = 3)
    }
  }
 
  graph <- function(which, mtxt, within=TRUE) {
    s.sets <- rev(which(sets))
    n.sets <- sum(sets)
    simu <- get(simuValue(1), envir=ENVIR)
    which <- paste0(which, ".show")
    U.col <- if (n.sets == 1) 1:7 else set.mixcol[s.sets]
    
    if (is.na(simu[[which]][2, 1])) { ## Zeitpunkt 2, 1. Individ
      PlotMessage(c("no image available for", mtxt))
      return()
    }
    ylim <- range(simu[[which]], na.rm=TRUE)

    if (all(is.finite(ylim))) {
      if (ylim[1] == ylim[2]) ylim <- ylim + c(-1,1)
      matplot(simu$Tseq.show, simu[[which]],
	      type="l", lty=1, col=U.col, ylim= ylim,
	      xlab="", ylab="", cex.lab=cex, cex.axis=cex)
      mtext("time", 1, line=xline, cex=cex.txt)
      LeftMargin(mtxt, within=within)
      if (n.sets > 1) {
	for (set in s.sets) {
	  simu <- get(simuValue(set), envir=ENVIR)
	  matlines(simu$Tseq.show, simu[[which]], lty=1, lwd=2,
		   col=colour[set], ylim=ylim)
	}
      }
    } else {
      x <- simu[[which]]
      x <- 1 / (1 + exp(-x))
      ylim <-range(0, x)
      matplot(simu$Tseq.show, x, ylim=ylim,
	      type="l", lty=1, col=U.col, cex.axis=cex,
	      xlab="", ylab="", cex.lab=cex, axes = FALSE, frame=TRUE)
      axis(1, cex.lab=cex, cex.axis=cex)
      axis(2, cex.lab=cex, cex.axis=cex, at=c(0, 0.27, 0.5, 0.73, 1),
	   labels=c(expression(-Inf), expression(-1), expression(0),
		    expression(1), expression(Inf)))
      mtext("time", 1, line=xline, cex=cex.txt)
      LeftMargin(mtxt, within=within)
      if (n.sets>1) {
	for (set in s.sets) {	
	  simu <- get(simuValue(set), envir=ENVIR)
	  x <- simu[[which]]
	  matlines(simu$Tseq.show, 1 / (1 + exp(-x)), lty=1, lwd=2,
                   col=colour[set], ylim=ylim)
	}
      }
    }
  }


  plotU <- function(within=TRUE) graph(which="U", expression("utility " * U),
          within=within)

  plotIc <- function(within=TRUE) 
    graph(which="Ic", mtxt = expression("cumulative social influence " * I[c]),
          within=within)
  
  plotIr <- function(within=TRUE) 
    graph(which="Ir", mtxt = expression("recent social influence " * I[r]),
          within=within)
  
  plotM <- function(within=TRUE) 
    graph(which="M", mtxt = expression("memory effect " * M),
          within=within)
  
  plotUp <- function(within=TRUE) 
    graph(which="Up", mtxt = expression("private utility " * U[p]),
          within=within)
 
  plotDeltaUp <- function(within=TRUE) 
    graph(which="DeltaUp", mtxt = expression("Delta Up " * Delta * U[p]),
          within=within)
  
 
 
  pictures <-       c(FALSE, TRUE,
		      TRUE,  FALSE,
		      TRUE,  FALSE,
		      FALSE, TRUE,
		      FALSE)
  names_pictures <- c("spatial", "Ic", 
		      "trials N(t)",  "Ir", 
		      "cumul trials", "M",
		      "Up", "U",
		      "Delta Up")
  
  
 
  screenshot.ok <- substr(screen.shot, 1, 5) != "xfce4" || !Windows
  ScreenShot <- function(...) {
    if (screenshot.ok) system(screen.shot)
    else Message("Screen shot function is disabled.")
  }

 
  fitted <- vector("list", models)
  
  fit <- function(..., modus = 0
                  ,select=TRUE ## for debugging only
                  ) {
    f <- get("fitted", envir = ENVIR)
    if (modus==0 && length(f[[currentClass]]) > 0) {
      f[[currentClass]] <- list()
      assign("fitted", f, envir = ENVIR)
      tkRreplot(imgLU)
      Message("Fitted values in '", names_classes[currentClass], "' deleted.")
      if (Windows && fit.dev %in% dev.list()) {
        dev.off(fit.dev)
        assign("fit.dev", -1, envir = ENVIR)
      }
      return()
    }

    EntryC <- function(m, par, all=FALSE) {
      set <- 1
      for (k in 1:n) {
        ev <- get(entryValue(I[k], J[k], set=set), envir=ENVIR)          
        tkValue(ev) <- as.character(par[k], Edigits)
        if (!fit_m && (k==n ||
                       (length(LL$CrossReferences)==0 && I[k+1] != I[k])))
          start_simu(update=all.names[I[k]])
      }
      if (fit_m) {
        ev <- get(entryValue(Idx.m, j=1, set=set), envir=ENVIR)
        tkValue(ev) <- as.character(m)
        start_simu(update="m")
       }
      if (all) {
       for (k in 1:n) {
         SliderValue <- par[k]
         if (strictpos[[currentClass]] [[I[k]]] [J[k]])
           SliderValue <- sqrt(SliderValue)
         sl <- get(slValue(I[k],J[k],set=set), envir=ENVIR)
         tkValue(sl) <- SliderValue
       }
      }
      position()
      tkRreplot(imgLU)        
      ## tkRreplot(imgLU)
    }

    RSS <- function(x, all=FALSE) {
      stopifnot(length(x) == n)
      if (PL > 1) Progress(".")
      if (tracefit > 1) EntryC(m, x, all)
      else {
        for (k in 1:n) entryAssign(I[k], J[k], set=set, genuinevalue=x[k]) 
        start_simu(fit.param=TRUE)
      }
      dNt <- matrix(0L, nrow=M, ncol=repetitions)
      U <- get("UpStart", envir=ENVIR) + 0 ## + 0 ultrawichtig
      Ir <- rep(as.double(NA), length(U))
      never.tried <- U < threshold

      dim(never.tried) <- c(m, repetitions)
      it <- 1    
      dNt[it, ] <- Nt <- as.integer(colSums(!never.tried))    
      LIc <- LL$Ic
      if (!is.function(LIc)) LIc<-function(param, Nt, m, start) rep(0, repm)
      alpha <- AlleWerte(Idx.alpha)
      beta <- AlleWerte(Idx.beta)    
      gamma <- AlleWerte(Idx.gamma)
      Icstart <- get("Ic.start", envir=ENVIR)    
      IcParam <- AlleWerte(Idx.Ic.param)
   
      Ic <- LIc(param=IcParam, Nt=Nt, m=m, start=Icstart)

      for (it in 2:M) {
        if (!is.null(LL$Utrafo)) U <- LL$Utrafo(U, threshold, m)      
        .Call(C_adoption, U, Ic, t.W, Ir, dt, DeltaUp, it-1L,
              threshold, never.tried, alpha, beta, gamma, dummy,
              FALSE, dNt, M, Nt, show.n.indiv, M.show,
              U.show, Ic.show, Ir.show, DeltaUp.show, Up.show, Never.Tried,
              cores)
        Ic <- LIc(param=IcParam, Nt=Nt, m=m, start=Icstart)
      }

      if (cumfit || all) {
        cum <- apply(dNt, 2, cumsum)
        ## dim(cum) <- dim(dNt)  ## fast version of next lines
        if (any(dim(dNt) == 1)) dim(cum) <- dim(dNt)
        else stopifnot(all(dim(cum) == dim(dNt)))        
       
        cum <- rowMeans(cum) ## the calculated N(t) of the model
        cum_rss <- sum((cumdata - cum)^2, na.rm=TRUE)
        if (!all) {
          return(cum_rss)
        }
      }
     
      rM <- rowMeans(dNt) / dt
      rss <- sum((data - rM)^2, na.rm=TRUE)   
      return(if (all) list(rss_N=cum_rss, rss_dN=rss, rM_N=rM, cum_N=cum)
             else rss)
    } ## end function RSS
        
    set <- 1    
    
    n <- totLaengen[currentClass] - globalParams
    I <- J <- integer(n)
    ok <- logical(n)
    lower <- upper <- start <- rep(NA, n)
    namen <- character(n)
    idx <- 0
    for (i in (globalParams + 1):length(all.names)) {
      if (Laengen[currentClass, i] > 0) {      
        NN <- 1:Laengen[currentClass, i]
        k <- idx + NN
        I[k] <- i
        J[k] <- NN
        
        ok[k] <- !is.na(Fitting[[currentClass]][[i]][NN]) &
          Fitting[[currentClass]][[i]][NN]
##       lower[k] <- minsets[[set]][[i]][NN] ## other models may have longer
##        upper[k] <-maxsets[[set]][[i]][NN] ## parameter vector in i 
        lower[k] <-
          minclasses[[currentClass]][[i]][NN] ## other models may have longer
        upper[k] <-maxclasses[[currentClass]][[i]][NN] ## parameter vector in i 
        for (j in NN)
          if (ok[idx + j]) {                        
            start[idx + j] <- Wert(i, j)
            namen[idx + j] <- labName(i, j, class=currentClass)
          }
      }
      idx <- idx + Laengen[currentClass, i]
    }

    stopifnot(idx == n)
    idx <- which(is.finite(lower) & is.finite(upper) & lower < upper & ok)
    if (length(idx) == 0) {
      Message("No variables to fit.")
      return()
    }

    idx <- idx[select]
    m <- if (fit_m) n.data else Wert(Idx.m)    
    n <- length(idx)
    I <- I[idx]
    J <- J[idx]
    LL <- L[[currentClass]]

    if (modus == 2) {
      setValue("m", m)
      EntryC(fitted[[currentClass]]$m, fitted[[currentClass]]$par, all=TRUE)

      Message("\nFinal setting for model '",
              names(modelclasses)[currentClass],
            "':\n",
            if (fit_m) paste("market size m\t=", m, "\n"),
            paste(collapse="\n", sep="", "'",
                  fitted[[currentClass]]$namen, "'\t= ",
                  signif(fitted[[currentClass]]$par, 4)),
            "\nRSS\t\t= ", round(if (cumfit) fitted[[currentClass]]$rss_N
                                 else fitted[[currentClass]]$rss_dN), "\n")
      return()
    }

    M <- min(nT, length(data))
    if (M < 2) stop(if (length(data)<2) "emprirical" else "theoretical",
                    "time series has a length less than 2")
    data <- data[1:M]
    cumdata <- cumdata[1:M]
    if (modus == 0)
      Message("Data consists of ", M, " instances and ", n.data,
              " trials in total.")

    
    start <- start[idx]
    lower <- lower[idx]
    upper <- upper[idx]
    namen <- namen[idx]
    parscale <- (abs(lower) + abs(upper)) / 4
 
    nfit <- length(idx) + fit_m * 3 ## '*3' as much more difficult to fit
    if (modus == 0)
      Message("Fitting ", length(idx) + fit_m, " variables: ",
            if (fit_m) "'m', ",
            paste("'", namen, "'", sep="", collapse=", "), ".\n\t*** ",
            if (nfit < 3) "Fitting may take a couple of seconds.."
            else if (nfit < 6) "Fitting may take several minutes."
            else if ( nfit < 8) "Fitting may take hours."
            else "Fitting will take hours.", " ***")


    if (length(show.n.indiv) == 1) show.n.indiv <- 1:show.n.indiv
    show.n.indiv <- show.n.indiv[show.n.indiv <= m]
    user.repet <- Wert(Idx.repetitions)
    repetitions <- max(user.repet, fit_repetitions)
    Message("Current number of repetitions when fitting is ", repetitions, ".")
    if (repetitions < 50 && modus == 0)
      Message("     NOTE: this value is far too small\nfor most models -- a save value is 200.")
    setValue("repetitions", repetitions)
    if (tracefit)
      EntryChanges(Idx.repetitions, j=1, set=set, genuinevalue=repetitions)

    dt <- Wert(Idx.dt)
    threshold <- as.double(Wert(Idx.Uthreshold))

    ## achtung: ungewolltes pointer matching vermeiden! Deshalb ReSe wiederholt.
    M.show <- matrix(as.double(NA), nrow=M, ncol=length(show.n.indiv))
    U.show <- matrix(as.double(NA), nrow=M, ncol=length(show.n.indiv))
    Up.show <- matrix(as.double(NA), nrow=M, ncol=length(show.n.indiv))
    Ic.show <-matrix(as.double(NA), nrow=M, ncol=length(show.n.indiv))
    Ir.show <- matrix(as.double(NA), nrow=M, ncol=length(show.n.indiv))
    DeltaUp.show <- matrix(as.double(NA), nrow=M, ncol=length(show.n.indiv))
    

    fnscale <- (n.data * 0.5 / nT)^2 * nT
    if (!fit_m) {
      repm <- repetitions * m
      setValue("m", m)
      start_simu(update="m")
      Never.Tried <- matrix(1L, nrow=M, ncol=m)

      Message("Investigating collinarity of parameters...")
      RSS(start)
      diff <- upper - lower
      x <- vector("list", length(lower))
      for (i in 1:length(lower)) 
        x[[i]] <- lower[i] + c(0.2, 0.5, 0.8) * (upper[i] - lower[i])
      x <- as.matrix(do.call("expand.grid", x)) ## 3^k grid of the k parameters
      ##                      calculated from the range of the parameter values
   
      for (count in 1:2) {
        x <- rbind(x, start) ## !!     
        median <- value <- numeric(nrow(x))      
        for (i in 1:nrow(x)) {
          if (i %% 40 == 0) Progress("*")
          z <- RSS(x[i, ], all=TRUE) ##
          value[i] <- z$rss_N
          w <- which(z$cum_N <= m / 2) 
          w <- w[length(w)] ## raw median of Delta N(t)
          if (w < length(z$cum_N)) ## more precise median of Delta N(t)
            w <- w + (m/2 - z$cum_N[w]) / (z$cum_N[w+1] - z$cum_N[w])
          median[i] <- w
        }
        
        if (count==2 || any(value < value[length(value)])) break;
        
        x <- vector("list", length(lower))
        boundary.dist <- pmin(start - lower, upper - start)
        stopifnot(all(boundary.dist > 0))
        for (i in 1:length(lower)) 
          x[[i]] <- start[i] + c(-1, 0, 1) * 0.3 * boundary.dist[i]
        x <- as.matrix(do.call("expand.grid", x))
        x <- x[-(nrow(x) + 1) / 2, , drop=FALSE] ## 3^k grid of the k parameters
        ##                      calculated as a neighbourhood of the user value
        
      }

      ## median of the empirical cdf
      w <- which(cumdata <= m / 2)
      w <- w[length(w)]
      rest <- cumdata[-1:-w]
      idx <- which(!is.na(rest))
      if (length(idx) > 0) {
        nw <- idx[1]
        w <- w + nw * (m/2 - cumdata[w]) / (rest[nw] - cumdata[w])
      }
      med.data <- w

      lf <- lsfit(x=x, y=median)$coefficients
      intercept <- lf[1]
       lf <- lf[-1]
      leader <- which(abs(lf) == max(abs(lf)))
      leading.coeff <- lf[leader]

      best <- which(value == min(value))[1]
      start <- x[best, ]
      ## \| x -start \|^2 under constraint that
      ## sum(start_i * lf_i) + intercept = median(data)     
      LE <-rbind(cbind(diag(length(start)), lf), c(lf, 0))
      RHS <- c(start, intercept - med.data)
      S  <-solve(LE, RHS)[1:length(start)]
      if (any(idx <- S < lower | S > upper)) { 
        start <- x[which(value == min(value))[1], ]
        Dmat <- diag(length(start))
        meq <- 1
        dvec <- start
        zaehler <- 0
        repeat {
          zaehler <- zaehler + 1
          Amat <- cbind(lf,
                      diag(length(lf)),
                      -diag(length(lf))
                      )
         bvec <- c(-intercept + med.data,
                    lower + (upper - lower) * 0.0001,
                    -upper + (upper - lower) * 0.0001)
          S <- try(solve.QP(Dmat, dvec, Amat, bvec, meq=1)$solution,
                   silent=TRUE)
          if (is.numeric(S)) break;
          if (zaehler > 5)  {
            S <- start
            break
          }
          ## must be improved !!!!!!
          Progress("@")
          lf[leader] <- lf[leader] + sign(lf[leader])
        }
      }
      if (modus == 0) Message("Investigation of collinarity finished.")
      if (best != nrow(x) && modus == 0)
        Message("Starting value given by the user is ignored.")
      
      RSSmodi <- function(x, all=FALSE) {
        delta.coeff <- as.vector((lf %*% (x-S)) / leading.coeff)
        new <- x[leader] + delta.coeff
        bounded <- max(min(upper[leader], new), lower[leader])
        x[leader] <- bounded
        ans <- RSS(x, all=all)
        if (is.numeric(ans)) ans + 1e8 * abs(new  - x[leader])
        else {
          ans$rss_N <- ans$rss_N + 1e8 * abs(new  - x[leader])
          ans
        }
      }
 

      opt <- optim(S, RSSmodi, method="L-BFGS-B", lower=lower, upper=upper,
                   control=list(fnscale=fnscale, parscale=parscale,
                                pgtol=pgtol, factr=factr))
      delta.coeff <- as.vector((lf %*% (opt$par - S)) / leading.coeff)
      opt$par[leader] <-  max(min(upper[leader], opt$par[leader] + delta.coeff),
                              lower[leader])
      
      close <- (opt$par - lower) < 1e-2
      if (any(close) && modus ==0)
        Message("Note: ", paste("'", namen[close], "'", sep="", collapse=", "),
                if (sum(close) == 1) " has its value" else " have their values",
                "\nclose to the lower boundary.")
      close <- (upper - opt$par) < 1e-2
      if (any(close) && modus == 0)
        Message("Note: ", paste("'", namen[close], "'", sep="", collapse=", "),
                if (sum(close) == 1) " has its value" else " have their values",
                "\nclose to the upper boundary.")

      rss <- RSS(opt$par, all=TRUE)
      f[[currentClass]] <- c(list(par=opt$par), rss)
      f[[currentClass]]$cum_N <- rss$cum_N
      f[[currentClass]]$rM_N <- rss$rM_N 
      f[[currentClass]]$m <- m
      f[[currentClass]]$namen <- namen
      assign("fitted", f, envir = ENVIR)

      EntryChanges(Idx.repetitions, j=1, set=set, genuinevalue = user.repet)
      EntryC(m, opt$par)
 
      if (PL > 1 && !Windows) cat("\n")
      if (modus == 0)
        Message("Fitting has finished for model '",
              names(modelclasses)[currentClass], "':")
      Message(paste(collapse="\n", sep="", "'", namen, "'\t= ",
                    signif(opt$par, 4)),
              "\nRSS\t\t= ", round(if (cumfit) rss$rss_N else rss$rss_dN) ,
              "\n")
       return()
    }
    
    factor <- 8
    m.opt <- list()
    m <- n.data
    i <- 1
    Message("phase 1: searching for upper endpoint for 'm' : ")
    while (i <= factor) {
      Progress("*")
      repm <- repetitions * m
      setValue("m", m)
   
      start_simu(update="m")
      Never.Tried <- matrix(1L, nrow=M, ncol=m)
      opt <- optim(start, RSS, method="L-BFGS-B", lower=lower, upper=upper,
                   control=list(fnscale=fnscale, parscale=parscale,
                                pgtol=pgtol, factr=factr))
      if (tracefit) EntryC(m, opt$par)
      m.opt[[i]] <- opt
      if (i == 1) {
        fnscale <- opt$value 
        start <- opt$par
        start.value <-  opt$value 
      } else {
        fnscale <- min(fnscale, opt$value)
        if (start.value > opt$value) {
          start <- opt$par
          start.value <- opt$value
        }
      }
      if (i > 1 && m.opt[[i]]$value > m.opt[[i-1]]$value) break
      m <- m * 2
      i <- i + 1
      gc()
    }
    if (modus == 0) Message("\nUpper end point for 'm' is ", m, ".")

    
    if (i > factor) { Message("no minimum found\n"); return() }
    m.max <- m
    m.max.opt <- m.opt[[i]]
    if (i > 2) {
      m.min.opt <- m.opt[[i - 2]] ## first!
      m.min <- m / 4
      m.opt <- m.opt[[i - 1]] ## second !
      m <- m / 2
   } else {
      stopifnot(i == 2)
      m.min.opt <- m.opt[[1]] ## first!
      m.min <- m / 2
      m.opt <- m.opt[[i]]  ## second !
      start <- m.opt$par
      start.value <- m.opt$value
      stopifnot(m.max.opt$value >  m.min.opt$value)
      if (modus == 0)
        Message("Best value for 'm' is close to the minimum,\nwhich is",
                n.data, ": ")
      while (m > m.min) {
        Progress("*")
        m <- as.integer( (m + m.min) / 2) ## first time m == m.max
        setValue("m", m)
        start_simu(update="m")
        repm <- repetitions * m
        Never.Tried <- matrix(1L, nrow=M, ncol=m)
        opt <- optim(start, RSS, method="L-BFGS-B",
                     lower=lower, upper=upper,
                     control=list(fnscale=fnscale, parscale=parscale,
                                  pgtol=pgtol, factr=factr))
        fnscale <- min(fnscale, opt$value)
        if (start.value > opt$value) {
          start <- opt$par
          start.value <- opt$value
        }       
        if (tracefit) EntryC(m, opt$par)
        m.opt <- opt
        if (m.opt$value < m.min.opt$value) break
        else {
          m.max.opt <- m.opt
          m.max <- m
        }
      }
      if (!Windows) cat("\n")
    }

    if (m.min < m.max - 1)
      Message("phase 2: searching best value for 'm' in [", m.min, ", ",
          m.max, "] : ", sep="")
    while (m.min < m.max - 1) {
      Progress("*")
      if (m.min == m - 1) {
        if (m.min.opt$value < m.opt$value) {
          m <- m.max <- m.min
          m.opt <- m.max.opt <- m.min.opt
          break
        } else {
          m.min <- m
          m.min.opt <- m.opt
        }
      }
      if (m.max == m + 1) {
        if (m.max.opt$value < m.opt$value) {
          m <- m.min <- m.max
          m.opt <- m.min.opt <- m.max.opt
          break
        } else {
          m.max <- m
          m.max.opt <- m.opt
        }
      }
      m.old <- m
      if (m.max - m > pmax(1, m - m.min)) { ## shorten interval to right of m
        m <- as.integer((m.max + m) / 2)
        setValue("m", m)
        start_simu(update="m")
        repm <- repetitions * m
        Never.Tried <- matrix(1L, nrow=M, ncol=m)
        opt <- optim(m.opt$par, RSS, method="L-BFGS-B",
                     lower=lower, upper=upper,
                     control=list(fnscale=fnscale, parscale=parscale,
                                  pgtol=pgtol, factr=factr))
        fnscale <- min(fnscale, opt$value)
       if (tracefit) EntryC(m, opt$par)
        if (opt$value > m.opt$value) {
          m.max <- m
          m.max.opt <- opt
          m <- m.old
        } else {
          m.min.opt <- m.opt
          m.min <- m.old
          m.opt <- opt
        }
      } else if (m - m.min > 1) {  ## shorten interval to left of m
        m <- as.integer((m.min + m) / 2)
        setValue("m", m)
        start_simu(update="m")
        repm <- repetitions * m
        Never.Tried <- matrix(1L, nrow=M, ncol=m)
        opt <- optim(m.opt$par, RSS, method="L-BFGS-B", lower=lower,
                     upper=upper,
                     control=list(fnscale=fnscale, parscale=parscale,
                                  pgtol=pgtol, factr=factr))
        fnscale <- min(fnscale, opt$value)
        if (tracefit) EntryC(m, opt$par)
        if (opt$value > m.opt$value) {
          m.min <- m
          m.min.opt <- opt
          m <- m.old
        } else {
          m.max <- m.old
          m.max.opt <- m.opt
          m.opt <- opt
        }
      }
    }
    if (m.min.opt$value < m.opt$value) {
      m.opt <- m.min.opt
      m <- m.min
    }
    if (m.max.opt$value < m.opt$value) {
      m.opt <- m.max.opt
      m <- m.max
    }

    setValue("m", m)
    start_simu(update="m")
    repm <- repetitions * m
    Never.Tried <- matrix(1L, nrow=M, ncol=m)
    rss <- RSS(m.opt$par, all=TRUE)

    stopifnot(rss$rss_N == m.opt$value || rss$rss_dN == m.opt$value)   
    f[[currentClass]] <- c(list(par=m.opt$par), rss)


    EntryChanges(Idx.repetitions, j=1, set=set, genuinevalue = user.repet)
    EntryC(m, m.opt$par, all=TRUE)
 
    simu <- get(simuValue(set), envir=ENVIR)
    f[[currentClass]]$cum_N <- simu$cum_N
    f[[currentClass]]$rM_N <- simu$rM_N
    f[[currentClass]]$m <- m
    f[[currentClass]]$namen <- namen
    assign("fitted", f, envir = ENVIR)
           
    if (first) tkRreplot(imgLU) ## otherwise the rss is not shown in the graph
    if (modus == 0) Message("\nFitting has finished for model '",
                            names(modelclasses)[currentClass], "':")
    Message("market size m\t= ", m, "\n",
            paste(collapse="\n", sep="", "'", namen, "'\t= ",
                  signif(opt$par, 4)),
            "\nRSS\t\t= ", round(if (cumfit) rss$rss_N else rss$rss_dN), "\n")
    return(NULL)
  }

  completefit <- function(..., select=TRUE) {
    if (length(fitted[[currentClass]]) > 0) fit() ## delete
    zaehler <- 0
    non_decreasing <- 0
    best <- list(rss_N = Inf)
    repeat{
      zaehler <- zaehler  + 1
      txt <- paste0("***    Complete Fit  --  Round ", zaehler, "    ***")
      if (!Windows) {
        stars <- paste(rep("*", nchar(txt)), collapse= "")
        txt <- paste0("\n\n", stars, "\n", txt, "\n", stars, "\n")
      }
      Message(txt)
      fit(modus = zaehler > 0, select=select)
      f <- get("fitted", envir = ENVIR)
      if (f[[currentClass]]$rss_N < best$rss_N) {
        best <- fitted[[currentClass]]
      } else {
        non_decreasing <- non_decreasing + 1
        if (non_decreasing >= max_increasing) break;
      }
      f[[currentClass]] <- list()
      assign("fitted", f, envir = ENVIR)
    }
    f[[currentClass]] <- best
    assign("fitted", f, envir = ENVIR)
    fit(modus = 2, select=select)
    position()
    tkRreplot(imgLU)
    
    assign("LineNumber", 0, envir=ENVIR)
  }


  
  Saveimages<- function(..., save = gui) {
    m <- Wert(Idx.m)
    Format <- function(datei.zaehler) {
      z <- as.character(datei.zaehler)
      paste0(paste(rep(0, 3-length(z)), collapse=""), z)
    }

    RDA <- ".rda"
    PDFEXT <- ".pdf"
    datei.zaehler <- 1
    if (save) {
      while (file.exists(paste0(f<-paste0(filename, Format(datei.zaehler)),
			      "_U", RDA)))
	datei.zaehler <- datei.zaehler + 1
      f <- tkValue(tkSaveFile(defaultextension=RDA, initialfile=f))
      if (nchar(f) == 0) return()
      f <- strsplit(f, RDA)[[1]]
       if (length(f) > 1) f <- paste0(f[1:length(f)- 1], sep=RDA)
      user <- GetL()
      if (is.null(user)) {
	Message("work space cannot be saved as there are \nincorrect or unfilled fields")
	return()
      }
      name <- paste0(f, RDA)
      Message("creating ", name)
      save(file=name, user, fitted)  ## OK
    } else f <- filename    

    PDF <- if (gui) {
             function(subname, w=5, h=5, devoff=TRUE) {
               if (devoff) dev.off()
               name <- paste0(f, "_", subname, PDFEXT)
               Message("creating ", name)
               pdf(file =name, width=w, height=h)
             }
           } else function(...) dev.new()    
   
    PDF("trials", devoff=FALSE); plotTrials(within=FALSE)    
    PDF("cumulTrials"); plotCumulTrials(within=FALSE)
    for (s in which(sets)) {
      PDF(paste0("spatial_set", s))
      try(plotSpatial(s), silent = TRUE)
    }
    
    PDF("U"); plotU(within=FALSE)
    PDF("Ic"); plotIc(within=FALSE)
    PDF("Ir"); plotIr(within=FALSE)
    PDF("M"); plotM(within=FALSE)
    PDF("Up"); plotUp(within=FALSE)
    PDF("DeltaUp"); plotDeltaUp(within=FALSE)
    dev.off()
    Message("")
    return()
  }

  
  position <- function(...) {
    set.idx <- which(sets)
    if (buttons2right) {
      col.return <- col.graphics <- col.models <- col.globals <-
	col.copy <- col.sl + length(set.idx) + 2
    }

    row <- 1
    title.count <- 1
     for (is in 1:length(set.idx)) {
      set <- set.idx[is]
      tkGridConf(get(shortname(set), envir=ENVIR),
		 column= col.sl + is - 1, row=  row, sticky="e")
    }

    tkGridConf(copyFctn, column=col.copy, row=col.row, sticky=sticky.copy)   
    
    row <- 2 - delta.row.title
    for (i in (globalParams + 1):length(Names)) {
      if (titles.guiloc[title.count] == i) {
	row <- row + delta.row.title
	if (sum(sets) > 1 && title.count == 1) {
	  for (is in 2:length(set.idx)) {
	    set <- set.idx[is]
 	    tkGridConf(get(delColumnButton(set), envir=ENVIR),
		       column=col.sl + is - 1,
		       row=row, sticky="w")
            tkGridConf(get(firstButton(set), envir=ENVIR),
		       column=col.sl + is - 1,
		       row= row, sticky="e")
	  }
	}
	tkGridConf(get(titles[title.count], envir=ENVIR),
		   column=col.sl, row=row)
	row <- row + 1
	title.count <- title.count + 1
      }

      if (maxLen[i] > 0)
	for (j in 1:maxLen[i]) {
	  for (is in 1:length(set.idx)) {
	    set <- set.idx[is]             
	    tkGridConf(get(labObj(i, j, set), envir=ENVIR), 
		       column= col.sl + is - 1,
		       row= row) #, sticky="w"
	  }	
	  row <- row + 1
	  for (is in 1:length(set.idx)) {
	    set <- set.idx[is]
	    if (set == 1) {
              class <- col_currentClass[set]
              if (n.data > 0 && length(Fitting[[class]][[i]]) > 0 &&
                  !is.na(Fitting[[class]][[i]][j])) {
                tkGridConf(get(fitButton(i, j), envir=ENVIR), 
                           column= col.fit, row= row) #, sticky="w"
              }
	      tkGridConf(get(slWidget(i,j), envir=ENVIR), column=col.sl,row=row,
			 sticky="w")
	      tkGridConf(get(entryWidget(i,j), envir=ENVIR), column=col.sl + 0,
			 row=row, sticky="e")
	    } else {
	      tkGridConf(get(entryWidget(i,j,set), envir=ENVIR),
			 column= col.sl + is - 1, row= row)
	    }
	  }
	  row <- row+1
	}
    }
    
 
    ## PLOT  
    tkGridConf(imgLU,
	       rowspan=round(2 * image.rowspan * line.row.compression),
	       padx=0, pady=0, ipadx=0, ipady=0, 
	       columnspan=round(2 * image.colspan * line.col.compression),
	       column=col.plot, row=row.plot, sticky="nw")
  
 
  
    ##  Buttons - new simulation (new seed), return 
    tkGridConf(buttonNewSimu,column=col.return, row=row<-row.return,
	       sticky=sticky)
    if (n.data > 0) {
      tkGridConf(buttonStepFitting,column=col.return, row=row<-row + 1,
                 sticky=sticky)
      tkGridConf(buttonCompleteFitting,column=col.return, row=row<-row + 1,
                 sticky=sticky)
    }
    tkGridConf(buttonScreenShot, column=col.return, row=row<-row + 1,
	       sticky=sticky)
    tkGridConf(buttonSaveimages, column=col.return, row=row<-row + 1,
	       sticky=sticky)
    tkGridConf(buttonReturn, column=col.return, row=row <- row + 1,
	       sticky=sticky)
   
    ## GRAPHICS: Picture choice
    row <- row.graphics
    tkGridConf(pictureTitle, column=col.graphics, row=row, sticky=sticky)
    row <- row + (1  - delta.row.graphics)
    for (s in 1:length(pictures)) {
      tkGridConf(get(pictureButton(s), envir=ENVIR),
		 column=col.graphics +
			    (((1/delta.row.graphics-1) * (s +1)) %% 2),
		 row=floor(row <- row + delta.row.graphics),
		 sticky="e")
    }
 
    ## MODELS
    if (models > 0) {
      row <- row.models
      tkGridConf(classTitle, column=col.models, row=row, sticky=sticky)
      for (s in 1:models) {
	tkGridConf(get(classButton(s), envir=ENVIR),
		   column=col.models, row = row <- row + 1,
		   sticky=sticky)
      }
    }

    ## GLOBALS: m, repet, etc
    row <- row.globals
    tkGridConf(globalTitle, column=col.globals, row=row, sticky=sticky)
     j <- 1
    for (i in 1:globalParams) {
      tkGridConf(get(labObj(i, j), envir=ENVIR), column=col.globals,
		 row=row <- row + 1, sticky=sticky)
      tkGridConf(get(slWidget(i, j), envir=ENVIR), column=col.globals,
		 row=row <- row + 1, sticky=sticky)
      tkGridConf(get(entryWidget(i, j), envir=ENVIR), column=col.globals,
		 row=row, sticky="e")
    }
    tkGridConf(buttonApplyAllOver, column=col.globals, row=row <- row + 1,
	       sticky=sticky)
  
  } ## end fct position
  
  
#### BODY PART ######

  command <- function(text) {
    f <- eval(parse(text=text))
    environment(f) <- ENVIR
    f
  }
 
  pictureButtons <- function() {
    for (s in 1:length(pictures)) {
      text <- paste("function(...) { pictures[", s, "] <- !pictures[", s,
		    "]; assign('pictures', pictures, envir=ENVIR);",
		    "tkRreplot(imgLU)}")
      Y <- pictureValue(s)
      assign(Y, tkVar(as.double(pictures[s])), envir=ENVIR)
      assign(pictureButton(s), envir=ENVIR,
	     tkCheckbutton(tt, text=names_pictures[s],
			   command=command(text),
			   variable = get(Y, envir=ENVIR),
			   onvalue = 1, offvalue=0,
			   anchor="w",
			   fg=clr.graph, font=tkfont
			   ))
    }
  }

  negateFit <- function(i, j) {
    class <- col_currentClass[1]
    if (j > Laengen[class, i] || 
        isinteger[[class]][[i]][j] ||
        (minclasses[[class]][[i]][j] == maxclasses[[class]][[i]][j])) {
      Y <- get(fitValue(i, j), envir=ENVIR)
      tkValue(Y) <- FALSE
      Message('This button is forbidden.')
    } else {
      Fitting[[class]][[i]][j] <- !Fitting[[class]][[i]][j]
      assign('Fitting', Fitting, envir=ENVIR);
    }
  }
  
  FitButtons <- function() {  
    class <- col_currentClass[1]
    for (i in 1:length(all.names)) {
      len <- Laengen[class, i]
      for (j in 1:maxLen[i]) {
        text <- paste0("function(...) negateFit(", i, ",", j, ")")
        Y <- fitValue(i, j)
        assign(Y, tkVar(as.double(FALSE)), envir=ENVIR)
        assign(fitButton(i, j), envir=ENVIR,
               tkCheckbutton(tt, command=command(text),
                             variable = get(Y, envir=ENVIR),
                             onvalue = 1.0, offvalue=0.0,
                             anchor="w",
                              fg=clr.graph,
                             font=tkfont
                             ))
      }
    }
  }

  getexactL <- function(set) {
    class <- col_currentClass[set]
    LL <- L[[class]]
    for (i in 1:length(Names)) {
      LLi <- LL[[i]]
      if (length(LLi) == 0) next
      for (j in 1:length(LLi)) {
	X <- get(entryValue(all.names[i],j, set), envir=ENVIR)
        ans <- try(as.numeric(tkValue(X)), silent = TRUE)
	LLi[j] <- if (is(ans, "try-error")) NA else ans
      }
      LL[[i]] <- LLi
    }
    return(LL)
  }

  putexactL <- function(class, set=1) {
    LL <- if (is.list(class)) class else L[[class]]
    for (i in 1:length(all.names)) {
      LLi <- LL[[i]]
      if (is.numeric(LLi) && maxLen[i] > 0) {
	for (j in 1:maxLen[i]) {
          if (n.data > 0) {
            Y <- get(fitValue(i, j), envir=ENVIR)
            tkValue(Y) <-
              !is.na(Fitting[[class]][[i]][j]) && Fitting[[class]][[i]][j]
          }
	  if (j <= length(LLi)) {
 	    SetEntry(i, j, set, genuinevalue=LLi[j])
	    if (i <= length(Names)) SetSlider(i, j, set, LLi[j])
 	  } else {
	    EmptyEntry(i, j, set)
	    EmptySlider(i, j, set)
	  }
	}
      }
    }    
  }

 
  classButtons <- function(currentClass, fromset=1, simu=TRUE) { ## keep names
    if (simu) assign("plotting", FALSE, envir=ENVIR) 
    LL <- getexactL(fromset)
  
    if (any(is.na(unlist(LL)))){
      Message("class or column cannot be changed as there are \nincorrect or unfilled fields")
      return(TRUE)
    }
    class <- col_currentClass[fromset]
    minclasses[[class]] <- minsets[[fromset]]
    maxclasses[[class]] <- maxsets[[fromset]]

    L[[class]] <- LL
    set <- 1
    col_currentClass[set] <- currentClass
    minsets[[set]] <- minclasses[[currentClass]]
    maxsets[[set]] <- maxclasses[[currentClass]]

    for (v in user_variables) assign(v, get(v), envir=ENVIR)
    putexactL(currentClass)
    for (s in 1:models) {
      tkConfigure(get(classButton(s), envir=ENVIR), bd =0, pady=0,
		  fg=if (s == currentClass) clr.alert else fg[1])
    }
  
   
    tkTitle(tt) <- paste(Titel, collapse=names_classes[currentClass])
    labels()
    if (simu) {
      position()
      start_simu(update="m")   
      assign("plotting", c("classB"=TRUE), envir=ENVIR) 
      tkRreplot(imgLU)
    }
        
    return(FALSE)
  }
  
  initclassButtons <- function() {
    for (s in 1:models) {
      cB <- command(paste("function(...) classButtons(", s, ")"))
      assign(classButton(s), envir=ENVIR,
	     tkButton(tt, text=names_classes[s], command=cB,
		      pady = button.pady,
		      fg=if (s == currentClass) clr.alert else fg[1],
		      font=tkfont
		      ))
    }
    tkTitle(tt) <- paste(Titel, collapse=names_classes[currentClass])
  }
    
  SetSlider <- function(i, j, set=1, genuinevalue) {
    class <- col_currentClass[set]

    V <- fromTo(from = minsets[[set]][[i]][j], genuinevalue=genuinevalue,
                to = maxsets[[set]][[i]][j],
		pos=strictpos[[class]][[i]][j], int=isinteger[[class]] [[i]][j])
     Y <- get(slValue(i, j, set), envir=ENVIR)
     
    # Print(from = minsets[[set]][[i]][j], genuinevalue=genuinevalue,
    #            to = maxsets[[set]][[i]][j],
    #       pos=strictpos[[class]][[i]][j], int=isinteger[[class]] [[i]][j],  V)


###   if(V$sliderTo < V$S || V$sliderFrom > V$S) {print("bye"); q()}

     tkConfigure(get(slWidget(i, j, set), envir=ENVIR), digits=Sdigits,
		from = V$sliderFrom, to = V$sliderTo, resolution= V$reso)
 
    if (V$SliderValue != tkValue(Y)) {
      tkValue(Y) <- V$SliderValue
      ev <- get(entryValue(i, j, set=set), envir=ENVIR)
      tkValue(ev) <- as.character(V$genuinevalue)
    }

  }

  EmptySlider <- function(i, j, set=1) {
     class <- col_currentClass[set]
    Y <- get(slValue(i, j, set), envir=ENVIR)
    tkConfigure(get(slWidget(i, j, set), envir=ENVIR),
		from = 1, to = 1, resolution=0)
    if (1 != tkValue(Y)) tkValue(Y) <- 1
  }

  init.sets <- function() {
    for (i in 1:length(all.names)) {
      if (maxLen[i] > 0) for (j in 1:maxLen[i]) {	
	for (set in 1:max.sets) {	  
	  ##  set sliders; for set > 1 not used in gui
	  sltext <- command(paste("function(...) SliderChanges(i=", i,
					  ", j=", j, ", set=", set, ")"))
	  Y <- slValue(i, j, set)
	  assign(Y, tkVar(1), envir=ENVIR) ## dummy value
	  assign(slWidget(i, j, set),
		 tkScale(tt, command = sltext,
			 from= -1, # dummy value
			 to = 2, # dummy value
			 showvalue=FALSE,
			 width = width.slider,
			 variable=get(Y, envir=ENVIR),
			 digits=Sdigits,
			 resolution=1, ## dummy value
			 orient="horizontal",
			 length=length.slider,
			 width=width.slider),
		 envir=ENVIR)
  
	  ## init entries
	  assign(entryValue(i, j, set), tkVar(1), envir=ENVIR) ## dummy value
	  X <- entryWidget(i, j, set)
	  assign(X, envir=ENVIR,
		 tkEntry(tt,
			 font = tkfont,
			 textvariable=get(entryValue(i,j,set), envir=ENVIR),
			 width=if (i <= globalParams) wide.width.entry
                         else width.entry,
                         borderwidth=1, selectborderwidth=1))
          if (simuOnTheFly) {
            text <- command(paste("function(...) EntryChanges(i=", i, ", j=", j,
                                  ", set=", set, ")"))            
            tkBind(get(X, envir=ENVIR),
                   if (simuOnTheFly > 1) "<KeyRelease>" else "<FocusOut>", text)
          }
	## init labels
	n <- labObj(i, j, set)
	  assign(n, envir=ENVIR,
		 tkLabel(tt, text = "dummy lab", fg=clr.alert, font=tkfont))
					#, bd=1
	}
      }
    }
  }

  abbr.label <- function(name) base::abbreviate(name, minlength=6, strict=TRUE)
  
  labels <-  function(set=1) {
    class <- col_currentClass[set]
    tkConfigure(get(shortname(set)), text=shortnames_classes[class],
		      bd=0, pady=0, font=tkfont)
    sub.n <- 1
    title.count <- 1
    for (i in 1:length(Names)) {
      if (titles.guiloc[title.count] == i) {
	importance <- titles.val[[class]][[title.count]]
	sub.n <- 1
	title.count <- title.count + 1
      }
      if (maxLen[i] > 0) for (j in 1:maxLen[i]) {
	if (j <= Laengen[class, i]) {
	  ln <- labName(i, j, class)
	  if (set > 1) ln <- abbr.label(ln)
          else if (strictpos[[class]][[i]][j]) ln <- paste(ln, ">0")

	  FG <- if (i<=globalParams) clr.global 
                else if (ln==unused) fg[length(fg)] 
                else fg[importance[sub.n]]

          #Print(i, j, ln, globalParams, ln==unused, importance, sub.n, FG)

	  tkConfigure(get(labObj(i, j, set), envir=ENVIR), text = ln,
                      fg=FG, bd=0, pady=0, font=tkfont)
	} else {
	  tkConfigure(get(labObj(i, j, set), envir=ENVIR), bd =0, pady=0,
		      text=unused, fg=clr.unused, font=tkfont)
	}
      }
      sub.n <- sub.n + 1
    }
  }


 
  main <- function() {
    ## Reihenfolge der ersten 3 Zeilen zwingend
 

    ##--- Buttons ----------------------------
#    str(minclasses)
#    str(maxclasses)
    if (n.data > 0) FitButtons()
    assign("buttonNewSimu", envir=ENVIR, 
	   tkButton(tt,text="new simulation", bg=clr.general, font=tkfont,
		    fg = fg[2], pady = button.pady,
		    command=function(...) {
		      start_simu(all=TRUE)
		      tkRreplot(imgLU)}))
    assign("buttonStepFitting", envir=ENVIR, 
	   tkButton(tt,text=" fit 1 step ", bg=clr.general, font=tkfont,
		    fg = fg[2], pady = button.pady, command=function(...) {
                      assign("LineNumber", 0, envir=ENVIR); fit()}))
    assign("buttonCompleteFitting", envir=ENVIR, 
	   tkButton(tt,text=" complete fit  ", 
                    fg = fg[1], bg=clr.general, font=tkfont,
		    pady = button.pady, command=completefit))
    txt <- if (substr(screen.shot, 1, 5) != "xfce4") "usr def scr shot"
           else if (Windows) "scr shot (inapt)" 
           else "scr shot (xfce4)" 
    assign("buttonScreenShot", envir=ENVIR, 
	   tkButton(tt, command=ScreenShot, text=txt, font=tkfont,
		    fg=fg[if (win.sys) 5
                          else if (Sys.info()["sysname"]=="Linux") 3 else 4],
                    pady = button.pady,
		    bg=clr.general))
    assign("buttonSaveimages", envir=ENVIR,
	   tkButton(tt, text= "save & pdf", command=Saveimages, font=tkfont,
		    fg = fg[2],pady = button.pady,bg=clr.general))
    assign("buttonReturn", envir=ENVIR,
	   tkButton(tt, text="     return      ", font=tkfont,
		    pady = button.pady,
		    command=OnReturn, fg = fg[1], bg=clr.general))
   
    assign("pictureTitle", envir=ENVIR,
	   tkLabel(tt, text="GRAPHICS", fg=clr.graph, font=tkfont))
    pictureButtons()
    assign("classTitle", envir=ENVIR,
	   tkLabel(tt, text="MODEL", fg=clr.alert, font=tkfont))
    assign("buttonApplyAllOver", envir=ENVIR,
	   tkButton(tt,text="apply all over", command=applyAllover,
		    pady = button.pady,
		    fg=clr.global, font=tkfont, bg=clr.general, font=tkfont))
    assign("globalTitle", envir=ENVIR,
	   tkLabel(tt, text="GLOBALS", fg=clr.global, font=tkfont))
    assign("copyFctn", envir=ENVIR,
	   tkButton(tt, text="cpy", command=copy, fg=clr.global,bg=clr.general,
		    padx=3, pady = button.pady,font=tkfont))
    assign(simuValue(1), NULL, envir=ENVIR)

    for (set in 2:max.sets) {
      assign(simuValue(set), NULL, envir=ENVIR)
      text <-  paste("function(...) deleteColumn(", set, ")")
      assign(delColumnButton(set),
	     tkButton(tt, text="d", command=command(text), font=tkfont,
                     borderwidth =0, padx=8,
		      pady = button.pady,bg=colour[set], fg=fgcol[set]),
	     envir=ENVIR)
      text <-  paste("function(...) first(", set, ")")
      assign(firstButton(set),
	     tkButton(tt, text="1", command=command(text), font=tkfont,
                      borderwidth = 0, padx=8,
		      pady = button.pady,bg=colour[set], fg=fgcol[set]),
             envir=ENVIR)    
      assign(shortname(set), envir=ENVIR,
	     tkLabel(tt, text=paste0("model", set), fg=colour[set],
                     font=tkfont))
    }
    assign(shortname(1), envir=ENVIR,
	   tkLabel(tt, text=shortnames_classes[currentClass],
		   fg=colour[1], font=tkfont))

    if (length(classinfo) > 0) {
      variables <- c("minclasses", "maxclasses", "titles.val")
      for (v in variables) get(v, envir=ENVIR)
   #  Print(length(classinfo))
      for (i in 1:length(classinfo)) {
 #       print(i)
	if (length(classinfo[[i]]) == 0) next
        if (length(classinfo[[i]]$minclasses) != 0)
          minclasses[[i]] <- classinfo[[i]]$minclasses
        if (length(classinfo[[i]]$maxclasses) != 0) 
          maxclasses[[i]] <- classinfo[[i]]$maxclasses
        if (length(classinfo[[i]]$titles.val) != 0) 
          titles.val[[i]] <- classinfo[[i]]$titles.val    
      }
   #   Print(classinfo, minclasses, maxclasses)
      for (v in variables) assign(v, get(v), envir=ENVIR)
    }

    for (i in 1:length(Names))
      assign(titles[i], envir=ENVIR,
	     tkLabel(tt, text=titles[i], fg=clr.title, bd=1, font=tkfont))

    assign("plotting", FALSE, envir=ENVIR)
    init.sets()
    putexactL(currentClass)
    labels()
 #   assign("imgLU", envir = ENVIR, tkrplot::tkrplot(tt, fun = plotSimulation,
#					  hscale=plothscale, vscale=plotvscale))


    if (length(setinfo) > 0) {
      allinfo <- 1:length(setinfo)
      allinfo <- c(allinfo[-1], 1)
      variables <- c("minsets", "maxsets")
      for (v in variables) get(v, envir=ENVIR)
      for (ii in 1:length(allinfo)) {
	i <- allinfo[ii]
	if (length(setinfo[[i]]) == 0) next
	info <- setinfo[[i]]$setinfo
	set <- info$set
	minsets[[set]] <- info$minset ## must be set before putexactL
        maxsets[[set]] <- info$maxset
      }
      for (v in variables) assign(v, get(v), envir=ENVIR)
        
      for (ii in 1:length(allinfo)) {
	i <- allinfo[ii]
 	if (length(setinfo[[i]]) == 0) next
	info <- setinfo[[i]]$setinfo
        LL <- setinfo[[i]]
	LL$setinfo <- NULL
	LL <- LL[sapply(LL, is.numeric)]	
	set <- info$set
	class <- info$class	  
	set_colClass(set, class)
    	putexactL(LL, set)
	assign(simuValue(set), info$simuvalue, envir=ENVIR)	
	labels(set)
      }
    }

    initclassButtons()
    position()
    start_simu(all=TRUE)
    assign("plotting", c(main=TRUE), envir=ENVIR)
    if (!gui && n.data > 0) completefit()
    tkRreplot(imgLU)
    if (!gui) {
      if (PL > 0) Saveimages()      
      assign(".adoption.exit", GetL(), envir=parent.ev)      
    }
    
    ## nur an dieser Stelle funktioniert 'tracefit'
    ##    print("del this line");
     ## completefit(select=c(TRUE, FALSE)); #fit(); fit() 
  }

  main()
}

