\name{Technology}
\alias{technology}
\alias{Automatic.transmission} \alias{Automobile} \alias{Cable.TV}
\alias{Cellular.phone} \alias{Central.heating} \alias{Colour.TV}
\alias{Computer} \alias{Dishwasher} \alias{Disk.brakes} \alias{Dryer}
\alias{Ebook.reader} \alias{Electric.Range} \alias{Electric.power}
\alias{Electronic.ignition} \alias{Flush.toilet} \alias{Freezer}
\alias{Home.air.conditioning} \alias{Household.refrigerator}
\alias{Internet} \alias{Iron} \alias{Landline} \alias{Microcomputer}
\alias{Microwave} \alias{Nox.pollution.controls} \alias{Podcasting}
\alias{Power.steering} \alias{RTGS.adoption} \alias{Radial.tires}
\alias{Radio} \alias{Refrigerator} \alias{Running.water}
\alias{Shipping.container.port} \alias{Smartphone.usage}
\alias{Social.media.usage} \alias{Stove} \alias{Tablet}
\alias{Television} \alias{Vacuum} \alias{Videocassette.recorder}
\alias{Washer} \alias{Washing.machine} \alias{Water.Heater} 


\docType{data}
\title{
  Technology Adoption
}
\description{
  Technology adoption by households in the United States, 
  available from \url{https://ourworldindata.org/technology-adoption},
  published by Hannah Ritchie and Max Roser.

  NOTE THAT THE DATA ARE PUBLISHED UNDER THE CC-BY LICENCE (2019),
  \url{https://ourworldindata.org/technology-adoption},
  WHOSE POLICY MUST BE STRICTLY FOLLOWED, SEE ALSO THE NOTE BELOW.
}
\usage{data(technology)}
\format{
  The data is a list of data frames, each data frame contains 
  the percentage of adoption and the year.
} 

\note{For citing any of the data sets, usually two sources have to be
  given, namely
  \url{https://ourworldindata.org/technology-adoption}
  and the original source, see
  \url{https://ourworldindata.org/grapher/technology-adoption-by-households-in-the-united-states?time=1860..2016&country=Automatic\%20transmission+Automobile+Cable\%20TV+Cellular\%20phone+Central\%20heating+Colour\%20TV+Computer+Dishwasher+Disk\%20brakes+Dryer+Ebook\%20reader+Electric\%20Range+Electric\%20power+Electronic\%20ignition+Flush\%20toilet+Freezer+Home\%20air\%20conditioning+Household\%20refrigerator+Internet+Iron+Landline+Microcomputer+Microwave+Nox\%20pollution\%20controls\%20(boilers)+Podcasting+Power\%20steering+RTGS\%20adoption+Radial\%20tires+Radio+Refrigerator+Running\%20water+Shipping\%20container\%20port\%20infrastructure+Smartphone\%20usage+Social\%20media\%20usage+Stove+Tablet+Television+Vacuum+Videocassette\%20recorder+Washer+Washing\%20machine+Water\%20Heater} .
See also the general instructions at \url{https://ourworldindata.org}.
}
 
\details{
  The list elements are
  \describe{
    \item{Automatic.transmission}{Automatic transmission
      vehicle in percentage of car output.} 
    \item{Automobile}{Percentage of US households that own an
      automobile. }
    \item{Cable.TV}{Percentage of US households with cable TV. }
    \item{Cellular.phone}{Percentage of US households
      with a cellular phone. } 
    \item{Central.heating}{Percentage of American
      dwelling units with central heating. } 
    \item{Colour.TV}{Percentage of US households with colour TV.}
    \item{Computer}{Computer: Adoption rates of computers by US households. }
    \item{Dishwasher}{Adoption rates of dishwashers in US
      households. } 
    \item{Disk.brakes}{Disk brakes in the percentage of car
      output. } 
    \item{Dryer}{Adoption rates of dryers in US households. }
    \item{Ebook.reader}{Percentage of US adults who own an
      Ebook reader. } 
    \item{Electric.Range}{Adoption rates of electric
      ranges in US households. } 
    \item{Electric.power}{Percentage of US households
      with electric power. } 
    \item{Electronic.ignition}{Cars with electronic
      ignition in percentage of car output.} 
    \item{Flush.toilet}{American households with access to a flush toilet.}
    \item{Freezer}{Diffusion rates of freezers in the US economy. }
    \item{Home.air.conditioning}{Home air conditioning: Percentage of US
      households with home air conditioning.} 
    \item{Household.refrigerator}{Percentage of
      US households that own a refrigerator. } 
    \item{Internet}{Percentage of US households with access to
      the internet.} 
    \item{Iron}{Diffusion rates of ironers in the US economy. }
    \item{Landline}{Percentage of US households with a landline. }
    \item{Microcomputer}{Percentage of US households with
      microcomputers.} 
    \item{Microwave}{Diffusion rates of microwaves in the US
      economy. } 
    \item{Nox.pollution.controls}{
      Percentage of boilers adopting Nox pollution control technologies. } 
    \item{Podcasting}{Percentage of Americans aged 12 or
      older who have listened to a podcast (ever)} 
    \item{Power.steering}{Power steering in vehicles in
      percentage of car output} 
     \item{RTGS.adoption}{Adoption rate of RTGS technology.}
    \item{Radial.tires}{Cars with radial tires in
      percentage of car output. } 
    \item{Radio}{Percentage of US households that own a radio. }
   \item{Refrigerator}{Diffusion rates of refrigerators in
     the US economy.} 
    \item{Running.water}{Percentage of households with running water}
    \item{Shipping.container.port}{(Shipping container port
      infrastructure) Percentage adoption 
      of port infrastructure.}
    \item{Smartphone.usage}{Percentage of US adults who own a
      smartphone. }
     \item{Social.media.usage}{Percentage of US
       adults who use at least one social media site. }    
    \item{Stove}{Percentage of US households with a stove. }
    \item{Tablet}{Percentage of US adults who own a tablet. }
    \item{Television}{Adoption rates of TV by US households. }
    \item{Vacuum}{Diffusion rates of vacuums in the US economy. }
    \item{Videocassette.recorder}{Adoption rates
      of videocassette recorders by US households. } 
    \item{Washer}{Diffusion rates of washers in the US economy. }
    \item{Washing.machine}{Percentage of US households
      that own a washing machine.} 
    \item{Water.Heater}{Diffusion rates of water heaters in the US economy. } 
  }
}

\source{ \url{https://ourworldindata.org/technology-adoption}}
\references{
  General
  \itemize{
    \item \adoptionpaper
    \item Ritchie, H.,  Roser, M. (2019) Technology Adoption
    \url{https://ourworldindata.org/technology-adoption}. Accessed 5
    March 2019.
    \item Roser, M., Ortiz-Ospina, E. (2019) Global Rise of
    Education. OurWorldInData.org. \url{https://ourworldindata.org/technology-adoption} 
  }

  
According to the licence, the references given by Max Roser and co-workers are
 repeated and completed where possible:
   \itemize{
     \item Bech, M.L., Hobijn, B. (2006), Technology diffusion within
    central banking: the case of real-time gross settlement. Staff
    report. Federal Reserve Bank of New York, \bold{260}.
   
    \item Bowden, S., Offer, A. (1994) Household appliances and the use
    of time: the United States and Britain since the
    1920s. \emph{Economic History Review}, \bold{47} 725--748. 

    \item Comin, D., Hobijn, B. (2004) Neoclassical Growth and the
    Adoption of Technologies. Working paper \bold{10733}. National
    Bureau of Economic Research.
    
     \item Dediu, H. (2004)

     \item Felton, N NTY (2014)
 
    \item Greenwood, J., Seshadri, A., Yorukoglu (2005) Engines of
    Liberation \emph{The Review of Economic Studies}, \bold{72}, 109--133.

    \item Gruebler, A. (1990) The Rise and Fall of Infrastructures:
    Dynamics of Evolution and Technological Change in Transport;
    Physica-Verlag Heidelberg.

    \item Isard (1942) A Neglected Cycle: The transport-building cycle.
    \emph{The Review of Economics and Statistics} \bold{24}, 149--158.
    
     
    \item Lebergott, S. (1976) The American Economy: Income, Wealth and
    Want; Princeton University Press, Princeton.

    \item Lebergott, S. (1993) Pursuring Happiness: American Consumers
    in the Twentieth Century.  Princeton University Press, Princeton.

 
    \item Nielsen Television Audience (2008)

    See also
    Nielsen Television Audience (2010--2011)
    \url{https://robertoigarza.files.wordpress.com/2008/10/rep-television-audience-2010-11-nielsen-2011.pdf}
   
 
    \item Pew Research Centre \url{https://www.pewresearch.org/internet/datasets/}

   \item Popp, D. (2010) Exploring links between innovation and
    diffusion: adoption of NO x control technologies at US coal-fired
    power plants. \emph{Environmental and Resource Economics},
    \bold{45}, 319--352.

  
    \item Popp, D., Hafner, T., Johnstone, N. (2007) Policy vs. Consumer
    Pressure: Innovation and Diffusion of Alternative Bleaching
    Technologies in the Pulp Industry. Working Paper. National Bureau of
    Economic Research \bold{13439}.

    \item Rua, G. (2013) Fixed costs, network effects, and the diffusion
    of container shipping. Federal Reserve Board.
    \url{https://cpb-us-west-2-juc1ugur1qwqqqo4.stackpathdns.com/sites.udel.edu/dist/2/425/files/2013/10/GRua_Containers_103113-1ymozt3.pdf}

   \item  The New York Times (2008) How Americans spend their money.
    \url{https://archive.nytimes.com/www.nytimes.com/imagepages/2008/02/10/opinion/10op.graphic.ready.html}
    
    
    \item US Census Bureau's data (1992--2011)
    \url{https://www.census.gov/programs-surveys/decennial-census/data/datasets.2010.html}

 
      }
}

\examples{
data(technology)
D <- technology$Microwave
indviduals <- 500
if (interactive())
  adoption(startwith=1, cumdata=round(indviduals * D$adoption / 100),
           dt=1, Tstart=D$year[1])

\dontshow{\dontrun{%% code that created the R data set from the csv file
t<-read.table("../lit/technology-adoption-by-households-in-the-united-states.csv", skip=1, sep=",")[,c(1,3,4)]
N<-as.character(t[,1])
Y <-as.integer(t[,2])
P<-as.double(t[,3])
u <- unique(N)
technology <- list()
for (i in 1:length(u)) {
  idx <- which(N == u[i])
  r <- range(Y[idx])
  year <- r[1]:r[2]
  adoption <- rep(NA, length(year))
  adoption[Y[idx] - r[1] + 1] <- P[idx]
  name <- paste(strsplit(u[i], " ")[[1]][-4:-10], collapse=".")
  cat(name, "")
  technology[[name]] <- data.frame(year=year, adoption=adoption)
}
save(file="../data/technology.rda", technology) # OK
}}


}
\keyword{datasets}
