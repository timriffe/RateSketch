\name{RateSketch-package}
\alias{RateSketch-package}
\alias{RateSketch}
\docType{package}
\title{
a function for hand sketching demographic and other 2-d functions
}
\description{
The user specifies axes limits, whether to log the y axis, the desired x coordinated to interpolate at, and optionally another function to serve as a guide. Then click points to be interpolated. Out put includes the points clicked, and linear, loess and spline interpolations at the desired x values.
}
\details{
\tabular{ll}{
Package: \tab RateSketch\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2011-07-21\cr
License: \tab General Public\cr
LazyLoad: \tab yes\cr
}
}
\author{
Tim Riffe

Maintainer: Tim Riffe <tim.riffe@gmail.com>

}


\keyword{ package }
\keyword{ iplot}
\seealso{
the package \code{\link{digitize}}, which lets users extract data from a graph by providing calibration points.
}
\examples{
# Examples
library(RateSketch)

FertSchedule1 <- RateSketch(xlim=c(15,50),ylim=c(0,.15),xnew=15:50)
FertSchedule2 <- RateSketch(xlim=c(15,50),ylim=c(0,.15),xnew=15:50,compare=FertSchedule1$LOESS)

MigSchedule1 <- RateSketch(xlim=c(0,90),ylim=c(0,.1),xnew=0:80)
MigSchedule2 <- RateSketch(xlim=c(0,90),ylim=c(0,.1),xnew=0:80,compare=MigSchedule1$LOESS)
MigSchedule3 <- RateSketch(xlim=c(0,90),ylim=c(0,.1),xnew=0:80,compare=cbind(MigSchedule1$LOESS,MigSchedule2$LOESS))

MortSchedule1 <- RateSketch(xlim=c(0,99),ylim=c(0.00005,.7),xnew=0:80,log=TRUE)
MortSchedule2 <- RateSketch(xlim=c(0,99),ylim=c(0.00005,.7),xnew=0:80,log=TRUE,compare=MortSchedule1$LINEAR)
}
