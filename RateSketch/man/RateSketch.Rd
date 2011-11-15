\name{RateSketch}
\alias{RateSketch}
\title{a function for hand sketching demographic and other 2-d functions}
\description{
The user specifies axes limits, whether to log the y axis, the desired x coordinated to interpolate at, and optionally another function to serve as a guide. Then click points to be interpolated. Out put includes the points clicked, and linear, loess and spline interpolations at the desired x values.
}
\usage{
RateSketch(xlim, ylim, xnew, log = FALSE, compare, span = 0.3)
}
\arguments{
  \item{xlim}{
vector with two elements specifying the lower and upper x axis limits, respectively, as with the \code{xlim} argument in \code{plot()}
}
  \item{ylim}{
vector with two elements specifying the lower and upper y axis limits, respectively, as with the \code{ylim} argument in \code{plot()}
}
  \item{xnew}{
a vector of the desired x coordinates at which y values should be interpolated.
}
  \item{log}{
logical, default = FALSE. Should the y axis be logged? This can either be specified logically, or per the normal \code{par} way, with \code{log="y"}. If you specify \code{log=TRUE} or \code{log="y"} (identical), then interpolation is carried out over the logged values, and the interpolated points are back-transformed for the output.
}
  \item{compare}{
an optional 2-column \code{matrix} or \code{data.frame} of values to plot as a guide in the background. The first column should be x values and the second column should be y values. Output from previous runs of this function can also be fed into a new run to re-sketch a function and makes changes. More than 1 such comparison functions can be given, as long as they can be stuck together using \code{cbind()}, in which case each function should still include both the x and y column.
}
  \item{span}{
the \code{loess()} smoothing parameter. Smaller = less smooth, but you need more points. Bigger = smoother. Default = .3
}
}
\details{
Specify reasonable x and y limits for the function you want to sketch, e.g. something like \code{xlim=c(15,50)} and \code{ylim=c(0,.2)} for fertility or \code{xlim=c(0,99)}, \code{ylim=c(.00005,.65)} and \code{log=TRUE} for mortality (whatever you think is reasonable). This function could also be used for migration schedules, labor force transition schedules, production and consumption by age, marriage, divorce, and other functions similar to demographic functions that vary over age and don't have crazy irregular patterns.
}
\value{
A list of 4 matrices, each with an x column and a y column.
  \item{Dots }{The x and y values for the points you clicked.}
  \item{LOESS }{column 1 are the values from \code{xnew}, and column 2 are the loess interpolated values. }
  \item{SPLINE }{column 1 are the values from \code{xnew}, and column 2 are the spline interpolated values. }
  \item{LINEAR }{column 1 are the values from \code{xnew}, and column 2 are the linear interpolated values. }
}
\author{
Tim Riffe
}
\note{
The \code{loess()} function does not extrapolate, so you might get \code{NAs} for the first and last values. This methods tends to give less erratic results compared with the spline output. The \code{spline()} function does extrapolate, so you will get a y value for all desired x coordinates. This  works well when curves are not abrupt and when you click more points. If you have a fast transition, the spline can be very volatile. For linear interpolation, if you have the package \code{Hmisc} installed, then the function \code{approxExtrap()} is used, and points will be interpolated, and extrapolated where necessary. Otherwise, \code{approx()} is used and points are only interpolated, meaning you might get \code{NAs} for the first and last \code{xnew} values. It may be desirable to take part of the output from one function and another part from a different function, or to use various of these outputs in the \code{compare} argument for a re-sketch.
}

\seealso{
There is package for taking plots from papers where you don't have the data and digitizing them into data, \code{\link{digitize}}.
}
\examples{
library(RateSketch)

FertSchedule1 <- RateSketch(xlim=c(15,50),ylim=c(0,.15),xnew=15:50)
FertSchedule2 <- RateSketch(xlim=c(15,50),ylim=c(0,.15),xnew=15:50,compare=FertSchedule1$LOESS)

MigSchedule1 <- RateSketch(xlim=c(0,90),ylim=c(0,.1),xnew=0:80)
MigSchedule2 <- RateSketch(xlim=c(0,90),ylim=c(0,.1),xnew=0:80,compare=MigSchedule1$LOESS)
MigSchedule3 <- RateSketch(xlim=c(0,90),ylim=c(0,.1),xnew=0:80,compare=cbind(MigSchedule1$LOESS,MigSchedule2$LOESS))

MortSchedule1 <- RateSketch(xlim=c(0,99),ylim=c(0.00005,.7),xnew=0:80,log=TRUE)
MortSchedule2 <- RateSketch(xlim=c(0,99),ylim=c(0.00005,.7),xnew=0:80,log=TRUE,compare=MortSchedule1$LINEAR)


## The function is currently defined as
function(xlim,ylim,xnew,log=FALSE,compare,span=.3){
	# a function to open a graphical device with the appropriate dimensions, axes and a reference grid:
	BaseGrid <- function(xlim=c(0,10),ylim=c(0,10),log=FALSE,xat,yat,scipen=0){
		
		# convert log to either "" or "y"
		log <- ifelse(log==FALSE,"",log)
		log <- ifelse(log==TRUE,"y",log)
		
		if (log=="y" & ylim[1]==0){
			cat("\ncan't have 0 as a lower y axis limit. The function will assume you want to plot a \nmortality hazard and change the lower y limit to 0.00005\n")
			ylim[1] <- 0.00005
		}
		
		plot(runif(1),type="n",xlim=xlim,ylim=ylim,log=log,axes=F,xlab="",ylab="")
		segments(xlim[1],axTicks(2),xlim[2],axTicks(2),lty=2,col="grey")
		segments(axTicks(1),ylim[1],axTicks(1),ylim[2],lty=2,col="grey")
		options(scipen=scipen)
		axis(2,pos=xlim[1],at=axTicks(2),las=2)
		axis(1,pos=ylim[1],at=axTicks(1))
		title(main="RateSketch: click points (more = better). to exit, click outside grid")
	}
	
	# a function for clicking points and interpolating/extrapolating them
	AddDots <- function(xlim=xlim,ylim=ylim,xnew=xnew,span=span,log=log){
		# starting point
		Dot <- locator(1)
		points(Dot$x,Dot$y,col="blue")
		xcoords <- Dot$x        ;       ycoords <- Dot$y
		# keep clicking. as long as dots are in region you get another click.
		# loop breaks once you click in the margin and the function continues
		while(Dot$x > xlim[1] & Dot$x < xlim[2] & Dot$y > ylim[1] & Dot$y < ylim[2]){
			Dot <- locator(1)
			if (Dot$x > xlim[1] & Dot$x < xlim[2] & Dot$y > ylim[1] & Dot$y < ylim[2]){
				points(Dot$x,Dot$y,col="blue")
				xcoords <- c(xcoords,Dot$x)
				ycoords <- c(ycoords,Dot$y)
				lines(xcoords,ycoords,col="blue")
				
			}
		}
		# if logged, then log for loess and spline
		if (log=="y"){
			ycoords <- log(ycoords)
		}
		# for interpolation/extrapolation, results are much better if done logged if the axes were logged
		Dots <- cbind(xcoords,ycoords)
		if (is.element("Hmisc",installed.packages()[,1])){
			LINEAR <- Hmisc:::approxExtrap(x=Dots[,1],y=Dots[,2],xout=xnew)
		} else {
			LINEAR <- approx(x=Dots[,1],y=Dots[,2],xout=xnew)
		}
		LINEAR <- cbind(LINEAR$x,LINEAR$y)
		
#	# if logged, then log for loess and spline
#	if (log=="y"){
#		ycoords <- log(ycoords)
#	}
		
		# interpolate in 3 ways:
		LOESS <- cbind(xnew,predict(loess(ycoords~xcoords,span=span),newdata=xnew))
		SPLINE <- spline(xcoords,ycoords,n=length(xnew),xmin=xnew[1],xmax=xnew[length(xnew)])
		SPLINE <- cbind(SPLINE$x,SPLINE$y)
		
		# convert back to absolute scale if necessary
		if (log=="y"){
			LOESS[,2] 	<- exp(LOESS[,2])
			SPLINE[,2] 	<- exp(SPLINE[,2])
			LINEAR[,2] 	<- exp(LINEAR[,2])
			Dots[,2] 	<- exp(Dots[,2])
		}
		
		# plot the results of LOESS and SPLINE so user can make decision
		lines(LOESS[,1],LOESS[,2],lty=2,col="blue")
		lines(SPLINE[,1],SPLINE[,2],col="blue",lty=3,lwd=2)
		
		# label for output
		colnames(LINEAR) <-colnames(Dots) <- colnames(LOESS) <- colnames(SPLINE) <- c("x","y")
		return(list("Dots"=Dots,"LOESS"=LOESS,"SPLINE"=SPLINE,"LINEAR"=LINEAR))
	}
	
	# begin work
	# base grid opens a device
	BaseGrid(xlim=xlim,ylim=ylim,log=log)
	
	# if guide functions are specified with compare, plot them in red
	if (missing(compare)==FALSE) {
		n <- ncol(compare)/2
		for (i in 1:n){
			lines(x=compare[,(i*2-1)],y=compare[,(i*2)],col="red",lty=2)
		}
	}
	
	# sketch function and show results of LOESS and SPLINE
	A <- AddDots(xlim=xlim,ylim=ylim,xnew=xnew,span=span,log=log)
	legend("topleft",legend=c("LOESS","SPLINE"),lty=c(2,3),col="blue",lwd=c(1,2),bg="white")
	
	# 'A' contains the dots clicked (Dots), and the LINEAR, LOESS and SPLINE interpolation / extrapolations
	return(A)
  }
}
\keyword{ iplot }

