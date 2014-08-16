RateSketch <-
function(xlim, ylim, xnew, log = FALSE, compare, span = .3){
	# a function to open a graphical device with the appropriate dimensions, axes and a reference grid:
	BaseGrid <- function(xlim = c(0, 10), ylim = c(0, 10), log = FALSE, xat, yat, scipen = 0){
		
		# convert log to either "" or "y"
		log <- ifelse(!log, "", log)
		log <- ifelse(log, "y", log)
		
		if (log == "y" & ylim[1] == 0){
			cat("\ncan't have 0 as a lower y axis limit. The function will assume you want to plot a \nmortality hazard and change the lower y limit to 0.00005\n")
			ylim[1] <- 0.00005
		}
		
		plot(runif(1), type = "n", xlim = xlim, ylim = ylim, log = log, axes = FALSE, xlab = "", ylab = "")
		segments(xlim[1], axTicks(2), xlim[2], axTicks(2), lty = 2, col = "grey")
		segments(axTicks(1), ylim[1], axTicks(1), ylim[2], lty = 2, col = "grey")
		options(scipen = scipen)
		axis(2, pos = xlim[1], at = axTicks(2), las = 2)
		axis(1, pos = ylim[1], at = axTicks(1))
		title(main = "RateSketch: click points (more = better). to exit, click outside grid")
	}
	
	# a function for clicking points and interpolating/extrapolating them
	AddDots <- function(xlim = xlim, ylim = ylim, xnew = xnew, span = span, log = log){
		# starting point
		Dot         <- locator(1)
		points(Dot$x, Dot$y, col = "blue")
		xcoords     <- Dot$x        
        ycoords     <- Dot$y
		# keep clicking. as long as dots are in region you get another click.
		# loop breaks once you click in the margin and the function continues
		while(Dot$x > xlim[1] & Dot$x < xlim[2] & Dot$y > ylim[1] & Dot$y < ylim[2]){
			Dot <- locator(1)
			if (Dot$x > xlim[1] & Dot$x < xlim[2] & Dot$y > ylim[1] & Dot$y < ylim[2]){
				points(Dot$x, Dot$y, col = "blue")
				xcoords     <- c(xcoords, Dot$x)
				ycoords     <- c(ycoords, Dot$y)
				lines(xcoords, ycoords, col = "blue")
				
			}
		}
		# if logged, then log for loess and spline
		if (log == "y"){
			ycoords <- log(ycoords)
		}
		# for interpolation/extrapolation, results are much better if done logged if the axes were logged
		Dots                <- cbind(xcoords, ycoords)
		if (is.element("Hmisc", installed.packages()[, 1])){
			LINEAR          <- Hmisc:::approxExtrap(x = Dots[, 1], y = Dots[, 2], xout = xnew)
		} else {
			LINEAR          <- approx(x = Dots[, 1], y = Dots[, 2], xout = xnew)
		}
		LINEAR              <- cbind(LINEAR$x, LINEAR$y)
		LOESS               <- cbind(xnew,predict(loess(ycoords~xcoords,span=span),newdata=xnew))
		SPLINE              <- spline(xcoords,ycoords,n=length(xnew),xmin=xnew[1],xmax=xnew[length(xnew)])
		SPLINE              <- cbind(SPLINE$x,SPLINE$y)
		
		# convert back to absolute scale if necessary
		if (log == "y"){
			LOESS[, 2] 	    <- exp(LOESS[, 2])
			SPLINE[, 2] 	<- exp(SPLINE[, 2])
			LINEAR[, 2] 	<- exp(LINEAR[, 2])
			Dots[, 2] 	    <- exp(Dots[, 2])
		}
		
		# plot the results of LOESS and SPLINE so user can make decision
		lines(LOESS[, 1], LOESS[, 2], lty = 2, col = "blue")
		lines(SPLINE[, 1], SPLINE[, 2], col = "blue", lty = 3, lwd = 2)
		
		# label for output
		colnames(LINEAR) <- colnames(Dots) <- colnames(LOESS) <- colnames(SPLINE) <- c("x", "y")
		return(list("Dots" = Dots, "LOESS" = LOESS, "SPLINE" = SPLINE, "LINEAR" = LINEAR))
	}
	
	# begin work
	# base grid opens a device
	BaseGrid(xlim = xlim, ylim = ylim, log = log)
	
	# if guide functions are specified with compare, plot them in red
	if (!missing(compare)) {
		n <- ncol(compare) / 2
		for (i in 1:n){
			lines(x = compare[, (i * 2 - 1)], y = compare[, (i * 2)], col = "red", lty = 2)
		}
	}
	
	# sketch function and show results of LOESS and SPLINE
	A <- AddDots(xlim = xlim, ylim = ylim, xnew = xnew, span = span, log = log)
	legend("topleft", legend = c("LOESS", "SPLINE"), lty = c(2, 3), col = "blue", lwd = c(1, 2), bg = "white")
	
	# 'A' contains the dots clicked (Dots), and the LINEAR, LOESS and SPLINE interpolation / extrapolations
	return(A)
}

