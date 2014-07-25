RateSketch
==========

Hand sketch rate schedules on an interactive graphics device- interpolates schedule to exact points using different methods

Installation
============

To download the most recent version of LifeTable:

1. make sure you have the most recent version of R
2. look at the notes below if you're on Windows or Mac.

Download the [zip ball](https://github.com/timriffe/DecompHoriuchi/zipball/master) or [tar ball](https://github.com/timriffe/DecompHoriuchi/tarball/master), decompress and run `R CMD INSTALL` on it in the terminal command line, or use the **devtools** package to install the development version:

```r
# install.packages("devtools")

library(devtools)
install_github("DecompHoriuchi", subdir = "DecompHoriuchi", username = "timriffe")
```

**Note**: Windows users need [Rtools](http://cran.r-project.org/bin/windows/Rtools/) to install from github as shown above. Get the most recent version of [R for Windows](http://cran.r-project.org/bin/windows/base/) and download and install the version of Rtools that corresponds to it.

**Note**: Mac users might be required to install the appropriate version [XTools](https://developer.apple.com/xcode/) from the [Apple Developer site](https://developer.apple.com/) in order to install the development version.  You may need to [register as an Apple developer](https://developer.apple.com/programs/register/).  An older version of XTools may also be required.
