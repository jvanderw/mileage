## Copyright (c) 2012 Jess VanDerwalker
##
## mileage.R
##
## Parse US gov mileage data for 2012
##
## Permission is hereby granted, free of charge, to any person obtaining
## a copy of this software and associated documentation files (the
## "Software"), to deal in the Software without restriction, including
## without limitation the rights to use, copy, modify, merge, publish,
## distribute, sublicense, and/or sell copies of the Software, and to
## permit persons to whom the Software is furnished to do so, subject to
## the following conditions:
##
## The above copyright notice and this permission notice shall be
## included in all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
## EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
## MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
## NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
## LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
## OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
## WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

# Required packages
library(ggplot2)

# Function to pause between graphs
pause <- function() {
  cat("\n", "Hit enter to continue:", "\n")
  s <- scan(n=1)
}

# Read in the file
all <- read.csv(file='2012_FEGuide.csv', header=TRUE)

# Only interested in manual transmission cars
manualTrans <- subset(all, subset=(Trans=="M"))

# Boxplot of all the manual
par(mar=c(4,12,4,2))
bp.all <- boxplot(manualTrans$Comb.FE..Guide....Conventional.Fuel ~ manualTrans$Mfr.Name,
                  main="Fuel Econonmy for 2012 Manual Transmission Vehicles by Manufacturer (Combined)",
                  ylab="",
                  xlab="MPG",
                  las=2,
                  horizontal=TRUE)
bp.all <- grid()
pause()

# Pull out all the vehicles that we don't care about
# Nothing under a combined MPG of 20
desired <- subset(manualTrans, subset=(Comb.FE..Guide....Conventional.Fuel > 20))
# Remove the classes of vehicles we aren't intereseted in.
desired <- subset(desired, subset=(Carline.Class.Desc != "Compact Cars"))
## desired <- subset(desired,
##                   subset=(Carline.Class.Desc != "Special Purpose Vehicle, SUV 2WD"))
## desired <- subset(desired,
##                   subset=(Carline.Class.Desc != "Special Purpose Vehicle, SUV 4WD"))
desired <- subset(desired,
                  subset=(Carline.Class.Desc != "Small Pick-up Trucks 2WD"))
desired <- subset(desired,
                  subset=(Carline.Class.Desc != "Small Pick-up Trucks 4WD"))
desired <- subset(desired, subset=(Carline.Class.Desc != "Two Seaters"))
desired <- subset(desired, subset=(Carline.Class.Desc != "Minicompact Cars"))
desired <- subset(desired, subset=(Carline.Class.Desc != "Subcompact Cars"))
# Remove the makes of cars we would never buy
desired <- subset(desired, subset=(Mfr.Name != "General Motors"))
desired <- subset(desired, subset=(Mfr.Name != "Chrysler Group LLC"))

desired$Carline.Class.Desc <- factor(desired$Carline.Class.Desc)
desired$Mfr.Name <- factor(desired$Mfr.Name)
desired$Fuel.Type <- factor(desired$Fuel.Usage....Conventional.Fuel)

# Now a boxplot of the vehicles in the "desired" range.
bp.desired <- boxplot(desired$Comb.FE..Guide....Conventional.Fuel ~ desired$Mfr.Name,
                      main="Fuel Econonmy for 2012 Manual Transmission\nCars, Station Wagons, and Minivans by Manufacturer (Combined)",
                      ylab="",
                      xlab="MPG",
                      las=2,
                      horizontal=TRUE)
bp.desired <- grid()
pause()

# Fix up some column names that are giving trouble
desired$numCyl <- desired$X..Cyl
desired$combFE <- desired$Comb.FE..Guide....Conventional.Fuel

# Plot against displacement of engine
cylmpg.p <- ggplot(desired, aes(Eng.Displ, combFE))
cylmpg.p <- cylmpg.p + geom_point(aes(color=factor(Mfr.Name), size=2))
cylmpg.p <- cylmpg.p + labs(x="Engine Displacement (liters)", y="MPG (Combined)")
cylmpg.p <- cylmpg.p + opts(title="Fuel Econonmy for 2012 Manual Transmission\nCars, Station Wagons, and Minivans by Manufacturer (Combined)")

mfgmpg.p <- ggplot(desired, aes(Mfr.Name, combFE))
mfgmpg.p <- mfgmpg.p + geom_point(aes(shape=factor(numCyl)), size=4)
mfgmpg.p <- mfgmpg.p + labs(x="Manufacturer", y="MPG (Combined)")
mfgmpg.p <- mfgmpg.p + opts(title="Fuel Econonmy for 2012 Manual Transmission\nCars, Station Wagons, and Minivans by Manufacturer (Combined)")

typempg.p <- ggplot(desired, aes(Mfr.Name, combFE))
typempg.p <- typempg.p + geom_point(aes(shape=factor(Carline.Class.Desc)),
                                    size=4)
typempg.p <- typempg.p + labs(x="Manufacturer", y="MPG (Combined)")
typempg.p <- typempg.p + opts(axis.text.x=theme_text(angle=-90, hjust=0),
                              title="Fuel Econonmy for 2012 Manual Transmission\nCars, Station Wagons, and Minivans by Manufacturer (Combined)")

# Display by the model of car (Carline)
carline.p <- ggplot(desired, aes(Carline, combFE))
carline.p <- carline.p + geom_point(aes(shape=factor(Fuel.Type), color=factor(Mfr.Name)),
                                    size=6)
carline.p <- carline.p + labs(x="Model", y="MPG (Combined)")
carline.p <- carline.p + opts(axis.text.x=theme_text(angle=-90, hjust=0),
                              title="Fuel Econonmy for 2012 Manual Transmission\nCars, Station Wagons, and Minivans by Model (Combined)")
print(carline.p)
