#There are two ways of plotting that I like to use
#first is just simply plot, the other is ggplot

#assuming you have run 'input script.R' just as is you should have these dataframes in your Global Environment.

#From the Enzyme dataframe: 
plot(ENZdat$SITE, ENZdat$BG_15C)
plot(ENZdat$pH, ENZdat$BG_15C)


#From the Biogeochemistry dataframe:
plot(BGCdat$SITE, BGCdat$Biomass_C_final)
plot(BGCdat$SoilMoisture_pcent, BGCdat$Biomass_C_final)

#From the FLUX dataframe:
plot(FLUXdat$SITE,FLUXdat$EFFLUX)
plot(FLUXdat$Tsoil_C,FLUXdat$EFFLUX)

#From the PYROLYSIS dataframe:
plot(PYRdat$Beetle[PYRdat$SITE=="FEF"],PYRdat$Phenol[PYRdat$SITE=="FEF"])
plot(PYRdat$SITE,PYRdat$Phenol)

plot(PYRdat$Protein,PYRdat$Lignin)

PYRdat$Beetle = as.factor(substr(PYRdat$PLOTID, 7,7))

ENZdat$Beetle = as.factor(substr(ENZdat$PLOTID, 7,7))
plot(ENZdat$Beetle,ENZdat$BG_15C)
plot(ENZdat$Beetle,ENZdat$XYL_15C)


#ggplot
#INSTALL PACKAGE ggplot2 & Pull the library
#install.packages("ggplot2")
library(ggplot2)


#you can find more examples for using ggplot2 at the following locations
# http://ggplot2.org/ 

#first DEFINE the plot you want to see *NOTE instead of using ENZdat$pH as x you define the dataframe FIRST and then the x and y axes
a <- ggplot(ENZdat, aes(x=pH, y=BG_15C))

#create a simple plot
a + geom_point()

# color code by another variable within the dataframe
a + geom_point(aes(colour = LAP_15C)) + scale_colour_gradient(low = "yellow", high="red")
# bad data in LAP_15C

#make a plot with big red symbols
a + geom_point(colour = "red", size = 6)

#Show the site differences with different shapes and sizes
a + aes(shape = factor(SITE)) +
  geom_point(aes(colour = factor(SITE)), size = 4) +
  geom_point(colour="grey90", size = 1.5)

#show a color gradient of pH
a + geom_point(colour="grey50", size = 4) + geom_point(aes(colour = pH)) + scale_colour_gradient(low = "yellow", high="red")

#show the pH as a color and a size gradient
a + geom_point(aes(fill=pH, size=pH), colour="black", shape=21) + scale_size(range = c(1, 20))

