dat = read.csv("enzymeFlux.csv",header=TRUE)
BGCdat = read.csv("rHighPark_DOC_TDN_Corrected.csv",header=TRUE)
#remove GIGANTIC LAP_15C
dat$LAP_15C[which(dat$LAP_15C>20000)]<-NA

Sdate = as.Date(strDates,"%m/%d/%y")
dat$Sdate = Sdate;


BGCSdate = as.Date(ISOdate(BGCdat$Year,BGCdat$Month, BGCdat$Day)) 
BGCdat$Sdate = BGCSdate

merge(BGCdat, dat, by = intersect(names(SAMPLEIDPLOT), names(y)),
      
      
      merge(BGCdat, dat, by = intersect(names(Sdate), names(SAMPLEIDPLOT)), all =TRUE)
            by.x = by, by.y = by, all = FALSE, all.x = all, all.y = all,
            sort = TRUE, suffixes = c(".x",".y"),
            incomparables = NULL, ...)


apple = merge(BGCdat,dat,by="Sdate","SAMPLEIDPLOT",all=T)
#merging failed ... SAMPLEIDPLOT from dat is not found ... even though it appears to be there. 
#it's possible that these values are not the same across both datasets


df3<-merge(df1,df2,by="a",all=T

intersect(names(Sdate), names(SAMPLEIDPLOT)), all =TRUE)
by.x = by, by.y = by, all = FALSE, all.x = all, all.y = all,
sort = TRUE, suffixes = c(".x",".y"),
incomparables = NULL, ...)



plot(dat$BG_15C[dat$LAP_15C<1000],dat$LAP_15C[dat$LAP_15C<1000])



plot(dat$BG_15C)

plot (dat$BG_15C[dat$site=='UMC'],dat$BG_15C[dat$site=='UMC'])
plot (dat$BG_15C[dat$site=='UMC'],dat$DOC[dat$site=='UMC'])
plot (dat$TDN[dat$site=='UMC'],dat$BG_15C[dat$site=='UMC'])
plot (dat$TDN[dat$site=='UMC'],dat$LAP_15C[dat$site=='UMC'])
plot (dat$MicrobialBiomassC[dat$site=='UMC'],dat$BG_15C[dat$site=='UMC'])
plot (dat$Tsoil_C[dat$site=='UMC'],dat$EFFLUX[dat$site=='UMC'])
plot (dat$Tsoil_C,dat$EFFLUX)

tapply(dat$LAP_15C,dat$site,mean, na.rm=TRUE)

tapply(BGCdat$ug.PO43..P.g.1.dry,BGCdat$Site,mean, na.rm=TRUE)

plot(BGCdat$Site,BGCdat$ug.PO43..P.g.1.dry)
plot(BGCdat$Site,BGCdat$NO3.)
plot(BGCdat$Site,BGCdat$NH4.)
plot(BGCdat$Site,BGCdat$DIN)
plot(BGCdat$Site,BGCdat$Biomass_C_final)

plot(dat$site,dat$EFFLUX)

a <- ggplot(dat, aes(x=Tsoil_C, y=EFFLUX))
a + geom_point()
a + geom_point(aes(colour = BG_15C))
a + geom_point(colour = "red", size = 6)
a + geom_point(colour="grey50", size = 4) + geom_point(aes(colour = pH)) + scale_colour_gradient(low = "yellow", high="red")
a + geom_point(aes(shape = factor(site)))
a + geom_point(aes(size = PCntcover, colour=pH))
a + geom_point(aes(size = pH)) + scale_area()
a + geom_point(aes(fill=pH, size=pH), colour="black", shape=21) + scale_size(range = c(1, 20))

p  <- ggplot(dat, aes(x=pH, y=EFFLUX))
p + geom_point(aes(fill=Tsoil_C, size=Tsoil_C), colour="black", shape=21) + scale_size(range = c(1, 20))


p + geom_point(aes(fill=PCntcover, size=PCntcover), colour="black", shape=21) + scale_size(range = c(1, 20))

p + geom_point(aes(fill=BG_15C, size=BG_15C), colour="black", shape=21) + scale_size(range = c(1, 20))
p + geom_point(aes(fill=CB_15C, size=CB_15C), colour="black", shape=21) + scale_size(range = c(1, 20))
p + geom_point(aes(fill=AG_15C, size=AG_15C), colour="black", shape=21) + scale_size(range = c(1, 20))
p + geom_point(aes(fill=XYL_15C, size=XYL_15C), colour="black", shape=21) + scale_size(range = c(1, 20))


p + geom_point(aes(fill=LAP_35C, size=LAP_35C), colour="black", shape=21) + scale_size(range = c(1, 20))
p + geom_point(aes(fill=NAG_15C, size=NAG_15C), colour="black", shape=21) + scale_size(range = c(1, 20))


b <- ggplot(dat, aes(x=BG_15C, y=EFFLUX))
b + geom_point(colour = "red", size = 6)
b + geom_point( size = 5) + geom_point(aes(colour = pH)) + scale_colour_gradient(low = "white", high="black")
b + geom_point(aes(shape = factor(site))) + geom_point(aes(size =2+pH))
               
c <- ggplot(dat, aes(x=pH, y=BG_15C))
c + geom_point(colour = "red", size = 4)        
c + geom_point(colour="grey50", size = 6) + geom_point(aes(colour = EFFLUX)) + scale_colour_gradient(low = "yellow", high="red")

c + geom_point(aes(fill=EFFLUX, size=EFFLUX), colour="black", shape=21)

+ scale_colour_gradient(low = "yellow", high="red")





hist(dat$BG_15C, breaks = "Sturges",
     freq = NULL, probability = !freq,
     include.lowest = TRUE, right = TRUE,
     density = NULL, angle = 45, col = NULL, border = NULL,
     main = paste("Histogram of" , xname),
     xlim = range(breaks), ylim = NULL,
     xlab = xname, ylab,
     axes = TRUE, plot = TRUE, labels = FALSE,
     nclass = NULL, warn.unused = TRUE)



reg1 = lm(EFLUX_bp ~ OL_bp)   #model syntax: y ~ x
reg1    		     # default return from lm
summary(reg1)			     # detailed summary of results
anova(reg1)			     # ANOVA table of results
plot(reg1)			     # diagnostic plots (4 panels)
plot(residuals(reg1))		# residuals by row
coef(reg1)			# parameter coefficients
vcov(reg1)			# parameter covariance matrix
plot(EFLUX_bp, OL_bp)
abline(reg1)			# adding regression line to the scatter plot




