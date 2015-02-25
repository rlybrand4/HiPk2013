#Calculate plot values to create comparable vectors;

pH_bp = tapply(dat$pH,dat[,c("SAMPLEIDPLOT")],mean, na.rm=TRUE)
Tsoil_bp = tapply(dat$Tsoil_C,dat[,c("SAMPLEIDPLOT")],mean, na.rm=TRUE)
EFFLUX_bp = tapply(dat$EFFLUX,dat[,c("SAMPLEIDPLOT")],mean, na.rm=TRUE)

#veg inputs
PcCover_bp = tapply(dat$PCntcover,dat[,c("SAMPLEIDPLOT")],mean, na.rm=TRUE) 
avDBH = tapply(dat$avgdbh,dat[,c("SAMPLEIDPLOT")],mean, na.rm=TRUE)  
avHT = tapply(dat$avgheight,dat[,c("SAMPLEIDPLOT")],mean, na.rm=TRUE) 
VegINDEX = avDBH*avHT*PcCover_bp;


#sapro inputs
OL_bp = tapply(dat$OL,dat[,c("SAMPLEIDPLOT")],mean, na.rm=TRUE) 
LitterD_bp = tapply(dat$Litter,dat[,c("SAMPLEIDPLOT")],mean, na.rm=TRUE)
OrgIn = OL_bp + LitterD_bp

#fire Metrics
AshD_bp = tapply(dat$Ashdepth,dat[,c("SAMPLEIDPLOT")],mean, na.rm=TRUE) 
Scorch_bp = tapply(dat$avgscorch,dat[,c("SAMPLEIDPLOT")],mean, na.rm=TRUE) 
FireINDEX = AshD_bp+Scorch_bp

#Plot Health Index
pred= VegINDEX/2000+OrgIn-FireINDEX/10+28;

#if Efflux represents the realised activity and the Enzyme activities give potential Microbial activity then the 
Efflux_deficit = PotEffl-EFFLUX_bp;

CB15_bp = tapply(dat$CB_15C,dat[,c("SAMPLEIDPLOT")],mean, na.rm=TRUE) 
BG15_bp = tapply(dat$BG_15C,dat[,c("SAMPLEIDPLOT")],mean, na.rm=TRUE) 
XYL15_bp = tapply(dat$XYL_15C,dat[,c("SAMPLEIDPLOT")],mean, na.rm=TRUE) 
AG15_bp = tapply(dat$AG_15C,dat[,c("SAMPLEIDPLOT")],mean, na.rm=TRUE) 
NAG15_bp = tapply(dat$NAG_15C,dat[,c("SAMPLEIDPLOT")],mean, na.rm=TRUE) 
PHOS15_bp = tapply(dat$PHOS_15C,dat[,c("SAMPLEIDPLOT")],mean, na.rm=TRUE) 
LAP15_bp = tapply(dat$LAP_15C,dat[,c("SAMPLEIDPLOT")],mean, na.rm=TRUE) 
#total activity;
act15 = CB15_bp+BG15_bp+XYL15_bp+AG15_bp+NAG15_bp+PHOS15_bp+LAP15_bp

MicBC_bp = tapply(BGCdat$Biomass_C_final,BGCdat[,c("SAMPLEIDPLOT")],mean, na.rm=TRUE) 

MicBN_bp = tapply(BGCdat$Biomass_N_final,BGCdat[,c("SAMPLEIDPLOT")],mean, na.rm=TRUE) 
Moisture_bp = tapply(BGCdat$Soil.moisture....,BGCdat[,c("SAMPLEIDPLOT")],mean, na.rm=TRUE) 



# Multiple Linear Regression Example 
fit <- lm(EFLUX_bp ~ OL_bp + CB15_bp + AG15_bp)
summary(fit) # show results

plot (AG15_bp,EFLUX_bp)
plot (act15,EFLUX_bp)

plot (FireINDEX,EFFLUX_bp)
plot (VegINDEXINDEX,EFFLUX_bp)




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
reg1      	     # default return from lm
summary(reg1)			     # detailed summary of results
anova(reg1)			     # ANOVA table of results
plot(reg1)			     # diagnostic plots (4 panels)
plot(residuals(reg1))		# residuals by row
coef(reg1)			# parameter coefficients
vcov(reg1)			# parameter covariance matrix
plot(EFLUX_bp, OL_bp)
abline(reg1)			# adding regression line to the scatter plot



