# R Code associated with the following publication:
# Title: Racial Disparities in Weather-Related Mortality in Virginia
#  Authors: Pamela B. DeGuzman, Wendy M. Novicoff, Gabriel Ramos,Melanie M. Pane, Murphy C. Johnson,Patrick C. Roney, Hannah V. Leigh, William Basener, Amber L. Curran, Bryan DeMarcy, Jungyun Jang, Christian Schroeder, Robert E. Davis
#
# Code written by Robert E. Davis, Patrick Roney, Melanie Pane, and Murphy Johnson
#

library(readxl)
library(dplyr)

# DLNM component
library(splines)
library(dlnm)
library(lubridate)

#Load Data
VA=VA_Final_Weather

VA <- read_excel("~/Desktop/EVSC_Research/VA.Final.Weather.xlsx",na="NA")
VA <- VA[1:5844, ]

#Convert Data to numeric

VA %>% mutate_if(is.character, as.numeric)

VA$MaxTC <- VA$`MaxT(C)`
VA$MinTC <- VA$`MinT(C)`
VA$MaxTDepC <- VA$`MaxTDep(C)`
VA$MinTDepC <- VA$`MinTDep(C)`
VA$ATC1am <- VA$`AT(C)1am`
VA$ATC7am <- VA$`AT(C)7am`
VA$ATC1pm <- VA$`AT(C)1pm`
VA$ATC7pm <- VA$`AT(C)7pm`
VA$DTRC <- VA$`DTR(C)`
VA$TC1am <- VA$`T(C)1am`
VA$TC7am <- VA$`T(C)7am`
VA$TC1pm <- VA$`T(C)1pm`
VA$TC7pm <- VA$`T(C)7pm`

for(i in 1:ncol(VA)){
  assign(names(VA)[i], VA[[i]])
}


#Create Trend Term

Trend = seq(1,5844,1)

#SPECIFY DEPENDENT AND INDEPENDENT VARIABLES

Mort <- White
MainSpline <- MinTC
#SecondSpline <- PM2.5REVISED
#ThirdSpline <- SLPhPa7pm
Factor1 <- dow
#Factor2 <- HeatWavesModerate
#Factor3 <- WinterWeather
#Factor4 <- ColdWavesModerate
#Factor5 <- holidays
#
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

#SecondSpline=SecondSpline,ThirdSpline=ThirdSpline,
#,Factor2=Factor2,Factor3=Factor3,Factor4=Factor4,Factor5=Factor5
# Create a data frame with lagged variables

lagframe=data.frame(Mort=Mort, Trend=Trend, MainSpline=as.numeric(MainSpline),Factor1=Factor1,datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)

#plot(time)
basis.MainSpline=crossbasis(lagframe$MainSpline,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

# Define the position and number of knots for spline functions
varknotsMainSpline=equalknots(lagframe$MainSpline,fun="bs",df=4,degree=2)
varknotsMainSpline
lagknotsMainSpline=logknots(21,2)
lagknotsMainSpline

# Create the basis matrix; set lag number ("lag=10 (days)"); cen = centering value
cb1.MainSpline=crossbasis(lagframe$MainSpline,lag=21,argvar=list(fun="bs",knots=varknotsMainSpline),arglag=list(knots=lagknotsMainSpline))
summary(cb1.MainSpline)

# Run the model

modelA1=glm(Mort~cb1.MainSpline+ns(Trend, 16*3)+as.factor(Factor1),family=quasipoisson(),lagframe)
summary(modelA1)
plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MainSpline=crosspred(cb1.MainSpline,modelA1,by=1)

# 3-D plot
plot(pred1.MainSpline,xlab="MainSpline",zlab="RR",theta=200,phi=40,lphi=30,main="VA Mort")


# Plot of lag effects at selected values ("var=??")
plot(pred1.MainSpline,ptype="slices",var=c(1),col='Total',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
plot(pred1.MainSpline,ptype="slices",var=c(25),col='Total',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MainSpline$allRRfit,pred1.MainSpline$allRRlow,pred1.MainSpline$allRRhigh)
plot(modelA1)

# Plot of "heat map"; save in "tiff" format
tiff("Davis.Race.Black.Heat.tiff", width = 4, height = 4, units = 'in', res = 1200)
plot(pred1.MainSpline,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Virginia Black (VA)",xlab="Minimum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

# Plot of the overall relationship across all lags
# This creates a tiff file, of some width x height in 1200 dpi (or whatever number you choose for the resolution)
tiff("Davis.Race.White.Curve.tiff", width = 4, height = 4, units = 'in', res = 1200)
plot(pred1.MainSpline,"overall",xlab="Minimum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Virginia White")
dev.off()

test <- pred1.MainSpline$allRRfit
MortDLNM <- pred1.MainSpline

Consensus_Curve_VA_Total
Consensus_Heat_VA_Total

#Image dimensions 602 x 482

#Heat map with more levels
noeff=1
levels=pretty(pred1.MainSpline$matRRfit,100)
col1 <- colorRampPalette(c("blue","Total"))
col2 <- colorRampPalette(c("Total","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
filled.contour(x=pred1.MainSpline$predvar,y=seq(0,21,1),z=pred1.MainSpline$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Minimum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Lynchburg (VA)"))
# 
# dev.off()
#  To adjust the heat map parameters from the default settings, please see:
# https://le-huynh.github.io/chva.extras/reference/plot_contour_dlnm.html


