#install.packages("readxl")
library("readxl")

getwd()
setwd("/Users/christiandetsch/Desktop")
d<-as.data.frame(read_xlsx("Data_for_R.xlsx"))
d
str(d)

#install.packages("lubridate")
library("lubridate")

d$DATE<-as.Date(paste0(d$YEAR,"-",d$MONTH,"-",d$DAY))

dagg<-aggregate(d[,11]~d$DATE + d$GENUS, FUN=mean)
colnames(dagg) <- c("DATE", "GENUS", "BIOMASS")
dagg

#######################################

unique(dagg$GENUS)

daggAll<-data.frame(DATE=unique(d$DATE))
for (i in 1:length(unique(dagg$GENUS))){
  daggCalanus<-dagg[dagg$GENUS == unique(dagg$GENUS)[i] ,] #subset only Calanus rows
  dDate<-data.frame(DATE=unique(d$DATE))
  daggCalanus<-merge(dDate, daggCalanus, all=T)
  daggCalanus[is.na(daggCalanus)] = 0 #replace na with 0
  daggCalanus2<-daggCalanus
  colnames(daggCalanus2)[3] <- unique(dagg$GENUS)[i]
  daggCalanus2 <- daggCalanus2[,c(1,3)]
  daggAll<-merge(daggAll, daggCalanus2)
}
#View(daggAll)

###################################################
################## Temperature ####################
###################################################
temp<-as.data.frame(read.csv("/Users/christiandetsch/Desktop/MBN_Temperature_0_to_100m.csv"))
temp <- temp[,c(2,3)]
colnames(temp) <- c("DATE","TEMPERATURE")
temp$DATE <-as.Date(temp$DATE)
str(temp)

tempts <- data.frame(Temperature=temp$TEMPERATURE)
tempts$Date <- as.Date((temp$DATE), format = "%Y-%m-%d")

tempts$d <- difftime(as.Date(tempts$Date), as.Date(tempts$Date[1]), units="days")

tempts <- tempts[year(tempts$Date) < 2019, ]

require(pastecs)
regDatatempts<-regul(as.vector(tempts$d), tempts$Temperature, frequency=12, units="daystoyears",
                     datemin="05/10/2005",
                     dateformat ="d/m/Y", tol=13,
                     n=13*12+2, method="constant") 

plot(regDatatempts)
tempts0<-tseries(regDatatempts)
plot(tempts0)

#time series in 2 spalten
as.matrix(tempts0)
time(tempts0)
Tempts1 <- data.frame(value=as.matrix(tempts0), date=time(tempts0))
require(zoo)
Tempts1$date <- as.Date(Tempts1$date, format = "%Y-%m-%d")

#median der Monate
tempMedian <- aggregate(Tempts1$value, by=list(month(Tempts1$date)), FUN="median")
colnames(tempMedian) <- c("MONTH", "MEDIAN")

# Oberes und unteres Quartil
tempLowQuartiles <- aggregate(Tempts1$value, by=list(month(Tempts1$date)), FUN=function(x) quantile(x, probs=c(0.25)))
tempUppQuartiles <- aggregate(Tempts1$value, by=list(month(Tempts1$date)), FUN=function(x) quantile(x, probs=c(0.75)))

tempMedian2 <- data.frame(MONTH=tempMedian$MONTH, MEDIAN=tempMedian$MEDIAN, UPPER_QUAR=tempUppQuartiles$x, LOWER_QUAR=tempLowQuartiles$x)


TempMedian0 <- data.frame(
  month = c(tempMedian[10:12, 1], tempMedian[1:9, 1]),
  median = c(tempMedian[10:12, 2], tempMedian[1:9, 2]))

repTempMedian <- data.frame(
  month = c(rep(TempMedian0[1:12,1], times=13), TempMedian0[1:2, 1]),
  median = c(rep(TempMedian0[1:12,2], times=13), TempMedian0[1:2, 2]))

#deseasonalize
TempDeseasMedian <- data.frame(date=Tempts1$date, biomass=Tempts1$value, seasonal=repTempMedian$median,  deseas=Tempts1$value-repTempMedian$median)

# Definiere die Koordinaten für das Unsicherheitsband
x <- c(tempMedian$MONTH, rev(tempMedian$MONTH))
y <- c(tempQuartiles$LOWER_QUARTILE, rev(tempQuartiles$UPPER_QUARTILE))

# Speichere den Plot in einer PDF-Datei
output_path <- "/Users/christiandetsch/Desktop/MedianPlots/Seas/"
plot_name <- paste0(output_path, "Median_Temp.pdf")
pdf(paste0(plot_name))
par(mar = c(4, 4, 4, 4) + 0.1) 
# Zeichne den Median
plot(tempMedian$MEDIAN ~ tempMedian$MONTH, xlab ="", ylab="", type ="l", lwd=1.8, las=1, xaxt="n", cex.axis=1.4, ylim= c(0, 4))
axis(1, at=c(1:12), label=F)
mtext(month_names, 1, at=c(1:12), line=1, las=1, cex=1.4)
abline(v=c(1:12), col="lightgrey", lty="1343")
polygon(c(tempMedian2$MONTH, rev(tempMedian2$MONTH)), c(tempMedian2$UPPER_QUAR, rev(tempMedian2$LOWER_QUAR)), col=rgb(0,0,0,0.1), border=NA)
title("A", adj=0, line = 0.7, cex.main=2.5)
title(paste0("Water Temperature"), adj = 0.18, line = 0.7, font.main=3, cex.main=1.3)
dev.off()


#install.packages("zoo")
require("zoo")

TempRunningMean <- rollmean(TempDeseasMedian$biomass, k=12, align = "center", fill = NA)

#biomass plot
output_path <- "/Users/christiandetsch/Desktop/MedianPlots/New/"
plot_name <- paste0(output_path,"Temp_Plot.pdf")
pdf(paste0(plot_name))

par(mar = c(4, 4, 4, 4) + 0.1) 
# Plot lines and points
plot(TempDeseasMedian$biomass ~ TempDeseasMedian$date, type = "l", col = "white", lwd = 1.2, xlab = "", ylab = "", las=1, cex.axis=1.4)
abline(v = c(as.Date(paste0(2006:2019, "-01-01"))), col = "lightgrey", lty = "1343")
points(TempRunningMean ~ TempDeseasMedian$date, type = "l", col="red", lwd = 1.7)
points(TempDeseasMedian$biomass ~ TempDeseasMedian$date, type = "l", col = "black", lwd = 1.2)

# Hintergrundsegmente einfärben
rect(as.Date("2005-01-01"), -200, as.Date("2008-12-31"), 1600, col="#66664534", border=NA)
rect(as.Date("2009-01-01"), -200, as.Date("2015-12-31"), 1600, col="#CCCCCC22", border=NA)
rect(as.Date("2016-01-01"), -200, as.Date("2019-12-31"), 1600, col="#33333333", border=NA)

title("A", adj=0, line = 0.7, cex.main=2.5)
title(paste0("Water Temperature"),
      adj = 0.2,
      line = 0.7,
      font.main=3,
      cex.main=1.3
)
dev.off()

###################################################
################### Salinity ######################
###################################################
sal<-as.data.frame(read.csv("/Users/christiandetsch/Desktop/MBN_Salinity_0_to_100m.csv"))
sal <- sal[,c(2,3)]
colnames(sal) <- c("DATE", "SALINITY")
sal$DATE <-as.Date(sal$DATE)
str(sal)

salts <- data.frame(Salinity=sal$SALINITY)
salts$Date <- as.Date((sal$DATE), format = "%Y-%m-%d")

salts$d <- difftime(as.Date(salts$Date), as.Date(salts$Date[1]), units="days")

salts <- salts[year(salts$Date) < 2019, ]

require(pastecs)
regDatasalts<-regul(as.vector(salts$d), salts$Salinity, frequency=12, units="daystoyears",
                    datemin="05/10/2005",
                    dateformat ="d/m/Y", tol=13,
                    n=13*12+2, method="constant") 

plot(regDatasalts)
salts0<-tseries(regDatasalts)
plot(salts0)

#time series in 2 spalten
as.matrix(salts0)
time(salts0)
Salts1 <- data.frame(value=as.matrix(salts0), date=time(salts0))
Salts1$date <- as.Date(Salts1$date)

#median der Monate
salMedian<-aggregate(Salts1$value, by=list(month(Salts1$date)), FUN="median")
colnames(salMedian) <- c("MONTH", "MEDIAN")

# Oberes und unteres Quartil
salLowQuartiles <- aggregate(Salts1$value, by=list(month(Salts1$date)), FUN=function(x) quantile(x, probs=c(0.25)))
salUppQuartiles <- aggregate(Salts1$value, by=list(month(Salts1$date)), FUN=function(x) quantile(x, probs=c(0.75)))

salMedian2 <- data.frame(MONTH=salMedian$MONTH, MEDIAN=salMedian$MEDIAN, UPPER_QUAR=salUppQuartiles$x, LOWER_QUAR=salLowQuartiles$x)

SalMedian0 <- data.frame(
  month = c(salMedian[10:12, 1], salMedian[1:9, 1]),
  median = c(salMedian[10:12, 2], salMedian[1:9, 2]))

repSalMedian <- data.frame(
  month = c(rep(SalMedian0[1:12,1], times=13), SalMedian0[1:2, 1]),
  median = c(rep(SalMedian0[1:12,2], times=13), SalMedian0[1:2, 2]))

#deseasonalize
SalDeseasMedian <- data.frame(date=Salts1$date, biomass=Salts1$value, seasonal=repSalMedian$median,  deseas=Salts1$value-repSalMedian$median)

# Speichere den Plot in einer PDF-Datei
output_path <- "/Users/christiandetsch/Desktop/MedianPlots/Seas/"
plot_name <- paste0(output_path, "Median_Sal.pdf")
pdf(paste0(plot_name))
par(mar = c(4, 4, 4, 4) + 0.1) 
# Zeichne den Median
plot(salMedian$MEDIAN ~ salMedian$MONTH, xlab ="", ylab="", type ="l", lwd=1.8, las=1, xaxt="n", cex.axis=1.4, ylim= c(31.3, 33.5))
axis(1, at=c(1:12), label=F)
mtext(month_names, 1, at=c(1:12), line=1, las=1, cex=1.4)
abline(v=c(1:12), col="lightgrey", lty="1343")
polygon(c(salMedian2$MONTH, rev(salMedian2$MONTH)), c(salMedian2$UPPER_QUAR, rev(salMedian2$LOWER_QUAR)), col=rgb(0,0,0,0.1), border=NA)
title("B", adj=0, line = 0.7, cex.main=2.5)
title(paste0("Salinity"), adj = 0.18, line = 0.7, font.main=3, cex.main=1.3)
dev.off()

#install.packages("zoo")
require("zoo")

SalRunningMean <- rollmean(SalDeseasMedian$biomass, k=12, align = "center", fill = NA)

#biomass plot
output_path <- "/Users/christiandetsch/Desktop/MedianPlots/New/"
plot_name <- paste0(output_path, "Sal_Plot.pdf")
pdf(paste0(plot_name))

par(mar = c(4, 4, 4, 4) + 0.1) 
# Plot lines and points
plot(SalDeseasMedian$biomass ~ SalDeseasMedian$date, type = "l", col = "white", lwd = 1.2, xlab = "", ylab = "", las=1, cex.axis=1.4)
abline(v = c(as.Date(paste0(2006:2019, "-01-01"))), col = "lightgrey", lty = "1343")
points(SalRunningMean ~ SalDeseasMedian$date, type = "l", col = "red", lwd = 1.7)
points(SalDeseasMedian$biomass ~ SalDeseasMedian$date, type = "l", col = "black", lwd = 1.2)

# Hintergrundsegmente einfärben

rect(as.Date("2005-01-01"), -200, as.Date("2008-12-31"), 1600, col="#66664534", border=NA)
rect(as.Date("2009-01-01"), -200, as.Date("2015-12-31"), 1600, col="#CCCCCC22", border=NA)
rect(as.Date("2016-01-01"), -200, as.Date("2019-12-31"), 1600, col="#33333333", border=NA)

title("B", adj=0, line = 0.7, cex.main=2.5)
title(paste0("Salinity"),
      adj = 0.15,
      line = 0.7,
      font.main=3,
      cex.main=1.3
)

dev.off()


#####################################################
############ Chlorophyll ############################
#####################################################

getwd()
setwd("/Users/christiandetsch/Desktop")
meta<-as.data.frame(read.csv("MBN_2D_data_v2.csv"))
chl <- data.frame(DATE=meta$Date, CHLOROPHYLL=meta$Chl_int)
chl$DATE <- as.Date((chl$DATE), format = "%Y-%m-%d")
chl$d <- difftime(as.Date(chl$DATE), as.Date(chl$DATE[1]), units="days")

Chlts <- chl[year(chl$DATE) < 2019, ]

require(pastecs)
regChl<-regul(as.vector(Chlts$d), Chlts$CHLOROPHYLL, frequency=12, units="daystoyears",
              datemin="05/10/2005",
              dateformat ="d/m/Y", tol=13,
              n=13*12+2, method="constant") 


plot(regChl)
Chlts0<-tseries(regChl)

as.matrix(Chlts0)
time(Chlts0)
Chlts1 <- data.frame(value=as.matrix(Chlts0), date=time(Chlts0))
Chlts1$date <- as.Date(Chlts1$date)

ChlMedian<-aggregate(Chlts1$value, by=list(month(Chlts1$date)), FUN="median")
colnames(ChlMedian) <- c("MONTH", "MEDIAN")

# Oberes und unteres Quartil
chlLowQuartiles <- aggregate(Chlts1$value, by=list(month(Chlts1$date)), FUN=function(x) quantile(x, probs=c(0.25)))
chlUppQuartiles <- aggregate(Chlts1$value, by=list(month(Chlts1$date)), FUN=function(x) quantile(x, probs=c(0.75)))

chlMedian2 <- data.frame(MONTH=chlMedian$MONTH, MEDIAN=chlMedian$MEDIAN, UPPER_QUAR=chlUppQuartiles$x, LOWER_QUAR=chlLowQuartiles$x)


ChlMedian0 <- data.frame(
  month = c(chlMedian[10:12, 1], chlMedian[1:9, 1]),
  median = c(chlMedian[10:12, 2], chlMedian[1:9, 2]))

repChlMedian <- data.frame(
  month = c(rep(ChlMedian0[1:12,1], times=13), ChlMedian0[1:2, 1]),
  median = c(rep(ChlMedian0[1:12,2], times=13), ChlMedian0[1:2, 2]))

#deseasonalize
ChlDeseasMedian <- data.frame(date=Chlts1$date, biomass=Chlts1$value, seasonal=repChlMedian$median,  deseas=Chlts1$value-repChlMedian$median)

#install.packages("zoo")
require("zoo")
ChlRunningMean <- rollmean(ChlDeseasMedian$biomass, k=12, align = "center", fill = NA)

#plot median seasonality Chl
# Speichere den Plot in einer PDF-Datei
output_path <- "/Users/christiandetsch/Desktop/MedianPlots/Seas/"
plot_name <- paste0(output_path, "Median_Chl.pdf")
pdf(paste0(plot_name))
par(mar = c(4, 4, 4, 4) + 0.1) 
# Zeichne den Median
plot(chlMedian$MEDIAN ~ chlMedian$MONTH, xlab ="", ylab="", type ="l", lwd=1.8, las=1, xaxt="n", cex.axis=1.4, ylim= c(0, 3.6))
axis(1, at=c(1:12), label=F)
mtext(month_names, 1, at=c(1:12), line=1, las=1, cex=1.4)
abline(v=c(1:12), col="lightgrey", lty="1343")
polygon(c(chlMedian2$MONTH, rev(chlMedian2$MONTH)), c(chlMedian2$UPPER_QUAR, rev(chlMedian2$LOWER_QUAR)), col=rgb(0,0,0,0.1), border=NA)
title("C", adj=0, line = 0.7, cex.main=2.5)
title(paste0("Chlorophyll a"), adj = 0.18, line = 0.7, font.main=3, cex.main=1.3)
dev.off()

#biomass plot
output_path <- "/Users/christiandetsch/Desktop/MedianPlots/New/"
plot_name <- paste0(output_path, "Chl_Plot.pdf")
pdf(paste0(plot_name))

par(mar = c(4, 4, 4, 4) + 0.1) 
# Plot lines and points
plot(ChlDeseasMedian$biomass ~ ChlDeseasMedian$date, type = "l", col = "white", lwd = 1.2, xlab = "", ylab = "", las=1, cex.axis=1.4)
abline(v = c(as.Date(paste0(2006:2019, "-01-01"))), col = "lightgrey", lty = "1343")
points(ChlRunningMean ~ ChlDeseasMedian$date, type = "l", col = "red", lwd = 1.7)
points(ChlDeseasMedian$biomass ~ ChlDeseasMedian$date, type = "l", col = "black", lwd = 1.2)

rect(as.Date("2005-01-01"), -200, as.Date("2008-12-31"), 1600, col="#66664534", border=NA)
rect(as.Date("2009-01-01"), -200, as.Date("2015-12-31"), 1600, col="#CCCCCC22", border=NA)
rect(as.Date("2016-01-01"), -200, as.Date("2019-12-31"), 1600, col="#33333333", border=NA)


title("C", adj=0, line = 0.7, cex.main=2.5)
title(paste0("Chlorophyll a"),
      adj = 0.17,
      line = 0.7,
      font.main=3,
      cex.main=1.3
)
dev.off()

###################################################
################### (semi) Loop ###################
###################################################
daggAll$Total <- daggAll$Acartia+daggAll$Calanus+daggAll$Centropages+daggAll$Metridia+daggAll$Microcalanus+daggAll$Microsetella+daggAll$Oithona+daggAll$Oncaea+daggAll$Pareuchaeta+daggAll$Pseudocalanus+daggAll$Temora
listtaxa <- colnames(daggAll)[-1]
taxats<-list()

i=7

taxats[[i]] <- data.frame(value=daggAll[,i+1])
taxats[[i]]$Date <- as.Date(daggAll$DATE, format = "%Y-%m-%d")
taxats[[i]]$d <- difftime(as.Date(daggAll$DATE), as.Date(daggAll$DATE)[1], units="days")
taxats[[i]] <- taxats[[i]][year(taxats[[i]]$Date) < 2019,]
require(pastecs)
regts<-regul(as.vector(taxats[[i]]$d), taxats[[i]]$value, frequency=12, units="daystoyears",
             datemin="05/10/2005",
             dateformat ="d/m/Y", tol=13,
             n=13*12+2, method="constant") 
plot(regts)
regts0<-tseries(regts)

as.matrix(regts0)
time(regts0)
require("zoo")
regts1 <- data.frame(value=as.matrix(regts0), date=time(regts0))
regts1$date <- as.Date(regts1$date)

taxaMedian<-aggregate(regts1$value, by=list(month(regts1$date)), FUN="median")
colnames(taxaMedian) <- c("MONTH", "MEDIAN")

taxaMedian0 <- data.frame(
  month = c(taxaMedian[10:12, 1], taxaMedian[1:9, 1]),
  median = c(taxaMedian[10:12, 2], taxaMedian[1:9, 2]))

repTaxaMedian <- data.frame(
  month = c(rep(taxaMedian0[1:12,1], times=13), taxaMedian0[1:2, 1]),
  median = c(rep(taxaMedian0[1:12,2], times=13), taxaMedian0[1:2, 2]))

TaxaDeseasMedian <- data.frame(date=regts1$date, biomass=regts1$value, seasonal=repTaxaMedian$median,  deseas=regts1$value-repTaxaMedian$median, month=repTaxaMedian$month)
#
#seasonal Plot
month_names <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

output_path <- "/Users/christiandetsch/Desktop/MedianPlots/Seas/"
plot_name <- paste0(output_path, listtaxa[i], "_Median_Seasonality.pdf")
pdf(paste0(plot_name))

par(mar = c(4, 4, 4, 4) + 0.1) 

plot(taxaMedian$MEDIAN~taxaMedian$MONTH, col="white", xlab ="", ylab="", type ="l",  lwd=1.8,  las=1, xaxt="n", cex.axis=1.4)
abline(v = c(1:12),col = "lightgrey", lty = "1343")
points(taxaMedian$MEDIAN~taxaMedian$MONTH, col="black", type ="l",  lwd=1.8)
axis(1, at=c(1:12), label=F)
mtext(month_names, 1, at=c(1:12), las=1, line=1, cex=1.4)
title("F", adj=0, line = 0.7, cex.main=2.5)
#add chlorophyll
par(new = TRUE)
plot(ChlMedian$MEDIAN~ChlMedian$MONTH, type ="l", lwd = 1.8, col="forestgreen", xlab = "", ylab = "", axes = FALSE, cex.axis=1.2)
axis(4, col.axis = "forestgreen", las = 1, cex.axis=1.4)
title(paste0(listtaxa[i], " spp."),
      adj = 0.15,
      line = 0.7,
      font.main=3,
      cex.main=1.3
)

dev.off()

#output_path <- "/Users/christiandetsch/Desktop/"
#plot_name <- "Legend_Median_Seasoanlity.new.pdf"
#pdf(paste0(plot_name))
#plot(taxaMedian$MEDIAN ~ taxaMedian$MONTH, type = "l", col = "white", lwd = 1.2, xlab = "", ylab = "",axes=FALSE,bty="n")
#legend("center", c("Seasonal Copepod Biomass","Seasonal Chlorophyll a Biomass"), lwd=5, col=c("black","forestgreen"), cex=1.3)
#dev.off()

#Plot Absolute Biomass
output_path <- "/Users/christiandetsch/Desktop/MedianPlots/Poster/"
plot_name <- paste0(output_path, listtaxa[i], "_Median_Biomass_new.pdf")
pdf(paste0(plot_name))

par(mar = c(4, 4, 4, 4) + 0.1) 

plot(TaxaDeseasMedian$biomass ~ TaxaDeseasMedian$date, type = "l", col = "white", lwd = 1.2,  xlab = "", ylab = "", las=1, cex.axis=1.4)
abline(v = c(as.Date(paste0(2006:2019, "-01-01"))), col = "lightgrey", lty = "1343")
#points(TaxaDeseasMedian$seasonal ~ TaxaDeseasMedian$date, type = "l", col = "red", lwd = 1.5)
points(TaxaDeseasMedian$biomass ~ TaxaDeseasMedian$date, type = "l", col = "black", lwd = 1.5)
title("B", adj=0, line = 0.7, cex.main=2.5)
title(paste0(listtaxa[i]," spp."), adj = 0.15, line = 0.7, font.main=3, cex.main=1.3)
# Hintergrundsegmente einfärben

rect(as.Date("2005-01-01"), -200, as.Date("2008-12-31"), 1600, col="#66664534", border=NA)
rect(as.Date("2009-01-01"), -200, as.Date("2015-12-31"), 1600, col="#CCCCCC22", border=NA)
rect(as.Date("2016-01-01"), -200, as.Date("2019-12-31"), 1600, col="#33333333", border=NA)


dev.off()

########################################
########################################
########################################



i=11

taxats[[i]] <- data.frame(value=daggAll[,i+1])
taxats[[i]]$Date <- as.Date(daggAll$DATE, format = "%Y-%m-%d")
taxats[[i]]$d <- difftime(as.Date(daggAll$DATE), as.Date(daggAll$DATE)[1], units="days")
taxats[[i]] <- taxats[[i]][year(taxats[[i]]$Date) < 2019,]
regts<-regul(as.vector(taxats[[i]]$d), taxats[[i]]$value, frequency=12, units="daystoyears",
             datemin="05/10/2005",
             dateformat ="d/m/Y", tol=13,
             n=13*12+2, method="constant") 
plot(regts)
regts0<-tseries(regts)

as.matrix(regts0)
time(regts0)
regts1 <- data.frame(value=as.matrix(regts0), date=time(regts0))
regts1$date <- as.Date(regts1$date)

taxaMedian<-aggregate(regts1$value, by=list(month(regts1$date)), FUN="median")
colnames(taxaMedian) <- c("MONTH", "MEDIAN")
TotalMedian<-taxaMedian



taxaMedian0 <- data.frame(
  month = c(taxaMedian[10:12, 1], taxaMedian[1:9, 1]),
  median = c(taxaMedian[10:12, 2], taxaMedian[1:9, 2]))

repTaxaMedian <- data.frame(
  month = c(rep(taxaMedian0[1:12,1], times=13), taxaMedian0[1:2, 1]),
  median = c(rep(taxaMedian0[1:12,2], times=13), taxaMedian0[1:2, 2]))

TaxaDeseasMedian[[i]] <- data.frame(date=regts1$date, biomass=regts1$value, seasonal=repTaxaMedian$median,  deseas=regts1$value-repTaxaMedian$median, month=repTaxaMedian$month)

month_names <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

TaxaDeseasMedian$logbiomass <- log(1+TaxaDeseasMedian$biomass)
TaxaDeseasMedian$logmedian <- log(1+TaxaDeseasMedian$seasonal)

TaxaDeseasMedian <- TaxaDeseasMedian %>%
  mutate(year = lubridate::year(date))

filtered_data <- TaxaDeseasMedian[TaxaDeseasMedian$year >= 2006, ]

# Plotting
output_path <- "/Users/christiandetsch/Desktop/MedianPlots/Idea/"
plot_name <- paste0(output_path, listtaxa[i], "_Changing_Seasonality.pdf")
pdf(paste0(plot_name))

plot(filtered_data$logbiomass~filtered_data$month, col="white",type="l", xaxt="n", xlab="", ylab="")

# Add lines for each year's biomass in gray
colors <- gray.colors(length(unique(filtered_data$year)), start = 0.3, end = 0.7)
for (year in unique(filtered_data$year)) {
  lines(filtered_data$month[filtered_data$year == year], 
        filtered_data$logbiomass[filtered_data$year == year], 
        #col = colors[year - min(unique(filtered_data$year)) + 1]
        col="grey"
        , lwd = 2)
}
points(1:12, tapply(filtered_data$logmedian, filtered_data$month, median),type = "l", col = "blue", lwd = 2,
       xlab = "Monat", ylab = "Median Saisonalität", main = "Median Saisonalität und Biomass pro Jahr")
axis(1, at=c(1:12), label=F)
mtext(month_names, 1, at=c(1:12), las=1, line=1, cex=1.4)
title(paste0(listtaxa[i]))
dev.off()

AllMedian <- data.frame(MONTH=AcartiaMedian$MONTH, 
                        Acartia=AcartiaMedian$MEDIAN, 
                        Calanus=CalanusMedian$MEDIAN,
                        Centropages=CentropagesMedian$MEDIAN,
                        Metridia=MetridiaMedian$MEDIAN,
                        Microcalanus=MicrocalanusMedian$MEDIAN,
                        Microsetella=MicrosetellaMedian$MEDIAN,
                        Oithona=OithonaMedian$MEDIAN,
                        Oncaea=OncaeaMedian$MEDIAN,
                        Pareuchaeta=PareuchaetaMedian$MEDIAN,
                        Pseudocalanus=PseudocalanusMedian$MEDIAN
                    )

AllMedianTotal <- rowSums(AllMedian[, -1])
AllMedian_perc <- sweep(AllMedian[, -1], 1, AllMedianTotal, "/")


species_colors <- c("blue", "darkgreen", "purple", "green", "red", "orange", "cyan", "brown", "yellow", "magenta")

# Balkendiagramm erstellen
output_path <- "/Users/christiandetsch/Desktop/MedianPlots/Idea/"
plot_name <- paste0(output_path,"total_Median.pdf")
pdf(paste0(plot_name))
par(mar = c(4, 4, 4, 4) + 0.1) 
barplot(t(AllMedian_perc), 
        xlab = "",
        ylab = "",
        col = species_colors,
        axes = FALSE
        #,legend.text = TRUE
        )
axis(2, las = 1, cex.axis=1.2)
title(paste0("Total Biomass Composition"), adj = 0.15, line = 0.7, font.main=3, cex.main=1.3)

par(new=TRUE)

plot(TotalMedian$MEDIAN~TotalMedian$MONTH, type ="l", lwd = 3, col="black", xlab = "", ylab = "", axes = FALSE, cex.axis=1.2)
axis(4, col.axis = "black", las = 1, cex.axis=1.2)
dev.off()

       
