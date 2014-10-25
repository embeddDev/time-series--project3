#TASK 1 - project 3

############################################################################################
#course:  Time Series Analysis, T-862-TIMA
#Date:    //2014
#Students: Daniel Bergmann Sigtryggsson, Lilja Bjorg Gudmundsdottir, Jon Vilberg Georgsson 
#
############################################################################################


# -------Brain storming excersice (10%)-------
# Identify causal variables and hypothize an econometric structural equation for the following variables;
# . Stock price index, for your home country,


#OMX =  beta0 -ISL_GENGI*beta1 - oliuverd*beta2 + fiskverd*beta3 + epsilon
#Heakandi kronu gengi laekkar hlutabrefavisitoluna, thi staerstu felogin gera upp i erlendum gjaldeyri.
#Haekandi oliuverd hefur neikvaed ahrif a felog eins og eimskip og N1
#Fiskverd hefur ahrif a felog eins og Granda og Eimskip og allt hagkerfid

#haegt ad nalgast upplysingar t.d. hja gamma.is

# . Scandinavian electricity market(Nordpool) or the electrical market of your home country(for foreign
#                  students only, as there is no spot market in Iceland),

#NORDPOOL = beta0 - utihitastig*beta1 season*beta2 + epsilon

# haerra utihitastig laekkar eftirspurn eftir rafmagni, vedurupplysingar eru audvelt ad nalgast
#tad er meiri eftirspurn eftir rafmagni a veturnar, tannig ad seaon hefur ahrif.

# . Inflation.
#INFLATION = beta0 - styrivextir * beta1 + VSK*beta2 + penigamagn_i_umferd + epsilon
# haerri styrivextri og haekkandi VSK laekka verdbolgu, gogn til hja t.d. hagstofu
#peningamagn i umferd eykur verdbolgu

# -------Housing Economics (40%)--------

Data = read.csv("hpi.csv", header=TRUE, sep= ";",dec=",")
attach(Data)
#TASK 1

#Task 1
#Does it make sense to use all of the data? Theorize, 

#ANS: NO, some of the variable contain very similar information, and should not improve the model very much due to cross correlation.

#Construcct a structural equation for house prices

# ANS:
#       Y ~ Beta0 + Beta1*CCI + Beta2*LBH + Beta3*PPI - Beta4*NHF + Beta5*LI + Beta6*inflation + Beta7*indexedLoans

# where CCI is construction cost index
# where LBH is Loans from banks to households
# where PPI is purchasing power index
# where NHF is new housing finished
# where LI is Lease index

#which data to use and explain your data selection.
# 
#ANS: 

# CCI - hvad kostar ad byggja hus hlytur ad hafa ahrif a husnaedisverd
        #aetti ad hafa mest ahrif.
# LBH - thvi meira sem bankarnir lana til household ?vi fleiri vilja kaupa hus/ibudir
# PPI - meiri kaupmattur leydir af ser haekkandi verd ?ar sem folk getur eytt meira i fasteign
# NHF - tvi fleiri hus tvi meira frambod, ?ar af leidir -
# LI - aetti ad vera endogenous samband
# Inflation -  ef verdbolga haekkar aetti feisteignaverd ad haekka
# Indexed Loans - ef lanum fj0lgar i hagkerfinu verda meiri peningar i umferd sem eykur verdbolgu

# Breytur sem eru ekki i modeli:
# numbers of real estate sales - vid holdum ad tad haldist ekki i hendur, fjoldi lana
                                #hefur staerri ahrif
# Central bank rates -  er i engu hlutfalli vid husnaedislan
# New housing starts - vildum ekki nota baedi tad og NHF, teljum NHF hafa meiri ?hrif
# Purchasing power of wages index -  vildum ekki velja baedi tad og PPI,
                    #toldum PPI meira relevant. Viljum ekki overfitta.
# Unindexed loans - tad mikil fylgni vid indexed loans, thurfum ekki baedi
# Currency dependent loans in the ecconomy - ekki tad stor hluti af heidlarlanum og
                    #ekki endilegi notad til husnaediskaupa.

HousePrice_mdl =lm(HousePriceIndex ~LeaseIndex+
                     ConstructionCostIndex+
                     LoansOfBanksToHouseholds+
                     PurchasingPower+
                     Inflation+
                     HousingFinished+
                     IndexLoans
                   )
summary(HousePrice_mdl)
HousePrices_resid = plot(residuals(HousePrice_mdl), type= 'l')

# see that New housing finished variable does not make a good fit. We are not ready to dump this variable yet. 
# Because we think it brings information to the system.
pairs(Data[4:11], upper.panel = panel.cor,lower.panel = panel.smooth, diag.panel = panel.hist)
pairs(Data[c(4,12:17)], upper.panel = panel.cor,lower.panel = panel.smooth, diag.panel = panel.hist)
#We can see from this pair plot that house price index is higly correlated with housing finished!




#Plot the relationship between house prices and leasing over time. Theorize on the relationship 
# between the two variables. Is there an endogenous relationship between the two? 

plot((HousePriceIndex-mean(HousePriceIndex))/sd(HousePriceIndex) ~YearMonth,
     type="l",
     main="Comparison",
     xlab="Time",
     ylab="House Price Index/Lease Index",
     ylim=c(-2, 2),
     col="black") 
lines((LeaseIndex-mean(LeaseIndex))/sd(LeaseIndex) ~YearMonth,
      type="l", 
      main="Lease index",
      col="dark red")
legend("topleft",
       c("House Price index","Lease index"),
       lty = 1,
       col=c('black', 'darkred'),
       cex=0.6)

#THEORY : There is an apparent linear relationship, but we dont see the economic crash effect with the lease index .
# We see a endogenous relationship.

# Do some preliminary analysis, plotting all variables with the house price index.

pairs(Data[4:11], upper.panel = panel.cor,lower.panel = panel.smooth, diag.panel = panel.hist)
pairs(Data[c(4,12:17)], upper.panel = panel.cor,lower.panel = panel.smooth, diag.panel = panel.hist)


# Plot House Price index vs. Construction Cost, Real estate Transactions, Loans of Banks to Households
layout(1:1)
plot((HousePriceIndex-mean(HousePriceIndex))/sd(HousePriceIndex) ~YearMonth,
     xlab="Time",
     ylab="Scaled indices/rates",
     type="l", 
     main="Comparison",
     col="black",
     ylim=c(-2, 2.3),
     lwd=4) 
lines((ConstructionCostIndex-mean(ConstructionCostIndex))/sd(ConstructionCostIndex) ~YearMonth,
      type="l", 
      col="dark red")
lines((RealEstateTransactions-mean(RealEstateTransactions))/sd(RealEstateTransactions) ~YearMonth,
      type="l", 
      col="green",
      )
lines((LoansOfBanksToHouseholds-mean(LoansOfBanksToHouseholds))/sd(LoansOfBanksToHouseholds) ~YearMonth,
      type = 'l',
      col="purple"
      )
legend("topleft",
       c("House Price index","Construction Cost index","Real Estate Transactions", "Loans of Banks to Housholds"),
       lty = 1,
       col=c('black', 'darkred','green','purple'),
       cex=0.6,
       lwd=4)
#Loans of Banks to households seem to share similar trend, with a lag of 1 or 2 months.       



# Plot House Price index vs. UnindexedLoans, CentralBankRates, Inflation
layout(1:1)
plot((HousePriceIndex-mean(HousePriceIndex))/sd(HousePriceIndex) ~YearMonth,
     xlab="Time",
     ylab="Scaled indexes/rates",
     type="l", 
     main="Comparison",
     col="black",
     ylim=c(-1.6, 3.2),
     lwd=4) 
lines((Un.indexedLoans-mean(Un.indexedLoans))/sd(Un.indexedLoans) ~YearMonth,
      type="l", 
      col="dark red")
lines((CentralBankRates-mean(CentralBankRates))/sd(CentralBankRates) ~YearMonth,
      type="l", 
      col="green")
lines((Inflation-mean(Inflation))/sd(Inflation) ~YearMonth,
      type = 'l',
      col="purple"
)
legend("topleft",
       c("House Price index","Unindexed Loans","Central Bank Rates", "Inflation"),
       lty = 1,
       col=c('black', 'darkred','green','purple'),
       cex=0.6,
       lwd=4)
#Central Bank Rates seem to share a similar trend but is is a known fact that its a blunt economic tool.

# Plot House Price index vs. PurchasingPower, PurchasingPowerOfWages, IndexLoans
layout(1:1)
plot((HousePriceIndex-mean(HousePriceIndex))/sd(HousePriceIndex) ~YearMonth,
     xlab="Time",
     ylab="Scaled indexes/rates",
     type="l", 
     main="Comparison",
     col="black",
     ylim=c(-3, 2.5),
     lwd=4) 
lines((PurchasingPower-mean(PurchasingPower))/sd(PurchasingPower) ~YearMonth,
      type="l", 
      col="dark red")
lines((PurchasingPowerOfWages-mean(PurchasingPowerOfWages))/sd(PurchasingPowerOfWages) ~YearMonth,
      type="l", 
      col="green")
lines((IndexLoans-mean(IndexLoans))/sd(IndexLoans) ~YearMonth,
      type = 'l',
      col="purple"
)
legend("topleft",
       c("House Price index","Purchasing Power","Purchasing Power of Wages", "Index Loans"),
       lty = 1,
       col=c('black', 'darkred','green','purple'),
       cex=0.6,
       lwd=4)
# index loans, seem to share a similar trend. But not purchasing power as we initially thought!

# Plot House Price index vs. CurrencyLoans, HousingStarts, HousingFinished
layout(1:1)
plot((HousePriceIndex-mean(HousePriceIndex))/sd(HousePriceIndex) ~YearMonth,
     xlab="Time",
     ylab="Scaled indexes/rates",
     type="l", 
     main="Comparison",
     col="black",
     ylim=c(-1.7, 4.5),
     lwd=4) 
lines((CurrencyLoans-mean(CurrencyLoans))/sd(CurrencyLoans) ~YearMonth,
      type="l", 
      col="dark red")
lines((HousingStarts-mean(HousingStarts))/sd(HousingStarts) ~YearMonth,
      type="l", 
      col="green")
lines((HousingFinished-mean(HousingFinished))/sd(HousingFinished) ~YearMonth,
      type = 'l',
      col="purple"
)
legend("topleft",
       c("House Price index","Currency Loans","Housing Starts", "Housing Finished"),
       lty = 1,
       col=c('black', 'darkred','green','purple'),
       cex=0.6,
       lwd=4)
#Housing finished seem to share a similar trend.

#now a comparison of variables with similar trend
plot((HousePriceIndex-mean(HousePriceIndex))/sd(HousePriceIndex) ~YearMonth,
     xlab="Time",
     ylab="Scaled indexes/rates",
     type="l", 
     main="Comparison of variables with similar trend",
     col="black",
     ylim=c(-1.5, 2.5),
     lwd=4) 
lines((CentralBankRates-mean(CentralBankRates))/sd(CentralBankRates) ~YearMonth,
      type="l", 
      col="dark red")
lines((LoansOfBanksToHouseholds-mean(LoansOfBanksToHouseholds))/sd(LoansOfBanksToHouseholds) ~YearMonth,
      type="l", 
      col="green")
lines((HousingFinished-mean(HousingFinished))/sd(HousingFinished) ~YearMonth,
      type = 'l',
      col="purple"
)
lines((ConstructionCostIndex-mean(ConstructionCostIndex))/sd(ConstructionCostIndex) ~YearMonth,
      type = 'l',
      col="blue"
)
legend("topleft",
       c("House Price index","Central bank rates","loans to households", "New housing finished","Construnction cost index"),
       lty = 1,
       col=c('black', 'darkred','green','purple','Blue'),
       cex=0.6,
       lwd=4)
#Loans to households seems like the most obvious variable here with lag of 1.


#-----Task 3----------
#Create a model for the house price index using all relevant variables.







#New intervention model for 90% loans in the economy 
Loans90 = rep(0,times=54) #initialize zeros up to 1 july 2004
#1 juli 2004 - 1okt 2008
n = 55:106
hamming_window = 0.54 - 0.46*cos((2*pi*n/(55-1)))
Loans90 = c(Loans90, hamming_window, rep(0,times=length(Month)-106))
plot(Loans90 ~YearMonth, type='l', main="90% morgage model")

#NEw model with intervention model
HousePrice_mdl =lm(HousePriceIndex 
                   ~ConstructionCostIndex+
                     lag(HousingFinished,1)+
                     lag(LoansOfBanksToHouseholds,1)+
                     Loans90
                    )

summary(HousePrice_mdl)
HousePrices_resid = plot(residuals(HousePrice_mdl),
                         main= "residuals of the new model",
                         type = 'l',
                         lwd =3,
                         col=2
                         )
grid()
#PLOT HPI on fitted model
plot(HousePriceIndex ~YearMonth, type='l', col="blue",lwd=4)
lines(HousePrice_mdl$fit, type='l', col="red",lwd=4)
legend("topleft",
       c("House Price index", "model fit"),
       lty = 1,
       col=c('black', 'red'),
       cex=0.6,
       lwd=4)


#----------END OF Housing economics ------------------------

#----------   Sales modeling   ---------------------------------
detach(Data)

# ------ Task 1 -------

# Y ~ beta0 + beta1*Mean.temperature(Oslo) - beta2*Total.precipitation(Oslo) + beta3*Mean.temperature(Bergen) - beta4*Total.precipitation(Bergen)
#    beta5*Print + beta6*InStore + beta7*DirectMarketing + beta8*RADIO + beta9*TV:Taktisk + beta10*TV.Image - beta11*Competitor.spending

# We don't want to use all of the data, for example we think that the sun hours in Oslo are not a significant variable, we think that total
# precipitaion is way more effective since this is a Building supply store and people are not buying and working as much when it rains/snows, 
# at least not building houses (outside work). We belive that temperature also has an effect on sales. We think that all kind of marketing will 
# increase the sales and therefore include all marketing variables with a plus sign. The Competitor spending could also affect our sale, the more 
# the competitor spends the lower sales in our store, hence the minus sign in front of that variable.


BSS = read.csv("BSS.csv", header=TRUE, sep= ";",dec=",")
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

BSS$TV.Taktisk = as.numeric.factor(BSS$TV.Taktisk)
BSS$TV.Image = as.numeric.factor(BSS$TV.Image)
BSS$Sales = as.numeric.factor(BSS$Sales)
BSS$Sol.Oslo = as.numeric.factor(BSS$Sol.Oslo)
BSS$Oslo...Mean.temperature = as.numeric.factor(BSS$Oslo...Mean.temperature)
BSS$Bergen...Mean.temperature = as.numeric.factor(BSS$Bergen...Mean.temperature)
BSS$Bergen...Total.precipitation = as.numeric.factor(BSS$Bergen...Total.precipitation)
BSS$Oslo...Total.precipitation = as.numeric.factor(BSS$Oslo...Total.precipitation)
BSS$Competitor.spending = as.numeric.factor(BSS$Competitor.spending)
BSS$Tracking = as.numeric.factor(BSS$Tracking)
BSS$Tracking.smoothed = as.numeric.factor(BSS$Tracking.smoothed)
BSS$Respons.Media = as.numeric.factor(BSS$Respons.Media)
BSS$Unemployment.rate = as.numeric.factor(BSS$Unemployment.rate)
BSS$Season = as.numeric(levels(BSS$Season)[BSS$Season])
Sales.ts = ts(data=BSS$Sales, start=c(2006,1),end=c(2009,26),f=52)
#making normal the default level
BSS = within(BSS,Kalendar <- relevel(Kalendar,ref="Normal"))
#BSS$Sales = ts(data=BSS$Sales, start =c(2006,1),end=c(2009,26),f=52)
attach(BSS)

#*************Task 2, preliminary analysis********** 

#Plot

plot(Sales.ts, type="l",main="Sales",col=1,lwd=1)
lines(lowess(Sales.ts, f=.1), col = 2,lwd=4)
legend("topleft",
       c("Sales observation", "Scatter plot smooting"),
       lty = 1,
       col=c('black', 'red'),
       cex=0.6,
       lwd=4)
# Plot yearly
weeks = c(1:52,1:52,1:52,1:25)
plot(as.numeric(Sales) ~weeks,pch=20,main="Yearly Sales")
lines(lowess(BSS$Sales~weeks, f=.15), col = 2,lwd=4)
legend("topleft",
       c("weekly Sales observations", "Scatter plot smooting"),
       lty = 1,
       col=c('black', 'red'),
       cex=0.6,
       lwd=4)
#pairs plot
pairs(BSS[2:8], upper.panel = panel.cor,lower.panel = panel.smooth, diag.panel = panel.hist)

pairs(BSS[c(2,9:14)], upper.panel = panel.cor,lower.panel = panel.smooth, diag.panel = panel.hist)
#We notice that oslo mean temperature crosses zero and is marginally correlated

pairs(BSS[c(2,16:22)], upper.panel = panel.cor,lower.panel = panel.smooth, diag.panel = panel.hist)


# ACF and PACF
layout(1:3)
acf(Sales) # Alot of MA, nonstationary TS
acf(diff(Sales,1)) 
pacf(diff(Sales,1)) #AR(3), maybe AR(2)
layout(1:1)
plot(stl(Sales.ts,s.window="periodic")) # trend seasonal decomposition
#We see alot of seasonality and trend in sales
plot(aggregate(Sales.ts)) # removing seasonal effect by aggregating the data to the annual level
boxplot(Sales.ts ~ cycle(Sales.ts))
#**********Task 3 - Preliminary sales model***************


# Modeling
mod.full<-lm(Sales ~ Print+
                InStore+
               DirectMarketing+
               RADIO+
               TV.Taktisk+
               TV.Image+
               Unemployment.rate+
               Sol.Oslo+
               Oslo...Mean.temperature+
               Oslo...Total.precipitation+
               Bergen...Mean.temperature+
               Bergen...Total.precipitation+
               Competitor.spending+
               Kalendar+
               Season
                #na.action = na.omit
            )
summary(mod.full)
plot(residuals(mod.full),type = 'l')
medaltal = rep(mean(residuals(mod.full)),times=length(residuals(mod.full)))
lines(medaltal,col=2)
legend("topleft",
       c("residuals of the full model", "mean of the residuals"),
       lty = 1,
       col=c('black', 'red'),
       cex=0.6,
       lwd=4)
#Mean is zero, but the variance is not constant. Thus, the residuals is not white noise
plot(Sales,type='l')
lines(mod.full$fit, col=2)
legend("topleft",
       c("Sales data", "fitted siles data"),
       lty = 1,
       col=c('black', 'red'),
       cex=0.6,
       lwd=4)


adstock<-function(X,a){
  Y<-rep(NA, length(X))
  Y[1]<-X[1]  
  for (i in 2:length(X)){
    Y[i]<-X[i]+a*Y[i-1]
  }
  return(Y)
}
BSS$print.adstock<-adstock(BSS$Print,.75) 
BSS$InStore.adstock<-adstock(BSS$InStore,.75)
BSS$DirectMarketing.adstock<-adstock(BSS$DirectMarketing,.75) 
BSS$RADIO.adstock<-adstock(BSS$RADIO,.75)  
BSS$TV.Taktisk.adstock<-adstock(BSS$TV.Taktisk,.75) 
BSS$TV.Image.adstock<-adstock(BSS$TV.Image,.75)  
BSS$Media.adstock<-(BSS$print.adstock+BSS$InStore.adstock  
  +BSS$DirectMarketing.adstock+BSS$RADIO.adstock
  +BSS$TV.Taktisk.adstock+BSS$TV.Image.adstock)

plot(BSS$print.adstock,type="l",col=1)
lines(BSS$Print, col=2)
legend("topleft",
       c("Print.adstock", "Print"),
       lty = 1,
       col=c('black', 'red'),
       cex=0.6,
       lwd=4)

plot(BSS$InStore.adstock,type="l",col=1)
lines(BSS$InStore, col=2)
legend("topleft",
       c("InStore.adstock", "InStore"),
       lty = 1,
       col=c('black', 'red'),
       cex=0.6,
       lwd=4)

plot(BSS$DirectMarketing.adstock,type="l",col=1)
lines(BSS$DirectMarketing, col=2)
legend("topleft",
       c("DirectMarketing.adstock", "DirectMarketing"),
       lty = 1,
       col=c('black', 'red'),
       cex=0.6,
       lwd=4)

plot(BSS$RADIO.adstock,type="l",col=1)
lines(BSS$RADIO, col=2)
legend("topleft",
       c("RADIO.adstock", "RADIO"),
       lty = 1,
       col=c('black', 'red'),
       cex=0.6,
       lwd=4)

plot(BSS$TV.Taktisk.adstock,type="l",col=1)
lines(BSS$TV.Taktisk, col=2)
legend("topleft",
       c("TV.Taktisk.adstock", "TV.Taktisk"),
       lty = 1,
       col=c('black', 'red'),
       cex=0.6,
       lwd=4)

plot(BSS$TV.Image.adstock,type="l",col=1)
lines(BSS$TV.Image, col=2)
legend("topleft",
       c("TV.Image.adstock","TV.image"),
       lty = 1,
       col=c('black', 'red'),
       cex=0.6,
       lwd=4)

plot(BSS$Media.adstock,type="l",col=1)
lines(BSS$Respons.Media, col=2)
legend("topleft",
       c("Media.adstock","Respons.Media"),
       lty = 1,
       col=c('black', 'red'),
       cex=0.6,
       lwd=4)

detach(BSS)
attach(BSS) 


#TASK 4
# Modeling
mod.full<-lm(Sales ~ Print+
               InStore+
               DirectMarketing+
               RADIO+
               TV.Taktisk+
               TV.Image+
               Unemployment.rate+
               Sol.Oslo+
               Oslo...Mean.temperature+
               Oslo...Total.precipitation+
               Bergen...Mean.temperature+
               Bergen...Total.precipitation+
               Competitor.spending+
               Kalendar+
               Season
             #na.action = na.omit
)
summary(mod.full)
plot(residuals(mod.full),type = 'l')
medaltal = rep(mean(residuals(mod.full)),times=length(residuals(mod.full)))
lines(medaltal,col=2)
legend("topleft",
       c("residuals of the full model", "mean of the residuals"),
       lty = 1,
       col=c('black', 'red'),
       cex=0.6,
       lwd=4)


# Plot the fit


#attached <- search()
#attached[!grepl("package", attached)]

#Task4
Seas = cycle(Sales.ts)
