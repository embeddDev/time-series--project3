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

<<<<<<< Updated upstream
#OMX =  beta0 -ISL_GENGI*beta1 - oliuverd*beta2 + fiskverd*beta3 + epsilon
#Heakandi kronu gengi laekkar hlutabrefavisitoluna, thi staerstu felogin gera upp i erlendum gjaldeyri.
#Haekandi oliuverd hefur neikvaed ahrif a felog eins og eimskip og N1
#Fiskverd hefur ahrif a felog eins og Granda og Eimskip og allt hagkerfid
=======
#OMX =  beta0 -ISL_GENGI * beta1 + epsilon
#Heakandi kronu gengi laekkar hlutabrefavisitoluna, thvi staerstu felogin gera upp i erlendum gjaldeyri.
>>>>>>> Stashed changes
#haegt ad nalgast upplysingar t.d. hja gamma.is

# . Scandinavian electricity market(Nordpool) or the electrical market of your home country(for foreign
#                  students only, as there is no spot market in Iceland),

<<<<<<< Updated upstream
#NORDPOOL = beta0 - utihitastig*beta1 season*beta2 + epsilon
=======
#NORDPOOL = beta0 - Utihitastig*beta1 + epsilon
>>>>>>> Stashed changes

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

# Construct a structural equation for house prices. Theorize on the expected sign of the parameter
# estimates on each of the variables. 
#ANS: 

#Does it make sense to use all of the data? Theorize, 

#ANS: NO, some of the variable contain very similar information, and should not improve the model very much due to cross correlation.

#which data to use and explain your data selection.
# 
#ANS: 

# ----------------Housing economics  -----------------

#Task 1
#Construcct a structural equation for house prices

# ANS:
#       Y ~ Beta0 + Beta1*CCI + Beta2*LBH + Beta3*PPI - Beta4*NHF + Beta5*LI + Beta6*inflation + Beta7*indexedLoans

# where CCI is construction cost index
# where LBH is Loans from banks to households
# where PPI is purchasing power index
# where NHF is new housing finished
# where LI is Lease index

# CCI - hvad kostar ad byggja hus hlytur ad hafa ahrif a husnaedisverd
        #aetti ad hafa mest ahrif.
# LBH - thvi meira sem bankarnir lana til household ?vi fleiri vilja kaupa hus/ibudir
# PPI - meiri kaupmattur leydir af ser haekkandi verd ?ar sem folk getur eytt meira i fasteign
# NHF - tvi fleiri hus tvi meira frambod, ?ar af leidir -
# LI - aetti ad vera endogenous samband
# Ifflation -  ef verdbolga haekkar aetti feisteignaverd ad haekka
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
#Loans of Banks to households seem to share similar trend.       



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
#Central Bank Rates seem to share a similar trend.

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
#Purchasing power of wages and index loans, seem to share a similar trend

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

#-----Task 3----------
#Create a model for the house price index using all relevant variables.



HousePrices_resid = plot(residuals(HousePrice_mdl))



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
                     LoansOfBanksToHouseholds+
                     HousingFinished+
                     IndexLoans+
                     Loans90
                    )

summary(HousePrice_mdl)
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


BuildingSupplyStore = read.csv("BuildingSupplyStore.csv", header=TRUE, sep= ";",dec=",")
<<<<<<< Updated upstream
BuildingSupplyStore = within(BuildingSupplyStore,BuildingSupplyStore$Kalendar <- relevel(BuildingSupplyStore$Kalendar,ref="Normal"))
#ikea <- within(ikea, Kalender <- relevel(Kalender, ref = "Normal"))
=======
BuildingSupplyStore = within(BuildingSupplyStore,Kalendar <- relevel(Kalendar,ref="Normal"))
#ikea <- within(ikea, Kalender <- relevel(Kalendar, ref = "Normal"))
>>>>>>> Stashed changes
attach(BuildingSupplyStore)




#*************Task 2, preliminary analysis********** 

#Plot
plot(Sales ~Date, type="l",main="Sales",col=3,lwd=5)
lines(lowess(Sales, f=.1), col = 2,lwd=4)
# Plot yearly
weeks = c(1:52,1:52,1:52,1:26)
plot(Sales~weeks,pch=20,main="Yearly Sales")
lines(lowess(Sales~weeks, f=.15), col = 2,lwd=4)
#pairs plot
pairs(BuildingSupplyStore[2:8], upper.panel = panel.cor,lower.panel = panel.smooth, diag.panel = panel.hist)
pairs(BuildingSupplyStore[c(2,9:14)], upper.panel = panel.cor,lower.panel = panel.smooth, diag.panel = panel.hist)
pairs(BuildingSupplyStore[c(2,16:22)], upper.panel = panel.cor,lower.panel = panel.smooth, diag.panel = panel.hist)


# ACF and PACF
acf(Sales) # Alot of MA, nonstationary TS
acf(diff(Sales,1)) 
pacf(diff(Sales,1)) #AR(3)

Sales.ts = ts(Sales, f=52, start=2006 ,end = c(2009,26))
plot(stl(Sales.ts,s.window="periodic"))


#**********Task 3 - Preliminary sales model***************

adStock = BuildingSupplyStore[17:22]
# Modeling
mod.full<-lm(Sales
            ~
              ,data=BuildingSupplyStore)
summary(mod.full)

# Plot the fit,