## Wheat Shock Scenarios


setwd("~/Desktop/DustBowl")

#load 2012-2016 average production, trade, and reserves matrices from "MatricCreationWheat.R"
load("WheatP0.Rdata")
load("WheatE0.Rdata")
load("WheatR0.Rdata")

#load list of all countries in wheat trade matrix
Countries<-read.csv("WheatTradeCountries.csv")
Countries<-Countries[!is.na(Countries$iso3c),] #drop countries without country codes 

Scenarios<-Countries

#High Severity Shock
Scenarios$NAF_High<-0 #North Atlantic Free Trade Agreement Countries
Scenarios$FSU_High<-0 #Former Soviet Union
Scenarios$CPA_High<-0 #Centrally Panned Asia
Scenarios$EUR_High<-0 #Europe
Scenarios$MBBF_High<-0 #Multiple Breadbasket Failure

#Mid Severity Shock
Scenarios$NAF_Mid<-0 #North Atlantic Free Trade Agreement Countries
Scenarios$FSU_Mid<-0 #Former Soviet Union
Scenarios$CPA_Mid<-0 #Centrally Panned Asia
Scenarios$EUR_Mid<-0 #Europe
Scenarios$MBBF_Mid<-0 #Multiple Breadbasket Failure

#Low Severity Shock
Scenarios$NAF_Low<-0 #North Atlantic Free Trade Agreement Countries
Scenarios$FSU_Low<-0 #Former Soviet Union
Scenarios$CPA_Low<-0 #Centrally Panned Asia
Scenarios$EUR_Low<-0 #Europe
Scenarios$MBBF_Low<-0 #Multiple Breadbasket Failure


NAF<-c("USA", "CAN", "MEX")
FSU<-c("Armenia", "Azerbaijan", "Belarus", "Georgia", "Kazakhstan",
       "Kyrgyzstan", "Republic of Moldova", "Russian Federation", "Tajikistan", 
       "Turkmenistan", "Ukraine", "Uzbekistan")
CPA<-c("CHN", "PRK", "MNG", "SGP", "VNM")
EUR<-c("Albania", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czechia",
       "Estonia", "Hungary", "Latvia", "Lithuania", "North Macedonia", "Malta",
       "Norway", "Poland", "Romania", "Serbia", "Montenegro", 
       "Slovakia", "Slovenia", "Switzerland") 


Scenarios$NAF_High[Scenarios$iso3c %in% NAF ] <-0.3
Scenarios$NAF_Mid[Scenarios$iso3c %in% NAF ] <-0.2
Scenarios$NAF_Low[Scenarios$iso3c %in% NAF ] <-0.1

Scenarios$FSU_High[Scenarios$CountryName %in% FSU ] <-0.3
Scenarios$FSU_Mid[Scenarios$CountryName %in% FSU] <-0.2
Scenarios$FSU_Low[Scenarios$CountryName %in% FSU] <-0.1

Scenarios$CPA_High[Scenarios$iso3c %in% CPA] <-0.3
Scenarios$CPA_Mid[Scenarios$iso3c %in% CPA] <-0.2
Scenarios$CPA_Low[Scenarios$iso3c %in% CPA] <-0.1

Scenarios$EUR_High[Scenarios$CountryName %in% EUR] <-0.3
Scenarios$EUR_Mid[Scenarios$CountryName %in% EUR] <-0.2
Scenarios$EUR_Low[Scenarios$CountryName %in% EUR] <-0.1

Scenarios$MBBF_High[Scenarios$EUR_High >0]<-0.3
Scenarios$MBBF_High[Scenarios$FSU_High >0]<-0.3
Scenarios$MBBF_High[Scenarios$NAF_High >0]<-0.3

Scenarios$MBBF_Mid[Scenarios$EUR_Mid >0]<-0.2
Scenarios$MBBF_Mid[Scenarios$FSU_Mid >0]<-0.2
Scenarios$MBBF_Mid[Scenarios$NAF_Mid >0]<-0.2

Scenarios$MBBF_Low[Scenarios$EUR_Low >0]<-0.1
Scenarios$MBBF_Low[Scenarios$FSU_Low >0]<-0.1
Scenarios$MBBF_Low[Scenarios$NAF_Low >0]<-0.1


# Run Shock Simulations 

##########################################################

#NAF High
NAF_High<-(Scenarios$NAF_High)
names(NAF_High)<-Scenarios$iso3c
NAF_High<-list(NAF_High)

NAF_High_Results<-sim_mc_multi(NAF_High, P0, R0, E0, 
                               cfrac = 0.01, asym = T, 
                               kmax = 1000, amin = 1E-5)

NAF_High_Results<-data.frame(P0 = NAF_High_Results$P0,
                                R0 = NAF_High_Results$R0,
                                dR = NAF_High_Results$dR,
                                dC = NAF_High_Results$dC)


write.csv(NAF_High_Results, "NAF_High_Results.csv")

#NAF Mid
NAF_Mid<-(Scenarios$NAF_Mid)  
names(NAF_Mid)<-Scenarios$iso3c
NAF_Mid<-list(NAF_Mid)

NAF_Mid_Results<-sim_mc_multi(NAF_Mid, P0, R0, E0, 
                              cfrac = 0.01, asym = T, 
                              kmax = 1000, amin = 1E-5)

NAF_Mid_Results<-data.frame(P0 = NAF_Mid_Results$P0,
                             R0 = NAF_Mid_Results$R0,
                             dR = NAF_Mid_Results$dR,
                             dC = NAF_Mid_Results$dC)

write.csv(NAF_Mid_Results, "NAF_Mid_Results.csv")

#NAF Low
NAF_Low<-(Scenarios$NAF_Low) 
names(NAF_Low)<-Scenarios$iso3c
NAF_Low<-list(NAF_Low)

NAF_Low_Results<-sim_mc_multi(NAF_Low, P0, R0, E0, 
                               cfrac = 0.01, asym = T, 
                               kmax = 1000, amin = 1E-5)

NAF_Low_Results<-data.frame(P0 = NAF_Low_Results$P0,
                             R0 = NAF_Low_Results$R0,
                             dR = NAF_Low_Results$dR,
                             dC = NAF_Low_Results$dC)

write.csv(NAF_Low_Results, "NAF_Low_Results.csv")

##########################################################

#CPA High
CPA_High<-(Scenarios$CPA_High)
names(CPA_High)<-Scenarios$iso3c
CPA_High<-list(CPA_High)

CPA_High_Results<-sim_mc_multi(CPA_High, P0, R0, E0, 
                               cfrac = 0.01, asym = T, 
                               kmax = 1000, amin = 1E-5)

CPA_High_Results<-data.frame(P0 = CPA_High_Results$P0,
                             R0 = CPA_High_Results$R0,
                             dR = CPA_High_Results$dR,
                             dC = CPA_High_Results$dC)

write.csv(CPA_High_Results, "CPA_High_Results.csv")

#CPA Mid
CPA_Mid<-(Scenarios$CPA_Mid)  
names(CPA_Mid)<-Scenarios$iso3c
CPA_Mid<-list(CPA_Mid)

CPA_Mid_Results<-sim_mc_multi(CPA_Mid, P0, R0, E0, 
                              cfrac = 0.01, asym = T, 
                              kmax = 1000, amin = 1E-5)

CPA_Mid_Results<-data.frame(P0 = CPA_Mid_Results$P0,
                             R0 = CPA_Mid_Results$R0,
                             dR = CPA_Mid_Results$dR,
                             dC = CPA_Mid_Results$dC)

write.csv(CPA_Mid_Results, "CPA_Mid_Results.csv")

#CPA Low
CPA_Low<-(Scenarios$CPA_Low) 
names(CPA_Low)<-Scenarios$iso3c
CPA_Low<-list(CPA_Low)

CPA_Low_Results<-sim_mc_multi(CPA_Low, P0, R0, E0, 
                              cfrac = 0.01, asym = T, 
                              kmax = 1000, amin = 1E-5)

CPA_Low_Results<-data.frame(P0 = CPA_Low_Results$P0,
                             R0 = CPA_Low_Results$R0,
                             dR = CPA_Low_Results$dR,
                             dC = CPA_Low_Results$dC)

write.csv(CPA_Low_Results, "CPA_Low_Results.csv")

##########################################################

#FSU High
FSU_High<-(Scenarios$FSU_High)
names(FSU_High)<-Scenarios$iso3c
FSU_High<-list(FSU_High)

FSU_High_Results<-sim_mc_multi(FSU_High, P0, R0, E0, 
                               cfrac = 0.01, asym = T, 
                               kmax = 1000, amin = 1E-5)

FSU_High_Results<-data.frame(P0 = FSU_High_Results$P0,
                             R0 = FSU_High_Results$R0,
                             dR = FSU_High_Results$dR,
                             dC = FSU_High_Results$dC)

write.csv(FSU_High_Results, "FSU_High_Results.csv")

#FSU Mid
FSU_Mid<-(Scenarios$FSU_Mid)  
names(FSU_Mid)<-Scenarios$iso3c
FSU_Mid<-list(FSU_Mid)

FSU_Mid_Results<-sim_mc_multi(FSU_Mid, P0, R0, E0, 
                              cfrac = 0.01, asym = T, 
                              kmax = 1000, amin = 1E-5)

FSU_Mid_Results<-data.frame(P0 = FSU_Mid_Results$P0,
                            R0 = FSU_Mid_Results$R0,
                            dR = FSU_Mid_Results$dR,
                            dC = FSU_Mid_Results$dC)

write.csv(FSU_Mid_Results, "FSU_Mid_Results.csv")

# FSU Low
FSU_Low<-(Scenarios$FSU_Low) 
names(FSU_Low)<-Scenarios$iso3c
FSU_Low<-list(FSU_Low)

FSU_Low_Results<-sim_mc_multi(FSU_Low, P0, R0, E0, 
                              cfrac = 0.01, asym = T, 
                              kmax = 1000, amin = 1E-5)

FSU_Low_Results<-data.frame(P0 = FSU_Low_Results$P0,
                            R0 = FSU_Low_Results$R0,
                            dR = FSU_Low_Results$dR,
                            dC = FSU_Low_Results$dC)

write.csv(FSU_Low_Results, "FSU_Low_Results.csv")


##########################################################

#EUR High
EUR_High<-(Scenarios$EUR_High)
names(EUR_High)<-Scenarios$iso3c
EUR_High<-list(EUR_High)

EUR_High_Results<-sim_mc_multi(EUR_High, P0, R0, E0, 
                               cfrac = 0.01, asym = T, 
                               kmax = 1000, amin = 1E-5)

EUR_High_Results<-data.frame(P0 = EUR_High_Results$P0,
                             R0 = EUR_High_Results$R0,
                             dR = EUR_High_Results$dR,
                             dC = EUR_High_Results$dC)

write.csv(EUR_High_Results, "EUR_High_Results.csv")

#EUR Mid
EUR_Mid<-(Scenarios$EUR_Mid)  
names(EUR_Mid)<-Scenarios$iso3c
EUR_Mid<-list(EUR_Mid)

EUR_Mid_Results<-sim_mc_multi(EUR_Mid, P0, R0, E0, 
                              cfrac = 0.01, asym = T, 
                              kmax = 1000, amin = 1E-5)

EUR_Mid_Results<-data.frame(P0 = EUR_Mid_Results$P0,
                            R0 = EUR_Mid_Results$R0,
                            dR = EUR_Mid_Results$dR,
                            dC = EUR_Mid_Results$dC)

write.csv(EUR_Mid_Results, "EUR_Mid_Results.csv")

#EUR Low
EUR_Low<-(Scenarios$EUR_Low) 
names(EUR_Low)<-Scenarios$iso3c
EUR_Low<-list(EUR_Low)

EUR_Low_Results<-sim_mc_multi(EUR_Low, P0, R0, E0, 
                              cfrac = 0.01, asym = T, 
                              kmax = 1000, amin = 1E-5)

EUR_Low_Results<-data.frame(P0 = EUR_Low_Results$P0,
                            R0 = EUR_Low_Results$R0,
                            dR = EUR_Low_Results$dR,
                            dC = EUR_Low_Results$dC)

write.csv(EUR_Low_Results, "EUR_Low_Results.csv")

##########################################################

#MBBF High
MBBF_High<-(Scenarios$MBBF_High)
names(MBBF_High)<-Scenarios$iso3c
MBBF_High<-list(MBBF_High)

MBBF_High_Results<-sim_mc_multi(MBBF_High, P0, R0, E0, 
                               cfrac = 0.01, asym = T, 
                               kmax = 1000, amin = 1E-5)

MBBF_High_Results<-data.frame(P0 = MBBF_High_Results$P0,
                             R0 = MBBF_High_Results$R0,
                             dR = MBBF_High_Results$dR,
                             dC = MBBF_High_Results$dC)

write.csv(MBBF_High_Results, "MBBF_High_Results.csv")

#MBBF Mei
MBBF_Mid<-(Scenarios$MBBF_Mid)  
names(MBBF_Mid)<-Scenarios$iso3c
MBBF_Mid<-list(MBBF_Mid)

MBBF_Mid_Results<-sim_mc_multi(MBBF_Mid, P0, R0, E0, 
                              cfrac = 0.01, asym = T, 
                              kmax = 1000, amin = 1E-5)

MBBF_Mid_Results<-data.frame(P0 = MBBF_Mid_Results$P0,
                            R0 = MBBF_Mid_Results$R0,
                            dR = MBBF_Mid_Results$dR,
                            dC = MBBF_Mid_Results$dC)

write.csv(MBBF_Mid_Results, "MBBF_Mid_Results.csv")

#MBBF Low
MBBF_Low<-(Scenarios$MBBF_Low) 
names(MBBF_Low)<-Scenarios$iso3c
MBBF_Low<-list(MBBF_Low)

MBBF_Low_Results<-sim_mc_multi(MBBF_Low, P0, R0, E0, 
                              cfrac = 0.01, asym = T, 
                              kmax = 1000, amin = 1E-5)

MBBF_Low_Results<-data.frame(P0 = MBBF_Low_Results$P0,
                            R0 = MBBF_Low_Results$R0,
                            dR = MBBF_Low_Results$dR,
                            dC = MBBF_Low_Results$dC)

write.csv(MBBF_Low_Results, "MBBF_Low_Results.csv")


