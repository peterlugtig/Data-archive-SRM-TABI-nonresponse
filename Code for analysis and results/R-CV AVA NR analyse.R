# Script om R/CV indicatoren uit te rekenen voor AVA NR analyse
# Versie: 21-01-2021
# Peter Lugtig & Barry Schouten

# Werkmap en achtergrondfuncties
setwd("W:/Werk/Tabi Verplaatsingen App/Data/Katie/Extra analyses artikel")
library(foreign)
install.packages("gplots")
library(gplots)
library(ggplot2)
source("RISQ_R-indicators_v2.1.r")
source("Plot_R-indicators.R")

# Inlezen data
AVA = read.spss(
"//cbsp.nl/productie/projecten/BPM/301707WaarnVnw_SEC1/Werk/Tabi Verplaatsingen App/Data/Aggregated Dataset/AVA-EVA Analysis data set - complete.sav")
AVA = as.data.frame(AVA)

# Prepareren variabelen voor R/CV model
#  Grootte huishouden
GrootteHH = function(AantalPersHH){
  if (is.na(AantalPersHH)){
    uit = "7. Unknown"
  } else {
    if (AantalPersHH>5) { 
      uit = "6. >5"
    } else { uit = paste(AantalPersHH,". ",AantalPersHH,sep="") }
  }
  
  return(uit)
}

# Bezit personenauto in huishouden
AutoHH = function(AantalAutos){
  if (is.na(AantalAutos)) {
    uit = "0. 0"
  } else { 
    if (AantalAutos<3) {
      uit = paste(AantalAutos,". ",AantalAutos,sep="") 
    } else { uit = "3. 3 or more"  }
  }
  
  uit
}

# Bezit brom- of snorfiets in huishouden
Bromfiets = function(Bromfiets){
  if (is.na(Bromfiets)) {
    uit = "0. No"
  } else { 
    if (Bromfiets=="Nee") {
      uit = "0. No"
    } else {uit = "1. Yes"}
  }
  
  uit
}

# Rijbewijsbezit steekproefpersoon
RBW = function(Rijbewijs){
  if (is.na(Rijbewijs)) {
    uit = "0. No"
  } else { 
    if (Rijbewijs == "Nee")
      {uit = "0. No"}
    else {uit = "1. Yes"}
    }
    
  uit
}

LftKlas15 = function(Leeftijd){ 
  Lft15 = " Unknown"
  if (!(is.na(Leeftijd))) {
    if (Leeftijd<4)  Lft15 = " <4"
    if ((Leeftijd>=4) & (Leeftijd<12)) Lft15 = "04-11"
    if ((Leeftijd>=12) & (Leeftijd<18)) Lft15 = "12-17"
    if ((Leeftijd>=18) & (Leeftijd<25)) Lft15 = "18-24"
    if ((Leeftijd>=25) & (Leeftijd<30)) Lft15 = "25-29"
    if ((Leeftijd>=30) & (Leeftijd<35)) Lft15 = "30-34"
    if ((Leeftijd>=35) & (Leeftijd<40)) Lft15 = "35-39"
    if ((Leeftijd>=40) & (Leeftijd<45)) Lft15 = "40-44"
    if ((Leeftijd>=45) & (Leeftijd<50)) Lft15 = "45-49"
    if ((Leeftijd>=50) & (Leeftijd<55)) Lft15 = "50-54"
    if ((Leeftijd>=55) & (Leeftijd<60)) Lft15 = "55-59"
    if ((Leeftijd>=60) & (Leeftijd<65)) Lft15 = "60-64"
    if ((Leeftijd>=65) & (Leeftijd<70)) Lft15 = "65-69"
    if ((Leeftijd>=70) & (Leeftijd<75)) Lft15 = "70-74"
    if (Leeftijd>=75)  Lft15 = " >=75"
  }
  
  Lft15
}

HerkomstGeneratie = function(Herk,Gen){
  herkgen = "Unknown"
  if ((!is.na(Herk)) & (!is.na(Gen))) {
    if (Herk==1) herkgen = "Native" 
    if ((Herk==3) & (Gen=="eerste generatie allochtoon")) herkgen = "1st generation non-western"    # 1e generatie niet-Westers
    if ((Herk==3) & (Gen=="tweede generatie allochtoon")) herkgen = "2nd generation non-western"    # 2e generatie niet-westers
    if ((Herk==2) & (Gen=="eerste generatie allochtoon")) herkgen = "1st generation western"                                                                    # 1e generatie westers
    if ((Herk==2) & (Gen=="tweede generatie allochtoon")) herkgen = "2nd generation western"                                                                    # 2e generatie westers
  }
  
  herkgen
}

Herkomst = function(herkomst){
  herkomstr = 0
  if (!is.na(herkomst)){
    if (herkomst=="Autochtonen") herkomstr = 1                                                                                               # Autochtoon
    if (herkomst=="Marokko") herkomstr = 3
    if (herkomst=="Turkije") herkomstr = 3 
    if (herkomst=="Suriname") herkomstr = 2 
    if (herkomst=="Nederlandse Antillen en Aruba") herkomstr = 2
    if (herkomst=="Overige niet-westerse allochtonen") herkomstr = 3
    if (herkomst=="Overige westerse allochtonen") herkomstr = 2
    if (herkomst=="Onbekend") herkomstr = 0
  }
  
  herkomstr
}

InkKlas6 = function(Inkomen){
  if (is.na(Inkomen)) Ink6="0. No registered income"
  else {
    if (Inkomen<=20) Ink6 = "1. 0-20% perc"
    if ((Inkomen>20) & (Inkomen<=40)) Ink6 = "2. 20-40% perc"
    if ((Inkomen>40) & (Inkomen<=60)) Ink6 = "3. 40-60% perc"
    if ((Inkomen>60) & (Inkomen<=80)) Ink6 = "4. 60-80% perc"
    if (Inkomen>80) Ink6 = "5. 80-100% perc"
  }
  
  Ink6
}

OplNiveau = function(oplniv) {
  if (is.na(oplniv)) { opl = "6. Unknown" }
  else {
    if (oplniv=="Basisonderwijs") { opl = "1. Primary" }
    if (oplniv=="Vmbo, avo onderbouw, mbo 1") { opl = "2. Pre-vocational"}
    if (oplniv=="Havo, vwo, mbo") { opl = "3. Secondary-vocational" }
    if (oplniv=="Hbo-, wo-bachelor") { opl = "4. Bachelor" }
    if (oplniv=="Hbo-, wo-master, doctor") { opl = "5. Master" }
  }
  
  opl
}


# Verwerken van de variabelen
AVA$SizeHH = as.factor(apply(array(AVA$VRLAANTALPERSHH),1,GrootteHH))
AVA$CarHH = as.factor(apply(array(as.numeric(AVA$AANTAUTOP)),1,AutoHH))
AVA$Moped = as.factor(apply(array(AVA$BROMMEROP),1,Bromfiets))
AVA$License = as.factor(apply(array(AVA$RIJBEWIJSP),1,RBW))
AVA$InkPerc = as.numeric(AVA$INHP100HGEST)
AVA$LFT = as.numeric(AVA$LFT)
AVA$Herkomst = apply(array(AVA$ETNGROEP3),1,Herkomst)
AVA$Urbanity = as.factor(AVA$STEDGEM)
AVA$AgeClasses = as.factor(apply(array(AVA$LFT),1,LftKlas15))
AVA$Ethnicity = "Onbekend"
for (i in 1:length(AVA$Herkomst))
{
  AVA$Ethnicity[i] = HerkomstGeneratie(AVA$Herkomst[i],AVA$VRLGBAGENERATIE[i])
}
AVA$Ethnicity = as.factor(AVA$Ethnicity)
AVA$IncomeClasses = as.factor(apply(array(AVA$InkPerc),1,InkKlas6))
AVA$EduLevel = as.factor(apply(array(AVA$OPLNIVHB),1,OplNiveau))

# Personen met lege registervariabelen
compleet = ((!is.na(AVA$VRLGBAGENERATIE)) & (!is.na(AVA$LFT)) & (!is.na(AVA$Urbanity)))
cat(nrow(AVA)-sum(compleet),"eenheden zijn verwijderd vanwege incomplete registergegevens\n")
AVA = AVA[compleet,]

#R/CV modellen
model1 = formula(AVAregist ~ Ethnicity + AgeClasses + IncomeClasses + Urbanity + CarHH + Moped + License)
model2 = formula(AVAactive ~ Ethnicity + AgeClasses + IncomeClasses + Urbanity + CarHH + Moped + License)
model3 = formula(AVAcomplete ~ Ethnicity + AgeClasses + IncomeClasses + Urbanity + CarHH + Moped + License)

# R/CV berekening
weights = rep(14*10^6/nrow(AVA),nrow(AVA))
indreg = getRIndicator(model1,AVA,sampleWeights = weights)
indact = getRIndicator(model2,AVA,sampleWeights = weights)
indcom = getRIndicator(model3,AVA,sampleWeights = weights)

# Visualisatie R/CV

#RR-plot
RRplot(c(indreg$propMean,indact$propMean,indcom$propMean),
       c(indreg$R,indact$R,indcom$R),
       c(indreg$RSE,indact$RSE,indcom$RSE))
ggsave("figure 3 - RRplot.pdf")

# Partial CV at var-level
PlotPartialCVVar(indreg,mode="Registration",adj=TRUE,SEu=0.029,SEc=0.029)
ggsave("figure 4 - Rind registration.pdf")
PlotPartialCVVar(indact,mode="Active",adj=TRUE,SEu=0.033,SEc=0.033)
ggsave("figure 4 - Rind activation.pdf")
PlotPartialCVVar(indcom,mode="Complete diary",adj=TRUE,SEu=0.036,SEc=0.036)
ggsave("figure 4 - Rind completion.pdf")

# Partial CV at cayt-level
PlotPartialCVCat(indreg,label="AgeClasses",view=0.4)
PlotPartialCVCat(indact,label="AgeClasses",view=0.4)
PlotPartialCVCat(indcom,label="AgeClasses",view=0.4)

PlotPartialCVCat(indreg,label="IncomeClasses",view=0.4)
PlotPartialCVCat(indact,label="IncomeClasses",view=0.4)
PlotPartialCVCat(indcom,label="IncomeClasses",view=0.4)

PlotPartialCVCat(indreg,label="License",view=0.4)
PlotPartialCVCat(indact,label="License",view=0.4)
PlotPartialCVCat(indcom,label="License",view=0.4)




#########################################################################

# R indicators OvIN
statdata <- read.csv2("//cbsp.nl/productie/projecten/BPM/301707WaarnVnw_SEC1/Werk/Tabi Verplaatsingen App/Data/OVIn 2016/OVIN1601CAWI220316V3.csv")
stpdata <- read.csv2("//cbsp.nl/productie/projecten/BPM/301707WaarnVnw_SEC1/Werk/Tabi Verplaatsingen App/Data/OVIn 2016/OVIN1602CAWI210316V3.csv")

### process the data

VerwerkXspec = function(stpdata,statdata){
  
  #  Grootte huishouden
  GrootteHH = function(AantalPersHH){
    if (is.na(AantalPersHH)){
      uit = "7. Onbekend"
    } else {
      if (AantalPersHH>5) { 
        uit = "6. >5"
      } else { uit = paste(AantalPersHH,". ",AantalPersHH,sep="") }
    }
    
    return(uit)
  }
  
  # Bezit personenauto in huishouden
  AutoHH = function(AantalAutos){
    if (is.na(AantalAutos)) {
      uit = "0. 0"
    } else { 
      if (AantalAutos<3) {
        uit = paste(AantalAutos,". ",AantalAutos,sep="") 
      } else { uit = "3. 3 of meer"  }
    }
    
    uit
  }
  
  # Bezit brom- of snorfiets in huishouden
  Bromfiets = function(Bromfiets){
    if (is.na(Bromfiets)) {
      uit = "0. Nee"
    } else { 
      if (Bromfiets==0) {
        uit = "0. Nee"
      } else {uit = "1. Ja"}
    }
    
    uit
  }
  
  # Rijbewijsbezit steekproefpersoon
  RBW = function(Rijbewijs){
    if (is.na(Rijbewijs)) {
      uit = "0. Nee"
    } else { uit = "1. Ja"}
    
    uit
  }
  
  # Verwerken van de variabelen
  stpdata$SizeHH = as.factor(apply(array(stpdata$AantalPersHH),1,GrootteHH))
  stpdata$CarHH = as.factor(apply(array(as.numeric(stpdata$AantalAutoHH)),1,AutoHH))
  stpdata$Moped = as.factor(apply(array(as.numeric(stpdata$Bromfiets)),1,Bromfiets))
  stpdata$License = as.factor(apply(array(as.numeric(stpdata$Rijbewijs)),1,RBW))
  
  statdata$RINPersoon = as.numeric(statdata$RINPersoon)
  stpdata$RINPersoon = as.numeric(stpdata$RINPersoon)
  sleutel = match(statdata$RINPersoon,stpdata$RINPersoon)
  statdata$SizeHH = stpdata[sleutel,"SizeHH"]
  statdata$CarHH = stpdata[sleutel,"CarHH"]
  statdata$Moped = stpdata[sleutel,"Moped"]
  statdata$License = stpdata[sleutel,"License"]
  
  statdata
}

####


names(statdata)
table(statdata$Eindresultaat)
# Verwerken van de variabelen
table(statdata$)

statdata$SizeHH = as.factor(apply(array(statdata$AantalPersHH),1,GrootteHH))
statdata$CarHH = as.factor(apply(array(as.numeric(statdata$AantalAutoHH)),1,AutoHH))
statdata$Moped = as.factor(apply(array(as.numeric(statdata$Bromfiets)),1,Bromfiets))
statdata$License = as.factor(apply(array(as.numeric(statdata$Rijbewijs)),1,RBW))
statdata$InkPerc = as.numeric(statdata$BVRPERCGESTINKH)
statdata$LFT = as.numeric(statdata$LFT)
statdata$Herkomst = apply(array(statdata$HERK3),1,Herkomst)
statdata$Urbanity = as.factor(statdata$STEDGEM)
statdata$AgeClasses = as.factor(apply(array(statdata$LFT),1,LftKlas15))
statdata$Ethnicity = "Onbekend"
for (i in 1:length(statdata$Herkomst))
{
  statdata$Ethnicity[i] = HerkomstGeneratie(statdata$Herkomst[i],statdata$VRLGBAGENERATIE[i])
}
statdata$Ethnicity = as.factor(statdata$Ethnicity)
statdata$IncomeClasses = as.factor(apply(array(statdata$InkPerc),1,InkKlas6))
statdata$EduLevel = as.factor(array(statdata$OPLNIVHB))

# Personen met lege registervariabelen
compleet = ((!is.na(statdata$VRLGBAGENERATIE)) & (!is.na(statdata$LFT)) & (!is.na(statdata$Urbanity)))
cat(nrow(statdata)-sum(compleet),"eenheden zijn verwijderd vanwege incomplete registergegevens\n")
statdata = statdata[compleet,]

model4 = formula(Eindresultaat ~ Ethnicity + AgeClasses + IncomeClasses + Urbanity + CarHH + Moped + License)


indOdIN = getRIndicator(model4,statdata,sampleWeights = statdata$Insluitgewicht)
statdata$