library(openxlsx) #working with Excel data
library(Rcpp) #dependent package
library(lubridate) #working with dates
library(reshape) #melt and reshape data
library(plyr) #splitting and combining data
library(RMark) #MARK
library(stringr) #working with character vectors
library(tidyr)
library(doBy)
library(dplyr)
library(ggplot2)

# SET WORKING DIRECTORY
setwd("C:/Users/Kira/Desktop/HMN/2017 Monitoring Reports/Data/")

# Check that csvfile doesn't have decimal times, ignore warning
#banding <- read.csv("HMN_ALL_2002-2017_new021518.csv")
banding <- read.csv("Excel_2002-2017BandingRecords_Formatted_4.csv")

##### FORMATTING ##################################################################################

#Fix issues with comments breaking at punctuation randomly
#Fixed within Excel, saved as csv, and will re-run checks
banding <- banding[1:58]

# Create column for checks
banding$Initials.check <- 0

# Capitalize all characters and factors
banding <-mutate_all(banding, .funs=toupper)

# Initials
unique(banding$Initials.Bdr)
# Remove all leading and trailing white spaces
banding$Initials.Bdr <- str_trim(banding$Initials.Bdr, side=c("both"))

for(i in 1:length(banding$Initials.Bdr)) {
  if (banding$Initials.Bdr[i]=="" | (is.na(banding$Initials.Bdr[i]==TRUE))) {
    banding$Initials.check[i] <- "1" 
  } else next
}

# Locations
unique(banding$Location)

banding$Location.check <-0

for(i in 1:length(banding$Location)) {
  if (banding$Location[i]=="") {
    banding$Location.check[i] <- "1" 
  } else next
}

# Year
unique(banding$year)

banding$year.check <-0

for(i in 1:length(banding$year)) {
  if (is.na(banding$year[i])) {
    banding$year.check[i] <- "1" 
  } else next
}

# Latitude
unique(banding$latitude)

banding$latitude.check <- 0

for(i in 1:length(banding$latitude)) {
  if (banding$latitude[i]=="") {
    banding$latitude.check[i] <- "1" 
  } else next
}

# Longitude
unique(banding$longitude)

banding$longitude.check <- 0

for(i in 1:length(banding$longitude)) {
  if (banding$longitude[i]=="") {
    banding$longitude.check[i] <- "1" 
  } else next
}


# Elevation
unique(banding$elevation)

banding$elevation.check <- 0

for(i in 1:length(banding$elevation)) {
  if (banding$elevation[i]=="") {
    banding$elevation.check[i] <- "1" 
  } else next
}


# Region
unique(banding$region)

banding$region.check <- 0

for(i in 1:length(banding$region)) {
  if (banding$region[i]=="") {
    banding$region.check[i] <- "1" 
  } else next
}

# Date
unique(banding$date)

# Time
unique(banding$time)

banding$time<-as.character(banding$time)

banding$time[which(banding$time=="10:05 a:m")] <- "1005"

banding$time <- str_trim(banding$time, side=c("both")) # take out leading white space

for(i in 1:length(banding$time)) {
  if (nchar(banding$time[i]) == 2) {
    banding$time[i] <- paste0(banding$time[i], "00")
  } else next
}

banding$time[which(banding$time=="")] <- "0000"

regexp <- "([[:punct:]]{1})([[:digit:]]{2})([[:blank:]]{1})([[:alpha:]]{2})" #reformat 10:00:00 AM
banding$time <- as.character(strsplit(banding$time,regexp))

banding$time <- gsub("([[:punct:]])","", banding$time) #Remove colons for next step

for(i in 1:length(banding$time)) {
  if (nchar(banding$time[i]) == 3) {
    banding$time[i] <- paste0("0",banding$time[i])
  } else next
}

banding$time<-gsub("(\\d\\d)(\\d\\d)","\\1:\\2",banding$time) # Then add colons back in

banding$time[which(banding$time=="10:05 AM")] <- "10:05"

banding$time.check <- 0

for(i in 1:length(banding$time)) {
  if (banding$time[i]=="") {
    banding$time.check[i] <- "1" 
  } else next
}



# Band.Status
unique(banding$Band.Status)
banding$Band.Status <- str_trim(banding$Band.Status, side=c("both"))

banding$BandStatus.check <- 0

for(i in 1:length(banding$Band.Status)) {
  if (banding$Band.Status[i]=="" | banding$Band.Status[i]=="8" | banding$Band.Status[i]=="9"
      | banding$Band.Status[i]=="X") {
    banding$BandStatus.check[i] <- "1" 
  } else next
}

# Species
unique(banding$Species)
banding$Species <- str_trim(banding$Species, side=c("both"))

banding$species.check <- 0

for(i in 1:length(banding$Species)) {
  if (banding$Species[i]=="") {
    banding$species.check[i] <- "1" 
  } else next
}

# Sex
unique(banding$Sex)
banding$Sex <- str_trim(banding$Sex, side=c("both"))

banding$Sex.check <- 0

for(i in 1:length(banding$Sex)) {
  if (banding$Sex[i]=="" | banding$Sex[i]=="0") {
    banding$Sex.check[i] <- "1" 
  } else next
}
#missing sex codes were BALO or BADE bands for 2017

# Age
unique(banding$Age)
banding$Age <- str_trim(banding$Age, side=c("both"))

banding$Age[banding$Age=="AHY"]<-"1"

banding$Age.check <- 0

for(i in 1:length(banding$Age)) {
  if (is.na(banding$Age[i])) {
    banding$Age.check[i] <- "1" 
  } else next
}

# Write out banding file
write.table(banding,file="2002-2017BandingRecords_Formatted_updated.csv", col.names = TRUE, row.names = FALSE, sep=",")
############################################################################################
# Vetting Checks

# If grooves are greater than 0, is the age 2?
grooves.question1 <- banding[which(banding$Grooves=="1" & banding$Age=="1"),]
grooves.question2 <- banding[which(banding$Grooves=="2" & banding$Age=="1"),]
grooves.question3 <- banding[which(banding$Grooves=="3" & banding$Age=="1"),]

banding$Grooves.check <- 0

for(i in 1:length(banding$Grooves)) {
  if (banding$Grooves[i]=="1" & banding$Age[i]=="1") {
    banding$Grooves.check[i] <- "1" 
  } else if (banding$Grooves[i]=="2" & banding$Age[i]=="1"){
    banding$Grooves.check[i] <- "1"
  } else if (banding$Grooves[i]=="3" & banding$Age[i]=="1"){
    banding$Grooves.check[i] <- "1"
  } else next
}

# Write out banding file
write.table(banding,file="2002-2017BandingRecords_Formatted.csv", col.names = TRUE, row.names = FALSE, sep=",")


# Check if bands are the same but sites are different and second capture is not "R"
## Also, checks to see if status is incorrect (more than Band.Status = 1 for a Band.Number)
# Sort by band, date, and time first
banding3 <- sort_df(banding2, vars=c("Band.Number","year","mo","day","time"))

banding3$Band.Status[which(banding3$Band.Status==" R")] <- "R"

banding3$new.status <- banding3$Band.Status
banding3$new.status[banding3$new.status=="R"]<-"0"
banding3$new.status <- as.integer(banding3$new.status)

junk.melt <- melt(banding3, measure.vars=c("new.status"))
X <- cast(junk.melt, Band.Number~Location,sum)

X$sum <- rowSums(X)

check1 <- X[which(X$sum>1 & X$sum<4),]


# When band numbers are the same ensure species age and sex are the same 
# Sort by band, date, and time first
banding3 <- sort_df(banding2, vars=c("Band.Number","year","mo","day","time"))

banding3$Age[which(banding3$Age=="HY")] <- "2"
banding3$Age[which(banding3$Age=="AHY")] <- "1"

banding3$Species[which(banding3$Species=="BCHU " | banding3$Species=="BCHU M" |
                         banding3$Species=="BCHU M " | banding3$Species=="BCHUM")] <- "BCHU"
banding3$Species[which(banding3$Species=="BTLH ")] <- "BTLH"

banding3$status <- rep(1,dim(banding3)[1])         
junk.melt.2 <- melt(banding3, measure.vars=c("status"))
Y <- cast(junk.melt.2, Band.Number~Species*Age*Sex,max)
Y[(Y=="-Inf")] <- 0

Y$sum <- rowSums(Y)

check2 <- Y[which(Y$sum>1),]


# Find bands out of range for sizes
banding3$size.test <- rep(0,dim(banding3)[1])

skip <- c(""," ","`","1","2","4")
BCHU.male.range <- c("B","C","D","E","F","G")
BCHU.female.range <- c("C","D","E","F","G","H")
ANHU.range <- c("D","E","F","G")
COHU.male.range <- c("B","C","D","E")
COHU.female.range <- c("C","D","E","F")
BTLH.male.range <- c("B","C","D","E","F")
BTLH.female.range <- c("C","D","E","F","G")
RUHU.AHY.male.range <- c("A","B","C","D","E","F")
RUHU.HY.male.range <- c("B","C","D","E","F")
RUHU.AHY.female.range <- c("C","D","E","F","G","H")
RUHU.HY.female.range <- c("C","D","E","F","G")
CAHU.male.range <- c("A","B","C","D")
CAHU.female.range <- c("B","C","D","E","F")
BBLH.male.range <- c("D","E","F","G")
BBLH.female.range <- c("D","E","F","G","H")
VCHU.range <- c("H","I","J")
MAHU.male.range <- c("G","H","I","J","K","L","M")
MAHU.female.range <- c("H","I","J","K","L","M","N")
BLUH.range <- c("I","J","K","L","M","N")

for(i in 1:dim(banding3)[1]){
  if (banding3$Size...Material[i] %in% skip){
    banding3$size.test[i] <- 0
} else if (banding3$Species[i]=="BCHU" & banding3$Sex[i]=="M" & banding3$Size...Material[i] %in% BCHU.male.range){
    banding3$size.test[i] <- 0
} else if (banding3$Species[i]=="BCHU" & banding3$Sex[i]=="F" & banding3$Size...Material[i] %in% BCHU.female.range){
  banding3$size.test[i] <- 0
} else if (banding3$Species[i]=="ANHU" & banding3$Size...Material[i] %in% ANHU.range){
  banding3$size.test[i] <- 0
} else if (banding3$Species[i]=="COHU" & banding3$Sex[i]=="M" & banding3$Size...Material[i] %in% BCHU.male.range){
  banding3$size.test[i] <- 0
} else if (banding3$Species[i]=="COHU" & banding3$Sex[i]=="F" & banding3$Size...Material[i] %in% BCHU.female.range){
  banding3$size.test[i] <- 0
} else if (banding3$Species[i]=="BTLH" & banding3$Sex[i]=="M" & banding3$Size...Material[i] %in% BCHU.male.range){
  banding3$size.test[i] <- 0
} else if (banding3$Species[i]=="BTLH" & banding3$Sex[i]=="F" & banding3$Size...Material[i] %in% BCHU.female.range){
  banding3$size.test[i] <- 0
} else if(banding3$Species[i]=="RUHU" & banding3$Sex[i]=="M" & banding3$Size...Material[i] %in% RUHU.AHY.male.range){
  banding3$size.test[i] <- 0
} else if(banding3$Species[i]=="RUHU" & banding3$Sex[i]=="M" & banding3$Size...Material[i] %in% RUHU.HY.male.range){
  banding3$size.test[i] <- 0
} else if (banding3$Species[i]=="RUHU" & banding3$Sex[i]=="F" & banding3$Size...Material[i] %in% RUHU.AHY.female.range){
  banding3$size.test[i] <- 0
} else if (banding3$Species[i]=="RUHU" & banding3$Sex[i]=="F" & banding3$Size...Material[i] %in% RUHU.HY.female.range){
  banding3$size.test[i] <- 0  
} else if (banding3$Species[i]=="CAHU" & banding3$Sex[i]=="M" & banding3$Size...Material[i] %in% CAHU.male.range){
  banding3$size.test[i] <- 0
} else if (banding3$Species[i]=="CAHU" & banding3$Sex[i]=="F" & banding3$Size...Material[i] %in% CAHU.female.range){
  banding3$size.test[i] <- 0  
} else if (banding3$Species[i]=="BBLH" & banding3$Sex[i]=="M" & banding3$Size...Material[i] %in% BBLH.male.range){
  banding3$size.test[i] <- 0   
} else if (banding3$Species[i]=="BBLH" & banding3$Sex[i]=="F" & banding3$Size...Material[i] %in% BBLH.female.range){
  banding3$size.test[i] <- 0
} else if (banding3$Species[i]=="VCHU" & banding3$Size...Material[i] %in% VCHU.range){
  banding3$size.test[i] <- 0
} else if (banding3$Species[i]=="MAHU" & banding3$Sex[i]=="M" & banding3$Size...Material[i] %in% MAHU.male.range){
  banding3$size.test[i] <- 0  
} else if (banding3$Species[i]=="MAHU" & banding3$Sex[i]=="F" & banding3$Size...Material[i] %in% MAHU.female.range){
  banding3$size.test[i] <- 0
} else if (banding3$Species[i]=="BLHU" & banding3$Size...Material[i] %in% BLUH.range){
  banding3$size.test[i] <- 0 
} else banding3$size.test[i] <- 1
}

check3 <- banding3[which(banding3$size.test==1),]


# Test location changes - finds errors with location code
# Sort by location, bandstatus, band,date, time
banding3 <- sort_df(banding3, vars=c("Location","Band.Status","Band.Number","year","mo","day","time"))
banding3$Band.Status[which(banding3$Band.Status==" R")] <- "R"
locations <- c("CFCK","DGS","ESC","IC","ML","MOCA","MPGF","MPGM","MV","PA","PCBNM","PP","RR","TQ","WCAT")

check4 <- banding3[which(!(banding3$Location %in% locations)),]


# Bandsize Test
# if (year is greater than 2008, if(band.status=1, if(band size is the same as two down), if(band size is the same as one down)) 
# Sort by status, band, date, and time
banding3 <- sort_df(banding2, vars=c("Band.Status","Band.Number","year","mo","day","time"))
banding3$BandsizeTest <- rep(0,dim(banding3)[1])
banding3$year <- as.numeric(banding3$year)
banding3$Size...Material[which(banding3$Size...Material=="`")] <- "1"
banding3$Size...Material[which(banding3$Size...Material==" ")] <- ""
banding3$Size...Material[which(banding3$Size...Material=="S ")] <- "S"


for(i in 1:dim(banding3)[1]){
  if (banding3$year[i] > 2008 & banding3$Band.Status[i]=="1" &
      (banding3$Size...Material[i]==banding3$Size...Material[i+2]) &
      (banding3$Size...Material[i]==banding3$Size...Material[i+1])){
   banding3$BandsizeTest[i] <- 0
  } else if (banding3$Band.Status[i]=="R" | banding3$Band.Status[i]=="4" | 
             banding3$Band.Status[i]=="5" | banding3$Band.Status[i]=="BADE") {
   banding3$BandsizeTest[i] <- 0
 } else if (banding3$Size...Material[i]=="1" | banding3$Size...Material[i]=="2" |
            banding3$Size...Material[i]=="4" | banding3$Size...Material[i]==""){
   banding3$BandsizeTest[i] <- 0
 } else banding3$BandsizeTest[i] <- 1
}


check5 <- banding3[which(banding3$BandsizeTest==1),]

write.table(check5,file="BandSizeTest.csv",sep=",",row.names=FALSE,col.names=TRUE)


# Make sure year is only for this year's season
banding3$year[which(banding3$year==2016)] <- 2017
banding3$year[which(is.na(banding3$year))] <- 2017


# Bands not in inventory (not for Idaho, BC)
# Red Rock sent all inventory bands back to Susan
# K=6100 J=5100 P=9100 L=4100

# MPG Inventory at end of 2017
# K34309-K34310, K34345-K34350, K34466-K34480,
# K38420-K38430, K38437-K38460, K38471-K38900
MPGinventory <- c(610034309,610034310,610034345,610034346,610034347,610034348,610034349,610034350,
                  610034466:610034480,610038420:610038430,610038437:610038460,610038471:610038900)
check6 <- banding3[which(banding3$Location=="MPGM" & banding3$Band.Number %in% MPGinventory),]
check7 <- banding3[which(banding3$Location=="MPGF" & banding3$Band.Number %in% MPGinventory),]

# MOCA Inventory at end of 2017
# K33920 to K33930, K33980, K34031 to K34065,
# K34107 to K34110, K34185 to K34200, K37701 to K38000
MOCAinventory <- c(610033920:610033930,610033980,610034031:610034065,610034107:610034110,
                   610034185:610034200,610037701:610038000)
check8 <- banding3[which(banding3$Location=="MOCA" & banding3$Band.Number %in% MOCAinventory),]

# ML Inventory at end of 2017
# J82851,J82852; K39181, K39183-K39189; P39678-P39680
# J82789-J82800; J82871-J82880; K39131-K39140; K39150-K39180
# K39053-K39070; K39111-K39130; J82613-J82630; J76391-J76400, L93600
# J82853-J82855, P41271, P41274-P41275; J82525, K39001-K39020
# J82440, K39071-K39090
MLinventory <- c(510082851,510082852,610039181,610039183:610039189,910039678:910039680,
                 510082789:510082800,510082871:510082880,610039131:610039140,
                 610039150:610039180,610039053:610039070,610039111:610039130,
                 510082613:510082630,510076391:510076400,410093600,510082853:510082855,
                 910041271,910041274:910041275,510082525,610039001:610039020,
                 510082440,610039071:610039090)
check9 <- banding3[which(banding3$Location=="ML" & banding3$Band.Number %in% MLinventory),]

# MV Inventory at end of 2017
# L89120, L89281-88, L89672
# P40220-29, P40414-15, 18, 25
# J79466, J84614, 634-40,  671-690, 729-30
# J84766, 769, 821, 831-839, J85773-79
# K34531-36, 691-700, 705-40, 749-750, K34766-777, 781-800
# K34646-650, K38101-116, 118-400
MVinventory <- c(410089120,410089281:410089288,410089672,910040220:910040229,
                 910040414:910040415,910040418,910040425,510079466,510084614,
                 510084634:510084640,510084671:510084690,510084729,510084730,
                 510084766,510084769,510084821,510084831:510084839,
                 510085773:510085779,610034531:610034536,610034691:610034700,
                 610034705:610034740,610034749,610034750,610034766:610034777,
                 610034781:610034800,610034646:610034650,610038101:610038116,
                 610038118:610038400)
check10 <- banding3[which(banding3$Location=="MV" & banding3$Band.Number %in% MVinventory),]


# PCBNM Inventory at end of 2017
# 610032405:610032420, 610035163:610035170, 610040207:610040210,610040301:610040320,
# 610040346:610040350, 610040374:610040380, 610040396:610040421,610040423:610030500
PCBNMinventory <- c(610032405:610032420, 610035163:610035170, 610040207:610040210,610040301:610040320,
                    610040346:610040350, 610040374:610040380, 610040396:610040421,610040423:610040500)
check11 <- banding3[which(banding3$Location=="PCBMN" & banding3$Band.Number %in% PCBNMinventory),]



