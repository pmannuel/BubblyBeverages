library(readxl)
library(dplyr)
library(reshape2)

##########################################################################################################
## CARBBEV ANALYSIS DF
##########################################################################################################

merged.dat <-read.csv("C:/Users/pmann/Documents/Autumn 2017/Database Design/Project/Carbbev_21Nov.csv")
merged.dat<-data.frame(merged.dat)[,-1]

outlet.freq.perPANID <- merged.dat %>% group_by(PANID,OUTLET) %>% summarise(Freq=n())
IRIkey.freq.perPANID <- merged.dat %>% group_by(PANID,IRI_KEY) %>% summarise(Freq=n())
package.freq.perPANID <- merged.dat %>% group_by(PANID,Package) %>% summarise(Freq=n())
merchandiseTyp.freq.perPANID <- merged.dat %>% group_by(PANID,Merchandise_Type) %>% summarise(Freq=n())
productSubtyp.freq.perPANID <- merged.dat %>% group_by(PANID,Product_Subtype) %>% summarise(Freq=n())
parentBrand.freq.perPANID <- merged.dat %>% group_by(PANID,ParentBrand) %>% summarise(Freq=n())
# category.freq.perPANID <- data.frame(merged.dat$PANID,rep.int(1,2020))

df1 <- dcast(outlet.freq.perPANID, PANID ~ OUTLET, fun.aggregate = sum)
df2 <- dcast(IRIkey.freq.perPANID, PANID ~ IRI_KEY, fun.aggregate = sum)
df3 <- dcast(package.freq.perPANID, PANID ~ Package, fun.aggregate = sum)
df4 <- dcast(merchandiseTyp.freq.perPANID, PANID ~ Merchandise_Type, fun.aggregate = sum)
df5 <- dcast(productSubtyp.freq.perPANID, PANID ~ Product_Subtype, fun.aggregate = sum)
df6 <- dcast(parentBrand.freq.perPANID, PANID ~ ParentBrand, fun.aggregate = sum)
# df7 <- category.freq.perPANID

colnames(df1) <- c("PANID", "CARBBEV_OUTLET1_DK", "CARBBEV_OUTLET2_GK", "CARBBEV_OUTLET3_MK")
colnames(df3) <- c("PANID", "CARBBEV_BULK_PURCH", "CARBBEV_SINGLE_PURCH")
colnames(df4) <- c("PANID", "MERCH4_Lowcal", "MERCH5_Allbrands", "MERCH6_PLUsoftdrink", "MERCH7_REGULARsoftdrink", "MERCH8_SeltzerTonicClub")
# colnames(df7) <- c("PANID", "CARBBEV_CATEGORY")

expediture.perPANID <- aggregate(merged.dat$DOLLARS, by=list(Category=merged.dat$PANID), FUN=sum)
colnames(expediture.perPANID) <- c("PANID","CARBBEV.TOT.DOLLARS")
units.perPANID <- aggregate(merged.dat$UNITS, by=list(Category=merged.dat$PANID), FUN=sum)
colnames(units.perPANID) <- c("PANID","CARBBEV.TOT.UNITS")

df8 <- merge(expediture.perPANID,units.perPANID, by='PANID')

carbbev.analysis.df <- Reduce(function(x, y) merge(x, y, by='PANID'), list(df1, df2, df3, df4, df5, df6, df8))
carbbev.analysis.df <- carbbev.analysis.df[order(carbbev.analysis.df$PANID),]
