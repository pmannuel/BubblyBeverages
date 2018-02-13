library(readxl)
library(dplyr)
library(reshape2)

##########################################################################################################
## BEER ANALYSIS DF
##########################################################################################################

merged.dat <-read_excel("C:/Users/pmann/Documents/Autumn 2017/Database Design/Project/Beer_21Nov.xlsx")
merged.dat<-data.frame(merged.dat)[,-1]

outlet.freq.perPANID <- merged.dat %>% group_by(PANID,OUTLET) %>% summarise(Freq=n())
IRIkey.freq.perPANID <- merged.dat %>% group_by(PANID,IRI_KEY) %>% summarise(Freq=n())
package.freq.perPANID <- merged.dat %>% group_by(PANID,Package) %>% summarise(Freq=n())
merchandiseTyp.freq.perPANID <- merged.dat %>% group_by(PANID,Merchandise_Type) %>% summarise(Freq=n())
productSubtyp.freq.perPANID <- merged.dat %>% group_by(PANID,Product_Subtype) %>% summarise(Freq=n())
parentBrand.freq.perPANID <- merged.dat %>% group_by(PANID,ParentBrands) %>% summarise(Freq=n())
# category.freq.perPANID <- data.frame(merged.dat$PANID,rep.int(1,2020))

df1 <- dcast(outlet.freq.perPANID, PANID ~ OUTLET, fun.aggregate = sum)
df2 <- dcast(IRIkey.freq.perPANID, PANID ~ IRI_KEY, fun.aggregate = sum)
df3 <- dcast(package.freq.perPANID, PANID ~ Package, fun.aggregate = sum)
df4 <- dcast(merchandiseTyp.freq.perPANID, PANID ~ Merchandise_Type, fun.aggregate = sum)
df5 <- dcast(productSubtyp.freq.perPANID, PANID ~ Product_Subtype, fun.aggregate = sum)
df6 <- dcast(parentBrand.freq.perPANID, PANID ~ ParentBrands, fun.aggregate = sum)
# df7 <- category.freq.perPANID

colnames(df1) <- c("PANID", "BEER_OUTLET1_DK", "BEER_OUTLET2_GK", "BEER_OUTLET3_MK")
colnames(df3) <- c("PANID", "BEER_BULK_PURCH", "BEER_SINGLE_PURCH")
colnames(df4) <- c("PANID", "MERCH1_Cider", "MERCH2_Domestic", "MERCH3_Imported")
# colnames(df7) <- c("PANID", "BEER_CATEGORY")

expediture.perPANID <- aggregate(merged.dat$DOLLARS, by=list(Category=merged.dat$PANID), FUN=sum)
colnames(expediture.perPANID) <- c("PANID","BEER.TOT.DOLLARS")
units.perPANID <- aggregate(merged.dat$UNITS, by=list(Category=merged.dat$PANID), FUN=sum)
colnames(units.perPANID) <- c("PANID","BEER.TOT.UNITS")

df8 <- merge(expediture.perPANID,units.perPANID, by='PANID')

beer.analysis.df <- Reduce(function(x, y) merge(x, y, by='PANID'), list(df1, df2, df3, df4, df5, df6, df8))
beer.analysis.df <- beer.analysis.df[order(beer.analysis.df$PANID),]

