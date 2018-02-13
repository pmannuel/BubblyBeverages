
#############
           
#  STEP 1

############



#Reading Beer Transactions File
setwd('C:/Users/pmann/Documents/Autumn 2017/Database Design/IRIData/Year12/External/beer')
beer.dk <- read.delim("beer_PANEL_DK_1687_1739.DAT",sep=",")
beer.gk <- read.delim("beer_PANEL_GK_1687_1739.DAT",sep=",")
beer.mk <- read.delim("beer_PANEL_MK_1687_1739.DAT",sep=",")

#Consolidating beer
beer.total<-rbind(beer.dk,beer.gk,beer.mk)

#Recoding the OUTLET column (DK=1,GK=2,MK=3)
beer.total$OUTLET<-as.factor(beer.total$OUTLET) 
table(beer.total$OUTLET)
levels(beer.total$OUTLET)<-c(1,2,3) #Recoding levels in OUTLET column (DK=1,GK=2,MK=3)

#Creating Vend_Item Column
library(stringr)
beer.total$Vend_Item<-beer.total$COLUPC%%10000000000

#Dropping columns not required
beer.total<-beer.total[,-c(8)]

#Removing Duplicate Records
beer.total<-beer.total[!duplicated(beer.total),]

#Write the final beer.total file
setwd('C:/Users/jalan/Desktop/IRI/New Files')
write.csv(beer.total, file = paste('beer.total.csv',sep = '\t'), row.names = T)

#############

#  STEP 2

############

#Reading attribute file
library(readxl)
parsed_beer <-read_excel("C:/Users/pmann/Documents/Autumn 2017/Database Design/IRIData/parsed stub files 2012/parsed stub files 2012/prod12_beer.xlsx")
parsed_beer<-data.frame(parsed_beer)
head(parsed_beer)

#Creating Vend_Item
class(parsed_beer$UPC)
parsed_beer$Vend_Item<-gsub("[^0-9]","",parsed_beer$UPC)
parsed_beer$Vend_Item<-as.numeric(parsed_beer$Vend_Item)
parsed_beer$Vend_Item<-parsed_beer$Vend_Item%%10000000000

#Droping not required column from parsed_beer
parsed_beer<-parsed_beer[,-c(1,3,4,6,7,8,9,10,11,12,13,15,18,19,20,21,22)]

#Merging 2 datasets by Vend_Item to remove not required rows in parsed file
Merged.data<-merge(beer.total,parsed_beer,by='Vend_Item')
#write.csv(Data1, file = paste('Data1.csv',sep = '\t'), row.names = T)
parsed_beer_filtered<-Merged.data[,-c(2:7)]
parsed_beer_filtered<-parsed_beer_filtered[!duplicated(parsed_beer_filtered),]

#Create product type variable from L2 - 
#Alcoholic cider=1, Domestic beer/ale=2, Imported beer/ale=3, PLU=4
parsed_beer_filtered$L2<-as.factor(parsed_beer_filtered$L2)
table(parsed_beer_filtered$L2)
levels(parsed_beer_filtered$L2)<-c(1,2,3,4)
colnames(parsed_beer_filtered)[colnames(parsed_beer_filtered)=="L2"] <- "Merchandise_Type"

#Creating Category Identifier (Beer=1)
parsed_beer_filtered$Category<-1


#Recoding TYPE.OF.BEER.ALE = Product_Subtype
colnames(parsed_beer_filtered)[colnames(parsed_beer_filtered)=="TYPE.OF.BEER.ALE"]<-'Product_Subtype'
parsed_beer_filtered$Product_Subtype<-as.character(parsed_beer_filtered$Product_Subtype)
parsed_beer_filtered$Product_Subtype[parsed_beer_filtered$Product_Subtype=="ALE"]<-1
parsed_beer_filtered$Product_Subtype[parsed_beer_filtered$Product_Subtype=="ASSORTED"]<-2
parsed_beer_filtered$Product_Subtype[parsed_beer_filtered$Product_Subtype=="DRAFT"]<-3
parsed_beer_filtered$Product_Subtype[parsed_beer_filtered$Product_Subtype=="LAGER"]<-4
parsed_beer_filtered$Product_Subtype[parsed_beer_filtered$Product_Subtype=="MALT BEVERAGE"]<-5
parsed_beer_filtered$Product_Subtype[parsed_beer_filtered$Product_Subtype=="PILSNER"]<-6
parsed_beer_filtered$Product_Subtype[!(parsed_beer_filtered$Product_Subtype %in% c(1,2,3,4,5,6))]<-7
table(parsed_beer_filtered$Product_Subtype)

#L5=Brands
parsed_beer_filtered$L5<-as.factor(parsed_beer_filtered$L5)
levels(parsed_beer_filtered$L5)<-c(1:518)
colnames(parsed_beer_filtered)[colnames(parsed_beer_filtered)=="L5"]<-'Brands'

#Recoding Package=Package_type
bulk.indicator <- c("CRTN", "BOX", "KEG", "BX", "SYSTEM", "TBX", "NCKBTLINBX", "GB", "CARTON")

parsed_beer_filtered$Package1 <- parsed_beer_filtered$PACKAGE

for(i in 1:length(bulk.indicator)){
  parsed_beer_filtered$Package1[grep(bulk.indicator[i], parsed_beer_filtered$Package1)] <- 1 #1 is Bulk
}

parsed_beer_filtered$Package1[parsed_beer_filtered$Package1!="1"] <- 2

table(parsed_beer_filtered$Package1)
table(parsed_beer_filtered$PACKAGE)
parsed_beer_filtered<-parsed_beer_filtered[,-c(6)]
colnames(parsed_beer_filtered)[colnames(parsed_beer_filtered)=="Package1"]<-'Package'

#Creating Parent_Brand variable
#Parent_Brand_labels <- read_csv("C:/Users/jalan/Desktop/IRI/Parent_brand.csv")
Parent_brand<-data.frame(Parent_brand)
head(Parent_brand)

PBlevels <- Parent_brand$Code
PBlabels <- Parent_brand$Parent_Brand

parsed_beer_filtered$ParentBrands <- parsed_beer_filtered$Brands

parsed_beer_filtered$ParentBrands <- factor(parsed_beer_filtered$ParentBrands, levels=PBlevels, labels=PBlabels)

#Recoding ParentBrands
parsed_beer_filtered$ParentBrands<-as.character(parsed_beer_filtered$ParentBrands)
levels(parsed_beer_filtered$ParentBrands)
charPBlevels <- unique(parsed_beer_filtered$ParentBrands)
numPBlabels <- seq(1,205)

parsed_beer_filtered$ParentBrands <- factor(parsed_beer_filtered$ParentBrands, levels=charPBlevels, labels=numPBlabels)
table(parsed_beer_filtered$ParentBrands)

#Writing the final files
setwd('C:/Users/jalan/Desktop/IRI/New Files')
write.csv(parsed_beer_filtered, file = paste('parsed_beer_filtered.csv',sep = '\t'), row.names = T)



