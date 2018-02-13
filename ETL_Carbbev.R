############
##  TRANSACTION DATA/.DAT FILES ETL
############

#Reading transaction file
setwd('C:/Users/pmann/Documents/Autumn 2017/Database Design/IRIData/Year12/External/carbbev')
carbbev.dk <- read.delim("carbbev_PANEL_DK_1687_1739.DAT",sep=",")
carbbev.gk <- read.delim("carbbev_PANEL_GK_1687_1739.DAT",sep=",")
carbbev.mk <- read.delim("carbbev_PANEL_MK_1687_1739.DAT",sep=",")

#Consolidating carbbev
carbbev<-rbind(carbbev.dk,carbbev.gk,carbbev.mk)

#Recoding the OUTLET column (DK=1,GK=2,MK=3)
carbbev$OUTLET <- as.factor(carbbev$OUTLET)
levels(carbbev$OUTLET) <- c(1,2,3) #Recoding levels in OUTLET column (DK=1,GK=2,MK=3)

# #Creating Vend_Item Column
# library(stringr)
# carbbev$Vend_Item<-carbbev$COLUPC%%10000000000
# #Check:
# head(as.character(carbbev$COLUPC))
# head(carbbev$OUTLET)

# #Dropping columns not required
# carbbev <- carbbev[,-c(8)]

#Removing Duplicate Records
carbbev<-carbbev[!duplicated(carbbev),]

#Write the final beer.total file
setwd('C:/Users/pmann/Documents/Autumn 2017/Database Design/Project')
write.csv(carbbev, file = paste('carbbev.total.csv',sep = '\t'), row.names = T)

############
##  ATTRIBUTE DATA/PARSED FILES ETL
############

#Reading attribute file
library(readxl)
parsed_carbbev <- read_excel("C:/Users/pmann/Documents/Autumn 2017/Database Design/IRIData/parsed stub files 2012/parsed stub files 2012/prod12_carbbev.xlsx")
parsed_carbbev <- data.frame(parsed_carbbev)

# #Creating Vend_Item
# class(parsed_carbbev$UPC)
# parsed_carbbev$Vend_Item <- gsub("[^0-9]","",parsed_carbbev$UPC)
# parsed_carbbev$Vend_Item <- as.numeric(parsed_carbbev$Vend_Item)
# parsed_carbbev$Vend_Item <- parsed_carbbev$Vend_Item%%10000000000
# #Check:
# head(as.character(parsed_carbbev$UPC))
# head(parsed_carbbev$Vend_Item)

# #Merging 2 datasets by Vend_Item to remove not required rows in parsed file
parsed_carbbev_filtered  <- merge(carbbev,parsed_carbbev,by='COLUPC')
# write.csv(Data1, file = paste('Data1.csv',sep = '\t'), row.names = T)
# carbbev_upc <- unique(carbbev$Vend_Item)

# parsed_carbbev_filtered<-parsed_carbbev[parsed_carbbev$COLUPC %in% carbbev$COLUPC,]

# parsed_carbbev_filtered<-Merged.data[!duplicated(Merged.data),]

#Create product type variable from L2 - 
#Low cal soft drink=4
#PLU - all brands soda=5
#PLU soft drinks=6
#Regular Soft Drinks=4
#Seltzer/tonic water/club soda=8
parsed_carbbev_filtered$L2 <- as.factor(parsed_carbbev_filtered$L2)
levels(parsed_carbbev_filtered$L2) <- c(4,5,6,7,8)
colnames(parsed_carbbev_filtered)[colnames(parsed_carbbev_filtered)=="L2"] <- "Merchandise_Type"
#Check:
table(parsed_carbbev_filtered$L2)
table(parsed_carbbev_filtered$Merchandise_Type)

#Creating Category Identifier (Carbbev=2)
parsed_carbbev_filtered$Category <- 2

#Recoding PRODUCT.TYPE = Product_Subtype
#Soda = 11
#Malt beverage = 9
#Seltzer water = 10
#Tonic water = 12
#Club soda = 8
parsed_carbbev_filtered$PRODUCT.TYPE <- as.factor(parsed_carbbev_filtered$PRODUCT.TYPE)
parsed_carbbev_filtered$Product_Subtype <- parsed_carbbev_filtered$PRODUCT.TYPE
levels(parsed_carbbev_filtered$Product_Subtype) <- c(8,9,10,11,12)
#Check:???
table(parsed_carbbev_filtered$PRODUCT.TYPE)
table(parsed_carbbev_filtered$Product_Subtype)

#generating parent brand label csv
# setwd('C:/Users/pmann/Documents/Autumn 2017/Database Design/Project')
# child_brand <- levels(as.factor(parsed_carbbev_filtered$L5))
# num_label <- seq(1, 153, 1)
# Parent_brand_carbbev <- data.frame(child_brand,num_label)
# write.csv(Parent_brand_carbbev, file = paste('Parent_brand_carbbev.csv',sep = '\t'), row.names = T)

#L5=Brands
parsed_carbbev_filtered$L5<-as.factor(parsed_carbbev_filtered$L5)
levels(parsed_carbbev_filtered$L5)<-seq(500, 652, 1)
colnames(parsed_carbbev_filtered)[colnames(parsed_carbbev_filtered)=="L5"]<-'Brands'
#Check:???
table(parsed_carbbev_filtered$L5) #before running levels
table(parsed_carbbev_filtered$Brands)

#Recoding Package=Package_type
bulk.indicator <- c("BX", "BOX", "CRTN", "PCK", "BTLS", "GB")

parsed_carbbev_filtered$Package1 <- parsed_carbbev_filtered$PACKAGE

for(i in 1:length(bulk.indicator)){
  parsed_carbbev_filtered$Package1[grep(bulk.indicator[i], parsed_carbbev_filtered$Package1)] <- 1 #1 is Bulk
}

parsed_carbbev_filtered$Package1[parsed_carbbev_filtered$Package1!="1"] <- 2
colnames(parsed_carbbev_filtered)[colnames(parsed_carbbev_filtered)=="Package1"]<-'Package'
#Check:???
table(parsed_carbbev_filtered$Package1)
table(parsed_carbbev_filtered$PACKAGE)

#Creating Parent_Brand variable
library(readr)
Parent_brand <- read_csv("C:/Users/pmann/Documents/Autumn 2017/Database Design/Project/map_parent_brand.csv")
Parent_brand<-data.frame(Parent_brand)
Parent_brand$Code <- as.factor(Parent_brand$Parent_Brand)
levels(Parent_brand$Code) <- seq(199,272,1)
Parent_brand$num_label <- seq(500, 652, 1)
write.csv(Parent_brand, file = paste('parent_brand_child_brand_map.csv',sep = '\t'), row.names = T)

PBlevels <- Parent_brand$num_label
PBlabels <- as.numeric(Parent_brand$Code)

parsed_carbbev_filtered$ParentBrand <- parsed_carbbev_filtered$Brands

parsed_carbbev_filtered$ParentBrand <- factor(parsed_carbbev_filtered$ParentBrand, levels=PBlevels, labels=PBlabels)

#Write the final beer.total file
setwd('C:/Users/pmann/Documents/Autumn 2017/Database Design/Project')
parsed_carbbev_final <- data.frame(parsed_carbbev_filtered$COLUPC,parsed_carbbev_filtered$Category,parsed_carbbev_filtered$Package,parsed_carbbev_filtered$Merchandise_Type,parsed_carbbev_filtered$Brands,parsed_carbbev_filtered$Product_Subtype,parsed_carbbev_filtered$VOL_EQ)
parsed_carbbev_final<-parsed_carbbev_final[!duplicated(parsed_carbbev_final),]
write.csv(parsed_carbbev_final, file = paste('parsed_carbbev_filtered.csv',sep = '\t'), row.names = T)

write.csv(parsed_carbbev_filtered, file = paste('merged_carbbev_filtered.csv',sep = '\t'), row.names = T)


# parsed_carbbev_final <- parsed_carbbev_filtered[,c(1,2,6,3,8,5,7,4,36,38,10,13,37,22,39)]
# write.csv(parsed_carbbev_final, file = paste('Carbbev_21Nov',sep = '\t'), row.names = T)
