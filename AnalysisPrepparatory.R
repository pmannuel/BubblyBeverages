final.analysis.df <- merge(beer.analysis.df,carbbev.analysis.df,by="PANID",all=TRUE)

final.analysis.df$BEER_CATEGORY <- rep(1,length(unique(final.analysis.df$PANID)))
final.analysis.df$CARBBEV_CATEGORY <- rep(1,length(unique(final.analysis.df$PANID)))

for(i in 1:3516){
  if(is.na(final.analysis.df$BEER.TOT.UNITS[i])){
    final.analysis.df$BEER_CATEGORY[i] <- 0
  }
  if(is.na(final.analysis.df$CARBBEV.TOT.UNITS[i])){
    final.analysis.df$CARBBEV_CATEGORY[i] <- 0
  }
}

setwd('C:/Users/pmann/Documents/Autumn 2017/Database Design/Project')
write.csv(final.analysis.df, file = paste('AnalysisDF.csv',sep = '\t'), row.names = T)

rbind(beer.analysis.df,carbbev.analysis.df)
