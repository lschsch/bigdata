#
#
# Calculating correlation with means
#
#
#
# 
#
country.num
all.countries <- c(country.num)
all.countries
head(all.countries)

all.candy <- d.t
head(all.candy)
years.tot <- unique(all.candy$Year)

# store how many countries you have data from
n.countries.data <- c(NA, length(years.tot))

# store the correlation coeeficients
data.correlations <- cbind.data.frame(year = years.tot, anx.vs.depr = rep(NA, length(years.tot)))
str(data.correlations)

i <- 1

for(i in 1:length(years.tot)){
  #i <- 1
  
  # make a subset of year i
  all.candy.t <- all.candy[ all.candy$Year == years.tot[i],]
  
  unique(all.candy.t$Year) # quick check to see if it works
  
  # how many countries do we have data for in year i?
  n.countries.data[i] <- length(unique(all.candy.t$Entity))
  unique(n.countries.data)
  
  # start your analysis
  
  # correlation
  
  cor.by.year <- cor(all.candy.t[,4:9])
  
  # just anxiety and deptression
  
  cor.by.year.ad <- cor(all.candy.t[,c(7,9)])
  
  # to extract just one value
  anxt.vs.depr <- cor.by.year.ad[1,2]
  
  # store each year in dataframe 
  
  # global.store.year <- cbind.data.frame(year = years.tot, 
  #                                        anx.vs.dep[i] = anxt.vs.depr)
  
  global.store.year <- data.correlations
  
  #global.store.year[i,2] <- anxt.vs.depr
  
  global.store.year[i,2] <- c(anxt.vs.depr)
  
  
  
  
  
}

global.store.year


i <- 1



for(i in 1:length(all.candy)){
  col <- which(all.candy$Entity == all.countries[i])
               print(col)
  if(all.candy$Entity == all.countries[i]){
   m.t <- mean(all.candy$Depression....)
   
  }
}

i <- 1
for(i in 1:length(all.candy)){
  col <- which(all.candy$Entity == all.countries$Entity[i])
  print(col)
  if(all.candy$Entity == all.countries[i]){
    m.t <- mean(all.candy$Depression....)
    
  }
}



#
#
#
#