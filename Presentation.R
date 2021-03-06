#
#
# PRESENTATION
#
#
#  Index
#     Cleaning attempt 
#     Cleaning loop
#     Correlation by year (manually) 
#        Freestyle coding
#     Correlation by year (in loop)
#     Making the correlation tables 
#
#  
# ---- Cleaning attempt ----

mental.global <- read.csv(file.choose(), stringsAsFactors = FALSE)

which(mental.global$Entity == "Russia")

which(mental.global$Depression.... > 6.5)
#2244-2253
mental.global$Entity[2244:2253]
#Greenland
mental.global$Year[2244:2253]
#1993-2002
mat.mental<- as.matrix(mental.global)
nrow(mat.mental)
str(mat.mental)

lm(mental.global$Depression....[mental.global$Entity("World")] 
   ~ mental.global$Anxiety.disorders.... [mental.global$Entity("World")])

country <- unique(mental.global$Entity)
country


#Taking away the "World" out of country
mental.global <- mental.global[-c(6357:6384),]

#Taking away Wesetrn Sub-Saharn Africa
which(mental.global$Entity == "Western Sub-Saharan Africa")
mental.global <- mental.global[-c(6329:6356),]
#Western Europe
which(mental.global$Entity == "Western Europe")
mental.global <- mental.global[-c(6301:6328),]

#United States Virgin Islands
delete.row<-which(mental.global$Entity == "United States Virgin Islands")
mental.global <- mental.global[-c(delete.row),]
country


# ---- Cleaning loop ----

# read csv
ment.and.sub <- read.csv("prevalence-by-mental-and-substance-use-disorder.csv",
                         header = TRUE, sep = ",",stringsAsFactors = FALSE)
#



country.num <- unique(ment.and.sub$Entity)
country.num


# to find the rows that are world
world.rows <- which(ment.and.sub$Entity == "World")
world.rows


# to remove the rows that are world
new.ment.and.sub <- ment.and.sub[-c(world.rows),]
new.ment.and.sub
world.rows.gone <- which(new.ment.and.sub$Entity == "World")
world.rows.gone

# now lets look at the other things we don't want
rm.ent <- unique(new.ment.and.sub$Entity)
rm.ent

# to remove the rest of things we don't want

dont.want <- c("Andean Latin America", "Central Asia", "Central Europe", 
               "Central Europe, Eastern Europe, and Central Asia", 
               "Central Latin America", "Central Sub-Saharan Africa", 
               "Eastern Europe", "Eastern Sub-Saharan Africa", "High SDI", 
               "High-income", "High-income Asia Pacific", "High-middle SDI", 
               "Latin America and Caribbean", "Low SDI", "Low-middle SDI", 
               "Middle SDI", "North Africa and Middle East", "North America", 
               "Oceania", "South Asia", "Southeast Asia", 
               "Southeast Asia, East Asia, and Oceania", 
               "Southern Latin America", "Southern Sub-Saharan Africa",
               "Sub-Saharan Africa", "Tropical Latin America", 
               "United Kingdom", "Western Europe", "Western Sub-Saharan Africa")

dont.want
length(dont.want)

# --- freestyle coding -----
# data frame new.ment.and.sub
str(new.ment.and.sub)

# make copy called d.t
d.t <- new.ment.and.sub

# let's go through Entity and exclude all entries in dont.want
for(i in 1:length(dont.want)){
  
  #i <- 1
  print(i)
  print(paste("exclude ", dont.want[i], sep = ""))
  # temprary data set
  # which( d.t$Entity == dont.want[i]) # where are the specific names
  print(paste ("how many entries: ", length(which(d.t$Entity == dont.want[i])), 
               sep = ""))
  
  d.t <- d.t[d.t$Entity != dont.want[i],]
  # (d.t$Entity == dont.want[i])
  #print(length(which(d.t$Entity == dont.want[i])))
  print(paste ("how many entries after: ", length(which(d.t$Entity == dont.want[i])), 
               sep = ""))
}

# go and check if it is indeed gone
unique(d.t$Entity)

# make a loop to subset per country
head(d.t)

d.tt <- d.t[d.t$Year == "2017",]
d.tt$Year

head(d.tt)


# ---- Correlation by year (manually) ----

## making sure it's the right (clean) data

Global.ment.years <- d.t


#Deleting all rows except for year 1990 and finding correlation
#1990
prevalence.1990 = Global.ment.years[Global.ment.years$Year == "1990",]
correlation.1990 = cor(prevalence.1990[, 4:10])
#1991
prevalence.1991 = Global.ment.years[Global.ment.years$Year == "1991",]
correlation.1991 = cor(prevalence.1991[, 4:10])
#1992
prevalence.1992 = Global.ment.years[Global.ment.years$Year == "1992",]
correlation.1992 = cor(prevalence.1992[, 4:10])
#1993
prevalence.1993 = Global.ment.years[Global.ment.years$Year == "1993",]
correlation.1993 = cor(prevalence.1993[, 4:10])
#1994
prevalence.1994 = Global.ment.years[Global.ment.years$Year == "1994",]
correlation.1994 = cor(prevalence.1994[, 4:10])
#1995
prevalence.1995 = Global.ment.years[Global.ment.years$Year == "1995",]
correlation.1995 = cor(prevalence.1995[, 4:10])
#1996
prevalence.1996 = Global.ment.years[Global.ment.years$Year == "1996",]
correlation.1996 = cor(prevalence.1996[, 4:10])
#1997 
prevalence.1997 = Global.ment.years[Global.ment.years$Year == "1997",]
correlation.1997 = cor(prevalence.1997[, 4:10])
#1998
prevalence.1998 = Global.ment.years[Global.ment.years$Year == "1998",]
correlation.1998 = cor(prevalence.1998[, 4:10])
#1999
prevalence.1999 = Global.ment.years[Global.ment.years$Year == "1999",]
correlation.1999 = cor(prevalence.1999[, 4:10])
#2000
prevalence.2000 = Global.ment.years[Global.ment.years$Year == "2000",]
correlation.2000 = cor(prevalence.2000[, 4:10])
#2001
prevalence.2001 = Global.ment.years[Global.ment.years$Year == "2001",]
correlation.2001 = cor(prevalence.2001[, 4:10])
#2002
prevalence.2002 = Global.ment.years[Global.ment.years$Year == "2002",]
correlation.2002 = cor(prevalence.2002[, 4:10])
#2003
prevalence.2003 = Global.ment.years[Global.ment.years$Year == "2003",]
correlation.2003 = cor(prevalence.2003[, 4:10])
#2004
prevalence.2004 = Global.ment.years[Global.ment.years$Year == "2004",]
correlation.2004 = cor(prevalence.2004[, 4:10])
#2005
prevalence.2005 = Global.ment.years[Global.ment.years$Year == "2005",]
correlation.2005 = cor(prevalence.2005[, 4:10])
#2006
prevalence.2006 = Global.ment.years[Global.ment.years$Year == "2006",]
correlation.2006 = cor(prevalence.2006[, 4:10])
#2007
prevalence.2007 = Global.ment.years[Global.ment.years$Year == "2007",]
correlation.2007 = cor(prevalence.2007[, 4:10])
#2008
prevalence.2008 = Global.ment.years[Global.ment.years$Year == "2008",]
correlation.2008 = cor(prevalence.2008[, 4:10])
#2009
prevalence.2009 = Global.ment.years[Global.ment.years$Year == "2009",]
correlation.2009 = cor(prevalence.2009[, 4:10])
#2010
prevalence.2010 = Global.ment.years[Global.ment.years$Year == "2010",]
correlation.2010 = cor(prevalence.2010[, 4:10])
#2011
prevalence.2011 = Global.ment.years[Global.ment.years$Year == "2011",]
correlation.2011 = cor(prevalence.2011[, 4:10])
#2012
prevalence.2012 = Global.ment.years[Global.ment.years$Year == "2012",]
correlation.2012 = cor(prevalence.2012[, 4:10])
#2013
prevalence.2013 = Global.ment.years[Global.ment.years$Year == "2013",]
correlation.2013 = cor(prevalence.2013[, 4:10])
#2014 
prevalence.2014 = Global.ment.years[Global.ment.years$Year == "2014",]
correlation.2014 = cor(prevalence.2014[, 4:10])
#2015
prevalence.2015 = Global.ment.years[Global.ment.years$Year == "2015",]
correlation.2015 = cor(prevalence.2015[, 4:10])
#2016
prevalence.2016 = Global.ment.years[Global.ment.years$Year == "2016",]
correlation.2016 = cor(prevalence.2016[, 4:10])
#2017
prevalence.2017 = Global.ment.years[Global.ment.years$Year == "2017",]
correlation.2017 = cor(prevalence.2017[, 4:10])


# ---- Correlation by year (in loop) ----

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




# ---- Making the correlation tables ----


upper.2010 <- round(correlation.2010, 3)
upper.2010[upper.tri(correlation.2010)] <-""
upper.2010 <- as.data.frame(upper.2010) 

upper.2011 <- round(correlation.2011, 3)
upper.2011[upper.tri(correlation.2011)] <-""
upper.2011 <- as.data.frame(upper.2011) 

upper.2012 <- round(correlation.2012, 3)
upper.2012[upper.tri(correlation.2012)] <-""
upper.2012 <- as.data.frame(upper.2012) 

upper.2013 <- round(correlation.2013, 3)
upper.2013[upper.tri(correlation.2013)] <-""
upper.2013 <- as.data.frame(upper.2013) 

upper.2014 <- round(correlation.2014, 3)
upper.2014[upper.tri(correlation.2014)] <-""
upper.2014 <- as.data.frame(upper.2014) 

upper.2015 <- round(correlation.2015, 3)
upper.2015[upper.tri(correlation.2015)] <-""
upper.2015 <- as.data.frame(upper.2015) 

upper.2016 <- round(correlation.2016, 3)
upper.2016[upper.tri(correlation.2016)] <-""
upper.2016 <- as.data.frame(upper.2016) 

upper.2017 <- round(correlation.2017, 3)
upper.2017[upper.tri(correlation.2017)] <-""
upper.2017 <- as.data.frame(upper.2017) 


#Changing Names on the Table From 2010 to 2017
rownames(upper.2010) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")
colnames(upper.2010) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")


rownames(upper.2011) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")
colnames(upper.2011) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")


rownames(upper.2012) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")
colnames(upper.2012) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")


rownames(upper.2013) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")
colnames(upper.2013) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")


rownames(upper.2014) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")
colnames(upper.2014) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")


rownames(upper.2015) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")
colnames(upper.2015) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")


rownames(upper.2016) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")
colnames(upper.2016) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")


rownames(upper.2017) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")
colnames(upper.2017) <- c("Schizophrenia", "Bipolar", "Eating Disorders", 
                          "Anxiety", "Drug Use Disorder", "Depression", 
                          "Alcohol Use Disorder")

#Merging Data Frames

merge.test<-merge(upper.2010, upper.2011, by = c("Schizophrenia", "Bipolar", "Eating Disorders", 
                                                 "Anxiety", "Drug Use Disorder", "Depression", 
                                                 "Alcohol Use Disorder"))

all_models <- rbind_list(
  upper.2010 %>% mutate(Year = 2010),
  upper.2011 %>% mutate(Year = 2011),
  upper.2012 %>% mutate(Year = 2012),
  upper.2013 %>% mutate(Year = 2013),
  upper.2014 %>% mutate(Year = 2014),
  upper.2015 %>% mutate(Year = 2015),
  upper.2016 %>% mutate(Year = 2016),
  upper.2017 %>% mutate(Year = 2017)
)

#Visualize the Table. Taken from:
# https://www.littlemissdata.com/blog/prettytables
install.packages("data.table")

install.packages("dplyr")

install.packages("formattable")

install.packages("tidyr")

#Load the libraries

library(data.table)

library(dplyr)

library(formattable)

library(tidyr)

#Creating Tables for 2010-2017

formattable(upper.2010, 
            align = c("l", rep("r", NCOL(upper.2010))), 
            list("Correlation 2010" = formatter("span", style = 
                                                  ~ style(color = "greay", font.weight = "bold"))
            ))
formattable(upper.2011, 
            align = c("l", rep("r", NCOL(upper.2011))), 
            list("Correlation 2010" = formatter("span", style = 
                                                  ~ style(color = "greay", font.weight = "bold"))
            ))

formattable(upper.2012, 
            align = c("l", rep("r", NCOL(upper.2012))), 
            list("Correlation 2010" = formatter("span", style = 
                                                  ~ style(color = "greay", font.weight = "bold"))
            ))
formattable(upper.2013, 
            align = c("l", rep("r", NCOL(upper.2013))), 
            list("Correlation 2010" = formatter("span", style = 
                                                  ~ style(color = "greay", font.weight = "bold"))
            ))
formattable(upper.2014, 
            align = c("l", rep("r", NCOL(upper.2014))), 
            list("Correlation 2010" = formatter("span", style =
                                                  ~ style(color = "greay", font.weight = "bold"))
            ))

formattable(upper.2015, 
            align = c("l", rep("r", NCOL(upper.2015))), 
            list("Correlation 2010" = formatter("span", style = 
                                                  ~ style(color = "greay", font.weight = "bold"))
            ))

formattable(upper.2016, 
            align = c("l", rep("r", NCOL(upper.2016))), 
            list("Correlation 2010" = formatter("span", style =
                                                  ~ style(color = "greay", font.weight = "bold"))
            ))

formattable(upper.2017, 
            align = c("l", rep("r", NCOL(upper.2017))),
            list("Correlation 2017" = formatter("span", style = 
                                                  ~ style(color = "greay", font.weight = "bold"))
            ))






# ---- Pretty Correlation ----

# currently just with one example (2017)

#renamed mental.global.2017
mental.global.2017 <-  d.tt
#removed non numerical data from table

Global.ment.sub <- subset(mental.global.2017, select = -c(Entity,Year,Code))

# ran correlation for numerical data 

correlation.ment <- cor(Global.ment.sub)


#Installed Corrplot Packages 
install.packages("corrplot")
library(corrplot)

#Display data in corrolation plot
cor<- corrplot(correlation.ment)
corrplot.mixed(correlation.ment)



# ---- END ----
#
#
#