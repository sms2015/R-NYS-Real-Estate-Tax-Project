---
title: "IS 607 Final Project,   
An Evaluation of New York State Property Taxes"
author: "Stacey Schwarcz"
date: "Friday, December 12, 2014"
output: pdf_document
---

#New York Property Taxes vs Inflation and Income, 2011 vs 2004

#MOTIVATION      
Two counties in New York State, Westchester and Nassau County, are known for having some of the highest property taxes in the country.  A few years ago I was evaluating NY neighborhoods within reasonable commuting distance of Manhattan as potential places to live. I considered various parts of Westchester County before deciding against Westchester due to the high property taxes.  At the time I was curious as to how much these rates had increased over the last few decades and conducted some basic online research on historical property tax rates in Westchester County.  What I found was that the property tax rates had increased faster than inflation, and much of this was due to the increase in the school tax rate.  My motivation for this project is that I'm interested in conducting a more in depth analysis of property tax data for New York State counties and see how it compares to Westchester County. 

#PROJECT SCOPE    
This project analyzes property tax rate changes in New York State counties and how these rates compare to the inflation rate (as measured by the CPI - Consumer Price Index) and income level (as measured by AGI - Adjusted Gross Income) changes between 2004 and 2011.  This purpose of this comparison is to evaluate how much the property tax burden has changed relative to inflation and income for New York State counties. While it would be interesting to look at data before 2004, in order to limit the scope of this project will focus just on the 2004 to 2012 period.  

#PROJECT WORKFLOW    
This project will follow a data science workflow, with the following Steps  
1. Acquire the data  
2. Clean and transform the data, including exploratory analysis and graphics to decribe the data  
3. Analyze and present the data including statistical analysis and graphics to support conclusions.  The analysis includes the 'map' feature of R to present results in a map of New York State Counties. This feature was not covered in class  
  
#STEP 1: DATA SOURCES    
The Real Property Tax Rates Levy Data and Municipality (2004-2012)   
URL:  https://data.ny.gov/Government-Finance/Real-Property-Tax-Rates-Levy-Data-By-Municipality-/iq85-sdzs  
file type: csv  
Source: New York State Department of Taxation and Finance  

Total Income Liability by Place of Residence    
URL:  https://data.ny.gov/Government-Finance/Total-Income-And-Tax-Liability-By-Place-Of-Residen/nacg-rg66  
file type: csv  
Source: New York State Department of Taxation and Finance  

CPI data
URL: http://www.multpl.com/inflation/table  
file type: web page and converted to a csv  
Source: U.S. Bureau of Labor Statistics  

New York State County Population:  
URL: http://labor.ny.gov/stats/nys/statewide-population-data.shtm  
file type: MS Excel file converted to a csv  
Source: 2010 Census

#Import Data into R  

```{r, message=FALSE,warning=FALSE}
options(stringsAsFactors = FALSE) 
#prevent data frame columns from being automatically converted to factors

propertytax<-
  read.csv("C:/temp/Real_Property_Tax_Rates_Levy_Data_By_Municipality__Beginning_2004.csv")

income<-
  read.csv("C:/temp/Total_Income_And_Tax_Liability_By_Place_Of_Residence__Beginning_Tax_Year_1999.csv")

inflation<-
  read.csv("C:/temp/Inflation_rates.csv")  #web data converted to csv

censusPop<-
  read.csv("C:/temp/NYCountyPopulation.csv")  #original Excel file edited and converted to csv

```

#STEP 2: part 1: Clean the data  
Adjust column names, adjust data formatting such as removing $ signs and changing data to numeric form.

```{r, echo=FALSE,message=FALSE,warning=FALSE}

#'Check existing propertytax data frame column names'
#colnames(propertytax)  

#Change the column names of propertytax and income data frames to be easier to work with  
colnames(propertytax)<-c('FiscYear','LevyYear','RollYear','MuniCode','Municipality','County',
                         'SchoolCode','SchoolName','CountyLevy','CountyOutRate','CountyInRate',
                         'MuniLevy','MuniOutRate','MuniInRate','SchoolLevy','SchoolRate')  

#New column names
#colnames(propertytax)


#Check existing propertytax data frame column names
#colnames(income)
colnames(income)<-c('TaxYear','ResidentType',  'Residence','Country','State','County',
                    'Disclosure',	'Returns', 'AGI.Total','Liability.Total','TaxableReturns',
                    'AGITaxable.Total','LiabilityTaxable.Total','NontaxableReturns',
                    'AGINontaxable.Total','LiabilityNontaxable.Total','AGI.Average','Tax.Average',
                    'AGITaxable.Average','TaxTaxable.Average','AGINontaxable.Average',
                    'CountySortOrder','Location')

#New column names'
#colnames(income)

#Remove the $ but keep negative sign and decimal point from propertytax and income.
proptax_matrix<-gsub("([.-])|[[:punct:]]", "\\1", as.matrix(propertytax)) 
proptax<-data.frame(proptax_matrix)

income_matrix<-gsub("([.-])|[[:punct:]]", "\\1",as.matrix(income))
inc<-data.frame(income_matrix)

#Select only relevant columns for this analysis from propertytax and income
proptax<-proptax[,c('FiscYear','MuniCode','Municipality','County','SchoolCode','SchoolName',
                     'CountyLevy','CountyOutRate','CountyInRate','MuniLevy','MuniOutRate',
                     'MuniInRate','SchoolLevy','SchoolRate')]


inc<-inc[,c('TaxYear','ResidentType','County','Returns','AGI.Total','AGI.Average')]

#Change columns to numeric instead of character so calculations can be performed on these columns
propcols = c(7:14)
proptax[,propcols] = apply(proptax[,propcols], 2, function(x) as.numeric(x))

inccols<-c(4:6)
inc[,inccols] = apply(inc[,inccols], 2, function(x) as.numeric(x))

```

#DATA ISSUES  
There were several data issues encountered in the data files, that were handled in one of three ways:  
a. editing the csv file before import (note: the files included with this report are the edited files)  
b. the data is edited via R coding  
c. some of the observations in the data were corrupted in some way, in this case various simplifying assumptions were made to move ahead with this analysis for the purpose of this project.  A comprehesive study on this subject would need involve inquiries to the data source to obtain corrected data.  

Specific Issues Encountered:  
The County of St. Lawrence appears differently in each data source. To resolve this issue either the original csv was edited or the folowing R code modifies the data frame, so that St Lawrence always appears as St Lawrence and not St. Lawrence. 

The Income data had a "Total, New York City" which needed to be changed to "New York City". It also had "Manhattan" instead of "New York" County which needed to be changed to make it consistent with the other data sources used in this analysis.

New York City data for 2004 doesn't exist in the file. To resolve this the 2005 data was manually copied to create a 2004 to create a proxy 2004 record.  

The property tax data had 'New York City', but did not have records for each borough. However, the property tax is the same for all five boroughs of NYC so the NYC records were copied for 2004 and 2011 to create individual borough records in the csv file.

The census data has New York City Boroughs but doesn't have a subtotal for NYC so a sum of the population of the five boroughs of New York City was added to the csv file


```{r, echo=FALSE,message=FALSE,warning=FALSE}
#The County of St. Lawrence appears differently in each data source. To resolve this issue 
#either the original csv was edited or the folowing R code modifies the data frame, so that 
#St Lawrence always appears as St Lawrence and not St. Lawrence.  

inc$County[inc$County == "St. Lawrence"] <- "St Lawrence"

#The Income data has a "Total, New York City" which needs to be changed to "New York City"
#It also has "Manhattan" instead of "New York" County which needs to be changed to make it
#consistent with the other data sources used in this analysis.
#following R code modifies the relevant data frame.
inc$County[inc$County == "Total New York City"] <- "New York City"
inc$County[inc$County == "Manhattan"] <- "New York"

#New York City data for 2004 doesn't exist in the file. To resolve this the 2005 data was 
#manually copied to create a 2004 to create a proxy 2004 record.  

#The property tax data has 'New York City', but does not have records for each borough. However, 
#the property tax is the same for all five boroughs of NYC so the NYC records were copied for 
#2004 and 2011 to create individual borough records in the csv file.

#The census data has New York City Boroughs but doesn't have a subtotal for NYC so a sum of the 
#population of the five boroughs of New York City was added to the csv file

```
#Step 2: Part 2, transform the data and use exploratory analysis and graphics to decribe the data 
This section will provide initial transformation and calculations to set up the data frames that will be used in the analysis. Transformations involved the use of dcast from the rshape2 package, join_all from the plyr package, and base R functions such as aggregate.     

1-Calculate the change in inflation rate between 2004 and 2011  
2-Create a data frame with the the percent change (between 2004 and 2011) for the school and non-school components of the property tax  
3-Create a histogram and density function using ggplot2 to get an overview of how tax rate increases distribute for NYC counties. These charts will provide an initial overview of how property taxes rates have changed between 2004 and 2011.  
4-Set up a dataframe with the rate of change in Adjusted Gross Income between 2004 and 2011

```{r,echo=FALSE,message=FALSE,warning=FALSE}

#Calculate inflation rate change from 2004 to 2011
inflation$US.Inflation.Rate<-as.numeric(inflation$US.Inflation.Rate)
newrate<-1
for (i in 2:8){
  newrate=(inflation$US.Inflation.Rate[i]+1)*newrate
}
inflation11vs04<-newrate-1
print('The inflation rate change between 2004 and 2011 is:')
inflation11vs04


library(reshape2)

#Using data transformations analyze the school portion of the property tax
school<-proptax[,c('FiscYear','Municipality','County','SchoolRate')]
MuniSchool<-dcast(school, Municipality+County~FiscYear, value.var="SchoolRate",mean)

#Add a column to the MuniSchool data frame to calculate school tax percent 
#increase between 2004 and 2011
MuniSchool$School<-MuniSchool$'2011'/MuniSchool$'2004'-1

#Using data transformations, analyze non-school portions of the property tax
NonSchool<-proptax[,c('FiscYear','Municipality','County','CountyOutRate','CountyInRate',
                      'MuniOutRate','MuniInRate')]

#Create a data frame for each portion of the Non-School property tax
CountyOut<-dcast(NonSchool, Municipality+County~FiscYear, value.var="CountyOutRate",mean)
CountyIn<-dcast(NonSchool, Municipality+County~FiscYear, value.var="CountyInRate",mean)
MuniOut<-dcast(NonSchool, Municipality+County~FiscYear, value.var="MuniOutRate",mean)
MuniIn<-dcast(NonSchool, Municipality+County~FiscYear, value.var="MuniInRate",mean)

#Calculate the tax rate percent increase between 2004 and 2011 for each tax type
CountyOut$County_Out<-CountyOut$'2011'/CountyOut$'2004'-1
CountyIn$County_In<-CountyIn$'2011'/CountyIn$'2004'-1
MuniOut$Muni_Out<-MuniOut$'2011'/MuniOut$'2004'-1
MuniIn$Muni_In<-MuniIn$'2011'/MuniIn$'2004'-1

#Create a data frame with a unique set of municipalities and counties
countyMunidf<-unique(proptax[,c('Municipality','County')])

#Create a data frame for each tax type
Schooldf<-MuniSchool[,c('Municipality','County','School')]
CountyOutdf<-CountyOut[,c('Municipality','County','County_Out')]
CountyIndf<-CountyIn[,c('Municipality','County','County_In')]
MuniOutdf<-MuniOut[,c('Municipality','County','Muni_Out')]
MuniIndf<-MuniIn[,c('Municipality','County','Muni_In')]

#Join all of the school and non-school tax type data frames together using plyr join_all
require(plyr)
PropTax11vs04<-join_all(list(Schooldf,CountyOutdf,CountyIndf,MuniOutdf,MuniIndf),
                        by=c('Municipality','County'),type='full')

#The PropTax11vs04 table shows rate change statistics for 2011 vs 2004
#The table structures is as follows:
print('The new transformed data frame structure for property tax is:')
head(PropTax11vs04)

#Reshape the data frame to plot each density function
PropTax11vs04Melt<-melt(PropTax11vs04,id=c('Municipality','County'),
                        value.name='Rate_Change_2011_vs_2004',
                        variable.name='Property_Tax_Type')

#Create a histogram and density function using ggplot2 to get an overview of how tax rate 
#increases distribute for NYC counties. These charts will provide an initial sense of how 
#many counties have increases by property tax type.
require(ggplot2)

#Plot the distribution of the changes between 2004 and 2011

#histogram
print('Histogram of Property Tax Rate Changes between 2004 and 2011')
ggplot(PropTax11vs04Melt, aes(x=Rate_Change_2011_vs_2004, fill=Property_Tax_Type)) + 
  geom_histogram(binwidth=.2, position="dodge")+
  ggtitle('New York County Property Tax Rate Changes')


#density function
print('Density function of Property Tax Rate Changes between 2004 and 2011')
ggplot(PropTax11vs04Melt, aes(x=Rate_Change_2011_vs_2004, colour=Property_Tax_Type)) + 
  geom_density()+ ggtitle('New York County Property Tax Rate Changes')

```

#Initial Observations  

Initial exploratory statistics and graphics indicate that there are many counties in NY State where property taxes rates have stayed flat or have been reduced, but there are also many counties where property taxe rates have increased significantly.

```{r, echo=FALSE,message=FALSE,warning=FALSE}
#Transform the income data for comparisons to property tax rates, using Total Adjusted 
#Gross Income (AGI) as an income proxy

#Select the relevant columns
AGIAvg<-inc[,c('TaxYear','County','AGI.Total','AGI.Average','Returns')]

#Transform the data
AGI<-dcast(AGIAvg, County~TaxYear, value.var="AGI.Total",mean)  #Total AGI by County
AGI.Average<-dcast(AGIAvg, County~TaxYear, value.var="AGI.Average",mean)  #Average AGI by County

#Calculate 2004 vs 2011 change in AGI
AGI$AGI<-AGI$'2011'/AGI$'2004'-1
AGI$AGIAverage<-AGI.Average$'2011'/AGI.Average$'2004'-1

#Join the fields into one data frame
countydf<-data.frame('County'=unique(inc[,'County']))
AGIdf<-AGI[,c('County','AGI','AGIAverage')]
Income11vs04<-join_all(list(countydf,AGIdf),by=c('County'),type='full')

#Show the dataframe structure
print('The new transformed data frame structure for income is:')
head(Income11vs04)

```
#STEP 3: Analyze and present the data including statistical analysis and graphics to support conclusions.  The analysis includes the 'map' feature of R to present results in a map of New York State Counties. This feature was not covered in class  
Data Issues Encountered during Step 3:  
During the analysis phase a significant data issue was discovered that was not correctable. The tax rate data which is supposed to be significantly less than $1000 was well over $500 for several counties. The problem appeared with Nassau, Suffolk,and Westchester, and at least one Municipality in Albany. However, since the issue appears to persist throughout the years the assumption made for the purpose of this project is that the year over year changes are equivalent to the year over year rate changes and that this data can still be used for computing rate change.  It was also assumed for the purpose of this project that the levy data was correct even where the rate data was obviously incorrect.  

```{r, echo=FALSE,message=FALSE,warning=FALSE}
#The following code was used to check for the rate errors in the data
proptaxrate<-subset(proptax, FiscYear='2011', 
                    select=c(FiscYear,County,CountyOutRate,MuniOutRate,SchoolRate))
proptaxrate$TotalRate=proptaxrate$CountyOutRate+proptaxrate$MuniOutRate+proptaxrate$SchoolRate

#Find counties where school tax rate is greater than 100
proptaxratesAgg<-aggregate(SchoolRate ~ County, data = proptaxrate, mean)
testing<-subset(proptax, County=='Nassau', 
                select=c(FiscYear,County,Municipality,CountyOutRate,MuniOutRate,SchoolRate))
testing2<-subset(proptax, SchoolRate>200 & FiscYear=='2011', 
                 select=c(FiscYear,County,Municipality,CountyOutRate,MuniOutRate,SchoolRate))

print('The following result shows some of the issues, specifically with the impossibly high school tax rates for Nassau, Suffolk and Westchester.  These counties are known to have very high property taxes but a rate of 50%+ is just not possible. The data is clearly corrupted for these counties')
proptaxratesAgg

```
#Property Tax Rate Analysis
This section analyzes the property tax rate change by county and will:  
1-Provide a color coded map of these changes, using the R map function  
2-Provide data on counties where school property tax rates increased faster than inflation between 2004 and 2011  
3-Display the percent of the New York State population that lives in these counties      

```{r, echo=FALSE,message=FALSE,warning=FALSE}

#Map property tax rate increase by county
PropTax11vs04Map<-aggregate(School ~ County, data = PropTax11vs04, mean)

require(maps)
require(ggmap)

PropTax11vs04Map=subset(PropTax11vs04Map, County!='New York City', select=c(County,School))

q1<-quantile(PropTax11vs04Map$School)
c1 <- as.numeric(cut(PropTax11vs04Map$School, breaks = c(-0.45,q1[2:4],inflation11vs04,q1[5])))

# Plot property tax rate increase by county
colors = c("#F1EEF6","#D4B9DA", "#C994C7", "#DF65B0", "#980043")

PropTax11vs04Map$colorBuckets <- c1
colorsmatched <- PropTax11vs04Map$colorBuckets

#Increase graph size
par(mar=c(1,1,1,1))

map("county", "NEW YORK",col = colors[colorsmatched], border='darkgrey', fill = TRUE, resolution = 0, 
    lty = 1)

map.text('county','new york', cex = 0.5, add=TRUE)

title("School Property Tax Rate change by New York County, 2011 vs 2004")

#bins
b1=paste0('< ',round(as.numeric(q1[2]),2)*100,'%')
b2=paste0(round(as.numeric(q1[2]),2)*100,'%',' -  ',round(as.numeric(q1[3]),2)*100,'%')
b3=paste0(round(as.numeric(q1[3]),2)*100,'%',' -  ',round(as.numeric(q1[4]),2)*100,'%')
b4=paste0(round(as.numeric(q1[4]),2)*100,'%',' -  ',round(inflation11vs04,2)*100,'%')
b5=paste0('> ',round(inflation11vs04,2)*100,'%')

leg.txt <- c(b1, b2, b3, b4, b5)
legend("bottomleft", leg.txt, horiz = F, fill = colors,pt.cex = 1,cex=.7)

#Return to regular margins
par(mar=c(5.1,4.1,4.1,2.1))

#School property tax vs. inflation by county, 2011 vs 2004 average change by county
CountySchoolTax<-aggregate(School ~ County, data = PropTax11vs04, mean)
Schooltax.Income<-join_all(list(CountySchoolTax,AGIdf,censusPop,proptaxratesAgg),
                           by=c('County'),type='full')

#Find counties where the rate of school property tax increase faster than inflation 
#between 2004 and 2011
Exceed.Inflation<-subset(Schooltax.Income, School>inflation11vs04, 
                         select=c(County,School,SchoolRate,AGI,Census.2010))
Exceed.Inflation$inflation<-inflation11vs04
#Remove New York City, since five boroughs are each represented
Exceed.Inflation<-subset(Exceed.Inflation, County!='New York City', 
                         select=c(County,School,SchoolRate,AGI,Census.2010,inflation))
#Order by property tax rate change
Exceed.Inflation<-Exceed.Inflation[ order(-Exceed.Inflation[,2]), ]

#Create printable column names for display purposes
ExceedInfprint<-Exceed.Inflation
colnames(ExceedInfprint)<-c('County','School_Rate_11vs04','School_Rate_11',
                            'AGI_11vs04','Population','Inflation_11vs04')

print('NY State counties where school property tax has increased faster then inflation:')
ExceedInfprint

print('Percent of NY State population living in a county where the school tax has increased faster then inflation.')
ExceedPop<-sum(Exceed.Inflation$Census.2010)
NYStatePop<-sum(censusPop$Census.2010)
PctNYpopExceed<-ExceedPop/NYStatePop
PctNYpopExceed

```
The map shows that the property tax rate increases have varied considerably across counties, while some counties have increased property taxes in excess of the rate of inflation (18.9%), many other counties appear to have reduced their tax rates between 2004 and 2011. New York State appears to look very different depending on the county.  With respect to the counties that have increased property taxes appear in the table above and as expected include New York City (Bronx,Kings,New York,Queens,and Richmond), Westchester, Nassau and Suffolk.  The population in these counties makes up 45.9% of the population of New York State, thus nearly half of the population of New York State lives in counties where the property tax rate increase exceeded inflation between 2004 and 2011.

#Property Tax Rates changes vs. income changes between 2004 and 2011
This section will provide tables that show:  
1-The counties where the school tax rate has increased faster than average income between 2004 and 2011  
2-The percent of the NY State population living in a county where the school tax has increased faster than income.  

```{r, echo=FALSE,message=FALSE,warning=FALSE}
#Get data from the PropTax11vs04 data frame created earlier
PropTaxSummary<-PropTax11vs04[,c('County','School','County_Out','County_In','Muni_Out','Muni_In')]

#Aggregate property tax type data by County
CountyOAgg<-aggregate(County_Out ~ County,data = PropTaxSummary,mean )
CountyIAgg<-aggregate(County_In ~ County,data = PropTaxSummary,mean )
MuniOAgg<-aggregate(Muni_Out ~ County,data = PropTaxSummary,mean )
MuniIAgg<-aggregate(Muni_In ~ County,data = PropTaxSummary,mean )

MasterTable<-join_all(list(CountyOAgg,CountyIAgg,MuniOAgg,MuniIAgg,Schooltax.Income,censusPop),
                      by=c('County'),type='full')
#Replace "inf" values with NA
MasterTable<-do.call(data.frame,lapply(MasterTable, function(x) replace(x, is.infinite(x),NA)))

#Find the counties where the school tax rate has increased faster than income (average AGI)
PropExceedAgi<-subset(MasterTable, School>AGIAverage, select=c(County,School,AGIAverage,Census.2010))
PropExceedAgiprint<-PropExceedAgi[,1:3]
colnames(PropExceedAgiprint)<-c('County','School_Rate_Change','AvgAGI_Change')
PropExceedAgiprint<-PropExceedAgiprint[ order(-PropExceedAgi[,3]), ]
print('Counties where the school tax rate has increased faster than income (average AGI):')
PropExceedAgiprint

print('Number of counties where the school tax rate has increased faster than income (average AGI):')
nrow(PropExceedAgi)

print('Percent of NY State population living in a county where the school tax has increased faster than income:')
ExceedAgiPop<-sum(PropExceedAgi$Census.2010)
PctNYpopExceedAgi<-ExceedAgiPop/NYStatePop
PctNYpopExceedAgi

```
There are 15 counties including the 5 counties/boroughs of New York City where the school tax rate has increased faster than average income. The population of these counties comprises 63.2% of the population of New York State, thus a majority of the population of New York State lives in a county where the school property tax rate has outpaced income increase rates.

#Property Tax Levy and Income Analysis  
The property tax rate does not take into account changes in property values which will result in higher property taxes even if the property tax rate has not increased. To analyze this aspect of propety taxes this section will evaluate property tax levies which are a function of the property tax rates and the property values, and thus implicitely account for changes in house prices.  

This section will explore the property tax burden, defined here as the property taxes paid as a proportion of income and how this proportion has changed between 2004 and 2011. The results of this analysis will be displayed in a color coded map of New York County. This map will display the change in the property tax burden between 2004 and 2011.

```{r, echo=FALSE,message=FALSE,warning=FALSE}
#total property tax levy for 2011
options("scipen"=100, "digits"=4)

#sum all property tax levy types by municipality
proptax$TotLevy<-proptax$CountyLevy+proptax$MuniLevy+proptax$SchoolLevy

#create a copy of the proptax dataframe
proptax2=proptax

#create new dataframes with aggregate county levy data for 2004 and 2011
Levy04.county<-subset(proptax2, FiscYear=='2004', select=c(FiscYear,County,TotLevy,SchoolLevy))
Levy11.county<-subset(proptax2, FiscYear=='2011', select=c(FiscYear,County,TotLevy,SchoolLevy))

Levy04Agg<-aggregate(cbind(TotLevy,SchoolLevy) ~ County,data = Levy04.county,sum )
colnames(Levy04Agg)[2:3]<-c('TotLevy.2004','SchoolLevy.2004')

Levy11Agg<-aggregate(cbind(TotLevy,SchoolLevy) ~ County,data = Levy11.county,sum )
colnames(Levy11Agg)[2:3]<-c('TotLevy.2011','SchoolLevy.2011')

#Create 2004 and 2011 income data frames (AGI, number of returns)
AGI04.county<-subset(AGIAvg, TaxYear=='2004', select=c(County,AGI.Total,Returns))
AGI11.county<-subset(AGIAvg, TaxYear=='2011', select=c(County,AGI.Total,Returns))
colnames(AGI04.county)[2:3]<-c('AGI.Total.2004','Returns.2004')
colnames(AGI11.county)[2:3]<-c('AGI.Total.2011','Returns.2011')

#Join 2004 and 2011 tax levy, income and population data into one data frame
LevyAGI0411<-join_all(list(Levy04Agg,Levy11Agg,AGI04.county,AGI11.county,censusPop),by='County',type='full')

#The resulting data frame has the following structure
print('The structure of the combined data frame created for tax levies, income, and population:')
head(LevyAGI0411)

#Calculate the increase in property tax levies as a percent of income
LevyAGI0411$LA04<-LevyAGI0411$TotLevy.2004/(LevyAGI0411$AGI.Total.2004*1000)
LevyAGI0411$LA11<-LevyAGI0411$TotLevy.2011/(LevyAGI0411$AGI.Total.2011*1000)
LevyAGI0411$LA11v04<-LevyAGI0411$LA11/LevyAGI0411$LA04-1
LevyVIncome<-LevyAGI0411[,c('County','LA11v04')]
LevyVIncomeMap<-LevyVIncome[1:63,]

#Create a data frame to be used to map the increases by county
LevyVIncomeMap<-subset(LevyVIncomeMap, County!='New York City', select=c(County,LA11v04))
colnames(LevyVIncomeMap)<-c('County','Levy_vs_Income_2011v2004')

q2<-quantile(LevyVIncomeMap$Levy_vs_Income_2011v2004)
c2 <- as.numeric(cut(LevyVIncomeMap$Levy_vs_Income_2011v2004, breaks = c(-.1,q2[2:5])))
# Plot property tax rate increase by county

print('Map of property tax rate increases by New York County between 2004 and 2011')
#colors: http://research.stowers-institute.org/efg/R/Color/Chart/
colors2 = colors()[c(405,44,124,566)]

LevyVIncomeMap$colorBuckets <- c2
colorsmatched2 <- LevyVIncomeMap$colorBuckets

#Increase graph size
par(mar=c(1,1,1,1))

map("county", "NEW YORK",col = colors2[colorsmatched2], border='darkgrey',fill = TRUE, resolution = 0, 
    lty = 1)

map.text('county','new york', cex = 0.5, add=TRUE)

title("Property Tax Burden (Levy/Income) by New York County, 2011 vs 2004")

#bins
b21=paste0('< ',round(as.numeric(q2[2]),2)*100,'%')
b22=paste0(round(as.numeric(q2[2]),2)*100,'%',' -  ',round(as.numeric(q2[3]),2)*100,'%')
b23=paste0(round(as.numeric(q2[3]),2)*100,'%',' -  ',round(as.numeric(q2[4]),2)*100,'%')
b24=paste0('> ',round(as.numeric(q2[4]),2)*100,'%')

leg.txt2 <- c(b21, b22, b23, b24)
legend("bottomleft", leg.txt2, horiz = F, fill = colors2,pt.cex = 1,cex=.7)

#return to regular margins
par(mar=c(5.1,4.1,4.1,2.1))

```
The map shows that the propety tax burden (total property tax levy/total income) has increased in all New York State counties between 2004 and 2011. Since the school portion of the property tax rate has fallen in some of these counties it would appear that this would be related to increased property values or possibly increases in other property tax rates which this project didn't explicitely explore. Interestingly the tax burden in the five New York City counties, Nassau,Suffolk, and Westchester does not appear to have increased as much as other areas, which appeared to fare better when comparing property tax rates to average income.  There are various factors that might affect this ratio including changes in property values, but a complete understanding would require further analysis that is beyond the scope of this project.

#FINAL CONCLUSIONS

This analysis explored property tax changes across New York State counties between 2004 and 2011. Through statistics and graphics including charts and maps, this analysis found that property tax rate increases have varied considerably across counties, while some counties have increased property taxes in excess of the rate of inflation (18.9%), many other counties appear to have reduced their tax rates between 2004 and 2011. New York State appears to look very different depending on the county; however, due to population concentrations in certain counties, nearly half of the population of New York State (45.9% ), lives in counties where the school portion of the property tax rate increased at a rate that exceeded inflation between 2004 and 2011. Furthermore, 63.2% of the population of New York State lives in a county where the school property tax rate has outpaced the rate of increase in incomes.

The property tax rate does not take into account increases in property values which will result in higher rates even if the property tax rate has not changed. To analyze this aspect of propety taxes this project analyzed property tax levies which are a function of the property tax rates and property values, and thus implicitely account for changes in house prices. More specifically the analysis explored the property tax burden (property taxes paid as a proportion of income) and how this has changed between 2004 and 2011. The map showed a different pattern than the rate analysis had shown, and the tax burden in New York City, Nassau, Suffolk, and Westchester counties does not appear to have increased as much as other areas. There are various factors that might affect this ratio including changes in property values, but a complete understanding would require further analysis that is beyond the scope of this project.
