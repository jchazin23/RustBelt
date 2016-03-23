# Raw data comes from the US Census Bureau: 
# https://www.census.gov/econ/cbp/download/01_data/
# Each year's complete county data is downloaded from it's own site
# Raw files are quite large ~ 150 MB

# Examples of how to generate a dataframe from the census data:

biz1997 <- as.data.frame(read.delim("cbp89co.txt",header = TRUE,","),row.names=NULL)
#this adds a column that identifies the year, in case multiple years are merged
biz1997$year = 1997

biz1993 <- as.data.frame(read.delim("cbp93co.txt",header = TRUE,","),row.names=NULL)
biz1993$year = 1993

biz1997 <- as.data.frame(read.delim("cbp97co.txt",header = TRUE,","),row.names=NULL)
biz1997$year = 1997

biz2001 <- as.data.frame(read.delim("cbp01co.txt",header = TRUE,","),row.names=NULL)
biz2001$year = 2001

biz2005 <- as.data.frame(read.delim("cbp05co.txt",header = TRUE,","),row.names=NULL)
biz2005$year = 2005

biz2009 <- as.data.frame(read.delim("cbp09co.txt",header = TRUE,","),row.names=NULL)
biz2009$year = 2009

biz2013 <- as.data.frame(read.delim("cbp13co.txt",header = TRUE,","),row.names=NULL)
biz2013$year = 2013

## isolate manufacturing data for each year:
mfgTotals1997 <- subset(biz1997,subset = biz1997$sic == "20--")
mfgTotals1993 <- subset(biz1993,subset = biz1993$sic == "20--")
mfgTotals1997 <- subset(biz1997,subset = biz1997$sic == "20--")
mfgTotals2001 <- subset(biz2001,subset = biz2001$naics == "31----")
mfgTotals2005 <- subset(biz2005,subset = biz2005$naics == "31----")
mfgTotals2009 <- subset(biz2009,subset = biz2009$naics == "31----")
mfgTotals2013 <- subset(biz2013,subset = biz2013$naics == "31----")

# to conserve memory, dump the original biz files
rm(biz2013)
rm(biz2009)
rm(biz2005)
rm(biz2001)
rm(biz1997)
rm(biz1993)
rm(biz1997)

## Overwrite Employee number 0's by cross-referencing "employee flags".
## To be conservative, we will use the bottom of the range (except for
## class A, where we will use 10 as the bucket value).

# Note - NAICS and SIC employee flag buckets use the same bucket methodology
## A 0-19
## B 20-99
## C 100-249
## E 250-499
## F 500-999
## G 1,000-2,499
## H 2,500-4,999
## I 5,000-9,999
## J 10,000-24,999
## K 25,000-49,999
## L 50,000-99,999
## M 100,000 or More

## call this fuction like so:
# mfgTotals2013 <- empFlagReplace(mfgTotals2013)
# here is function definition:
empFlagReplace <- function(mfg_subset) {
  for (i in 1:dim(mfg_subset)[1]) {
    if (mfg_subset[i,]$empflag == "A"){
      mfg_subset[i,]$emp = 10
    }else if (mfg_subset[i,]$empflag == "B"){
      mfg_subset[i,]$emp = 20
    }else if (mfg_subset[i,]$empflag == "C"){
      mfg_subset[i,]$emp = 100
    }else if (mfg_subset[i,]$empflag == "E"){
      mfg_subset[i,]$emp = 250
    }else if (mfg_subset[i,]$empflag == "F"){
      mfg_subset[i,]$emp = 500
    }else if (mfg_subset[i,]$empflag == "G"){
      mfg_subset[i,]$emp = 1000
    }else if (mfg_subset[i,]$empflag == "H"){
      mfg_subset[i,]$emp = 2500
    }else if (mfg_subset[i,]$empflag == "I"){
      mfg_subset[i,]$emp = 5000
    }else if (mfg_subset[i,]$empflag == "J"){
      mfg_subset[i,]$emp = 10000
    }else if (mfg_subset[i,]$empflag == "K"){
      mfg_subset[i,]$emp = 25000
    }else if (mfg_subset[i,]$empflag == "L"){
      mfg_subset[i,]$emp = 50000
    }else if (mfg_subset[i,]$empflag == "M"){
      mfg_subset[i,]$emp = 100000
    }
  }
  return(mfg_subset)
}

#Now call on all mfgTotals
mfgTotals1997 <- empFlagReplace(mfgTotals1997)
mfgTotals1993 <- empFlagReplace(mfgTotals1993)
mfgTotals1997 <- empFlagReplace(mfgTotals1997)
mfgTotals2001 <- empFlagReplace(mfgTotals2001)
mfgTotals2005 <- empFlagReplace(mfgTotals2005)
mfgTotals2009 <- empFlagReplace(mfgTotals2009)
mfgTotals2013 <- empFlagReplace(mfgTotals2013)

#Final data trimming - shrinking dataframs down to the following columns:
#fipstate, fipscty, employees, year
mfgTotals1997 <- mfgTotals1997[,c(1,2,5,24)]
mfgTotals1993 <- mfgTotals1993[,c(1,2,5,24)]
mfgTotals1997 <- mfgTotals1997[,c(1,2,5,24)]
mfgTotals2001 <- mfgTotals2001[,c(1,2,5,24)]
mfgTotals2005 <- mfgTotals2005[,c(1,2,5,24)]
mfgTotals2009 <- mfgTotals2009[,c(1,2,6,27)]
mfgTotals2013 <- mfgTotals2013[,c(1,2,6,27)]

# Now, add county names 
# first, load in county name text file 
counties <- as.data.frame(read.delim("counties.txt",header = TRUE,","),row.names=NULL)

# and then perform lookup and re-name columns
mfgTotals1989 <- merge(mfgTotals1989, counties, by = c("fipstate","fipscty"),all.X=TRUE)
mfgTotals1989 <- mfgTotals1989[,c(1,2,5,3,4)]
colnames(mfgTotals1989) <- c("fipstate","fipscty","County Name","Num Mfg Employees","Year")

mfgTotals1993 <- merge(mfgTotals1993, counties, by = c("fipstate","fipscty"),all.X=TRUE)
mfgTotals1993 <- mfgTotals1993[,c(1,2,5,3,4)]
colnames(mfgTotals1993) <- c("fipstate","fipscty","County Name","Num Mfg Employees","Year")

mfgTotals1997 <- merge(mfgTotals1997, counties, by = c("fipstate","fipscty"),all.X=TRUE)
mfgTotals1997 <- mfgTotals1997[,c(1,2,5,3,4)]
colnames(mfgTotals1997) <- c("fipstate","fipscty","County Name","Num Mfg Employees","Year")

mfgTotals2001 <- merge(mfgTotals2001, counties, by = c("fipstate","fipscty"),all.X=TRUE)
mfgTotals2001 <- mfgTotals2001[,c(1,2,5,3,4)]
colnames(mfgTotals2001) <- c("fipstate","fipscty","County Name","Num Mfg Employees","Year")

mfgTotals2005 <- merge(mfgTotals2005, counties, by = c("fipstate","fipscty"),all.X=TRUE)
mfgTotals2005 <- mfgTotals2005[,c(1,2,5,3,4)]
colnames(mfgTotals2005) <- c("fipstate","fipscty","County Name","Num Mfg Employees","Year")

mfgTotals2009 <- merge(mfgTotals2009, counties, by = c("fipstate","fipscty"),all.X=TRUE)
mfgTotals2009 <- mfgTotals2009[,c(1,2,5,3,4)]
colnames(mfgTotals2009) <- c("fipstate","fipscty","County Name","Num Mfg Employees","Year")

mfgTotals2013 <- merge(mfgTotals2013, counties, by = c("fipstate","fipscty"),all.X=TRUE)
mfgTotals2013 <- mfgTotals2013[,c(1,2,5,3,4)]
colnames(mfgTotals2013) <- c("fipstate","fipscty","County Name","Num Mfg Employees","Year")

# Final output will be a combined csv file with all years as individual columns
testAll <- mfgTotals1989
testAll <- mfgTotals1989[,1:4]
colnames(testAll) <- c("fipstate","fipscty","County Name","Mfg Emp 1989")
## merge in 1993
testAll <- merge(testAll, mfgTotals1993, by = "County Name", all = TRUE)
testAll <- testAll[,c(2,3,1,4,7)]
colnames(testAll) <- c("fipstate","fipscty","County Name","Mfg Emp 1989","Mfg Emp 1993")
## merge in 1997
testAll <- merge(testAll, mfgTotals1997, by = "County Name", all = TRUE)
testAll <- testAll[,c(2,3,1,4,5,8)]
colnames(testAll) <- c("fipstate","fipscty","County Name","Mfg Emp 1989","Mfg Emp 1993","Mfg Emp 1997")
## merge in 2001
testAll <- merge(testAll, mfgTotals2001, by = "County Name", all = TRUE)
testAll <- testAll[,c(2,3,1,4,5,6,9)]
colnames(testAll) <- c("fipstate","fipscty","County Name","Mfg Emp 1989","Mfg Emp 1993","Mfg Emp 1997","Mfg Emp 2001")
## merge in 2005
testAll <- merge(testAll, mfgTotals2005, by = "County Name", all = TRUE)
testAll <- testAll[,c(2,3,1,4,5,6,7,10)]
colnames(testAll) <- c("fipstate","fipscty","County Name","Mfg Emp 1989","Mfg Emp 1993","Mfg Emp 1997","Mfg Emp 2001","Mfg Emp 2005")
## merge in 2009
testAll <- merge(testAll, mfgTotals2009, by = "County Name", all = TRUE)
testAll <- testAll[,c(2,3,1,4,5,6,7,8,11)]
colnames(testAll) <- c("fipstate","fipscty","County Name","Mfg Emp 1989","Mfg Emp 1993","Mfg Emp 1997","Mfg Emp 2001","Mfg Emp 2005","Mfg Emp 2009")
## merge in 2013
testAll <- merge(testAll, mfgTotals2013, by = "County Name", all = TRUE)
testAll <- testAll[,c(2,3,1,4,5,6,7,8,9,12)]
colnames(testAll) <- c("fipstate","fipscty","County Name","Mfg Emp 1989","Mfg Emp 1993","Mfg Emp 1997","Mfg Emp 2001","Mfg Emp 2005","Mfg Emp 2009","Mfg Emp 2013")

## Finally, export CSV
write.csv(testAll,file = "MfgEmployeesByCountyByYear.csv",row.names = FALSE)
