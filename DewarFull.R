#################################################################
#
# R-Studio @ Macquarie University, Sydney, Australia
# 15.05.2017 - Adela Sobotkova
#
# Topic: Dealing with Contemporaneity in Survey data through a Census approach (a la Dewar 1991)
# 
# Version 2.0 - automatically reads in data and calculates the probabilistic estimates 
#             - for mean contemporary occupied sites, needs parameters checked
#              
#
################################################################

#### Set Workspace, if your data is tabular

#setwd("D:/Users/MQ20149304/Documents/RStudio/Workshop1")

#### Read in your data
sites = read.csv("D:/Users/MQ20149304/Documents/RStudio/Workshop1/Data/DewarCertain.csv", header=TRUE)
phases = read.csv("D:/Users/MQ20149304/Documents/RStudio/Workshop1/Data/Phases.csv", header=TRUE)

# Look at the data
head(sites)
class(sites)
sites[1,5:ncol(sites)]
sites[1:10,8:ncol(sites)]
sites[,8:15]
avg_diag = apply(sites[,6:15], 1, mean)

head(phases)
phase.here <-phases[4:10,]
class(phase.here) #check what format the phase columns are reading as

# Read a column and call a previous column
n=10
period_prec = (sites[,n-1])>0
period_prec
sum(period_prec == TRUE)  # sum preceding periods
print(colnames(sites)[n-1])

period = sites[,n]
period_now = period>0
period_now
sum(period_now)   # sum current periods
print(colnames(sites)[n])

period_succ = (sites[,n+1])>0
class(period_succ)
data <- as.numeric(sum(period_succ))
print(colnames(sites)[n+1])
sum(period_succ)

     
# Trying to call out a single column label
sites[,7]
names(sites[,1:5]) # why does this work despite the comma
names(sites[5])  # Dont list comma if referring to a single column by names/colnames!!!



#######       DEWAR BREAKDOWN OF SITES BY OCCUPATION TYPE

# Nominate the column where meaningful temporal data begin. n-1 (the preceding column)
# also needs meaningful data for the analysis to be meaningful

n=8  #LBA, the first column with meaningful data for Dewar analysis

# Create a matrix in which you will aggregate the interim data
period = sites[,n]
type.results <- matrix(period, nrow = 0, ncol = 4,byrow=T)  
dimnames(type.results)<-list(
  c(),  # row names
  c("Type A", "Type B", "Type C", "Type D")) # column names 
print(type.results)

# Experiment to name the rows
#rownames(type.results) <- paste(colnames(sites)[n:ncol])

#### PERFECTED VERSION

s_type=function(sites,n){
  while(n < ncol(sites)){ 
    # in a given table where information about site occupation starting at phase n-1, figure out where 
    # the site exists during n and calculate site type a, b,c,or d. A given site can be only one of these
    # at any given time
    period = sites[,n]
    print(colnames(sites)[n]) # name of current phase
    period_now <- period>0
    #print((period_now))   # list of current phase occupied sites
    #print(sum(period_now)) # number of tru phases
    period_prec <- (sites[,n-1])>0
    #print(sum(period_prec))
    period_succ <- (sites[,n+1])>0
    a <- sum(period_now == TRUE & period_prec == TRUE & period_succ == FALSE)
    b <- sum(period_now == TRUE & period_prec == TRUE & period_succ == TRUE)
    c <- sum(period_now == TRUE & period_prec == FALSE & period_succ == TRUE)
    d <- sum(period_now == TRUE & period_prec == FALSE & period_succ == FALSE)
    data <- as.numeric(c(a,b,c,d))
    type.results <- rbind(type.results,data)
    vill <- a+b
    print(vill)
    settle <- c+d
    aban <- a+d
    n = n+1
    #data <- cbind(x = rowSums(b>0), y = rowSums(c>0))
  } 
  print(type.results)  # produces a vertical list of phases and a+b components
}

dewarbg <- s_type(sites,8) # produces a table with site type values

# Add row names (phases) manually. 

rownames(dewarbg) <- paste(colnames(sites)[8:14]) #replace 8:14 with n:ncol to add start and end n as variables?
rownames(dewarbg)

# Add phase length column with column name
head(phases)
phases$Phaselength[4:10]

# Add phase length column without column name
#dewarbg1 <- dewarbg
#dewarbg1 <- cbind(dewarbg,phases$Phaselength[4:10])  # adds phaselength without column names
#dewarbg1
#colnames(dewarbg1)[5] <- "Phase"
#colnames(dewarbg1) <- c("A","B","C","Phase")

dewarbg2 <- dewarbg
dewarbg2 <- cbind(dewarbg2,phases[4:10,])  # adds both phase and phaselength columns with labels 
dewarbg2

# Calculate Aban and Settle vectors and add to Site component table
Aban <- (dewarbg2$`Type A`+dewarbg2$`Type D`)/dewarbg2$Phaselength
Settle <- (dewarbg2$`Type C`+dewarbg2$`Type D`)/dewarbg2$Phaselength
Vill <- dewarbg2$`Type A`+dewarbg2$`Type B`
dewarbg2 <- cbind(dewarbg2,Vill,Aban,Settle)
dewarbg2 


#####   PART TWO - PROBABILISTIC PART OF DEWAR

# Create matrices for aggregating the interim data

phase.results<-matrix(year, nrow = 0, ncol = 3,byrow=T)  
dimnames(phase.results)<-list(
  c(),  # row names
  c("Year", "CurrentVillages", "SD")) # column names 
print(phase.results)

stat.results<-matrix(dewarbg2$Phase, nrow = 0, ncol = 4,byrow=T)  
dimnames(stat.results)<-list(
  c(),  # row names
  c("Occ","Occ.sd","Use.span","Error")) # column names 
print(stat.results)


# Monte Carlo loop for random number generation and mean village growth calculation
# mc function does not work well when embedded in the final loop , so I included only 
# the while loop in the upper section

mc <- function(dewarbg2){
  while(year<=phase){
    iter <- sample(1:1000,999,rep=T) # creates 1000 random numbers for every year of the phase
    iter3 <- ifelse(iter<=aban*1000,1,0) 
    # checks if the random number is below the abandonment rate and if so, output 1, signalling that a village was abandoned
    # I multiple the site abandonment rate * 1000 to match random integer data generated by sample function)
    iter2 <- ifelse(iter<settle*1000,1,0) 
    # checks if the random number is below the settlement rate and if so, output 1, signalling that a settlement was newly inhabited
    #site establishment rate * 1000 (to match random integer data generated by sample function)
    new<-iter2+vill-iter3  # adds the new and substract the abandoned settlements
    vill<-mean(new)  # calculates the mean new villages in a given year
    x<-sd(new)   # calculates the standard deviation of new villages in a given year
    year.results<-c(year,vill,x) #creates a vector of data from this iteration
    phase.results<-rbind(phase.results,c(year,vill,x)) #combines all results for individual phase years
    year<-year+1
  }
}
phase.results



# Derive phase means and SDs from Monte Carlo (annual) results 

stats <- function(phase.results) {
  Occ<-mean(phase.results[,2])
  #print(Occ)
  Occ.sd<-sd(phase.results[,2])
  #print(Occ.sd)
  Use.span<-1/(aban/Occ)
  #print(aban)
  #print(Use.span)
  Low.span<-1/(aban/(Occ-Occ.sd))
  Hi.span<-1/(aban/(Occ+Occ.sd))
  Error<- (Hi.span-Low.span)/2
  #print(Error)
  result <-c(Occ,Occ.sd, Use.span, Error)
  print(result)
  #message("During ", dewarbg$Phase," the mean number of simultaneously occupied villages is ",Occ,",plus minus ",Occ.sd)
  #message("During ", dewarbg$Phase," the average span of occupation was ",Use.span, " years plus/minus ",Error)
}
stats(phase.results)


# Run a loop that generates random numbers for every year of the current phase 
# and executes a block of code while the various conditions are true 

i <- 1 #row number in the site component table where you want to start

for (i in 1:nrow(dewarbg2)) {
  year <- 1  #starting year of the phase iteration
  phase <- as.numeric(dewarbg2$Phaselength[i]) # i helps fetch the phase in the given row, instead of fetching the whole vector for every i 
  phase.results<- phase.results[0,]
  print(phase.results)
  vill <- dewarbg2$Vill[i]
  aban <- dewarbg2$Aban[i]
  settle <- dewarbg2$Settle[i]
  while(year<=phase){
    iter <- sample(1:1000,999,rep=T) # creates 1000 random numbers for every year of the phase
    iter3 <- ifelse(iter<=aban*1000,1,0) 
    # checks if the random number is below the abandonment rate and if so, output 1, signalling that a village was abandoned
    # I multiple the site abandonment rate * 1000 to match random integer data generated by sample function)
    iter2 <- ifelse(iter<settle*1000,1,0) 
    # checks if the random number is below the settlement rate and if so, output 1, signalling that a settlement was newly inhabited
    #site establishment rate * 1000 (to match random integer data generated by sample function)
    new<-iter2+vill-iter3  # adds the new and substract the abandoned settlements
    vill<-mean(new)  # calculates the mean new villages in a given year
    x<-sd(new)   # calculates the standard deviation of new villages in a given year
    year.results<-c(year,vill,x) #creates a vector of data from this iteration
    phase.results<-rbind(phase.results,c(year,vill,x)) #combines all results for individual phase years
    year<-year+1
  }
  y <-stats(phase.results)
  #stat.results <-rbind(stat.results, c(Occ,Occ.sd,Use.span,Error))
  stat.results <-rbind(stat.results,c(y))
  i <- i+1
  print(i)
}

# Bind the statistical results (estimates) to Site Component table

FinalResult <- cbind(dewarbg2, stat.results[1:7,])

write.csv(FinalResult,"D:/Users/MQ20149304/Documents/RStudio/Dewar/DewarResult.csv")
