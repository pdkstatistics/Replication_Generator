# Basic cleaning
rm(list=ls(all=TRUE))
options(max.print = 99999)

# Libraries
library(dplyr) # for 


# Import the data
# Data format
# D:\RESULTS\R\BASIC_STATISTICS\Replication generator
DF=read.table("clipboard",header=TRUE)


# Data check
dim(DF)

# Number of Replications Required
N.Rep=4

# Limits variable wise
Limits = c(0.2, 0.1, 0.2, 0.01, 1)
    

# Data frame of Replications
DF.Rep=c()
for (i in 1:ncol(DF)) {
    
    Means= DF[,i]
    
    Rep=c()
    for (j in 1:(N.Rep-1)) {
        Rep=cbind(Rep,runif(length(Means),
                            Means-Limits[i],
                            Means+Limits[i]))
    }
    
    # Last Column
    Rep=cbind(Rep,((N.Rep*Means)-rowSums(Rep)))
    
    # Data frame of Replications
    DF.Rep=cbind(DF.Rep,c(Rep))
    
}

# Rename the row and column names
Results = DF.Rep %>% 
    as.data.frame() %>% 
    setNames(variable.names(DF)) %>% 
    mutate(Rep=rep(1:N.Rep,each=dim(DF)[1])) %>%
    relocate(Rep, .before = 1) %>% 
    mutate(F1=rep(row.names(DF),times=N.Rep)) %>%
    relocate(F1, .before = 1)


# Export to the clipboard
clipr::write_clip(Results)

# Export to Notepad
sink("####Results.txt")
Results
sink()
