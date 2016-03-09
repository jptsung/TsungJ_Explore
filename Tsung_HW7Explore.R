#Write a function named explore that takes data frame, bin sizes,correlel
explore <- function(dataframe,vector,correl){ 
  library(ggplot2) #loads ggplot2 package
  
#creating different dataframes for each df column
  df_fact <- dataframe[which(lapply(dataframe, is.factor) == TRUE)] #returns TRUE or FALSE depending on whether its argument is of type factor or not; for factor column
  df_logic <- dataframe[which(lapply(dataframe, is.logical) == TRUE)] #for logical column
  df_fact_logic<-cbind(df_fact, df_logic) #binding factor and logical columns
  #want to combine the variables within same class so create a new df frame with above columns
  df_numeric <- dataframe[which(lapply(dataframe, is.numeric) == TRUE)] #for numeric columns
  
#1: Plot a pair of blue histograms with a vertical red line at the mean for every numerical variable at each bin size
#one using counts, second using density
  
  if(length(df_numeric)!=0){ #if there is a numeric column (df_numeric) in dataframe (not zero)
    for(i in 1:length(df_numeric)){ #loop through the df_numeric and get length of vector
      for(j in vector){ #loop bin size vector
        counts<-ggplot(df_numeric, aes(x=df_numeric[[i]]), environment=environment()) 
        #set variable 'counts' to ggplot of df_numeric and x values in ith column
        counts<-counts+geom_histogram(fill='blue', binwidth=j)+ #adds blue histogram layer and loops through binwidths for each variable
                geom_vline(xintercept=colMeans(df_numeric[i]), colour='red')+ #adds vertical red line at the mean for every numerical variable at each bin size specified
                labs(x=names(df_numeric[i])) #label the x-axis as the name of the ith column of df_numeric, and the main title as the jth binwidth
        print(counts) #prints counts graph
        
        #density part:
        dens<-counts + aes(y=..density..) #set dens as density of counts with y-var as density
        dens<-dens + labs(y="Data Density") #set y title as Data Density
        print(dens) #prints density graph
      } #close the binwidth loop 
    } #close dataframe loop 
  } #all plots plotted
  
#2: Plot a gray bar graph for every categorical and binary variable  
    
  if(length(df_fact_logic)!=0){ #if there is more than 0 factor/logical columns in the df
    for(k in 1:length(df_fact_logic)){ #for the 
      barplot<-ggplot(df_fact_logic, aes(x=df_fact_logic[[k]]), 
                      environment=environment()) #plots ggplot bar graph of every categorical and binary variable
      print(barplot) #prints the bar graph
    } #closes for loop
  } #closes if 
  else{
    barplot <- NA #barplot is not applicable
    print("Cannot plot bar graph") #else-if loop to print an error message
  } #closes else loop
  
  
#3: Calculate the R-square value for every pair of numerical variables
#Will be done within #4 because the r-sq will have to be added into data frame anyway
#so calculation is within R list
  
#4: Return the following in an R list
#a. A frequency table for every categorical and binary variable
#b. Numerical variables: summary statistics, data frame of each pair and r-sq/corrcoef
  
  #Part 4a: frequency table for every categorical and binary variable
  if(length(df_fact_logic)!=0){ #if at least one column exists (more than 0)
    freq_table<-table(df_fact_logic) #then create freq_table to take frequency table of df_fact_logic
  } 
  else{ #otherwise
    freq_table <- NA #the frequency table is assigned as NA
    print("Cannot return frequency table") #print error message
  }
  
  if(length(df_numeric)!=0){
    correl_num <- cor(df_numeric, method="Pearson") #set correl_num as the Pearson CorrCoeff of dataframe df_numeric
    
    #dataframes
    new_df <- data.frame(cbind(pair, num)) #set new_df as dataframe of pairs and nums
    names(new_df)[1] <- "Pair" #set column title as "Pair"
    names(new_df)[2] <- "Num"#set column title as "Num"
    pair <- rep("names", 21) #set pair to hold 21 "names"
    num <- rep(0.0, 21) #set num to hold 21 0.0's
    
    #Set thresholds with null values
    threshold <- correl #set threshold as correlation value
    corr_list <- NULL
    corr_names <- NULL
    r_sq <- NULL 
    rsq_comb <- NULL
    #defines above variables with empty values
    
    length <- length(correl_num[1,]) #length of one dimension matrices
    
    for (i in (1:(length-1))) { #for loop of correl coeff matrix minus last value
      for (j in ((i+1):length)) { #for loop of correl coeff matrix minus first value 
        
        #Form the name pair and add to the named pair vector
        comb <- paste(names(correl_num[,1])[[i]],names(correl_num[1,])[[j]],sep="-") 
        #sets comb as the nums of ith row first column and first row jth column. Seperated by "-"
        
        #Pairs every combination without repeat pairs
        rsq_comb <- c(rsq_comb, comb) #sets rsq_comb as a list of pair names
        
        
#3 snippet: Calculate the R-square value for every pair of numerical variables
        
        r_sq <- c(r_sq, correl_num[i,j]^2) #adds r_sq to num vector
        #sets r_sq as r-sq value of all pairs with no repeats

        #End of #3
        
        #if correl > threshold, add names and list to vectors
        if (correl_num[i,j] > threshold) { #if threshold less than correlation coefficient
          corr_names <- c(corr_names, comb) #then add pairs to corr_names
          corr_list <- c(corr_list, correl_num[i,j]) #then add corr coeff to corr_list
        } #closes if loop
      } #closes for loop
    } #closes for loop
    
    #create the dataframes and label the columns from part 4b
    #sets corr_df as dataframe for correlation coefficients
    corr_df <- data.frame(cbind(corr_names, corr_list)) #sets corr_df as names of the pairs, correl coeff that exceed threshold
    names(corr_df)[1] <- "Pair" #sets column title as "Pair"
    names(corr_df)[2] <- "Num" #sets column title as "Num"
    
    #sets rsq_df as dataframe for r-square values
    rsq_df <- data.frame(cbind(rsq_comb, r_sq)) #create the dataframe rsq_df as the names of the combinations of variables and thier correlesponding r-square values
    names(rsq_df)[1] <- "Pair" #sets column title as "Pair"
    names(rsq_df)[2] <- "Num" #sets column title as "Num"
    
    #sets sum as the dataframe for summary statistics
    sum<-summary(df_numeric) #sets sum as summary statistics
    grid<-list(freq_table, sum, rsq_df, corr_df) #sets grid as freq_table, sum, rsq_df, and corr_df
    return(grid) #return grid list
  } 
  else{ #else
    corr_df <- NA
    rsq_df <- NA
    sum <- NA
    print("Cannot return dataframes") #prints error message
  }
}

#5: Test your function using diamonds data frame expanded to include a logical column
#Test function using mtcars data

library(ggplot2) #loads ggplot2 package

data(diamonds) #loads diamonds data
str(diamonds)
explore(diamonds, c(5, 20, 50), .25)

data(mtcars) #loads mtcars data
str(mtcars)
explore(mtcars, c(5, 20, 50), .25) 
