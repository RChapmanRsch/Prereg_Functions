#Script to generate codebook from original data dictionary for clothing IRT project

#Codebook_list is a list object with this structure: 
#"Description" - string of description of the variable
#"Value" - vector of numeric values for the data, e.g., 1-5
#"Labelled" - vector of labelled response options, e.g., "never", "sometimes", "always"

#Read in the original data dictionary
Original_DD=read.csv("data/data_dictionary_092821.csv")

#Remove quotation marks, \", from file
Original_DD$Values=gsub("\"","",Original_DD$Values)

#Generate the 'Codebook_List' object... 
Codebook_list=
  #...with an sapply statement iterating over for each variable name in the codebook
  sapply(Original_DD$Variable, simplify=FALSE, USE.NAMES = TRUE, function(Variable){
    i=which(Original_DD$Variable==Variable) #'i' is a row index for each variable in the codebook
    #if there are a non-empty variable values...
    if(!Original_DD$Values[i]==""){
      #...create a 'Value_Labels' list object which contains vectors representing
      #all numeric values and all character labels in a codebook variable 'i'
      Values_Labels=sapply(strsplit(strsplit(Original_DD$Values[i],split=", ")[[1]],"="), simplify=FALSE,
                           function(x){c(as.numeric(x[1]),as.character(x[2]))})
      #if the second Values numeric vector does not equals 2....
      #which indicates that only max/min values are present in the 'values' vector
      # e.g., c(1,5) vs c(1,2,3,4,5)
      if(Values_Labels[[2]][1]!="2"){
        j=as.numeric(Values_Labels[[length(Values_Labels)]][1]) #'j' is last/max numeric value in variable 'i'
        #recreate 'Values_Labels' list object for codebook variable 'i' with an lapply statement iterating 
        #across 1:'j' numeric values with numeric value index 'k'  
        Values_Labels=lapply(1:j,function(k){
          #First value label (min value) is the same as in the original 'Value_Labels'
          if(k==as.numeric(Values_Labels[[1]][1])){Values_Labels[[1]]}else{  
          #Last value label (max value) is the same as in the original 'Value_Labels'
          #everything in between is filled in with the 'k' index value and a blank character, ""
            if(k==as.numeric(Values_Labels[[2]][1])){Values_Labels[[2]]}else{c(k,"")}}
        })
      }
      #split the 'Values_Labels' list object for codebook variable 'i'
      #into two vectors, numeric values: 'Values' & charact labels: 'Labels'
      Values=matrix(unlist(Values_Labels),nrow=2)[1,]
      Labels=matrix(unlist(Values_Labels),nrow=2)[2,]
    }
    else{Values=NA;Labels=""} #for codebook variables which don't have listed values/labels, empty num/char values 

    #final call to sapply to return a 'Codebook_list' object 
    list("Description"=Original_DD[i,which(names(Original_DD)=="Label")],
         "Value"=as.numeric(Values),
         "Labelled"=Labels)
    
  })

#Save out the generated 'Codebook_list' object as an RDS object
saveRDS(Codebook_list,"data/Codebook_list.RDS")


