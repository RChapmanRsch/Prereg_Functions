
#read in Codebook and set sample size for sim data
Codebook=readRDS("data/Codebook_list.RDS")
SampleSize=2000

#Codebook is a list object with this structure: 
#"Description" - string of description of the variable
#"Value" - vector of numeric values for the data, e.g., 1-5
#"Labelled" - vector of labelled response options, e.g., "never", "sometimes", "always"



#create dataframe of sim data from SampleSize (above) and number of variable codebook
Data=data.frame(matrix(NA, nrow=SampleSize, ncol=length(Codebook)))
names(Data)=names(Codebook)

#Create Sim data based on "values" in codebook list for each variables
for(Var in names(Data)){
  if(!all(is.na(Codebook[[Var]]$Value))){
    Data[,Var]=sample(Codebook[[Var]]$Value,SampleSize,replace = TRUE)}
  else if(Var=="respid"){Data[,Var]=c(1:SampleSize)}
  else {Data[,Var]=sample(1:100, SampleSize, replace = TRUE)}

###Are all items present?
if(any(!(names(Data) %in% names(Codebook)))){
  names(Data)[which(!(names(Data) %in% names(Codebook)))]
  names(Codebook)[which(!(names(Codebook) %in% names(Data)))]}

###Are all items and response formatted correctly - e.g., numeric v factor v value labels
sapply(Data, class) #need to manually review for factor and labels, etc.

#Out of Range values 
sapply(names(Codebook), function(Var){
    if(!all(is.na(Codebook[[Var]]$Value))){
      unique(Data[,Var])[!(unique(Data[,Var]) %in% Codebook[[Var]]$Value)]}})

