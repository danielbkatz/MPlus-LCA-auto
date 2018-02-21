mplusbasicmix <- function(filename, ext, title_mp, namedata, data_set, variableuse, missflag, classes, starts, refinestarts, categoricallist=NULL)
{
  mplusinptoclass <- list()
  cl<- 1:classes
  savedata <- list()

  tablefile <- paste(namedata, ".dat", sep="")
  write.table(data_set, file=tablefile, row.names=FALSE, col.names = FALSE, sep="\t", quote=FALSE)

#should use a loop eventually instead. Get names of variables in data set
  varlistnames <- names(data_set)
  varlistpaste <- paste(varlistnames, collapse="\n")
    
  variableusenames <- names(variableuse)
  variableusepaste <- paste(variableusenames, collapse="\n")
  
  cat.null=is.null(categoricallist)
  
  cat.list <- names(categoricallist)
  cat.list.paste <- paste(cat.list, collapse="\n")

    
#varlist <- paste(variablelist)
#varlist <- deparse(substitute(varlist))
  filename1<- paste(filename, cl, ext, sep="")
  filename2 <- paste(filename,cl, ".dat", sep = "")
  

#this is for creating the syntax for an LPA and LCA. There's probably a smoother way of doing this.
  fintitle <- paste("Title:", title_mp, ";", sep=" ")
  data <- paste("data: File is", tablefile, ";", sep=" ") 
  variablelist <- paste("Variable: Names are", (varlistpaste), ";")
  usev <- paste("Usev=", variableusepaste, ";")
  categorical <- paste("categorical=", cat.list.paste,";")
  missflag <- paste("Missing are all","(",missflag,")", ";")
  analysis <- paste("Analysis: type=Mixture;")
  starts <- paste("starts=", starts, refinestarts, ";", sep =" ")
  processors <- "processors=4(starts);"
  output <- paste("Output:", "sampstat", "Tech11", "Tech14", ";") 
  plot <- paste("plot: type=plot3;", "series=", variableusepaste,"(*)", ";", sep="\n")
  

  #FOR LPA
  if(cat.null==TRUE){
    for(i in 1:classes){
      classes[[i]] <- paste("class=c","(", i, ")",";", sep="")
      savedata[[i]] <- paste("savedata: results are ",i,".dat", sep="" )
      mplusinptoclass[[i]] <- paste(fintitle, data, variablelist, usev, missflag, classes[[i]], 
                                    analysis, starts, processors, output, plot, savedata[[i]], sep="\n")}}
#FOR LCA
  else{
    categoricallist <- categoricallist %>%mutate_all(funs(replace(., missflag, 999)))

    
    #categoricallist2 <- categoricallist2%>%
      #mutate_at(., funs(factor(.)))
    
    #ncat<- apply(categoricallist2, 2, nlevels(categoricallist2))
    
    for(i in 1:classes){
        classes[[i]] <- paste("class=c","(", i, ")",";", sep="")
        savedata[[i]] <- paste("savedata: results are ", filename, i,".dat", ";", sep="" )
        mplusinptoclass[[i]] <- paste(fintitle, data, variablelist, usev, categorical, missflag, classes[[i]],                                   analysis, starts, processors, output, plot, savedata[[i]], sep="\n")}
    
  }
#create each mplus file
for(q in 1:length(filename1)){
  cat(mplusinptoclass[[q]], file = filename1[[q]])}


#creating batch file
  bat.string <- paste("call mplus.exe ", filename, cl, ext, sep = "")
  bat.file.name <- paste(filename, ".bat", sep = "")
  cat(bat.string, file=bat.file.name, sep="\n")


#running batch file
shell.exec(file.path(getwd(), bat.file.name))
#returnlist <- list(ncat)
return(categoricallist2)}


#need this packagage for the data cleaning
install.packages("dplyr")
library("dplyr")




#This creates a fit table for your LCA
lcatab <- function(lcamod){
 
  
  #creates all the lists and tables so for loops can be run. I can probably get rid of some of these
numclass <- length(lcamod)
fitlist <- list()
newfit <- list()
tablefit2class <- list()
tablefit2class2<- list()
tablefit2class3<- list()
fitmulti <- data.frame()
 
 #don't even need this anymore, I don't think
 #fitmultilist <- as.data.frame(matrix(nrow=numclass, ncol=12, 0))
 
 #this reads in the table from savedata in mplus with fit criteria
 fit.list <- lapply(lcamod, read.table, blank.lines.skip = FALSE, fill = TRUE, sep = "")
 
 #this identifies the row in which H0 Log Likelihood resides. Obviously this won't work if you ll is greater than -500
  fitlist <- lapply(fit.list, function(x) which(x[,1]<=-500))

  #this gets rid of the rows above log likelihood value
  fitdrop <- paste(lapply(fitlist, function(x){x-1}))
  
  #this just constructs the table for the first one class model
  for(l in 1:length(fit.list)){
  newfit[[l]] <- fit.list[[l]][-c(1:fitdrop[[l]]),]}
  tablefit1class <- as.data.frame(newfit[[1]])
  
  #getsrid of uneeded fit criteria
  tablefit1class <- tablefit1class[1, -c(2, 4, 8:10)]
  
  #makes the table conformable
  tablefit1class[6:11] <- NA
  
  #labels the rows
  names(tablefit1class) <- c("H0_LogLikelihood", "Num_Free_Par", "BIC", "SaBIC", "Entropy", "Condition Number", "LMR adjust p-value", "BootStrap P-value", "LRTS", "BF", "cmP_K" )
  
  #this constructs the list for the classes greater than 1
  for(m in 2:length(newfit))
  {
  tablefit2class[[m]] <-newfit[[m]][1, -c(2,4,8:10)]
  tablefit2class2[[m]] <- newfit[[m]][2, -c(1:3, 5:10)]  
  tablefit2class3[[m]] <- newfit[[m]][3, -c(1, 3:5,7)]}
  
  #this constructs the table for classes greater than 1
  tablefit2class <- Reduce("rbind", tablefit2class)
  tablefit2class2 <- Reduce("rbind", tablefit2class2)
  tablefit2class3 <- Reduce("rbind", tablefit2class3)
  
  
  #puts a list of the tables together for classes greater than 1 and labels them
  listslca_plus <- list(tablefit2class, tablefit2class2, tablefit2class3)
  combiningmulti <- Reduce("cbind", listslca_plus)
  
  #labels the columns
  names(combiningmulti) <- c("H0_LogLikelihood", "Num_Free_Par", "BIC", "SaBIC", "Entropy", "Condition Number", "LMR adjust p-value", "BootStrap P-value", "LRTS", "BF", "cmP_K" )
  
  
  #merges the 1 and many class model together
  finalmerge <- rbind(tablefit1class, combiningmulti)
  
  #labels the rows
  row.names(finalmerge) <- c(1:nrow(finalmerge))
  
  #computes fit criteria
  finalmerge <- finalmerge %>%
    mutate(LRTSdiff=-2*(H0_LogLikelihood-lag(H0_LogLikelihood, default = first(H0_LogLikelihood[2]))))%>%
    mutate(SIC=.5*BIC) %>%
    mutate(BF=exp(SIC-lag(SIC, default = first(SIC[2])))) %>%
    mutate(expsicmax =exp(SIC-max(SIC)))%>%
    mutate(cmP_K=expsicmax/(sum(expsicmax)))
  
  #gets rid of an empty row because I'm lazy
  finalmerge <- finalmerge[,-9]
    
  return(finalmerge)
  #return(namefile)
  
  plot(finalmerge[,3], type="p")
  
  #this isn't working
  write.csv(finalmerge, file=print(namefile))
  }




#example usage for LPA # the extension code (".inp") is just for my own testing so I can test with text files, which is easier to deal with.
mod1 <- mplusbasicmix("testt", ".inp", "this is an mplus file", "formd", formdcleanuse, formdcleanuse, 999, 3, 100, 20)

#example usage for LCA

testinp <- mplusbasicmix("testrun", ".inp", "this is a test of my function",  "formdata", formdcleanuse, formdcleanuse, 999, 3, 500, 100, formdcleanuse)


#to save the fit table
write.csv(testinp, "preferred_filename.csv" )
