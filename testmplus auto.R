mplusbasicmix <- function(filename, ext, title_mp, namedata, data_set, variableuse, missflag, classes, starts, refinestarts, categoricallist=NULL)
{
  mplusinptoclass <- list()
  cl<- 1:classes

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
  
  
  #this is for creating the syntax for an LPA
  if(cat.null==TRUE){
    #this creates the Mplus syntax
    fintitle <- paste("Title:", title_mp, ";", sep=" ")
    data <- paste("data: File is", tablefile, ";", sep=" ") 
    variablelist <- paste("Variable: Names are", (varlistpaste), ";")
    usev <- paste("Usev=", variableusepaste, ";")
    missflag <- paste("Missing are all","(",missflag,")", ";")
    analysis <- paste("Analysis: type=Mixture;")
    starts <- paste("starts=", starts, refinestarts, ";", sep =" ")
    processors <- "processors=4(starts);"
    output <- paste("Output:", "sampstat", "Tech11", "Tech14", ";") 
    plot <- paste("plot: type=plot3;", "series=", variableusepaste,"(*)", ";")
      for(i in 1:classes){
        classes[[i]] <- paste("class=c","(", i, ")",";", sep="")
        mplusinptoclass[[i]] <- paste(fintitle, data, variablelist, usev, missflag, classes[[i]], analysis, starts, processors, output, plot, sep="\n")}
    
    #create each mplus file
    for(q in 1:length(filename1)){
      cat(mplusinptoclass[[q]], file = filename1[[q]])}}
  

#this is for creating the syntax for an LCA. There's probably a smoother way of doing this.
else{
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
  plot <- paste("plot: type=plot3;", "series=", variableusepaste,"(*)", ";")
  
  for(i in 1:classes){
    classes[[i]] <- paste("class=c","(", i, ")",";", sep="")
    mplusinptoclass[[i]] <- paste(fintitle, data, variablelist, usev, categorical, missflag, classes[[i]], analysis, starts, processors, output, plot, sep="\n")}

#create each mplus file
for(q in 1:length(filename1)){
  cat(mplusinptoclass[[q]], file = filename1[[q]])}}



#creating batch file
  bat.string <- paste("call mplus.exe ", filename, cl, ext, sep = "")
  bat.file.name <- paste(filename, ".bat", sep = "")
  cat(bat.string, file=bat.file.name, sep="\n")

#running batch file
shell.exec(file.path(getwd(), bat.file.name))}





#example usage for LPA # the extension code (".inp") is just for my own testing so I can test with text files, which is easier to deal with.
mplusbasicmix("testt", ".inp", "this is an mplus file", "formd", formdcleanuse, formdcleanuse, 999, 3, 100, 20)

#example usage for LCA

mplusbasicmix("testrun", ".inp", "this is a test of my function",  "formdata", formdcleanuse, formdcleanuse, 999, 4, 500, 100, formdcleanuse )

