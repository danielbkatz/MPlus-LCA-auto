mplusbasicmix <- function(filename, ext, title_mp, namedata, data_set, variableuse, categoricallist=FALSE, missflag, classes, starts, refinestarts)
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

cat.list <- names(categoricallist)
cat.list.paste <- paste(cat.list, collapse="\n")

    
#varlist <- paste(variablelist)
#varlist <- deparse(substitute(varlist))
filename1<- paste(filename, cl, ext, sep="")

if(categoricallist==FALSE){
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

#this loops through each class enumeration


#creating batch file
  bat.string <- paste("call mplus.exe ", filename, cl, ext, sep = "")
  bat.file.name <- paste(filename, ".bat", sep = "")
  cat(bat.string, file=bat.file.name, sep="\n")

#running batch file
shell.exec(file.path(getwd(), bat.file.name))
}




#example usage for LPA 
mplusbasicmix("testrun", ".inp", "this is a test of my function", "formdata", formdclean, formduse, categoricallist = FALSE, 999, 4, 500, 100 )

#example usage for LCA

mplusbasicmix("testrun", ".inp", "this is a test of my function",  "formdata", formdclean, formduse, formduse, 999, 4, 500, 100 )

