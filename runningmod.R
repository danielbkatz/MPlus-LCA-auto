


formaa<- formaclean[1:31] 


jj <- apply(formaa, 2, function(x){all(is.na(x))}) 

jj
formamiss<- formaa[,!jj]
  
  mutate_all(funs(replace(., is.na(.), 999)))


mplusbasicmix("forma", ".inp", "this is an mplus file", "forma", formamiss, formamiss, 999, 5, 100, 20, formamiss)
test <- mplusbasicmix("forma", ".inp", "this is an mplus file", "forma", formamiss, formamiss, 999, 2, 100, 20, categoricallist =  formamiss)


testlist2 <- lcatab(test, "wondeful2.csv")
class(testlist)

View(testlist)
testlist

write.csv(testlist, file=wonderful2.csv)
nrow(formamiss$QMA6)

