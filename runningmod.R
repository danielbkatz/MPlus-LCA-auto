


formaa<- formaclean[16:50] 


jj <- apply(formaa, 2, function(x){all(is.na(x))}) 

jj
formamiss<- formaa[,!jj]

  lssidtest <- lssid[4:18]
  lssidtest2 <- lssidtest %>% na.omit()

  formamisspl <- formamiss

mplusbasicmix("forma", ".inp", "this is an mplus file", "forma", formamiss, formamiss, 999, 5, 100, 20, formamiss)
test <- mplusbasicmix("forma", ".inp", "this is an mplus file", "forma", formamiss, formamiss, 999, 5, 100, 20, formamiss)
testtab <- lcatab(test)


testls <- mplusbasicmix("lssid", ".inp", "this is an mplus file", "lssid", lssidtest2, lssidtest2, -99, 2, 100, 20)
testls[[2]]

testlist2 <- lcatab(testls)
testlist2
class(testlist)

View(testlist)
testlist

write.csv(testlist, file=wonderful2.csv)
nrow(formamiss$QMA6)

class(test)
View(test)

nlevels(test$QMA1)
nlev2 <- as.data.frame(lapply(test, nlevels))

nlev2-1

test[3]