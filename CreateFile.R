ds1 <- read.csv('~/Downloads/R-Friendly Study Data/T001.csv')
ds1$Subject <- rep('T001', length(ds1$Time))
ds2 <- read.csv('~/Downloads/R-Friendly Study Data/T002.csv')
ds2$Subject <- rep('T002', length(ds2$Time))

ds <- rbind(ds1, ds2)


for (i in 3:88) {
  if(i <= 9) {
    txt <- paste("T00", as.character(i), sep='')
  }
  else{
    txt <- paste("T0", as.character(i), sep='')
  }
  filename <- paste('~/Downloads/R-Friendly Study Data/', txt, '.csv', sep='')
  if (file.exists(filename)) {
    dsIn <- read.csv(filename)
    dsIn$Subject <- rep(txt, length(dsIn$Time))
    ds <- rbind(ds, dsIn)
  }
  
}
write.csv(ds, '~/Downloads/R-Friendly Study Data/AppendedDataSet.csv')







