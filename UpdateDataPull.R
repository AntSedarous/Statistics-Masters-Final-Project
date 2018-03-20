install.packages('gdata')
library(dplyr)
library(gdata)



df <- read.xls("~/Downloads/FACS Files/T001-001.xlsx", pattern='Frame#' )
df[,1:10] %>%
  mutate(rowSumz=Anger+Contempt+Disgust+Fear+Joy+Sad+Surprise+Neutral) %>%
  dplyr::filter(rowSumz>0) %>%
  group_by(round(Time)) %>%
  summarise(meanAnger = mean(Anger), 
            meanContempt = mean(Contempt),
            meanDisgust = mean(Disgust),
            meanFear = mean(Fear),
            meanJoy = mean(Joy),
            meanSad = mean(Sad),
            meanSurprise = mean(Surprise),
            meanNeutral = mean(Neutral)) -> df2

names(df2) <- c('Time', 'Anger', 'Contempt', 'Disgust', 'Fear', 'Joy', 'Sad', 'Surprise', 'Neutral')
df2$Subject = rep("T001", length(df2$Time))
df2$Drive = rep(1, length(df2$Time))


for (i in 1:88) {
  for (j in 1:8) {
    if(!(i==1 & j==1) ) {
      if(i <= 9) {
        txt <- paste("T00", as.character(i), "-00",as.character(j), sep='')
      }
      else{
        txt <- paste("T0", as.character(i), "-00",as.character(j), sep='')
      }
      filename <- paste('~/Downloads/FACS Files/', txt, '.xlsx', sep='')
      if (file.exists(filename)) {
        dfXL <- read.xls(filename, pattern='Frame#' )
        dfXL[,1:10] %>%
          mutate(rowSumz=Anger+Contempt+Disgust+Fear+Joy+Sad+Surprise+Neutral) %>%
          dplyr::filter(rowSumz>0) %>%
          group_by(round(Time)) %>%
          summarise(meanAnger = mean(Anger), 
                    meanContempt = mean(Contempt),
                    meanDisgust = mean(Disgust),
                    meanFear = mean(Fear),
                    meanJoy = mean(Joy),
                    meanSad = mean(Sad),
                    meanSurprise = mean(Surprise),
                    meanNeutral = mean(Neutral)) -> dfXL2
        
        names(dfXL2) <- c('Time', 'Anger', 'Contempt', 'Disgust', 'Fear', 'Joy', 'Sad', 'Surprise', 'Neutral')
        dfXL2$Subject = rep(txt, length(dfXL2$Time))
        dfXL2$Drive = rep(j, length(dfXL2$Time))
        df2 <- rbind(df2, dfXL2)
        print(paste(i, j))
      }
    }
  }
}

df3 <- df2
df3$Subject = substr(df2$Subject, 1, 4)

write.csv(df3, '~/Downloads/R-Friendly Study Data/EmotionsDSv2.csv')
