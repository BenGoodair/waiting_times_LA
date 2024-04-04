if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools, dplyr, tidyverse, tidyr, stringr,  curl, plm, readxl, zoo, stringr, patchwork)

#2010

file_vector <- list.files(path = "C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/waiting_times_LA/Data/Raw_Data/2015")
#file_vector %>% head()
csv_list <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/waiting_times_LA/main/Data/Raw_Data/2010/csvs/xl_list.csv"))[2]

df <- data.frame()

for (i in c(1,4,7,10,13,16)) {
  waits <- read.csv(paste("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/waiting_times_LA/Data/Raw_Data/2010/csvs/",csv_list$x[i],".csv", sep=""))
  if (nrow(df) == 0) {
    df <- waits
  } else {
    df <- rbind(df, waits)
  }
}

for (i in c(1, 4, 7, 10, 13, 16)) {
  waits <- read.csv(paste("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/waiting_times_LA/Data/Raw_Data/2015/", csv_list$x[i], sep=""))
  if (nrow(df) == 0) {
    df <- waits
  } else {
    df <- rbind(df, waits)
  }
}
