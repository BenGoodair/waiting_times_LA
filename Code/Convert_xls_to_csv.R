if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools, dplyr, tidyverse, tidyr, stringr,  curl, plm, readxl, zoo, stringr, patchwork)


file_vector <- list.files(path = "C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/waiting_times_LA/Data/Raw_Data/2015")
#file_vector %>% head()
xl_list <- file_vector[grepl(".xls",file_vector)]
write.csv(xl_list, "C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/waiting_times_LA/Data/Raw_Data/2015/csvs/xl_list.csv")


for (i in 1:18) {
  waits <- read_excel(paste("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/waiting_times_LA/Data/Raw_Data/2015/",xl_list[i], sep=""),
                      sheet = "Commissioner", skip=13)
  write.csv(waits, file = paste("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/waiting_times_LA/Data/Raw_Data/2015/csvs/",xl_list[i], ".csv", sep=""))
  
}


file_vector <- list.files(path = "C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/waiting_times_LA/Data/Raw_Data/2010")
#file_vector %>% head()
xl_list <- file_vector[grepl(".xls",file_vector)]
write.csv(xl_list, "C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/waiting_times_LA/Data/Raw_Data/2010/csvs/xl_list.csv")


for (i in 1:18) {
  waits <- read_excel(paste("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/waiting_times_LA/Data/Raw_Data/2010/",xl_list[i], sep=""),
                      sheet = "PCT", skip=5)
  write.csv(waits, file = paste("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/waiting_times_LA/Data/Raw_Data/2010/csvs/",xl_list[i], ".csv", sep=""))
  print(i)
}



for (i in c(1:4,6,7,9:13,15,16,18)) {
  waits <- read_excel(paste("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/waiting_times_LA/Data/Raw_Data/2010/",xl_list[i], sep=""),
                      sheet = "PCT", skip=5)
  write.csv(waits, file = paste("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/waiting_times_LA/Data/Raw_Data/2010/csvs/",xl_list[i], ".csv", sep=""))
  
}

for (i in c(5,8,14,17)) {
  waits <- read_excel(paste("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/waiting_times_LA/Data/Raw_Data/2010/",xl_list[i], sep=""),
                      sheet = "PCT", skip=6)
  write.csv(waits, file = paste("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/waiting_times_LA/Data/Raw_Data/2010/csvs/",xl_list[i], ".csv", sep=""))
  
}




