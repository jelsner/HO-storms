
rm(list = ls())

# number  of heat wave days during the weeks 1-13 
# number  of heat wave days during the weeks 14-28
# number  of heat wave days after the weeks 29 ~

combine_data_all= {}
for (i in 1981:2022){
  
  setwd("D:/Research/11_GRP/1_data/1_FL/Jihoon/0_birth_Emily/20_definition_final")
  
  file_name = paste("birth_all_final_", i, ".Rdata", sep = "")
  load(file_name)
  combined_data = combined_data[,c(1,83,4,5,112)]
  combined_data$ID = 1:nrow(combined_data)
  combined_data$ID = paste(i, "_", combined_data$ID, sep = "")
  
  if (sum(combined_data$GESTATION_WEEKS == 99, na.rm =T) != 0){
    combined_data[which(combined_data$GESTATION_WEEKS ==99),]$GESTATION_WEEKS = NA
  }

  if (sum(combined_data$GESTATION_WEEKS == "NULL", na.rm = T) != 0){
    combined_data[which(combined_data$GESTATION_WEEKS == "NULL"),]$GESTATION_WEEKS = NA
  }
  
  combined_data$GESTATION_WEEKS = as.numeric(combined_data$GESTATION_WEEKS)
  
  combined_data$trimester1_S = NA
  combined_data$trimester1_e = NA
  combined_data$trimester2_S = NA
  combined_data$trimester2_e = NA
  combined_data$trimester3_s = NA
  
  # part 1
  
  combined_data_part1 = combined_data[which(combined_data$GESTATION_WEEKS >= 28),]
  if (nrow(combined_data_part1) != 0){
    combined_data_part1$trimester1_S = combined_data_part1$GESTATION_WEEKS * 7
    combined_data_part1$trimester1_e = (combined_data_part1$GESTATION_WEEKS - 13) * 7 + 1
    combined_data_part1$trimester2_S = (combined_data_part1$GESTATION_WEEKS - 13) * 7 
    combined_data_part1$trimester2_e = (combined_data_part1$GESTATION_WEEKS - 28) * 7 + 1
    combined_data_part1$trimester3_s = (combined_data_part1$GESTATION_WEEKS - 28) * 7
  }
  
  # part 2
  
  combined_data_part2 = combined_data[which(combined_data$GESTATION_WEEKS < 28 & combined_data$GESTATION_WEEKS >= 13),]
  if (nrow(combined_data_part2) != 0){
    combined_data_part2$trimester1_S = combined_data_part2$GESTATION_WEEKS * 7 
    combined_data_part2$trimester1_e = (combined_data_part2$GESTATION_WEEKS - 13) * 7 + 1
    combined_data_part2$trimester2_S = (combined_data_part2$GESTATION_WEEKS - 13) * 7 
    combined_data_part2$trimester2_e = 0
    combined_data_part2$trimester3_s = NA
  }
  # part 3
  
  combined_data_part3 = combined_data[which(combined_data$GESTATION_WEEKS < 13 & combined_data$GESTATION_WEEKS >= 1),]
  if (nrow(combined_data_part3) != 0){
    combined_data_part3$trimester1_S = combined_data_part3$GESTATION_WEEKS * 7 
    combined_data_part3$trimester1_e = 0
    combined_data_part3$trimester2_S = NA
    combined_data_part3$trimester2_e = NA
    combined_data_part3$trimester3_s = NA
  }
  
  combine_data_part123 = rbind(combined_data_part1, combined_data_part2, combined_data_part3)
  combine_data_all = rbind(combine_data_all, combine_data_part123)
  
}

setwd("D:/Research/6_Elsner")
save(combine_data_all, file = "combine_data_all.Rdata")

rm(list = ls())

load("D:/Research/6_Elsner/combine_data_all.Rdata")

combine_data_all$DATE_OF_BIRTH = as.Date(combine_data_all$DATE_OF_BIRTH, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))

combine_data_all$trimester1_s_date = combine_data_all$DATE_OF_BIRTH - combine_data_all$trimester1_S
combine_data_all$trimester1_e_date = combine_data_all$DATE_OF_BIRTH - combine_data_all$trimester1_e
combine_data_all$trimester2_s_date = combine_data_all$DATE_OF_BIRTH - combine_data_all$trimester2_S
combine_data_all$trimester2_e_date = combine_data_all$DATE_OF_BIRTH - combine_data_all$trimester2_e
combine_data_all$trimester3_s_date = combine_data_all$DATE_OF_BIRTH - combine_data_all$trimester3_s

combine_data_all = combine_data_all[,c(1:4,11:15)]
write.csv(combine_data_all, "all_births_hurricane.csv")


