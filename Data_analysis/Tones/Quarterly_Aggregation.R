

###################################
##
## Tones 
##
####################################


Total_Tones <- read.csv("/FED-Communications-Project/Data_analysis/Tones/Total_tone_count.csv")

Master_2 <- read.csv("/FED-Communications-Project/Data_analysis/Model/Data_model_temp/Master.csv")


colnames(Total_Tones)
colnames(Master_2)
head(Total_Tones$Speech_Id)
Master_2$Speech_Id <- paste0(Master_2$author, Master_2$date)

###
### Bringing President Labels to Total Tones
###


Total_Tones$Match <- match(Total_Tones$Speech_Id, Master_2$Speech_Id)

Total_Tones$Fed_Presidents <- Master_2[Total_Tones$Match,]$Fed_President

Total_Tones$Speaker <- Master_2[Total_Tones$Match,]$author

head(Total_Tones)
table(is.na(Total_Tones$Match))


###
### Quaters Labels
###

Total_Tones$Date <- substr(Total_Tones$Speech_Id, nchar(Total_Tones$Speech_Id)-9, nchar(Total_Tones$Speech_Id))
head(Total_Tones$Date)
Total_Tones$Date <- as.Date(Total_Tones$Date)
Total_Tones$Quaters <- quarters(Total_Tones$Date)
Total_Tones$Year <- substr(Total_Tones$Date,0,4)
head(Total_Tones$Year)
Total_Tones$Year_Quarter <- paste0(Total_Tones$Year,"-", Total_Tones$Quaters)
head(Total_Tones$Year_Quarter)

###
### Quarterly Tone Aggtregation
###
Total_Tones$X <- NULL
table(Total_Tones$Speaker)
length(unique(Total_Tones$Speaker))
Quarterly_Tones_By_Position <- aggregate(Total_Tones[,2:5], list(Total_Tones$Fed_Presidents, Total_Tones$Year_Quarter), FUN = mean, na.rm=TRUE)
Quarterly_Tones_All <- aggregate(Total_Tones[,2:5], list(Total_Tones$Year_Quarter), FUN = mean, na.rm=TRUE)

write.csv(Quarterly_Tones_All, file = "/FED-Communications-Project/Data_analysis/Tones/Quarterly_Tones_All.csv", row.names = FALSE)
write.csv(Quarterly_Tones_By_Position, file = "/FED-Communications-Project/Data_analysis/Tones/Quarterly_Tones_By_Position.csv", row.names = FALSE)


