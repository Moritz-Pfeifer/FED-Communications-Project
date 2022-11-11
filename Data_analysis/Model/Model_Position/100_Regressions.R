
###
### Modeling little Guy Big Guy to Increase number of observations
###

setwd("/FED-Communications-Project")

Master_1 <- read.csv("/FED-Communications-Project/Data_analysis/Tones/Tone_data_model.csv")

colnames(Master_1)[1] <- c("Quarter_Year")

Master_1 <- Master_1[1:224,1:10]
colnames(Master_1)


Little_Guy <- read.csv("/FED-Communications-Project/Data_analysis/Tones/Data_With_Position.csv")
colnames(Little_Guy)
colnames(Little_Guy)[1]<- c("Position")
colnames(Little_Guy)[2]<- c("Date")


Little_Guy$Match <- match(Little_Guy$Date, Master_1$Quarter_Year)

Little_Guy$Spread.Households <- Master_1[Little_Guy$Match,]$Spread.Households
Little_Guy$Spread.Firms <- Master_1[Little_Guy$Match,]$Spread.Firms
Little_Guy$Spread.Banks <- Master_1[Little_Guy$Match,]$Spread.Banks
Little_Guy$Spread.Government <- Master_1[Little_Guy$Match,]$Spread.Government


Little_Guy$Periods <- Master_1[Little_Guy$Match,]$Periods
Little_Guy$Position

Little_Guy$Period_Position <- paste0(Little_Guy$Periods,Little_Guy$Position)


colnames(Little_Guy)
table(Little_Guy$Periods)

Great_Inflation <- subset(Little_Guy, Little_Guy$Periods== "Post-Crisis and after")
rownames(Great_Inflation)<- NULL


Great_Inflation <- Great_Inflation[with(Great_Inflation, order(Great_Inflation$Date, decreasing = FALSE)),]
rownames(Great_Inflation)<- NULL

Great_Inflation$Lead_Helper <- as.numeric(rownames(Great_Inflation))+2
Great_Inflation$Tone_Household_Lead_1 <- Great_Inflation[Great_Inflation$Lead_Helper,]$Tone_Household


Check_1 <- Great_Inflation[,c("Position", "Date","Tone_Household","Tone_Household_Lead_1")]

colnames(Great_Inflation)[3]<- c("Lag_Tone_Household")

Check_1 <- Great_Inflation[,c("Position", "Date","Lag_Tone_Household","Tone_Household_Lead_1")]
rm(Check_1)


Great_Inflation$Position_Lag_1 <- Great_Inflation[Great_Inflation$Lead_Helper,]$Position

Check_1 <- Great_Inflation[,c("Position", "Position_Lag_1", "Date","Lag_Tone_Household","Tone_Household_Lead_1")]


table(Little_Guy$Periods)
Model_1<- lm(Spread.Households~ Position*Lag_Tone_Household,data = Great_Inflation[3:53,])

summary(Model_1)
Model_1$coefficients[3]

lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[3:53,])$coefficients[c(3,4)]

Coeff_Results <- as.data.frame(c(1:108))
colnames(Coeff_Results)[1]<- c("Model_Number")
Coeff_Results$Estimated_Beta_Yes <- paste("")
Coeff_Results$Estimated_Beta_No <- paste("")


#rm(Coeff_Results)

Coeff_Results$Estimated_Beta_Yes[1]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[1:51,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[2]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[2:52,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[3]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[3:53,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[4]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[4:54,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[5]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[5:55,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[6]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[6:56,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[7]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[7:57,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[8]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[8:58,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[9]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[9:59,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[10]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[10:60,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[11]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[11:61,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[12]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[12:62,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[13]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[13:63,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[14]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[14:64,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[15]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[15:65,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[16]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[16:66,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[17]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[17:67,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[18]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[18:68,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[19]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[19:69,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[20]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[20:70,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[21]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[21:71,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[22]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[22:72,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[23]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[23:73,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[24]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[24:74,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[25]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[25:75,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[26]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[26:76,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[27]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[27:77,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[28]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[28:78,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[29]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[29:79,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[30]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[30:80,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[31]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[31:81,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[32]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[32:82,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[33]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[33:83,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[34]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[34:84,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[35]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[35:85,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[36]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[36:86,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[37]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[37:87,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[38]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[38:88,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[39]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[39:89,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[40]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[40:90,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[41]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[41:91,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[42]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[42:92,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[43]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[43:93,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[44]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[44:94,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[45]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[45:95,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[46]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[46:96,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[47]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[47:97,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[48]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[48:98,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[49]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[49:99,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[50]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[50:100,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[51]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[51:101,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[52]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[52:102,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[53]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[53:103,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[54]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[54:104,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[55]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[55:105,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[56]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[56:106,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[57]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[57:107,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[58]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[58:108,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[59]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[59:109,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[60]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[60:110,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[61]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[61:111,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[62]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[62:112,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[63]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[63:113,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[64]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[64:114,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[65]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[65:115,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[66]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[66:116,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[67]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[67:117,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[68]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[68:118,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[69]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[69:119,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[70]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[70:120,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[71]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[71:121,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[72]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[72:122,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[73]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[73:123,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[74]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[74:124,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[75]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[75:125,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[76]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[76:126,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[77]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[77:127,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[78]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[78:128,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[79]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[79:129,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[80]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[80:130,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[81]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[81:131,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[82]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[82:132,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[83]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[83:133,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[84]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[84:134,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[85]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[85:135,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[86]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[86:136,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[87]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[87:137,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[88]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[88:138,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[89]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[89:139,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[90]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[90:140,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[91]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[91:141,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[92]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[92:142,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[93]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[93:143,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[94]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[94:144,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[95]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[95:145,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[96]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[96:146,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[97]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[97:147,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[98]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[98:148,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[99]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[99:149,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[100]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[100:150,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[101]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[101:151,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[102]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[102:152,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[103]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[103:153,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[104]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[104:154,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[105]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[105:155,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[106]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[106:156,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[107]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[107:157,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[108]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[108:158,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[109]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[109:159,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[110]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[110:160,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[111]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[111:161,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[112]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[112:162,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[113]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[113:163,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[114]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[114:164,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[115]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[115:165,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[116]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[116:166,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[117]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[117:167,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[118]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[118:168,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[119]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[119:169,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[120]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[120:170,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[121]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[121:171,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[122]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[122:172,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[123]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[123:173,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[124]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[124:174,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[125]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[125:175,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[126]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[126:176,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[127]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[127:177,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[128]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[128:178,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[129]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[129:179,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[130]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[130:180,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[131]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[131:181,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[132]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[132:182,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[133]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[133:183,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[134]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[134:184,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[135]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[135:185,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[136]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[136:186,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[137]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[137:187,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[138]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[138:188,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[139]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[139:189,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[140]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[140:190,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[141]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Inflation[141:191,])$coefficients[4]

mean(as.numeric(Coeff_Results$Estimated_Beta_Yes[1:104]))
mean(as.numeric(Coeff_Results$Estimated_Beta_No[1:104]))

t.test(as.numeric(Coeff_Results$Estimated_Beta_Yes[1:104]), alternative = "two.sided")

t.test(as.numeric(Coeff_Results$Estimated_Beta_No[1:104]), alternative = "two.sided")


hist(Coeff_Results$Estimated_Beta_No, breaks = 20)




###
### Lag_2 Model Households
###


Great_Inflation$Lead_Helper <- as.numeric(rownames(Great_Inflation))+2
Great_Inflation$Tone_Household_Lead_1 <- Great_Inflation[Great_Inflation$Lead_Helper,]$Tone_Household


Check_1 <- Great_Inflation[,c("Position", "Date","Tone_Household","Tone_Household_Lead_1")]

colnames(Great_Inflation)[3]<- c("Lag_Tone_Household")

Check_1 <- Great_Inflation[,c("Position", "Date","Lag_Tone_Household","Tone_Household_Lead_1")]
rm(Check_1)














