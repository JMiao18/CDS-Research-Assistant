data = read.csv(file = "M:/A Master of Science in Marketing Sciences/CDC Research Assistant/climate_mit_data_factor.csv", header = TRUE)
attach(data)

Employment
data$Tedious.comment = as.character(data$Tedious.comment)
data$Confusing.comment = as.character(data$Confusing.comment)
data$Additional.comment = as.character(data$Additional.comment)


names(data)
# [1] "X"                    "Serial"               "Cur_hit"              "Path"
# [5] "Personal_intention"   "Societal_intention"   "Personal_confidence"  "Societal_confidence"
# [9] "Personal_concern"     "Societal_concern"     "Part1_1"              "Part1_2"
# [13] "Part1_3"              "Part1_4"              "Part1_5"              "Part1_6"
# [17] "Part1_7"              "Part1_8"              "Part1_9"              "Part1_10"
# [21] "Part1_11"             "Part1_12"             "Part1_13"             "Part1_14"
# [25] "Part1_15"             "Part1_16"             "Part2_1"              "Part2_2"
# [29] "Part2_3"              "Part2_4"              "Part2_5"              "Part2_6"
# [33] "Part2_7"              "Part2_8"              "Part2_9"              "Part2_10"
# [37] "Part2_11"             "Part2_12"             "Part2_13"             "Part2_14"
# [41] "Part2_15"             "Part2_16"             "Gender"               "BirthYear"
# [45] "MartialStatus"        "Children"             "Race"                 "Ethnicity"
# [49] "Education"            "Salary"               "Employment"           "zipcode"
# [53] "political"            "climatechange"        "renewableenergy"      "carbondioxide"
# [57] "academicsurvey"       "academicsurveyweek"   "Confusing"            "Confusing.comment"
# [61] "Tedious"              "Tedious.comment"      "Additional.comment"   "Care"
# [65] "Fairness"             "Loyalty"              "Respect"              "Purity"
# [69] "Co_benefits"          "Feasibility"          "Consequences"         "Race_recode"
# [73] "Age"                  "MaritalStatus_recode" "Factor1"

cor(cbind(political,Care,Fairness,Loyalty,Respect,Purity))
install.packages("Hmisc")
install.packages("scales")
install.packages("colorspace")
install.packages("lazyeval")

library(lazyeval)
library(scales)
library(colorspace)
library(Hmisc)

## •	correlate political with care, fairness, loyalty, respect and purity. Make sure that you get the r value as well as the significance level.
corpearson = rcorr(cbind(political,Care,Fairness,Loyalty,Respect,Purity), type="pearson")
corspearman = rcorr(cbind(political,Care,Fairness,Loyalty,Respect,Purity), type="spearman")
## •	regress political * feasibility * consequences * co_benefits -> Factor 1 (the stars are to indicate to look at all the indicates
model1 = lm(Factor1~political*Feasibility*Consequences*Co_benefits,data = data)
summary(model1)
## •	regress political + feasibility * consequences * co_benefits -> Factor 1 (the stars are to indicate to look at all the indicates
model2 = lm(Factor1~political+Feasibility*Consequences*Co_benefits,data = data)
summary(model2)

install.packages("plyr")
install.packages("dplyr")
install.packages("stringr")
install.packages("reshape2")
install.packages("ggplot2")
install.packages("psych")
install.packages("tcltk")
install.packages("Hmisc")
install.packages("LMERConvenienceFunctions")
install.packages("GGally")
install.packages("survival")
install.packages("RODBC")
library(plyr)
library(dplyr)
library(stringr)
library(reshape2)
library(ggplot2)
library(psych)
library(tcltk)
library(LMERConvenienceFunctions)
library(GGally)
library(survival)
library(RODBC)

install.packages("M:/A Master of Science in Marketing Sciences/CDC Research Assistant/decider_1.6.tar.gz", repos=NULL, type="source")

library(decider)
mj = c("political","Care","Fairness","Loyalty","Respect","Purity")
index = c()
for (i in 1:length(mj))
{
  name = mj[i]
  index = cbind(index,grep(name, colnames(data)) )
}
splom_CDS(data[, index],plotTitle = "scatterplot matrix for political, care, fairness, loyalty, respect and purity")
