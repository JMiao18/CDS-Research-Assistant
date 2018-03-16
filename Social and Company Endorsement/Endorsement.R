data = readRDS(file="M:/A Master of Science in Marketing Sciences/CDC Research Assistant/HealthScreening2_1.rds")
attach(data)
defaultchoice = as.numeric(defaultchoice)
## regress the default column -> choice column (and make a bar graph of this).

lm(choice~default)

counts <- table(choice,default)
barplot(counts, main="Barplot for Default versus Choice",
        xlab="default", col=c("darkblue","red"),
        ylim=c(0,1000))
legend("topright", legend = c("choice = 0","choice = 1"), fill = c("darkblue","red"))

## regress socialendorsement and company endorsement -> choice (look at them both in individual models and in the same model)
socialendorsement = as.numeric(socialendorsement)
companyendorsement = as.numeric(companyendorsement)

lm(choice~socialendorsement)
lm(choice~companyendorsement)
lm(choice~socialendorsement + companyendorsement)

## regress socialendorsement and company endorsement -> defaultchoice (look at them both in individual models and in the same model)
defaultchoice = as.numeric(defaultchoice)
lm(defaultchoice~socialendorsement)
lm(defaultchoice~companyendorsement)
lm(defaultchoice~socialendorsement + companyendorsement)

## regress endorsementcondition -> choice
lm(choice~endorsementcondition)

## regress endorsementcondition-> defaultchoice
lm(defaultchoice~endorsementcondition)

## subset the data based on the type column to only the "natural" observations. Then regress smrd -> choice, baltho -> choice, default -> smrd, default -> baltho, smrd-> defaultchoice, baltho -> defaultchoice

natural = subset(data, type == "natural")
attach(natural)
smrd = as.numeric(smrd)
baltho = as.numeric(baltho)
defaultchoice = as.numeric(natural$defaultchoice)

model10 = lm(choice~smrd)
model11 = lm(choice~baltho)
screenreg(list(model10, model11))

model12 = lm(smrd~default)
model13 = lm(baltho~default)
screenreg(list(model12, model13))

model14 = lm(smrd~defaultchoice)
model15 = lm(baltho~defaultchoice)
screenreg(list(model14, model15))
texreg::htmlreg(l = list(model4, model5), center = TRUE, stars = c(0.001, 0.01, 0.05, 0.1),bold = 0.1, caption = "Effects Of Variables on Factor1", caption.above = TRUE, digits = 4)

## regress different demographics on choice (age, gender, race, ethnicity, etc.) This can be done in all one model.
attach(data)
Age = as.numeric(data$Age)
Gender = as.factor(data$Gender)
Marital = as.factor(data$Marital)
Children = as.numeric(data$Children)
Ethnicity = as.factor(data$Ethnicity)
## Education = as.factor(Education)
data$BMI
Exercise = as.numeric(data$Exercise)
## Should "Exercise" be as.numeic or as.factor?
Smoke = as.factor(data$Smoke)
Own_Health = as.numeric(data$Own_Health)
## Should "OwnHealth" be as.numeic or as.factor?
Family = as.numeric(data$Family)
Friend = as.numeric(data$Friend)
Career = as.numeric(data$Career)
PersonalHealth = as.numeric(data$PersonalHealth)
Money = as.numeric(data$Money)

model16 = lm(choice~Age + Gender + Marital + Children + Ethnicity + BMI + Exercise + Smoke + Own_Health + Family + Friend + Career + PersonalHealth + Money)
screenreg(model16)
summary(model16)

model17 = lm(data$defaultchoice~Age + Gender + Marital + Children + Ethnicity + BMI + Exercise + Smoke + Own_Health + Family + Friend + Career + PersonalHealth + Money)
screenreg(model17)
summary(model17)

## Regress type*default -> baltho
attach(data)
data$baltho = as.numeric(data$baltho)
type = as.factor(type)
default = as.factor(default)
model18 = lm(data$baltho~type + default + type * default)
summary(model18)

force = subset(data, type == "forced")
attach(force)
fit <- aov(baltho ~ default, data = force)
summary(fit)

install.packages("xtable")


### mediation Analysis
library(mediation)

