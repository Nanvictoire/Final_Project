install.packages("mvnormtest")
install.packages("car")
install.packages("IDPmisc")
install.packages("dplyr")
install.packages("rcompanion")
install.packages("ggplot2")


## Are the numbers of prevalence of antibodies similar between Hispanic and Asian?
## Are the numbers of prevalence of antibodies similar between Black and White?
## Are the numbers of prevalence of antibodies similar between the age of 16-29 years and 30-49 years?
## Are the numbers of prevalence of antibodies similar between Male and Female?


## Load Libraries

library("mvnormtest")
# test for multi variance

library("car")
# to run anova itself and other assumption

library("IDPmisc")
# to deal with missing Data

library("dplyr")

library("rcompanion")

library("ggplot2")

## Data Wrangling
## Renaming Columns in R

BloodDonorSeroprevalence3$`n Total Prevalence`

BloodDonorSeroprevalence3$`n Hispanic Prevalence`
BloodDonorSeroprevalence3$`n Asian Prevalence`
BloodDonorSeroprevalence3$`n Black Prevalence`
BloodDonorSeroprevalence3$`n White Prevalence`
BloodDonorSeroprevalence3$`n Male Prevalence`
BloodDonorSeroprevalence3$`n Female Prevalence`
BloodDonorSeroprevalence3$`n 16-29 years Prevalence`

BloodDonorSeroprevalence3$`n 30-49 years Prevalence`


BloodDonorSeroprevalence3$`n Total Prevalence`



names(BloodDonorSeroprevalence)[names(BloodDonorSeroprevalence) == "n Hispanic Prevalence"] <- "Hispanic"

names(BloodDonorSeroprevalence)[names(BloodDonorSeroprevalence) == "n Total Prevalence"] <- "nTotalPrevalence"
names(BloodDonorSeroprevalence)[names(BloodDonorSeroprevalence) == "n Asian Prevalence"] <- "Asian"
names(BloodDonorSeroprevalence)[names(BloodDonorSeroprevalence) == "n Black Prevalence"] <- "Black"
names(BloodDonorSeroprevalence)[names(BloodDonorSeroprevalence) == "n White Prevalence"] <- "White"
names(BloodDonorSeroprevalence)[names(BloodDonorSeroprevalence) == "n Male Prevalence"] <- "Male"
names(BloodDonorSeroprevalence)[names(BloodDonorSeroprevalence) == "n Female Prevalence"] <- "Female"
names(BloodDonorSeroprevalence)[names(BloodDonorSeroprevalence) == "n 30-49 years Prevalence"] <- "30-49"
names(BloodDonorSeroprevalence)[names(BloodDonorSeroprevalence) == "n 16-29 years Prevalence"] <- "16-29"

View(BloodDonorSeroprevalence)

### Make sure DVs are numeric

str(BloodDonorSeroprevalence$Hispanic)
str(BloodDonorSeroprevalence$Asian)
str(BloodDonorSeroprevalence$Black)
str(BloodDonorSeroprevalence$White)
str(BloodDonorSeroprevalence$Male)
str(BloodDonorSeroprevalence$Female)






View("BloodDonorSeroprevalence2")

View("BloodDonorSeroprevalence3")


## Subset and format as a matrix

keeps <- c("n Total Prevalence", "n Hispanic Prevalence", "n Asian Prevalence")
BloodDonorSeroprevalence3 <- BloodDonorSeroprevalence2[keeps]
View(BloodDonorSeroprevalence2)

BloodDonorSeroprevalence4 <- as.matrix(BloodDonorSeroprevalence3)

## Test Assumptions

## Sample Size

## Multivariate Normality

mshapiro.test(t(BloodDonorSeroprevalence4))



## Homogeneity of Variance

leveneTest(BloodDonorSeroprevalence$`Hispanic`, BloodDonorSeroprevalence$`nTotalPrevalence`, data=`BloodDonorSeroprevalence`)

leveneTest(BloodDonorSeroprevalence$`Asian`, BloodDonorSeroprevalence$`nTotalPrevalence`, data=`BloodDonorSeroprevalence`)


## Absence of Multicollinearity

cor.test(BloodDonorSeroprevalence$`Hispanic`, BloodDonorSeroprevalence$`Asian`, method="pearson", use="complete.obs")


## Analysis


MANOVA <- manova(cbind(`Hispanic`, `Asian`) ~ `nTotalPrevalence`, data = `BloodDonorSeroprevalence`)
summary(MANOVA)

## Post hoc


summary.aov(MANOVA, test = "wilks") 
## The prevalence between Hispanic and Asian are Significant and it violates the Homogeneity of Variance

## Computing ANOVAs with Unequal Variance (Violated Homogeneity of Variance Assumption)

ANOVA <- lm(Hispanic ~ Asian, data=BloodDonorSeroprevalence)
Anova(ANOVA, Type="II", white.adjust=TRUE)


##Computing Post Hocs When You've Violated the Assumption of Homogeneity of Variance

t.test(BloodDonorSeroprevalence$Hispanic, BloodDonorSeroprevalence$Asian, paired = TRUE, p.adjust="bonferroni", pool.sd = FALSE)

## It is very significant






## Subset and format as a matrix

keeps <- c("n Total Prevalence", "n Black Prevalence", "n White Prevalence")
BloodDonorSeroprevalence3 <- BloodDonorSeroprevalence2[keeps]
View(BloodDonorSeroprevalence2)

BloodDonorSeroprevalence4 <- as.matrix(BloodDonorSeroprevalence3)

## Test Assumptions

## Sample Size

## Multivariate Normality

mshapiro.test(t(BloodDonorSeroprevalence4))



## Homogeneity of Variance

leveneTest(BloodDonorSeroprevalence$`Black`, BloodDonorSeroprevalence$`nTotalPrevalence`, data=`BloodDonorSeroprevalence`)

leveneTest(BloodDonorSeroprevalence$`White`, BloodDonorSeroprevalence$`nTotalPrevalence`, data=`BloodDonorSeroprevalence`)


## Absence of Multicollinearity

cor.test(BloodDonorSeroprevalence$`Black`, BloodDonorSeroprevalence$`White`, method="pearson", use="complete.obs")


## Analysis


MANOVA <- manova(cbind(`Black`, `White`) ~ `nTotalPrevalence`, data = `BloodDonorSeroprevalence`)
summary(MANOVA)

## Post hoc


summary.aov(MANOVA, test = "wilks") 


## The prevalence between Black and White are Significant and it violates the Homogeneity of Variance

## Computing ANOVAs with Unequal Variance (Violated Homogeneity of Variance Assumption)

ANOVA <- lm(Black ~ White, data=BloodDonorSeroprevalence)
Anova(ANOVA, Type="II", white.adjust=TRUE)


##Computing Post Hocs When You've Violated the Assumption of Homogeneity of Variance

t.test(BloodDonorSeroprevalence$Black, BloodDonorSeroprevalence$White, paired = TRUE, p.adjust="bonferroni", pool.sd = FALSE)

## It is very significant







## Subset and format as a matrix

keeps <- c("n Total Prevalence", "n Male Prevalence", "n Female Prevalence")
BloodDonorSeroprevalence3 <- BloodDonorSeroprevalence2[keeps]
View(BloodDonorSeroprevalence2)

BloodDonorSeroprevalence4 <- as.matrix(BloodDonorSeroprevalence3)

## Test Assumptions

## Sample Size

## Multivariate Normality

mshapiro.test(t(BloodDonorSeroprevalence4))



## Homogeneity of Variance

leveneTest(BloodDonorSeroprevalence$`Male`, BloodDonorSeroprevalence$`nTotalPrevalence`, data=`BloodDonorSeroprevalence`)

leveneTest(BloodDonorSeroprevalence$`Female`, BloodDonorSeroprevalence$`nTotalPrevalence`, data=`BloodDonorSeroprevalence`)


## Absence of Multicollinearity

cor.test(BloodDonorSeroprevalence$`Male`, BloodDonorSeroprevalence$`Female`, method="pearson", use="complete.obs")


## Analysis


MANOVA <- manova(cbind(`Male`, `Female`) ~ `nTotalPrevalence`, data = `BloodDonorSeroprevalence`)
summary(MANOVA)

## Post hoc


summary.aov(MANOVA, test = "wilks") 



## The prevalence between Black and White are Significant and it violates the Homogeneity of Variance

## Computing ANOVAs with Unequal Variance (Violated Homogeneity of Variance Assumption)

ANOVA <- lm(Male ~ Female, data=BloodDonorSeroprevalence)
Anova(ANOVA, Type="II", white.adjust=TRUE)


##Computing Post Hocs When You've Violated the Assumption of Homogeneity of Variance

t.test(BloodDonorSeroprevalence$Male, BloodDonorSeroprevalence$Female, paired = TRUE, p.adjust="bonferroni", pool.sd = FALSE)


## It is very significant



























































