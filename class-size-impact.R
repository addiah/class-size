STARdata <- read.csv(file.choose())

## Race = Student's race
## Classtype = Type of kindergarten class size (regular, small, reg w/ aid)
## g4math = Math scores at grade 4
## g4reading = Reading scores at grade 4
## yearssmall = Number of years assigned to small class
## hsgrad = Whether or not the student graduated highschool

View(STARdata)
summary(STARdata)
dim(STARdata)

## Label the levels appropriately
STARdata$kinder <- ifelse(STARdata$classtype == "1", "small", 
                          ifelse(STARdata$classtype == "2", "regular", "aid"))

## Label demographics
STARdata$race <- as.factor(STARdata$race)
levels(STARdata$race)[1] <- "White"
levels(STARdata$race)[2] <- "Black"
levels(STARdata$race)[4] <- "Hispanic"
levels(STARdata$race)[c(3,5,6)] <- "other"
levels(STARdata$race)

STAR.small <- subset(STARdata, subset = (STARdata$kinder == "small"))
summary(STAR.small)

STAR.reg <- subset(STARdata, subset = (STARdata$kinder == "regular"))
summary(STAR.reg)

STAR.aid <- subset(STARdata, subset = (STARdata$kinder == "aid"))
summary(STAR.aid)

mean(STAR.small$g4math, na.rm = TRUE) - 
  mean(STAR.reg$g4math, na.rm = TRUE)
## On average, students in small classes (mean = ~709.2) 
## performed ~0.3 pts lower than their average class 
## size (mean = ~709.5) peers. 

mean(STAR.small$g4reading, na.rm = TRUE) - 
  mean(STAR.reg$g4reading, na.rm = TRUE)
## On average, students in small classes (mean = ~723.4) 
## performed ~3.5 pts better than their average class 
## size (mean = 719.9) peers.

sd(STAR.small$g4math, na.rm = TRUE) # ~41
sd(STAR.reg$g4math, na.rm = TRUE) # ~43.5

sd(STARdata$g4reading, na.rm = TRUE) #~52.4
sd(STARdata$g4math, na.rm = TRUE) #~43.1

quantile(STAR.small$g4math, na.rm = TRUE, probs = seq(0,1,0.33)) -
  quantile(STAR.reg$g4math, na.rm = TRUE, probs = seq(0,1,0.33)) 
## 0% 33% 66% 99% 
## 0  -2   2   0 

quantile(STAR.small$g4reading, na.rm = TRUE, probs = seq(0,1,0.33)) -
  quantile(STAR.reg$g4reading, na.rm = TRUE, probs = seq(0,1,0.33))
## 0% 33% 66% 99% 
## 0   0   1   0 

quantile(STAR.aid$g4math, na.rm = TRUE, probs = seq(0,1,0.33)) - 
  quantile(STAR.reg$g4math, na.rm = TRUE, probs = seq(0,1,0.33))
## 0% 33% 66% 99% 
## 0   0   1   0 

quantile(STAR.aid$g4reading, na.rm = TRUE, probs = seq(0,1,0.33)) - 
  quantile(STAR.reg$g4reading, na.rm = TRUE, probs = seq(0,1,0.33)) 
## 0% 33% 66% 99% 
## 0   0  -2   0 

## The differences appear to be only a few points,
## combined with the small difference in means, compared 
## to sd, it suggests that small class size does not improve
## tests scores later on under these circumstances.

prop.table(table(STARdata$yearssmall, STARdata$kinder))
##           aid     regular       small
## 0 0.315573123 0.310039526 0.000000000
## 1 0.015335968 0.015019763 0.091067194
## 2 0.009486166 0.009169960 0.043003953
## 3 0.012332016 0.012648221 0.030830040
## 4 0.000000000 0.000000000 0.135494071

## Compare by years small
tapply(STARdata$g4math, STARdata$yearssmall, mean, na.rm = TRUE)
##        0        1        2        3        4 
## 707.9793 707.5524 711.9140 709.6170 710.0519 

tapply(STARdata$g4math, STARdata$yearssmall, median, na.rm = TRUE)
##   0   1   2   3   4 
## 710 709 714 712 711 

tapply(STARdata$g4reading, STARdata$yearssmall, mean, na.rm = TRUE)
##        0        1        2        3        4 
## 719.8754 723.1471 717.8681 719.8986 724.6651 

tapply(STARdata$g4reading, STARdata$yearssmall, median, na.rm = TRUE)
##     0     1     2     3     4 
## 722.0 724.5 720.0 721.0 726.0 

## Compare achievement and retention of class-size benefits by race
summary(STAR.small$race)
## White    Black   other Hispanic     NA's 
## 1294      593        8        4        1 
summary(STAR.reg$race)
## White    Black   other Hispanic     NA's 
## 1472      710       10        0        2 
    
tapply(STARdata$g4math, STARdata$race == "White", mean, na.rm = TRUE)
## White students scored on average ~15 pts higher
## across all class-sizes than minority students

tapply(STARdata$g4reading, STARdata$race == "White", mean, na.rm = TRUE)
## White students scored on average ~32 pts higher 
## across all class-sizes than minority students

mean(STAR.small$g4math[STAR.small$race == "White"], na.rm = TRUE) - 
  mean(STAR.small$g4math[STAR.small$race != "White"], na.rm = TRUE)
## White students scored ~12.5 pts higher than  
## their minority small-class peers in math

mean(STAR.small$g4reading[STAR.small$race == "White"], na.rm = TRUE) - 
  mean(STAR.small$g4reading[STAR.small$race != "White"], na.rm = TRUE)
## White students scored ~27.4 pts higher than  
## their minority small-class peers in reading

summary(STAR.reg$race)
## White    Black   other Hispanic     NA's 
## 1472      710       10        0        2 

mean(STAR.reg$g4math[STAR.reg$race == "White"], na.rm = TRUE) - 
  mean(STAR.reg$g4math[STAR.reg$race != "White"], na.rm = TRUE)
## White students scored ~12.4 pts higher than  
## their minority regular-class peers in math

mean(STAR.reg$g4reading[STAR.reg$race == "White"], na.rm = TRUE) - 
  mean(STAR.reg$g4reading[STAR.reg$race != "White"], na.rm = TRUE)
## White students scored ~34.1 pts higher than  
## their minority regular-class peers in reading

tapply(STAR.small$g4math, STAR.small$race, mean, na.rm = TRUE) - 
  tapply(STAR.reg$g4math, STAR.reg$race, mean, na.rm = TRUE)
##      White     Black    other  Hispanic 
## -0.220348 -1.027910 15.000000        NA 
##
## Small class size in kindergarten does not appear
## to have a major impact on grade 4 math scores for
## White or Black students, sample size is too small
## to draw conclusions for other minorities

tapply(STAR.small$g4reading, STAR.small$race, mean, na.rm = TRUE) - 
  tapply(STAR.reg$g4reading, STAR.reg$race, mean, na.rm = TRUE)
##     White     Black    other  Hispanic 
## 2.722997  9.259196 28.000000        NA
##
## Small class size in kindergarten seems to have some
## benefit on grade 4 reading scores for minority students

tapply(STARdata$hsgrad, STARdata$kinder, mean, na.rm = TRUE)
##        aid   regular     small 
## 0.8392857 0.8251619 0.8359202 

tapply(STARdata$hsgrad, STARdata$yearssmall, mean, na.rm = TRUE)
##        0         1         2         3          4
## 0.8286020 0.7910448 0.8131868 0.8324607 0.8775510 
##
## Graduation rates are somewhat improved by spending 
## all four observed years in small classes

## Checking for any racial gap in graduation rates
tapply(STARdata$hsgrad[STARdata$race == "White"], 
       STARdata$kinder[STARdata$race == "White"], mean, na.rm = TRUE) -
  tapply(STARdata$hsgrad[STARdata$race != "White"], 
         STARdata$kinder[STARdata$race != "White"], mean, na.rm = TRUE)
##       aid   regular     small 
## 0.1413746 0.1181304 0.1195707 
##
## White students graduate at a higher rate 
## regardless of kindergarten class size

tapply(STARdata$hsgrad[STARdata$race == "White"], 
       STARdata$yearssmall[STARdata$race == "White"], mean, na.rm = TRUE) -
  tapply(STARdata$hsgrad[STARdata$race != "White"], 
         STARdata$yearssmall[STARdata$race != "White"], mean, na.rm = TRUE)
##          0          1          2          3          4 
## 0.13804217 0.06111111 0.09706514 0.12804878 0.11859053
##
## White students still graduate at a similar higher rate 
## even when students are assigned to small class sizes for
## all four observed years. Perhaps this is because teachers
## bias their attention towards White students, which is a 
## known factor in student outcomes.
