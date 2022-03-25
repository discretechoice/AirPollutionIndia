###############################################################################
#   
# Mariel P., Khan M.A. and Meyerhoff, J. (2022). Valuing individuals' preferences 
# for air quality improvement: Evidence from a discrete choice experiment in South Delhi. 
# Economic Analysis and Policy. https://doi.org/10.1016/j.eap.2022.03.006
#
#  Dataset: Data_AirPollution_SouthDelhi_2019.csv
# 
#  Date: 14-12-2021
#
#  

# Preliminaries
rm(list = ls(all = TRUE))             # Clear workspace
Sys.setenv(LANG = "en")               # Set language
setwd(".")                            # Set working directory             
# set.seed(12345)

# Loading R packages
# install.packages("https://cran.r-project.org/src/contrib/Archive/mlogit/mlogit_1.0-2.tar.gz", repos=NULL,type="source")
library(mded)       # Performs Poe test for comparing distributions
library(maxLik)     # maximum simulated likelihood method for the
library(gmnl)       # estimation of multinomial logit models with random coefficients
library(mlogit)     # enables the estimation of the multinomial logit

# Read database
data.MNL <- read.delim("Data_AirPollution_SouthDelhi_2019.csv",
                        header=TRUE,
                        stringsAsFactors=FALSE,
                        sep = ",", 
                        na.strings = "NA")

# Dropping out individuals with NA in choices

individuals.with.NA.in.choice        <- unique(data.MNL$id.individual[which(is.na(data.MNL$Q12.choice))])
num.of.individuals.with.NA.in.choice <-  length(individuals.with.NA.in.choice)

for(i in seq(1:num.of.individuals.with.NA.in.choice)){
      one.inidividual <- which(data.MNL$id.individual == individuals.with.NA.in.choice[i])
      data.MNL        <- data.MNL[-one.inidividual,]
                          }
# Setting parameters
n.choices           <- 5                                        # Number of choice occasions of one individual 
n.individuals       <- length(data.MNL$id.individual)/n.choices # Number of individuals

#   Re-code education >3 to education == 3
#   Re-code income   > 3 to income == 3
data.MNL$Q19.educ[data.MNL$Q19.educ     > 3]     <- 3
data.MNL$Q22.income[data.MNL$Q22.income > 3]     <- 3

cat("*************************************************************","\n",
    "NUMBER OF INDIVIDUALS IN THE SAMPLE:", length(data.MNL$id.individual)/5, "\n",
    "*************************************************************")

#
# Reshaping data from wide to long format using mlogit.data from mlogit package
#
data.MNL.mlogit <- mlogit.data(data.MNL,
                               id.var  = "id.individual",
                               choice  = "Q12.choice",
                               varying = 11:26,            # columns containing attributes
                               shape   = "wide",           # wide or long shape of the data
                               sep     = ".")              # how the number of alternative is separated from the attribute name

# Creating variables for ASCs
data.MNL.mlogit$asc1 <- as.numeric(data.MNL.mlogit$alt == 1)
data.MNL.mlogit$asc2 <- as.numeric(data.MNL.mlogit$alt == 2)
data.MNL.mlogit$asc3 <- as.numeric(data.MNL.mlogit$alt == 3)

####################################################################
# Creating vectors of socio-demographic for allocation function
####################################################################
individuals.in.the.sample <- unique(data.MNL$id.individual)

vector.id.individual <- data.MNL[which(data.MNL$choice.card == 1)   ,"id.individual"]
vector.gender.female <- data.MNL[which(data.MNL$choice.card == 1)   ,"Q17.gender.female"]
vector.age           <- data.MNL[which(data.MNL$choice.card == 1)   ,"Q18.age"]
vector.educ          <- data.MNL[which(data.MNL$choice.card == 1)   ,"Q19.educ"]
vector.income        <- data.MNL[which(data.MNL$choice.card == 1)   ,"Q22.income"]
vector.household     <- data.MNL[which(data.MNL$choice.card == 1)   ,"Q23.household"]

###################################################
#
#  DEFINITION OF WEIGHTS by education and income
#
###################################################
table(vector.educ, vector.income)
#
#   Real Data (nº of observations)
#                   income
#                  1   2   3  
#              1  34  13   1       
#       educ   2  25  45   6      
#              3  33 190 132      
#
educ.income.matrix.data      <- matrix(c(34, 13,  1,
                                         25, 45,  6,
                                         33,190,132),
                                       nrow=3,
                                       ncol=3,
                                       byrow = TRUE)

#   Expected Data in South Delhi (%)
#                      income
#                   1    2    3
#              1   10    8    3
#       educ   2   11    9    9
#              3   13   15   22  
#
#   Expected shares (to be changed for other sites)
educ.income.matrix.exp      <- matrix(c(10, 8, 3,
                                        11, 9, 9,
                                        13,15,22),
                                      nrow=3,
                                      ncol=3,
                                      byrow = TRUE)

educ.income.matrix.exp.rel  <- educ.income.matrix.exp/sum(educ.income.matrix.exp)
educ.income.matrix.data.rel <- educ.income.matrix.data/sum(educ.income.matrix.data)
educ.income.matrix.weights  <- educ.income.matrix.exp.rel/educ.income.matrix.data.rel

data.MNL.mlogit$reweight.educ.income <-  (data.MNL.mlogit$Q19.educ == 1)*(data.MNL.mlogit$Q22.income == 1)*educ.income.matrix.weights[1,1] +
                                         (data.MNL.mlogit$Q19.educ == 1)*(data.MNL.mlogit$Q22.income == 2)*educ.income.matrix.weights[1,2] +
                                         (data.MNL.mlogit$Q19.educ == 1)*(data.MNL.mlogit$Q22.income == 3)*educ.income.matrix.weights[1,3] +
                                         (data.MNL.mlogit$Q19.educ == 2)*(data.MNL.mlogit$Q22.income == 1)*educ.income.matrix.weights[2,1] +
                                         (data.MNL.mlogit$Q19.educ == 2)*(data.MNL.mlogit$Q22.income == 2)*educ.income.matrix.weights[2,2] +
                                         (data.MNL.mlogit$Q19.educ == 2)*(data.MNL.mlogit$Q22.income == 3)*educ.income.matrix.weights[2,3] +
                                         (data.MNL.mlogit$Q19.educ == 3)*(data.MNL.mlogit$Q22.income == 1)*educ.income.matrix.weights[3,1] +
                                         (data.MNL.mlogit$Q19.educ == 3)*(data.MNL.mlogit$Q22.income == 2)*educ.income.matrix.weights[3,2] +
                                         (data.MNL.mlogit$Q19.educ == 3)*(data.MNL.mlogit$Q22.income == 3)*educ.income.matrix.weights[3,3] 

data.MNL.mlogit$reweight.educ.income  <- data.MNL.mlogit$reweight.educ.income /mean(data.MNL.mlogit$reweight.educ.income ,na.rm=TRUE)     

##################
# LCM  3 CLASSES
################

starting.values.CL3 <- c(
                           -1.9534419, #               class.1.asc1
                           -2.6595263, #               class.1.asc2
                           -2.3755695, #               class.1.asc3
                            0.1429156, #               class.1.infmortality
                           -0.1748442, #               class.1.reducedvis
                            0.3220639, #               class.1.morbidity
                           -0.0314551, #               class.1.cost
                            8.9676431, #               class.2.asc1
                            8.5610240, #               class.2.asc2
                            7.6029319, #               class.2.asc3
                           -0.7218080, #               class.2.infmortality
                            0.0601118, #               class.2.reducedvis
                           -0.3194217, #               class.2.morbidity
                           -0.6457496, #               class.2.cost
                            3.7992773, #               class.3.asc1
                            3.4710042, #               class.3.asc2
                            3.5312982, #               class.3.asc3
                           -0.1315399, #               class.3.infmortality
                           -0.0023395, #               class.3.reducedvis
                           -0.1432024, #               class.3.morbidity
                           -0.0133263, #               class.3.cost
                            0.5835124, #               (class)2
                           -1.0602037, #               (class)3
                            0.6511180, #               Q17.gender.female:class2
                            0.8762000, #               Q17.gender.female:class3
                           -0.0194365, #               class2:Q18.age
                           -0.0244282, #               class3:Q18.age
                           -0.6655715, #               class2:Q19.educ
                            0.6703864, #               class3:Q19.educ
                            1.0931102, #               class2:Q22.income
                            0.9067071  #               class3:Q22.income
                                    )

LCM.output      <- gmnl(Q12.choice ~  asc1
                                    + asc2
                                    + asc3
                                    + infmortality
                                    + reducedvis
                                    + morbidity
                                    + cost
                                    | 0 | 0 | 0 | (  Q17.gender.female
                                                   + Q18.age
                                                   + Q19.educ
                                                   + Q22.income
                                                    + 1 ),
                        data        = data.MNL.mlogit,
                        model       = 'lc',
                        panel       = TRUE,
                        print.init  = TRUE,
                        print.level = TRUE,
                        method      = "NM",
                        iterlim     = 10000,
                        start       = starting.values.CL3,
                        Q = 3,                           # number of classes
                        weights = reweight.educ.income)

summary(LCM.output)

######################################
#  Table 6. LCM parameter estimates
######################################
# 
# The estimation took: 0h:0m:55s 
# 
# Coefficients:
#                           Estimate Std. Error z-value  Pr(>|z|)    
# class.1.asc1             -1.9518607  1.5466991 -1.2620 0.2069659    
# class.1.asc2             -2.6643604  1.5188109 -1.7542 0.0793892 .  
# class.1.asc3             -2.3766580  1.5590215 -1.5245 0.1273951    
# class.1.infmortality      0.1446459  0.0874328  1.6544 0.0980529 .  
# class.1.reducedvis       -0.1761201  0.0689117 -2.5557 0.0105964 *  
# class.1.morbidity         0.3254774  0.1563309  2.0820 0.0373445 *  
# class.1.cost             -0.0311917  0.0144489 -2.1588 0.0308694 *  
# class.2.asc1              8.9386388  9.3316738  0.9579 0.3381225    
# class.2.asc2              8.5853110 10.0103015  0.8576 0.3910871    
# class.2.asc3              7.5942787 10.0050118  0.7590 0.4478242    
# class.2.infmortality     -0.7233132  0.6218543 -1.1632 0.2447665    
# class.2.reducedvis        0.0638970  0.3587093  0.1781 0.8586206    
# class.2.morbidity        -0.3183765  0.4715589 -0.6752 0.4995757    
# class.2.cost             -0.6443074  0.1033831 -6.2322 4.598e-10 ***
# class.3.asc1              3.8013014  0.5841140  6.5078 7.626e-11 ***
# class.3.asc2              3.4724782  0.5941731  5.8442 5.089e-09 ***
# class.3.asc3              3.5326209  0.5879859  6.0080 1.878e-09 ***
# class.3.infmortality     -0.1313429  0.0137245 -9.5699 < 2.2e-16 ***
# class.3.reducedvis       -0.0023565  0.0090714 -0.2598 0.7950376    
# class.3.morbidity        -0.1431378  0.0270280 -5.2959 1.184e-07 ***
# class.3.cost             -0.0133429  0.0018238 -7.3159 2.558e-13 ***
# (class)2                  0.5598104  0.3495455  1.6015 0.1092578    
# (class)3                 -1.0533443  0.3375879 -3.1202 0.0018072 ** 
# Q17.gender.female:class2  0.6615147  0.1660690  3.9834 6.794e-05 ***
# Q17.gender.female:class3  0.8842566  0.1531519  5.7737 7.754e-09 ***
# class2:Q18.age           -0.0193339  0.0053846 -3.5906 0.0003299 ***
# class3:Q18.age           -0.0245981  0.0049862 -4.9332 8.088e-07 ***
# class2:Q19.educ          -0.6582393  0.1084211 -6.0711 1.270e-09 ***
# class3:Q19.educ           0.6682066  0.0981294  6.8094 9.798e-12 ***
# class2:Q22.income         1.0927765  0.1109528  9.8490 < 2.2e-16 ***
# class3:Q22.income         0.9079245  0.1002350  9.0580 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1
# 
# Optimization of log-likelihood by Nelder-Mead maximization
# Log Likelihood: -2034.9
# Number of observations: 2395
# Number of iterations: 2118
# Exit of MLE: successful convergence 

##############################################################################################################
# Creating allocation function
##############################################################################################################

alloc.fun.CL2     <- ( coef(LCM.output)["(class)2"]  
                     + coef(LCM.output)["Q17.gender.female:class2"] * vector.gender.female
                     + coef(LCM.output)["class2:Q18.age"]           * vector.age
                     + coef(LCM.output)["class2:Q19.educ"]          * vector.educ
                     + coef(LCM.output)["class2:Q22.income"]        * vector.income
                    #+ LCM.output$coefficients[20] * vector.household 
                   )
                    
alloc.fun.CL3     <- ( coef(LCM.output)["(class)3"]  
                       + coef(LCM.output)["Q17.gender.female:class3"] * vector.gender.female
                       + coef(LCM.output)["class3:Q18.age"]           * vector.age
                       + coef(LCM.output)["class3:Q19.educ"]          * vector.educ
                       + coef(LCM.output)["class3:Q22.income"]        * vector.income
                       #+ LCM.output$coefficients[20] * vector.household 
)


prob.class1   <- exp(0)            /(exp(0) + exp(alloc.fun.CL2) + exp(alloc.fun.CL3) )
prob.class2   <- exp(alloc.fun.CL2)/(exp(0) + exp(alloc.fun.CL2) + exp(alloc.fun.CL3) )
prob.class3   <- exp(alloc.fun.CL3)/(exp(0) + exp(alloc.fun.CL2) + exp(alloc.fun.CL3) )

#####################################################################
# Figure 3. Allocation probabilities obtained from the LCM estimation
#####################################################################
par(oma=c(0,0,0,0))  
par(mar=c(4,1,2,1))  

layout(1)
boxplot(prob.class1   , 
        prob.class2   ,  
        prob.class3   ,
        horizontal=TRUE,
        col=c("grey20",
              "grey50",
              "grey70"),
        outline=FALSE,
        xlab=" ",
        main = "Allocation Probabilities")

legend (0.69,1.1, c( "Class 3",
                     "Class 2",
                     "Class 1"),
        fill=c("grey70", "grey50","grey20"),
        cex = .90)

###############################################
# wtp_infmortality
#
# multiplied by 10 because cost divided by 10 
# in the data due to the maximization process
###############################################

wtp.infmortality.cl1 <- 10*(coef(LCM.output)["class.1.infmortality"] / coef(LCM.output)["class.1.cost"]  )
wtp.infmortality.cl2 <- 10*(coef(LCM.output)["class.2.infmortality"] / coef(LCM.output)["class.2.cost"]  )
wtp.infmortality.cl3 <- 10*(coef(LCM.output)["class.3.infmortality"] / coef(LCM.output)["class.3.cost"]  )

wtp.infmortality <-  (   prob.class1 * wtp.infmortality.cl1  
                      +  prob.class2 * wtp.infmortality.cl2
                      +  prob.class3 * wtp.infmortality.cl3 )

###############################################
# wtp_reducvis (10 days)  (cost rescaled)
###############################################
wtp.reducedvis.cl1 <- 10*10*(coef(LCM.output)["class.1.reducedvis"] / coef(LCM.output)["class.1.cost"]  )
wtp.reducedvis.cl2 <- 10*10*(coef(LCM.output)["class.2.reducedvis"] / coef(LCM.output)["class.2.cost"]  )
wtp.reducedvis.cl3 <- 10*10*(coef(LCM.output)["class.3.reducedvis"] / coef(LCM.output)["class.3.cost"]  )

wtp.reducedvis <-  (        prob.class1 * wtp.reducedvis.cl1  
                         +  prob.class2 * wtp.reducedvis.cl2
                         +  prob.class3 * wtp.reducedvis.cl3 )
###############################################
# WTP morbidity  (cost rescaled)
###############################################
wtp.morbidity.cl1 <- 10*(coef(LCM.output)["class.1.morbidity"] / coef(LCM.output)["class.1.cost"]  )
wtp.morbidity.cl2 <- 10*(coef(LCM.output)["class.2.morbidity"] / coef(LCM.output)["class.2.cost"]  )
wtp.morbidity.cl3 <- 10*(coef(LCM.output)["class.3.morbidity"] / coef(LCM.output)["class.3.cost"]  )

wtp.morbidity <-  (         prob.class1 * wtp.morbidity.cl1  
                         +  prob.class2 * wtp.morbidity.cl2
                         +  prob.class3 * wtp.morbidity.cl3 )

########################################################
# Figure 4. WTP value distributions obtained from LCM
########################################################

layout(1)
boxplot(wtp.infmortality   , 
        wtp.reducedvis   , 
        wtp.morbidity ,
        horizontal=TRUE,
        col=c("grey20",
              "grey50",
              "grey70"),
        outline=FALSE,
        xlab="Indian rupees",
        main = "WTP Distributions",
        cex.main = 0.8,
        cex.axis = 0.8)


legend (x = c(87, 165), y = c(0.5, 1.0),
        c( "Morbidity (decline of 1,000 cases) ",
           "Red.visibility (decline of 10 days)",
           "Inf.mortality (decline of 1,000 deaths)"),
        fill=c("grey70", "grey50",  "grey20"),
        cex = 0.7)

###############################################
# Table 7
###############################################
summary(wtp.infmortality)
summary(wtp.reducedvis)
summary( wtp.morbidity)
sd(wtp.infmortality, na.rm = TRUE)
sd(wtp.reducedvis, na.rm = TRUE)
sd(wtp.morbidity, na.rm = TRUE)

###############################################
# WTP for SUBGROUPS
###############################################

###############################################
# Differences in AGE
###############################################
summary(vector.age)

wtp.infmortality.young   <- wtp.infmortality[vector.age <  22]
wtp.infmortality.old     <- wtp.infmortality[vector.age >= 40]

wtp.reducedvis.young   <- wtp.reducedvis[vector.age <  22]
wtp.reducedvis.old     <- wtp.reducedvis[vector.age >= 40]

wtp.morbidity.young   <- wtp.morbidity[vector.age <  22]
wtp.morbidity.old     <- wtp.morbidity[vector.age >= 40]

###############################################
# Figure 5a. WTP age differences 
###############################################
layout(1)
boxplot(wtp.infmortality.young  , 
        wtp.infmortality.old ,  
        wtp.reducedvis.young     ,
        wtp.reducedvis.old  ,
        wtp.morbidity.young      ,
        wtp.morbidity.old    ,
        horizontal=TRUE,
        col=c("grey20",
              "grey60",
              "grey30",
              "grey70",
              "grey40",
              "grey80"),
        outline=FALSE,
        xlab="Indian rupees",
        main = "WTP Age Differences",
        cex.main = 0.8,
        cex.axis = 0.8)

legend (x=c(130,280),y=c(0.5,3.2),
        c("Morbidity-decline of 1,000 (age > 40)",
          "Morbidity-decline of 1,000 (age < 22) ",
          "Red.visibility-decline of 10 (age > 40)",
          "Red.visibility-decline of 10 (age < 22)",
          "Inf.mortality-decline of 1,000 (age > 40)",
          "Inf.mortality-decline of 1,000 (age < 22) "
        ),
        fill=c("grey80", "grey40",  "grey70",
               "grey30", "grey60",  "grey20"),
        cex = 0.8)

###############################################
# Poe test for differences in age (Table 8)
###############################################

mded(distr1 = as.numeric(na.omit(wtp.morbidity.young)) , 
     distr2 = as.numeric(na.omit(wtp.morbidity.old)) , 
     detail = TRUE)
mded(distr1 = as.numeric(na.omit(wtp.morbidity.old)) , 
     distr2 = as.numeric(na.omit(wtp.morbidity.young)) , 
     detail = TRUE)

mded(distr1 = as.numeric(na.omit(wtp.reducedvis.old)) , 
     distr2 = as.numeric(na.omit(wtp.reducedvis.young  )) , 
     detail = TRUE)
mded(distr1 = as.numeric(na.omit(wtp.reducedvis.young)) , 
     distr2 = as.numeric(na.omit(wtp.reducedvis.old)) , 
     detail = TRUE)

mded(distr1 = as.numeric(na.omit(wtp.infmortality.young)) , 
     distr2 = as.numeric(na.omit(wtp.infmortality.old  )) , 
     detail = TRUE)
mded(distr1 = as.numeric(na.omit(wtp.infmortality.old)) , 
     distr2 = as.numeric(na.omit(wtp.infmortality.young  )) , 
     detail = TRUE)

###############################################
# Differences in GENDER
###############################################
summary(vector.gender.female)

wtp.infmortality.female   <- wtp.infmortality[vector.gender.female == 1]
wtp.infmortality.male     <- wtp.infmortality[vector.gender.female == 0]

wtp.reducedvis.female     <- wtp.reducedvis[vector.gender.female == 1]
wtp.reducedvis.male       <- wtp.reducedvis[vector.gender.female == 0]

wtp.morbidity.female      <- wtp.morbidity[vector.gender.female == 1]
wtp.morbidity.male        <- wtp.morbidity[vector.gender.female == 0]

#####################################################################
# Figure 5b. WTP gender differences 
#####################################################################

layout(1)
boxplot(wtp.infmortality.female , 
        wtp.infmortality.male ,  
        wtp.reducedvis.female     ,
        wtp.reducedvis.male  ,
        wtp.morbidity.female      ,
        wtp.morbidity.male    ,
        horizontal=TRUE,
        col=c("grey20",
              "grey60",
              "grey30",
              "grey70",
              "grey40",
              "grey80"),
        outline=FALSE,
        xlab="Indian rupees",
        main = "WTP Gender Differences",
        cex.main = 0.8,
        cex.axis = 0.8)

legend (x=c(140,220),y=c(0.5,3.5), c( "Morbidity male",
                   "Morbidity female ",
                   "Red.visibility male",
                   "Red.visibility female",
                   "Inf.mortality male",
                   "Inf.mortality female "
),
fill=c("grey80", "grey40",  "grey70",
       "grey30", "grey60",  "grey20"),
cex = 0.9)

###############################################
# Poe test for differences in gender (Table 8)
###############################################

mded(distr1 = as.numeric(na.omit(wtp.morbidity.female)) , 
     distr2 = as.numeric(na.omit(wtp.morbidity.male)) , 
     detail = TRUE,
     independent = TRUE)
mded(distr1 = as.numeric(na.omit(wtp.morbidity.male)) , 
     distr2 = as.numeric(na.omit(wtp.morbidity.female)) , 
     detail = TRUE,
     independent = TRUE)

mded(distr1 = as.numeric(na.omit(wtp.reducedvis.female)), 
     distr2 = as.numeric(na.omit(wtp.reducedvis.male  )), 
     detail = TRUE)
mded(distr1 = as.numeric(na.omit(wtp.reducedvis.male)) , 
     distr2 = as.numeric(na.omit(wtp.reducedvis.female  )), 
     detail = TRUE)

mded(distr1 = as.numeric(na.omit(wtp.infmortality.female)) , 
     distr2 = as.numeric(na.omit(wtp.infmortality.male  )) , 
     detail = TRUE)
mded(distr1 = as.numeric(na.omit(wtp.infmortality.male))   , 
     distr2 = as.numeric(na.omit(wtp.infmortality.female)) , 
     detail = TRUE)

###############################################
# Differences in EDUCATION
###############################################

summary(vector.educ)

wtp.infmortality.educ1   <- wtp.infmortality[vector.educ == 1]
wtp.infmortality.educ2   <- wtp.infmortality[vector.educ == 2]
wtp.infmortality.educ3   <- wtp.infmortality[vector.educ == 3]
wtp.reducedvis.educ1     <- wtp.reducedvis[vector.educ == 1]
wtp.reducedvis.educ2     <- wtp.reducedvis[vector.educ == 2]
wtp.reducedvis.educ3     <- wtp.reducedvis[vector.educ == 3]
wtp.morbidity.educ1      <- wtp.morbidity[vector.educ == 1]
wtp.morbidity.educ2      <- wtp.morbidity[vector.educ == 2]
wtp.morbidity.educ3      <- wtp.morbidity[vector.educ == 3]

#####################################################################
# Figure 5c. WTP education differences 
#####################################################################

layout(1)
boxplot(wtp.infmortality.educ1   , 
        wtp.infmortality.educ2   , 
        wtp.infmortality.educ3 ,
        wtp.reducedvis.educ1   , 
        wtp.reducedvis.educ2   , 
        wtp.reducedvis.educ3 ,
        wtp.morbidity.educ1   , 
        wtp.morbidity.educ2   , 
        wtp.morbidity.educ3,
        horizontal=TRUE,
        col=c("grey20",
              "grey50",
              "grey70",
              "grey30",
              "grey60",
              "grey80",
              "grey40",
              "grey70",
              "grey90" ),
        outline=FALSE,
        xlab="Indian rupees",
        main = "WTP Education Differences",
        cex.main = 0.8,
        cex.axis = 0.8)

legend (x=c(190,280),y=c(5.3,9.8), 
                c( "Morbidity education 3",
                   "Morbidity education 2",
                   "Morbidity education 1",
                   "Red.visibility education 3",
                   "Red.visibility education 2",
                   "Red.visibility education 1",
                   "Inf.mortality education 3" ,
                   "Inf.mortality education 2",
                   "Inf.mortality education 1"
                   ),
        fill=c("grey90", "grey70",  "grey40",
               "grey80", "grey60",  "grey30",
               "grey70", "grey50",  "grey20"
               ),
        cex = 0.6)

###############################################
# Poe test for differences in educ distr (Table 8)
###############################################

mded(distr2 = as.numeric(na.omit(wtp.infmortality.educ2))  , 
     distr1 = as.numeric(na.omit(wtp.infmortality.educ3)) , 
     detail = TRUE)
mded(distr2 = as.numeric(na.omit(wtp.infmortality.educ3))  , 
     distr1 = as.numeric(na.omit(wtp.infmortality.educ2)) , 
     detail = TRUE)

mded(distr2 = as.numeric(na.omit(wtp.infmortality.educ1))  , 
     distr1 = as.numeric(na.omit(wtp.infmortality.educ2)) , 
     detail = TRUE)
mded(distr2 = as.numeric(na.omit(wtp.infmortality.educ2))  , 
     distr1 = as.numeric(na.omit(wtp.infmortality.educ1)) , 
     detail = TRUE)

mded(distr2 = as.numeric(na.omit(wtp.reducedvis.educ2))  , 
     distr1 = as.numeric(na.omit(wtp.reducedvis.educ1)) , 
     detail = TRUE)
mded(distr2 = as.numeric(na.omit(wtp.reducedvis.educ1))  , 
     distr1 = as.numeric(na.omit(wtp.reducedvis.educ2)) , 
     detail = TRUE)

mded(distr2 = as.numeric(na.omit(wtp.reducedvis.educ3))  , 
     distr1 = as.numeric(na.omit(wtp.reducedvis.educ2)) , 
     detail = TRUE)
mded(distr2 = as.numeric(na.omit(wtp.reducedvis.educ2))  , 
     distr1 = as.numeric(na.omit(wtp.reducedvis.educ3)) , 
     detail = TRUE)

mded(distr2 = as.numeric(na.omit(wtp.morbidity.educ1))  , 
     distr1 = as.numeric(na.omit(wtp.morbidity.educ2)) , 
     detail = TRUE)
mded(distr2 = as.numeric(na.omit(wtp.morbidity.educ2))  , 
     distr1 = as.numeric(na.omit(wtp.morbidity.educ1)) , 
     detail = TRUE)

mded(distr2 = as.numeric(na.omit(wtp.morbidity.educ2))  , 
     distr1 = as.numeric(na.omit(wtp.morbidity.educ3)) , 
     detail = TRUE)
mded(distr2 = as.numeric(na.omit(wtp.morbidity.educ3))  , 
     distr1 = as.numeric(na.omit(wtp.morbidity.educ2)) , 
     detail = TRUE)


###############################################
# Differences in INCOME
###############################################
summary(vector.income)

wtp.infmortality.income1   <- wtp.infmortality[vector.income == 1]
wtp.infmortality.income2   <- wtp.infmortality[vector.income == 2]
wtp.infmortality.income3   <- wtp.infmortality[vector.income == 3]
wtp.reducedvis.income1     <- wtp.reducedvis[vector.income == 1]
wtp.reducedvis.income2     <- wtp.reducedvis[vector.income == 2]
wtp.reducedvis.income3     <- wtp.reducedvis[vector.income == 3]
wtp.morbidity.income1      <- wtp.morbidity[vector.income == 1]
wtp.morbidity.income2      <- wtp.morbidity[vector.income == 2]
wtp.morbidity.income3      <- wtp.morbidity[vector.income == 3]

###############################################
# Figure 5d. WTP income differences 
###############################################

layout(1)
boxplot(wtp.infmortality.income1   , 
        wtp.infmortality.income2   , 
        wtp.infmortality.income3  ,
        wtp.reducedvis.income1   , 
        wtp.reducedvis.income2   , 
        wtp.reducedvis.income3   ,
        wtp.morbidity.income1   , 
        wtp.morbidity.income2   , 
        wtp.morbidity.income3 ,
        horizontal=TRUE,
        col=c("grey20",
              "grey50",
              "grey70",
              "grey30",
              "grey60",
              "grey80",
              "grey40",
              "grey70",
              "grey90" ),
        outline=FALSE,
        xlab="Indian rupees",
        main = "WTP Income Differences",
        cex.main = 0.8,
        cex.axis = 0.8)

legend (x=c(200,280),y=c(4.5,9.7), 
                c( "Morbidity income 3",
                   "Morbidity income 2",
                   "Morbidity income 1",
                   "Red.visibility income 3",
                   "Red.visibility income 2",
                   "Red.visibility income 1",
                   "Inf.mortality income 3" ,
                   "Inf.mortality income 2",
                   "Inf.mortality income 1"
),
fill=c("grey90", "grey70",  "grey40",
       "grey80", "grey60",  "grey30",
       "grey70", "grey50",  "grey20"
),
cex = 0.68)

###############################################
# Poe test for differences in income distr (Table 8)
###############################################

mded(distr2 = as.numeric(na.omit(wtp.infmortality.income1))  , 
     distr1 = as.numeric(na.omit(wtp.infmortality.income2)) , 
     detail = TRUE)
mded(distr2 = as.numeric(na.omit(wtp.infmortality.income2))  , 
     distr1 = as.numeric(na.omit(wtp.infmortality.income1)) , 
     detail = TRUE)

mded(distr2 = as.numeric(na.omit(wtp.infmortality.income2))  , 
     distr1 = as.numeric(na.omit(wtp.infmortality.income3)) , 
     detail = TRUE)
mded(distr2 = as.numeric(na.omit(wtp.infmortality.income3))  , 
     distr1 = as.numeric(na.omit(wtp.infmortality.income2)) , 
     detail = TRUE)

mded(distr2 = as.numeric(na.omit(wtp.reducedvis.income2))  , 
     distr1 = as.numeric(na.omit(wtp.reducedvis.income1)) , 
     detail = TRUE)
mded(distr2 = as.numeric(na.omit(wtp.reducedvis.income1))  , 
     distr1 = as.numeric(na.omit(wtp.reducedvis.income2)) , 
     detail = TRUE)

mded(distr2 = as.numeric(na.omit(wtp.reducedvis.income3))  , 
     distr1 = as.numeric(na.omit(wtp.reducedvis.income2)) , 
     detail = TRUE)
mded(distr2 = as.numeric(na.omit(wtp.reducedvis.income2))  , 
     distr1 = as.numeric(na.omit(wtp.reducedvis.income3)) , 
     detail = TRUE)

mded(distr2 = as.numeric(na.omit(wtp.morbidity.income1))  , 
     distr1 = as.numeric(na.omit(wtp.morbidity.income2)) , 
     detail = TRUE)
mded(distr2 = as.numeric(na.omit(wtp.morbidity.income2))  , 
     distr1 = as.numeric(na.omit(wtp.morbidity.income1)) , 
     detail = TRUE)


mded(distr2 = as.numeric(na.omit(wtp.morbidity.income2))  , 
     distr1 = as.numeric(na.omit(wtp.morbidity.income3)) , 
     detail = TRUE)
mded(distr2 = as.numeric(na.omit(wtp.morbidity.income3))  , 
     distr1 = as.numeric(na.omit(wtp.morbidity.income2)) , 
     detail = TRUE)


###############################################
# END OF THE SCRIPT
###############################################
