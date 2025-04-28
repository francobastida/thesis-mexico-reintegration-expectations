#Step 0. Package Loading
pacman::p_load(haven, descr, table1, ggplot2, dplyr, car)
options(scipen = 999)

ENPOL <- read.csv("clean_ENPOL.csv", encoding = "latin1")

#Step 1: Re-coding for Logistic Regression Analysis

#DEP Variable: PRISON REENTRY EXPECTATIONS
ENPOL$REENTRY <- NA
ENPOL$REENTRY[ENPOL$P10_7 %in% c(1, 2, 3)] <- "Expected Reentry"
ENPOL$REENTRY[ENPOL$P10_7==4]  <- "No Reentry"
ENPOL$REENTRY <- as.factor(ENPOL$REENTRY)

#MODEL 1: MARRIAGE/CHILDREN

#GENDER
ENPOL$GENDER <- NA
ENPOL$GENDER[ENPOL$P1_2==1] <- "Men"
ENPOL$GENDER[ENPOL$P1_2==2] <- "Women"
ENPOL$GENDER<- as.factor(ENPOL$GENDER)

#COHORT
ENPOL$COHORT <- NA
ENPOL$COHORT[ENPOL$P1_3 >= 18 & ENPOL$P1_3 <= 25] <- "Cohort 18-25"
ENPOL$COHORT[ENPOL$P1_3 >= 26 & ENPOL$P1_3 <= 35] <- "Cohort 26-35"
ENPOL$COHORT[ENPOL$P1_3 >= 36 & ENPOL$P1_3 <= 45] <- "Cohort 36-45"
ENPOL$COHORT[ENPOL$P1_3 >= 46] <- "Cohort 46+"

#EDU
#Initialized as numeric or integer to work
ENPOL$P1_18_N <- as.numeric(as.character(ENPOL$P1_18_N))
ENPOL$EDU <- NA
ENPOL$EDU[ENPOL$P1_18_N %in% 0:2] <- "Low Education" 
ENPOL$EDU[ENPOL$P1_18_N %in% 3:7] <- "Mid Education" 
ENPOL$EDU[ENPOL$P1_18_N %in% 8:9 ] <- "High Education" 
#Low (Basic or No Education), Mid (Middle School to High School), High (University or Above)

#CHILD
ENPOL$CHILD <- NA
ENPOL$CHILD[ENPOL$P1_8==1] <- "Yes" 
ENPOL$CHILD[ENPOL$P1_8==2] <- "No" 

#PARTNER
ENPOL$PARTNER <- NA
ENPOL$PARTNER[ENPOL$P1_7 %in% c(1, 4)] <- "Married/Partner" 
ENPOL$PARTNER[ENPOL$P1_7 %in% c(2, 3, 6, 7)] <- "Divorced/Separated"
ENPOL$PARTNER[ENPOL$P1_7==5] <- "Single" 
ENPOL$PARTNER <- as.factor(ENPOL$PARTNER)

#Married (Married or in Free Union)
#Divorced/Separated (also includes Widowers, who were once previously partnered)
#Single, for those who were never married

#ENTENCE (TIME)
ENPOL$SENT <- NA
ENPOL$SENT[ENPOL$P1_1A==1] <- "Less than 6 months" 
ENPOL$SENT[ENPOL$P1_1A >= 2 & ENPOL$P1_1A <= 4] <- "6 months to 2 years" 
ENPOL$SENT[ENPOL$P1_1A==5] <- "Over 2 years" 
 
#PREVIOUS SENTENCE Y/N
ENPOL$PRIOROFF <- NA
ENPOL$PRIOROFF[ENPOL$P9_1==1] <- "Yes" 
ENPOL$PRIOROFF[ENPOL$P9_1==2] <- "No" 
ENPOL$PRIOROFF<- as.factor(ENPOL$PRIOROFF)

#NUMBER OF CHILDREN
ENPOL$P1_9 <- as.character(ENPOL$P1_9)
ENPOL$NUMCHILDREN <- "NA"
ENPOL$NUMCHILDREN[ENPOL$P1_9=="1"] <- "At least 1 child"
ENPOL$NUMCHILDREN[ENPOL$P1_9=="2" | ENPOL$P1_9=="3"] <- "Between 2-3 children"
ENPOL$NUMCHILDREN[ENPOL$P1_9 %in% c("4", "5", "6", "7", "8", "9")] <- "Between 4 and more"
ENPOL$NUMCHILDREN[ENPOL$P1_9=="99" | is.na(ENPOL$P1_9)] <- "No Response/Blank"

#Model II: FAMILY SUPPORT

#FAMILY/FRIENDS VISITATION
ENPOL$VISIT <- NA
ENPOL$VISIT[ENPOL$P7_25==1] <- "Had Visits"
ENPOL$VISIT[ENPOL$P7_25==2] <- "No Visits"
ENPOL$VISIT<- as.factor(ENPOL$VISIT)

ENPOL$P7_27 <- as.character(ENPOL$P7_12)
ENPOL$VISITFREQ <- NA
ENPOL$VISITFREQ[ENPOL$P7_27==1] <- "1-2 times per month"
ENPOL$VISITFREQ[ENPOL$P7_27==2] <- "3-4 times per month"
ENPOL$VISITFREQ[ENPOL$P7_27==3] <- "5-6 times per month"
ENPOL$VISITFREQ[ENPOL$P7_27==4] <- "Over 6 times per month"
ENPOL$VISITFREQ[ENPOL$P7_27==9 | is.na(ENPOL$P7_27)] <- "No Response/Blank"

#MOST FREQUENT VISITOR
ENPOL$MOSTVISIT <- NA
ENPOL$MOSTVISIT[ENPOL$P7_26==1] <- "Father"
ENPOL$MOSTVISIT[ENPOL$P7_26==2] <- "Mother"
ENPOL$MOSTVISIT[ENPOL$P7_26==3] <- "Sibling"
ENPOL$MOSTVISIT[ENPOL$P7_26==4] <- "Partner"
ENPOL$MOSTVISIT[ENPOL$P7_26==5] <- "Children"
ENPOL$MOSTVISIT[ENPOL$P7_26==6] <- "Other Family"
ENPOL$MOSTVISIT[ENPOL$P7_26==7] <- "Friends"
ENPOL$MOSTVISIT[ENPOL$P7_26==9 | is.na(ENPOL$P7_26)| ENPOL$P7_26==""] <- "No Response/Blank"

#INSTRUMENTAL SUPPORT
ENPOL$INSTSUPPORT <- ifelse(
  rowSums(
    ENPOL[, grep("^P7_28_", names(ENPOL)), drop = FALSE][, 
          !colnames(ENPOL[, grep("^P7_28_", names(ENPOL)), drop = FALSE]) %in% 
          c("P7_28_09", "P7_28_10", "P7_28_98", "P7_28_99")], 
    na.rm = TRUE) > 0, "Yes", "No"
)

#Considers: Food, Clothes, Shoes, Money, Medicine, Selling of Items, Work Supplies, and Personal Hygiene Items

#CAREGIVING SUPPORT
ENPOL$CAREGIVINGSUPPORT <- ifelse(
  rowSums(
    ENPOL[, grep("^P1_11_", names(ENPOL)), drop = FALSE][, 
          !colnames(ENPOL[, grep("^P1_11_", names(ENPOL)), drop = FALSE]) %in% 
          c("P1_11_7", "P1_11_8", "P1_11_9")], 
    na.rm = TRUE) > 0, "Yes", "No"
)

#CAREGIVER TYPOLOGY
ENPOL <- ENPOL %>%
  mutate(
    OTHERCAREGIVER = case_when(
      P1_11_2 == 1 ~ "Nuclear (Parent)",  # keep your nice label
      P1_11_3 == 1 | P1_11_4 == 1 ~ "Family Support",
      P1_11_5 == 1 | P1_11_7 == 1 ~ "Non-Family Support",
      P1_11_6 == 1 ~ "No Support",
      TRUE ~ NA_character_
    ),
    OTHERCAREGIVER = factor(OTHERCAREGIVER, levels = c(
      "Nuclear (Parent)", "Family Support", "Non-Family Support", "No Support"
    ))
  )

#Model III: FAMILY EXPERIENCES

#History of Positive Experiences of Reassurance
ENPOL$POSITIVEEXP <- NA
ENPOL$POSITIVEEXP[ENPOL$P9_9_01==1] <- "Yes"
ENPOL$POSITIVEEXP[ENPOL$P9_9_01==2] <- "No"
ENPOL$POSITIVEEXP<- as.factor(ENPOL$POSITIVEEXP)

#History of Parental Alcoholism
ENPOL$ALCOHOLCONS <- NA
ENPOL$ALCOHOLCONS[ENPOL$P9_9_02==1] <- "Yes"
ENPOL$ALCOHOLCONS[ENPOL$P9_9_02==2] <- "No"
ENPOL$ALCOHOLCONS<- as.factor(ENPOL$ALCOHOLCONS)

#History of Drug Consumption
ENPOL$DRUGCONS <- NA
ENPOL$DRUGCONS[ENPOL$P9_9_03==1] <- "Yes"
ENPOL$DRUGCONS[ENPOL$P9_9_03==2] <- "No"
ENPOL$DRUGCONS<- as.factor(ENPOL$DRUGCONS)

#History of Shouting, Insults 
ENPOL$VERBALINSULTS <- NA
ENPOL$VERBALINSULTS[ENPOL$P9_9_04==1] <- "Yes"
ENPOL$VERBALINSULTS[ENPOL$P9_9_04==2] <- "No"
ENPOL$VERBALINSULTS<- as.factor(ENPOL$VERBALINSULTS)

#History of Physical Aggression 
ENPOL$PHYSAGGRESSION <- NA
ENPOL$PHYSAGGRESSION[ENPOL$P9_9_05==1] <- "Yes"
ENPOL$PHYSAGGRESSION[ENPOL$P9_9_05==2] <- "No"
ENPOL$PHYSAGGRESSION<- as.factor(ENPOL$PHYSAGGRESSION)

#History of Beating and Bruises 
ENPOL$PHYSINJURY <- NA
ENPOL$PHYSINJURY[ENPOL$P9_9_06==1] <- "Yes"
ENPOL$PHYSINJURY[ENPOL$P9_9_06==2] <- "No"
ENPOL$PHYSINJURY<- as.factor(ENPOL$PHYSINJURY)

#History of Sexual Aggression
ENPOL$SEXAGGRESSION <- NA
ENPOL$SEXAGGRESSION[ENPOL$P9_9_07==1] <- "Yes"
ENPOL$SEXAGGRESSION[ENPOL$P9_9_07==2] <- "No"
ENPOL$SEXAGGRESSION<- as.factor(ENPOL$SEXAGGRESSION)

freq(ENPOL$SEXAGGRESSION)

#History of Criminal Involvement
ENPOL$CRIMINVOLVEMENT <- NA
ENPOL$CRIMINVOLVEMENT[ENPOL$P9_9_08==1] <- "Yes"
ENPOL$CRIMINVOLVEMENT[ENPOL$P9_9_08==2] <- "No"
ENPOL$CRIMINVOLVEMENT<- as.factor(ENPOL$CRIMINVOLVEMENT)

#History of Imprisonment
ENPOL$PARENTIMPRISONMENT <- NA
ENPOL$PARENTIMPRISONMENT[ENPOL$P9_9_09==1] <- "Yes"
ENPOL$PARENTIMPRISONMENT[ENPOL$P9_9_09==2] <- "No"
ENPOL$PARENTIMPRISONMENT<- as.factor(ENPOL$PARENTIMPRISONMENT)

#History of Abandonment or Death
ENPOL$ABANORDEATH <- NA
ENPOL$ABANORDEATH[ENPOL$P9_9_10==1] <- "Yes"
ENPOL$ABANORDEATH[ENPOL$P9_9_10==2] <- "No"
ENPOL$ABANORDEATH<- as.factor(ENPOL$ABANORDEATH)

#NEGATIVE FAMILY EXPERIENCES
ENPOL$NEG_FAMILY_EXP <- ifelse(
  rowSums(
    ENPOL[, c("ALCOHOLCONS", "DRUGCONS", "VERBALINSULTS", 
              "PHYSAGGRESSION", "PHYSINJURY", "SEXAGGRESSION", 
              "CRIMINVOLVEMENT", "PARENTIMPRISONMENT", "ABANORDEATH")] == "Yes", 
    na.rm = TRUE
  ) > 0, 
  "Yes", 
  "No"
)
ENPOL$NEG_FAMILY_EXP <- as.factor(ENPOL$NEG_FAMILY_EXP)
#Considers History of: Parental Alcohol Consumption, Drugs, Verbal Aggression
#Physical Aggression, Serious Physical Harm, Sexual Aggression, Criminal Involvement
#Parental Imprisonment, Abandonment or Parental Death

freq(ENPOL$NEG_FAMILY_EXP)

#Check for Multicollinearity
negative_famhistory_check <- glm(
  REENTRY ~ INSTSUPPORT + OTHERCAREGIVER + ALCOHOLCONS + DRUGCONS + VERBALINSULTS +
            PHYSAGGRESSION + PHYSINJURY + SEXAGGRESSION + CRIMINVOLVEMENT + 
            PARENTIMPRISONMENT + ABANORDEATH + COHORT + EDU + SENT,
  data = ENPOL,
  family = binomial(link = "logit")
)

vif(negative_famhistory_check)


#Additional: CRIMINAL INVOLVEMENT (Not Used after model adjustment)

#Gang Membership

#Organized Crime
ENPOL$ORGCRIMESENT <- NA
ENPOL$ORGCRIMESENT[ENPOL$P5_11_20==1] <- "Yes"
ENPOL$ORGCRIMESENT[ENPOL$P5_11_20==0 | is.na(ENPOL$P5_11_20) | ENPOL$P5_11_20=="" ] <- "No Response/Blank"
ENPOL$ORGCRIMESENT<- as.factor(ENPOL$ORGCRIMESENT)

#There could be multi-colinearity with regular previous crime offenses P9_1
#Not all organized crime activities will be sentenced as org. crime

ENPOL$GANGINVYOUTH <- NA
ENPOL$GANGINVYOUTH[ENPOL$P9_10_4==1] <- "Yes"
ENPOL$GANGINVYOUTH[ENPOL$P9_10_4==2] <- "No"
ENPOL$GANGINVYOUTH<- as.factor(ENPOL$GANGINVYOUTH)

freq(ENPOL$GANGINVYOUTH)

write.csv(ENPOL, "logitmodel_ENPOL.csv", row.names = FALSE)