#Step 0. Package Loading
pacman::p_load(haven, descr, dplyr)
options(scipen = 999)

ENPOL <- read_dta("ENPOLraw/ENPOL2021_SOC.dta", encoding = "latin1")

#Step 1. Cleaning of Socio-demographic Data
ENPOL <- ENPOL %>%
  filter(SEXO %in% c(1, 2),             # gender, men or women
         P1_1A >= 1 & P1_1A <= 7,       # prison time, eliminate no response
  			 P1_3 >= 18 & P1_3 <= 97,			  # age: 18 to 97
  			 P1_4 == 1,			                # only people born in mexico
         P1_7 >= 1 & P1_7 <= 7,         # marriage status: 1 to 7, only valid
         P1_8 %in% c(1, 2),             # kids: 1 or 2, only valid responses
         P1_18_N >= 0 & P1_18_N <= 9,   # education: none to higher education
				 P1_29 ==1 | P1_29 ==2,
				 P1_42_2 != 98 | P1_42_2 != 99, # alcohol use background, valid
				 P1_42_3 != 98 | P1_42_3 != 99, # marijuana use background, valid
				 P1_42_4 != 98 | P1_42_4 != 99, # inhalables use background, valid
				 P1_42_5 != 98 | P1_42_5 != 99, # LSD use background, valid
				 P1_42_6 != 98 | P1_42_6 != 99, # hallucinogens use background, valid
				 P1_42_7 != 98 | P1_42_7 != 99, # cocaine use background, valid
				 P1_42_8 != 98 | P1_42_8 != 99, # paste use background, valid
				 P1_42_9 != 98 | P1_42_9 != 99, # crack use background, valid
				 P1_42_10 != 98 | P1_42_10 != 99, # heroin use background, valid
				 P1_42_11 != 98 | P1_42_11 != 99, # opioids use background, valid
				 P1_42_12 != 98 | P1_42_12 != 99, # tranquilizers use background, valid
				 P1_42_13 != 98 | P1_42_13 != 99 # amphetamines use background, valid
  )

#Dropped 1,278 out of 61,449 observations (2.07%) - valid responses only

#Step 2. Merge of Data, Cleaning of DV, IV, CV
ENPOL_detention <- read_dta("ENPOLraw/ENPOL2021_2_3.dta", encoding = "latin1")
ENPOL_centropen <- read_dta("ENPOLraw/ENPOL2021_6.dta", encoding = "latin1")
ENPOL_vidacarcelaria <- read_dta("ENPOLraw/ENPOL2021_7.dta", encoding = "latin1")
ENPOL_antecedentes <- read_dta("ENPOLraw/ENPOL2021_8_9_10_11.dta", encoding = "latin1")
ENPOL_crimebackground <- read_dta("ENPOLraw/ENPOL2021_5.dta", encoding = "latin1")

#General Background
ENPOL <- left_join(
	x = ENPOL,
	y = ENPOL_antecedentes,
	by = "ID_PER")

#Other Conditions and Expectations
ENPOL <- left_join(
	x = ENPOL,
	y = ENPOL_vidacarcelaria,
	by = "ID_PER")

#Penitentiary Center 
ENPOL <- left_join(
	x = ENPOL,
	y = ENPOL_centropen,
	by = "ID_PER")

#Pre Detention and Detention 
ENPOL <- left_join(
	x = ENPOL,
	y = ENPOL_detention,
	by = "ID_PER")

#Y, Dependent Variable: Prison Reentry Expectations 
ENPOL <- ENPOL %>%
	filter(P10_7 >= 1 & P10_7 <= 4) #removed do not know/no response,
				
#Dropped 699 out of 60,171 observations (1.16%) - valid responses only

#X, Independent Variable: Pre-Filtering
#Family  Contact
ENPOL <- ENPOL %>%
	filter(P7_11 == 1) #yes/no, contact - call to family/friends

#Dropped 2,256 out of 59,472 observations (3.75%) - valid responses only

#Family Ties - Visitations (Non-Intimate & Intimate)
ENPOL <- ENPOL %>%
	filter(P7_25 >= 1 & P7_25 <= 2)  #yes/no, visit - family/friends

#Dropped 165 out of 57,216 (0.28%) - valid responses only

#Family Background: Positive and Negative Experiences
ENPOL <- ENPOL %>%
  filter(P9_9_01 != 8 & P9_9_01 != 9,   # made you feel protected/loved
  			 P9_9_02 != 8 & P9_9_02 != 9,		# frequent alcohol consumption
  			 P9_9_03 != 8 & P9_9_03 != 9,		# drug use
  			 P9_9_04 != 8 & P9_9_04 != 9,		# verbal insults, yelling
  			 P9_9_05 != 8 & P9_9_05 != 9,		# beating
  			 P9_9_06 != 8 & P9_9_06 != 9,		# beating, leaving bruises
  			 P9_9_07 != 8 & P9_9_07 != 9,		# sexual harassment
  			 P9_9_08 != 8 & P9_9_08 != 9,		# accused of committing a crime
  			 P9_9_09 != 8 & P9_9_09 != 9,		# confined in a correctional facility
  			 P9_9_10 != 8 & P9_9_10 != 9,		# deceased or left home
  )

#Dropped 271 out of 57,051 (0.47%) - valid responses only

ENPOL_crimebackground <- ENPOL_crimebackground %>%
	select(ID_PER, P5_11_15, P5_11_18, P5_11_20, P5_11_21)
ENPOL_crimebackground$ID_PER <- as.numeric(ENPOL_crimebackground$ID_PER)

#Choose 5_11 type of sentences relevant for family support/literature
#Topics: family violence, sexual violence, organized crime, other sexual crimes

ENPOL$ID_PER <- as.character(ENPOL$ID_PER)
ENPOL_crimebackground$ID_PER <- as.character(ENPOL_crimebackground$ID_PER)

ENPOL <- left_join(
	x = ENPOL,
	y = ENPOL_crimebackground,
	by = "ID_PER")

#Additional pre filtering, after coding for the first time
ENPOL <- ENPOL %>%
  filter(P7_26 != 8 & P7_26 !=9 | is.na(P7_26) # made you feel protected/loved
  			 #!(P1_9 %in% c(10, 11, 12, 13, 14, 15, 19, 22, 98)), #number of children
  			 #P7_27 != 8 | is.na(P7_27) #frequency of visitation
  			 #P9_10_4 != 8 & P9_10_4 != 9 #yes/no, member of a gang before 15
  			 ) 

#Dropped 26 out of 56,780 (0.04%) - valid responses only

write.csv(ENPOL, "clean_ENPOL.csv", row.names = FALSE)