

#####################################
########## Adee Weller ##############
## Census data, Mozambique
#####################################

# Clear workspace
rm(list = ls())

# Load or install necessary packages
pkgs <- c("tmap", "dplyr", "plm", "fixest", "kableExtra", "stargazer", "ggplot2", "modelsummary", "ggthemes", "did", "fwildclusterboot", "bacondecomp", "spdep", "xtable", "broom", "purrr", "lfe", "did2s", "didimputation", "glue", "MASS", "sjPlot", "lmtest", "gridExtra", "rddtools", "rdrobust", "rdd", "rddensity", "stringr", "haven", "DescTools", "broom", "stringdist", "fuzzyjoin", "readxl")

for (pkg in pkgs) {
  if (!pkg %in% rownames(installed.packages())) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# Set working directory (update if needed)
setwd("C:\\Users\\adeew\\OneDrive\\Documents\\Mozambique\\")


# === Load Data === #

census <- read_sav("Moz_data\\Censo2017.PUF_10%_Final_19042022\\Censo2017.PUF_10%_Final_19042022.sav")

names(census)

# select on Cabo Delgado
unique(census$A2_PROV)

dim(census)

census$prov <- as.character(census$A2_PROV)

census_cd <- subset(census, census$prov == "02")

dim(census_cd)

# get the total number of population per posto
unique(census_cd$A4_POSTO)

# to do: get the percent of each measure at the posto level


# p23: education
# p36 industry (gvt workers/corp)
# d1: habitation indicators
# e1: accessible items


head(census_cd$P01_NUMERO_DA_PESSOA)

census_cd$primary <- ifelse(as.numeric(census_cd$P23_NIVEL_DE_ENSINO_FREQUENTADO) >= 4, 1, 0)

census_cd$secondary <- ifelse(as.numeric(census_cd$P23_NIVEL_DE_ENSINO_FREQUENTADO) >= 6, 1, 0)

census_cd$gvt_employee <- ifelse(as.numeric(census_cd$P36_TIPO_DE_TRABALHADOR) == 1, 1, 0)

census_cd$public_company_employee <- ifelse(as.numeric(census_cd$P36_TIPO_DE_TRABALHADOR) == 3, 1, 0)

census_cd$pvt_company_employee <- ifelse(as.numeric(census_cd$P36_TIPO_DE_TRABALHADOR) == 4, 1, 0)

census_cd$cement_home <- ifelse(as.numeric(census_cd$D06_PAREDES
	) == 1, 1, 0)

census_cd$adobe_home <- ifelse(as.numeric(census_cd$D06_PAREDES
	) == 4, 1, 0)

census_cd$metal_roof <- ifelse(as.numeric(census_cd$D07_COBERTURA
	) == 1, 1, 0)

census_cd$wood_floor <- ifelse(as.numeric(census_cd$D08_PAVIMENTO
	) == 1, 1, 0)

# focus on goods that indicate public infrastructure investments

census_cd$water_in_house <- ifelse(as.numeric(census_cd$D10_AGUA_BEBER) == 1, 1, 0)

census_cd$water_outside_house <- ifelse(as.numeric(census_cd$D10_AGUA_BEBER) == 1, 1, 0)

census_cd$electricity <- ifelse(as.numeric(census_cd$D13_ENERGIA	) == 1, 1, 0)


census_cd$radio <- ifelse(as.numeric(census_cd$E1A_RADIO) == 1, 1, 0)

census_cd$tv <- ifelse(as.numeric(census_cd$E1B_TELEVISAO) == 1, 1, 0)

census_cd$computer <- ifelse(as.numeric(census_cd$E1D_COMPUTADOR) == 1, 1, 0)

census_cd$internet <- ifelse(as.numeric(census_cd$E1E_INTERNET) == 1, 1, 0)

census_cd$stove <- ifelse(as.numeric(census_cd$E1H_FOGAO_ELECTRICO_GAS) == 1 | as.numeric(census_cd$E1G_FOGAO_CARVAO_LENHA) == 1, 1, 0)

census_cd$car <- ifelse(as.numeric(census_cd$E1J_CARRO) == 1, 1, 0)

census_cd$moto <- ifelse(as.numeric(census_cd$E1K_MOTORIZADA) == 1, 1, 0)

census_cd$bicycle <- ifelse(as.numeric(census_cd$E1L_BICICLETA) == 1, 1, 0)




pop_counts <- census_cd %>%
	group_by(CODIGO_POSTO) %>%
	summarise(num_respondents = sum(P01_NUMERO_DA_PESSOA),
		num_surveys = n(),
		primary_school = (sum(primary, na.rm = TRUE)/num_surveys),
		secondary_school = (sum(secondary, na.rm = TRUE)/num_surveys),
		gvt_employee = (sum(gvt_employee, na.rm = TRUE)/num_surveys),
		public_company_employee = (sum(public_company_employee, na.rm = TRUE)/num_surveys),
		pvt_company_employee = (sum(pvt_company_employee, na.rm = TRUE)/num_surveys),
		cement_home = (sum(cement_home, na.rm = TRUE)/num_surveys),
		adobe_home = (sum(adobe_home, na.rm = TRUE)/num_surveys),
		metal_roof = (sum(metal_roof, na.rm = TRUE)/num_surveys),
		wood_floor = (sum(wood_floor, na.rm = TRUE)/num_surveys),
		water_in_house = (sum(water_in_house, na.rm = TRUE)/num_surveys),
		water_outside_house = (sum(water_outside_house, na.rm = TRUE)/num_surveys),
		electricity = (sum(electricity, na.rm = TRUE)/num_surveys),
		radio = (sum(radio, na.rm = TRUE)/num_surveys),
		tv = (sum(tv, na.rm = TRUE)/num_surveys),
		computer = (sum(computer, na.rm = TRUE)/num_surveys),
		internet = (sum(internet, na.rm = TRUE)/num_surveys),
		stove = (sum(stove, na.rm = TRUE)/num_surveys),
		car = (sum(car, na.rm = TRUE)/num_surveys),
		moto = (sum(moto, na.rm = TRUE)/num_surveys),
		bicycle = (sum(bicycle, na.rm = TRUE)/num_surveys))


# total respondents
sum(pop_counts$num_respondents)

head(pop_counts)

# add indicators for ADI, AII, and JOA

# Palma 21501
# Olumbe 21502
# Pundanhar 21503
adi <- c(21501, 21502, 21503)


# pemba 20199
# diaca 20902
# quiterajo 20604
# chai 20602
# Palma 21501
# Olumbe 21502
# Pundanhar 21503
# mucojo 20603
# Mocimboa da Praia Sede 20901
# Mbau 20903
# mueda 21101
# ngapa 21105
# imbuho 21108
# ntamba 21402
# quissonga 21701
# chitunda 21202
# nangade 21401
# muidumbe/Muambula 21201
joa <- c(20199, 20902, 20604, 20602, 21501, 21502, 21503, 20603, 20901, 20903, 21101, 21105, 21108, 21402, 21701, 21202, 21401, 21201)


class(pop_counts$CODIGO_POSTO)

pop_counts$adi <- ifelse(as.numeric(pop_counts$CODIGO_POSTO) %in% adi, 1, 0)

pop_counts$joa <- ifelse(as.numeric(pop_counts$CODIGO_POSTO) %in% joa, 1, 0)


pop_joa <- subset(pop_counts, pop_counts$joa == 1)
pop_not_joa <- subset(pop_counts, pop_counts$joa == 0)
pop_adi <- subset(pop_counts, pop_counts$adi == 1)
pop_not_adi <- subset(pop_counts, pop_counts$adi == 0)


primary_text <- t_test_viz_combined(pop_joa$primary_school, pop_not_joa$primary_school, pop_adi$primary_school, pop_not_adi$primary_school, 
                              "JOA", "Not JOA", "ADI", "AII", 
                              "JOA vs Not JOA", "ADI vs AII")



primary_text$results_df

plot_results(primary_text$results_df, "Voting History")

primary_lm <- lm(primary_school ~ adi, pop_counts)
summary(primary_lm)
primary_lm2 <- lm(primary_school ~ joa, pop_counts)
summary(primary_lm2)


secondary_school_lm <- lm(secondary_school ~ adi, pop_counts)
summary(secondary_school_lm)
secondary_school_lm2 <- lm(secondary_school ~ joa, pop_counts)
summary(secondary_school_lm2)


gvt_employee_lm <- lm(gvt_employee ~ adi, pop_counts)
summary(gvt_employee_lm)
gvt_employee_lm2 <- lm(gvt_employee ~ joa, pop_counts)
summary(gvt_employee_lm2)


public_company_employee_lm <- lm(public_company_employee ~ adi, pop_counts)
summary(public_company_employee_lm)
public_company_employee_lm2 <- lm(public_company_employee ~ joa, pop_counts)
summary(public_company_employee_lm2)



pvt_company_employee_lm <- lm(pvt_company_employee ~ adi, pop_counts)
summary(pvt_company_employee_lm)
pvt_company_employee_lm2 <- lm(pvt_company_employee ~ joa, pop_counts)
summary(pvt_company_employee_lm2)


cement_home_lm <- lm(cement_home ~ adi, pop_counts)
summary(cement_home_lm)
cement_home_lm2 <- lm(cement_home ~ joa, pop_counts)
summary(cement_home_lm2)



adobe_home_lm <- lm(adobe_home ~ adi, pop_counts)
summary(adobe_home_lm)
adobe_home_lm2 <- lm(adobe_home ~ joa, pop_counts)
summary(adobe_home_lm2)



metal_roof_lm <- lm(metal_roof ~ adi, pop_counts)
summary(metal_roof_lm)
metal_roof_lm2 <- lm(metal_roof ~ joa, pop_counts)
summary(metal_roof_lm2)



wood_floor_lm <- lm(wood_floor ~ adi, pop_counts)
summary(wood_floor_lm)
wood_floor_lm2 <- lm(wood_floor ~ joa, pop_counts)
summary(wood_floor_lm2)




water_in_house_lm <- lm(water_in_house ~ adi, pop_counts)
summary(water_in_house_lm)
water_in_house_lm2 <- lm(water_in_house ~ joa, pop_counts)
summary(water_in_house_lm2)



water_outside_house_lm <- lm(water_outside_house ~ adi, pop_counts)
summary(water_outside_house_lm)
water_outside_house_lm2 <- lm(water_outside_house ~ joa, pop_counts)
summary(water_outside_house_lm2)



electricity_lm <- lm(electricity ~ adi, pop_counts)
summary(electricity_lm)
electricity_lm2 <- lm(electricity ~ joa, pop_counts)
summary(electricity_lm2)



radio_lm <- lm(radio ~ adi, pop_counts)
summary(radio_lm)
radio_lm2 <- lm(radio ~ joa, pop_counts)
summary(radio_lm2)




tv_lm <- lm(tv ~ adi, pop_counts)
summary(tv_lm)
tv_lm2 <- lm(tv ~ joa, pop_counts)
summary(tv_lm2)




computer_lm <- lm(computer ~ adi, pop_counts)
summary(computer_lm)
computer_lm2 <- lm(computer ~ joa, pop_counts)
summary(computer_lm2)




internet_lm <- lm(internet ~ adi, pop_counts)
summary(internet_lm)
internet_lm2 <- lm(internet ~ joa, pop_counts)
summary(internet_lm2)




stove_lm <- lm(stove ~ adi, pop_counts)
summary(stove_lm)
stove_lm2 <- lm(stove ~ joa, pop_counts)
summary(stove_lm2)




car_lm <- lm(car ~ adi, pop_counts)
summary(car_lm)
car_lm2 <- lm(car ~ joa, pop_counts)
summary(car_lm2)




moto_lm <- lm(moto ~ adi, pop_counts)
summary(moto_lm)
moto_lm2 <- lm(moto ~ joa, pop_counts)
summary(moto_lm2)




bicycle_lm <- lm(bicycle ~ adi, pop_counts)
summary(bicycle_lm)
bicycle_lm2 <- lm(bicycle ~ joa, pop_counts)
summary(bicycle_lm2)


adi1 <- list(primary_lm,
secondary_school_lm,
gvt_employee_lm,
public_company_employee_lm,
pvt_company_employee_lm,
cement_home_lm,
adobe_home_lm,
metal_roof_lm)

adi2 <- list(wood_floor_lm,
water_in_house_lm,
water_outside_house_lm,
electricity_lm,
radio_lm,
tv_lm,
computer_lm,
internet_lm,
stove_lm,
car_lm,
moto_lm,
bicycle_lm)
  


latex_code <- modelsummary(
  adi,
  output = "latex",
  stars = TRUE,
  col.names = c(
    "", "Primary School", "Secondary School", "Govt. employee",
    "Public Company employee", "Private Company employee",
    "Cement Home", "Adobe Home", "Metal Roof", "Wood floor",
    "Water in home", "Water outside home", "Electricity",
    "Radio", "TV", "Computer", "Internet", "Stove",
    "Car", "Motorcycle", "Bicycle"
  ),
  gof_omit = ".*",
  escape = FALSE
)

cat("\\begin{landscape}\n", latex_code, "\n\\end{landscape}")




joa <- list(primary_lm2,
secondary_school_lm2,
gvt_employee_lm2,
public_company_employee_lm2,
pvt_company_employee_lm2,
cement_home_lm2,
adobe_home_lm2,
metal_roof_lm2,
wood_floor_lm2,
water_in_house_lm2,
water_outside_house_lm2,
electricity_lm2,
radio_lm2,
tv_lm2,
computer_lm2,
internet_lm2,
stove_lm2,
car_lm2,
moto_lm2,
bicycle_lm2)

modelsummary(joa,
	stars = TRUE,
  output = "latex",
  col.names = c("", "Primary School", "Secondary School", "Govt. employee", "Public Company employee", "Private Company employee", "Cement Home", "Adobe Home", "Metal Roof", "Wood floor", "Water in home", "Water outside home", "Electricity", "Radio", "TV", "Computer", "Internet", "Stove", "Car", "Motorcycle", "Bicycle"),
) %>%
  kable_styling(latex_options = c("hold_position", "repeat_header", "scale_down", "longtable"))

