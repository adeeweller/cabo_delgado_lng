
#####################################
########## Adee Weller ##############
## Afrobarometer data, Mozambique
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
setwd("C:\\Users\\adeew\\OneDrive\\Documents\\Mozambique")


# === Load Data === #


##############################################
########## Add Afrobarometer Data ############
##############################################


# start with Round 9 (2022)

af_v9 <- read_sav("MOZAMBIQUE_R9.data.final.wtd_release.local_info.03Oct23.sav")

head(af_v9)

unique(af_v9$REGION)

af_v9 <- subset(af_v9, af_v9$REGION == 549)

dim(af_v9)

View(af_v9)


# in V9, there are 16 observations, 8 in Pemba and 8 in Metuge (near Pemba)

# V8 (2021)

af_v8 <- read_sav("MOZ_R8.Data_31Aug21.wtd.final.release.local_info.sav")

head(af_v8)
unique(af_v8$REGION)

af_v8 <- subset(af_v8, af_v8$REGION == 549)

unique(af_v8$DISTRICT)
dim(af_v8)

# they clearly drove West on N14 for selecting places
# in V8, there are 23 observations, 8 in Cidade da Pemba, 10 in Cidade da Montepuez, and 4 in Kwekwe, Balama


# V7 (2018)

af_v7 <- read_sav("Mozambique_R7.Data.English_7Feb19_release.w.local.info.sav")

head(af_v7)
unique(af_v7$REGION)

af_v7 <- subset(af_v7, af_v7$REGION == 549)

unique(af_v7$DISTRICT)
dim(af_v7)

tab_af_v7 <- af_v7 %>%
	group_by(DISTRICT) %>%
	summarise(type_count = n())

tab_af_v7









# there are 192 observations, 8 from Ancuabe/Metoro, 16 Balama, 24 Chiure, 24 Pemba, 8 Macomia, 8 Mecufi, 8, Metuge, 16 Mocimboa da Praia, 24 Montepuez, 16 Mueda, 8 Muidumbe, 8 Namuno, 8 Nangade/Ntamba, 8 Palma, 8 Mahate/Quissanga


# V6 (2017)

af_v6 <- read_excel("MOZ_r6.csv.xlsx")

head(af_v6)
unique(af_v6$region)

af_v6 <- subset(af_v6, af_v6$region == 549)

unique(af_v6$locationlevel1)
dim(af_v6)

tab_af_v6 <- af_v6 %>%
	group_by(locationlevel1, locationlevel2) %>%
	summarise(type_count = n())

tab_af_v6

# there are 176 observations, 16 from Ancuabe/Metoro, Ancuabe/Meza, 16 Chiure/Katapua, 16 Chiure/Namogelia, 8 Ibo, 32 Macomia/Chai, 8 Mocimboa da praia, 16 Muidumbe, 16 Namuno/Machoca, 24 Pemba


# V5 (2011-2013)

af_v5 <- read_excel("MOZ_r5.csv.xlsx")

head(af_v5)
unique(af_v5$region)

af_v5 <- subset(af_v5, af_v5$region == 549)

unique(af_v5$locationlevel1)
dim(af_v5)

tab_af_v5 <- af_v5 %>%
	group_by(locationlevel1) %>%
	summarise(type_count = n())

tab_af_v5

# there are 184 observations at the district level, 32 in Ancuabe, 16 Balama, 8 Chiure, 40 Macomia, 40 Montepuez, 16 Mueda, 16 Namuno, 16 Pemba


# V4 (2008)

af_v4 <- read_excel("MOZ_r4.csv.xlsx")

head(af_v4)
unique(af_v4$region)

af_v4 <- subset(af_v4, af_v4$region == 550)

unique(af_v4$district)
dim(af_v4)

tab_af_v4 <- af_v4 %>%
	group_by(district) %>%
	summarise(type_count = n())

tab_af_v4

# there are 96 observations, 16 in Ancuabe, 24 in Chiure, 8 in Mocimboa da praia, 16 in Montepuez, 16 in Namuno, 8 Palma, 8 Pemba

# get average responses per posto in each place and time

names(af_v7) <- tolower(names(af_v7))

names(af_v5)
names(af_v6)

setdiff(names(af_v7), names(af_v6))

# districts with number (Labelled)
unique(af_v7$region)
# postos
unique(af_v7$district)



# postos
unique(af_v6$locationlevel1)
# districts with number 
unique(af_v6$region)








# # do most people have electricity
# as_svc_a, af_v5$ea_svc_a
# # piped water
# as_svc_b, af_v5$ea_svc_b
# # sewage
# as_svc_c, af_v5$ea_svc_c
# # cell phone
# as_svc_d, af_v5$ea_svc_d

# # post office
# ea_fac_a
# # school
# ea_fac_b
# # police station
# ea_fac_c
# # health clinic
# ea_fac_d
# # market
# ea_fac_e
# # bank
# ea_fac_f # af_v7 and af_v6 only
# # public transport
# ea_fac_g # af_v7 and af_v6 only

# # visible police
# ea_sec_a
# # visible military
# ea_sec_b
# # roadblocks_state
# ea_sec_c
# # roadblocks_local
# ea_sec_e

# # paved road
# ea_road_a, af_v5$ea_road

# # your living conditions
# q4, af_v5$q3b
# # your living conditions compared to others
# q5, af_v5$q4

# # felt unsafe walking
# q10a, af_v5$q9a
# # feared crime in home
# q10b, af_v5$q9b
# # experienced theft
# q11a, af_v5$q10a
# # experienced violence
# q11b, af_v5$q10b


# # discuss politics
# q13, af_v5$q15
# # freedom of speech
# q14, af_v5$q17a
# # change in freedom of speech
# q19a # af_v7 and af_v6 only

# # voting history
# q22, af_v5$q27
# # rally attendance
# q24a, af_v5$q29a
# # campaign for party/candidate
# q24b, af_v5$q29c
# # contacted local politician
# q25a, af_v5$q30a
# # contacted national politician
# q25b, af_v5$q30b
# # attended community meeting
# af_v7$q21a == af_v6$q20a == af_v5$q26a

# # participated in community request of government
# q26a, af_v5$q26b
# # contacted media about complaint
# q26b # af_v7 and af_v6 o
# # complained to the gvt
# q26c # af_v7 and af_v6 o
# # refused to pay fee/tax
# q26d, af_v5$q26c
# # protested
# q26e, af_v5$q26d

# # democracy
# af_v7$q35 == af_v6$q40 == af_v54q42
# # satistfaction with democracy
# af_v7$q36 == af_v6$q41 == af_v54q43
# # voters can remove officials
# af_v6$q47b


# # corruption (govt officials)
# af_v7$q44c == af_v6$q53c == af_v5$q60c
# # corruption (local govt officials)
# af_v7$q44d == af_v6$q53d == af_v5$q60d
# # corruption (business excs)
# af_v7$q44i == af_v6$q53j
# # corruption increasing?
# af_v7$q45 == af_v6$a54


# # trust in local govt
# af_v7$q43d == af_v6$q52e == af_v5$q59e
# # trust in ruling party
# af_v7$q43e == af_v6$q52f == af_v5$q59f

names(af_v7) <- tolower(names(af_v7))


# Rename columns in af_v5
af_v5 <- af_v5 %>%
  rename(
    electricity_access = ea_svc_a,
    piped_water = ea_svc_b,
    sewage_access = ea_svc_c,
    cell_phone_access = ea_svc_d,
    paved_road = ea_road,
    living_conditions = q3b,
    living_conditions_comparison = q4,
    unsafe_walking = q9a,
    fear_crime_home = q9b,
    experienced_theft = q10a,
    experienced_violence = q10b,
    discuss_politics = q15,
    freedom_of_speech = q17a,
    voting_history = q27,
    rally_attendance = q29a,
    campaign_participation = q29c,
    contacted_local_politician = q30a,
    contacted_national_politician = q30b,
    community_request_participation = q26b,
    refused_to_pay_fee_tax = q26c,
    protest_participation = q26d,
    trust_local_govt = q59e,
    trust_ruling_party = q59f,
    corruption_govt_officials = q60c,
    corruption_local_officials = q60d
  )

# Rename columns in af_v6
af_v6 <- af_v6 %>%
  rename(
    change_freedom_of_speech = q19a,  # Exists in af_v7 and af_v6 only
    contacted_media_complaint = q26b,  # Exists in af_v7 and af_v6 only
    complained_to_govt = q26c,  # Exists in af_v7 and af_v6 only
    democracy_support = q40,
    satisfaction_democracy = q41,
    corruption_business_execs = q53j,
    corruption_increasing = q54,
    voters_remove_officials = q47b
  )

# Rename columns in af_v7
af_v7 <- af_v7 %>%
  rename(
    change_freedom_of_speech = q19a,
    contacted_media_complaint = q26b,
    complained_to_govt = q26c,
    democracy_support = q35,
    satisfaction_democracy = q36,
    corruption_business_execs = q44i,
    corruption_increasing = q45
  )

View(af_v6)

# Standardize column names across all datasets
af_v6 <- af_v6 %>%
	rename(
  electricity_access = ea_svc_a,
  piped_water = ea_svc_b,
  sewage_access = ea_svc_c,
  cell_phone_access = ea_svc_d,
  post_office = ea_fac_a,
  school = ea_fac_b,
  police_station = ea_fac_c,
  health_clinic = ea_fac_d,
  market = ea_fac_e,
  bank = ea_fac_f,
  public_transport = ea_fac_g,
  visible_police = ea_sec_a,
  visible_military = ea_sec_b,
  roadblocks_state = ea_sec_c,
  roadblocks_local = ea_sec_e,
  paved_road = ea_road_a,
  living_conditions = q4a,
  living_conditions_comparison = q5,
  unsafe_walking = q10a,
  fear_crime_home = q10b,
  experienced_theft = q11a,
  experienced_violence = q11b,
  discuss_politics = q13,
  freedom_of_speech = q14,
  voting_history = q22,
  rally_attendance = q24a,
  campaign_participation = q24b,
  contacted_local_politician = q25a,
  contacted_national_politician = q25b,
  attended_community_meeting = q21,
  community_request_participation = q26a,
  refused_to_pay_fee_tax = q26d,
  protest_participation = q26e
)



# Standardize column names across all datasets
af_v7 <- af_v7 %>%
	rename(
  electricity_access = ea_svc_a,
  piped_water = ea_svc_b,
  sewage_access = ea_svc_c,
  cell_phone_access = ea_svc_d,
  post_office = ea_fac_a,
  school = ea_fac_b,
  police_station = ea_fac_c,
  health_clinic = ea_fac_d,
  market = ea_fac_e,
  bank = ea_fac_f,
  public_transport = ea_fac_g,
  visible_police = ea_sec_a,
  visible_military = ea_sec_b,
  roadblocks_state = ea_sec_c,
  roadblocks_local = ea_sec_e,
  paved_road = ea_road_a,
  living_conditions = q4a,
  living_conditions_comparison = q5,
  unsafe_walking = q10a,
  fear_crime_home = q10b,
  experienced_theft = q11a,
  experienced_violence = q11b,
  discuss_politics = q13,
  freedom_of_speech = q14,
  voting_history = q22,
  rally_attendance = q24a,
  campaign_participation = q24b,
  contacted_local_politician = q25a,
  contacted_national_politician = q25b,
  community_request_participation = q26a,
  refused_to_pay_fee_tax = q26d,
  protest_participation = q26e
)


names(af_v7)



unique_columns <- c('district', 'locationlevel1',
  "electricity_access", "piped_water", "sewage_access", "cell_phone_access", 
  "paved_road", "living_conditions", "living_conditions_comparison", 
  "unsafe_walking", "fear_crime_home", "experienced_theft", "experienced_violence", 
  "discuss_politics", "freedom_of_speech", "voting_history", "rally_attendance", 
  "campaign_participation", "contacted_local_politician", "contacted_national_politician", 
  "community_request_participation", "refused_to_pay_fee_tax", "protest_participation", 
  "trust_local_govt", "trust_ruling_party", "corruption_govt_officials", 
  "corruption_local_officials", "change_freedom_of_speech", "contacted_media_complaint", 
  "complained_to_govt", "democracy_support", "satisfaction_democracy", 
  "corruption_business_execs", "corruption_increasing", "voters_remove_officials", 
  "post_office", "school", "police_station", "health_clinic", "market", "bank", 
  "public_transport", "visible_police", "visible_military", "roadblocks_state", 
  "roadblocks_local", "attended_community_meeting"
)

af_v7_small <- af_v7 %>%
	dplyr::select(any_of(unique_columns))
af_v6_small <- af_v6 %>%
	dplyr::select(any_of(unique_columns))
af_v5_small <- af_v5 %>%
	dplyr::select(any_of(unique_columns))


af_v7_small <- af_v7_small %>%
	rename(posto = district)
af_v6_small <- af_v6_small %>%
	rename(posto = locationlevel1)
af_v5_small <- af_v5_small %>%
	rename(posto = locationlevel1)


af_v7_small$Year <- 2018
af_v6_small$Year <- 2017
af_v5_small$Year <- 2013

combined_af <- bind_rows(af_v7_small, af_v6_small, af_v5_small)

dim(combined_af)

View(combined_af)

combined_af <- combined_af %>%
	mutate(posto = tolower(posto))

unique(combined_af$posto)

combined_af$adi <- ifelse(combined_af$posto == 'palma', 1, 0)
combined_af$aii <- ifelse(combined_af$posto == 'palma', 0, 1)

sum(combined_af$adi)
sum(combined_af$joa)

combined_af$joa <- ifelse(combined_af$posto == 'palma' | combined_af$posto == 'mocimboa da praia' | combined_af$posto == 'mueda' | combined_af$posto == 'muidumbe', 1, 0)
combined_af$not_joa <- ifelse(combined_af$posto == 'palma' | combined_af$posto == 'mocimboa da praia' | combined_af$posto == 'mueda' | combined_af$posto == 'muidumbe', 0, 1)

names(combined_af)

# test pretreatment
# pre_treat_voting <- felm(voting_history ~ adi*Year, data = combined_af)
# summary(pre_treat_voting)

# pre_treat_rally <- felm(rally_attendance ~ adi*Year, data = combined_af)
# summary(pre_treat_rally)

# pre_treat_campaign <- felm(campaign_participation ~ adi*Year, data = combined_af)
# summary(pre_treat_campaign)




# test pretreatment
pre_treat_voting2 <- felm(voting_history ~ joa*Year, data = combined_af)
summary(pre_treat_voting2)

pre_treat_rally2 <- felm(rally_attendance ~ joa*Year, data = combined_af)
summary(pre_treat_rally2)

pre_treat_campaign2 <- felm(campaign_participation ~ joa*Year, data = combined_af)
summary(pre_treat_campaign2)

joa_afro <- list(pre_treat_voting2, pre_treat_rally2, pre_treat_campaign2)

cm_pa <- c('joa' = 'JOA',
	'Year' = 'Year',
	'joa:Year' = 'JOA × Year'
)

footnote_text <- "Significance levels: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001."

modelsummary(joa_afro, 
  estimate = "{estimate}{stars}",
  output = "latex", 
  col.names = c('', 'Voting History', 'Rally Attendence', 'Campaign Participation'),
  coef_map = cm_pa,
  gof_map = c("nobs", "r.squared"),
  title = "Reported Political Behavior in Cabo Delgado, 2011-2018",
  notes = footnote_text
  )




afro_voting <- ggplot(combined_af, aes(x = Year, y = voting_history, color = factor(joa))) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Reported Voting History", x = "Year", y = "Voting History") +
  scale_color_manual(name = "Group", values = c("0" = "blue", "1" = "red"),
                     labels = c("Non-JOA", "JOA")) +
  theme_minimal() +
  guides(color = guide_legend(title = NULL)) + 
  theme(
      text = element_text(family = "serif"), 
      axis.text.y = element_text(family = "serif", face = "bold"))


afro_rally <- ggplot(combined_af, aes(x = Year, y = rally_attendance, color = factor(joa))) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Reported Rally Attendence", x = "Year", y = "Rally Attendence") +
  scale_color_manual(name = "Group", values = c("0" = "blue", "1" = "red"),
                     labels = c("Non-JOA", "JOA")) +
  theme_minimal() +
  guides(color = guide_legend(title = NULL)) + 
  theme(
      text = element_text(family = "serif"), 
      axis.text.y = element_text(family = "serif", face = "bold"))


afro_campaign <- ggplot(combined_af, aes(x = Year, y = campaign_participation, color = factor(joa))) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Reported Campaign Participation", x = "Year", y = "Campaign Participation") +
  scale_color_manual(name = "Group", values = c("0" = "blue", "1" = "red"),
                     labels = c("Non-JOA", "JOA")) +
  theme_minimal() +
  guides(color = guide_legend(title = NULL)) + 
  theme(
      text = element_text(family = "serif"), 
      axis.text.y = element_text(family = "serif", face = "bold"))


afro <- afro_voting + afro_rally + afro_campaign + 
      plot_layout(ncol=3) +
  plot_annotation(title = "Political Behavior in Cabo Delgado, Afro-barometer (2011-2018)")


# save figures
fig_path <- 'C:\\Users\\adeew\\OneDrive\\Documents\\Mozambique\\'

ggsave(paste0(fig_path, 'afro_barometer.pdf', sep = ''), 
       plot = afro, width = 36, height = 18, dpi = 300)
