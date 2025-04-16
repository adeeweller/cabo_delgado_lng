
######################################
########## Add ACLED Data ############
######################################

### read panel and ACLED data
lng <- read.csv("cabo_delgado_LNG.csv")
acled <- read.csv("ACLED_Mozambique.csv")

head(lng)
head(acled)

acled <- subset(acled, acled$admin1 == "Cabo Delgado")

acled <- subset(acled, acled$year > 2013)

dim(acled)

unique(acled$sub_event_type)

# get by posto/localidade and year
acled2 <- acled %>%
	group_by(admin2, admin3, year) %>%
	summarise(event_count = n())

acled3 <- acled %>%
	group_by(admin2, admin3, year, event_type, sub_event_type) %>%
	summarise(type_count = n())

acled2
acled3

acled_postos <- unique(acled2$admin3)
panel_postos <- unique(lng$Postos)

panel_postos %>% sort()
# need to do a rough match

names(acled2)[2] <- "Postos"
names(acled2)[3] <- "Year"

acled2_test <- acled2[,c(2:4)]

lng_postos <- lng$Postos
acled2_postos <- acled2$Postos


acled2$Matched_Postos <- ""

for (i in 1:dim(acled2)[1]) {   
	x <- agrep(acled2$Postos[i], lng$Postos, ignore.case=TRUE, value=TRUE,  max.distance = 0.05, useBytes = TRUE)   
	x <- paste0(x,"") 
	acled2$Matched_Postos[i] <- x 
} 

# currently did not get Mahate (200:202), M'Tamba (146:153), Muidumbe (137:142), Mbau (93:99), Imbuo (120) and Mocimboa da Praia (100:107)
print(acled2, n = 208)

acled_postos %>% sort()

acled2$Matched_Postos[120] <- "Imbuo"
acled2$Matched_Postos[93:99] <- "Mbau"
acled2$Matched_Postos[100:107] <- "Mocimboa-Sede"
acled2$Matched_Postos[137:142] <- "Muambula"
acled2$Matched_Postos[146:153] <- "Ntamba"
acled2$Matched_Postos[200:202] <- "Quissanga-Sede"


matched_data <- merge(lng, acled2, by.x = c('Postos', 'Year'), by.y = c("Matched_Postos", "Year"), all.x = TRUE)

head(matched_data)

matched_data <- matched_data %>%
  dplyr::select(-admin2, -Postos.y) 

matched_data <- matched_data %>%
	mutate(event_count = ifelse(is.na(matched_data$event_count), 0, matched_data$event_count))


# also get types of attacks

head(acled3)

names(acled3)[2] <- "Postos"
names(acled3)[3] <- "Year"

acled3_wider <- acled3 %>%
  tidyr::pivot_wider(
    names_from = event_type,  
    values_from = type_count, 
    values_fill = 0           
  )

names(acled3_wider)

acled3_wider <- acled3_wider %>%
	mutate(sub_event_type_count = sum(acled3_wider[,c(6:10)]))

acled3_wider <- acled3_wider %>%
  tidyr::pivot_wider(
    names_from = sub_event_type,  
    values_from = sub_event_type_count, 
    values_fill = 0           
  )


head(acled3_wider)

acled3_wider <- acled3_wider[,-1]

names(acled3_wider)[c(3, 7, 8, 10:13, 15, 16, 18:28, 30)] <- c("viol_against_civilians",	"strategic_dev",  "explosions_remote_viol", "peace_prot", "armed_clash", "mob_viol", "changed_group_activity", "disappear", "looting", "police_force_prot", "viol_dem", "air_strike", "sexual_viol", "NSA_takes_territory", "transfer_terr", "IED", "prot_intervention", "govt_terr", "missile", "disrupt_weapons", "HQ_est")

acled3_wider$Matched_Postos <- ""

for (i in 1:dim(acled3_wider)[1]) {   
	x <- agrep(acled3_wider$Postos[i], lng$Postos, ignore.case=TRUE, value=TRUE,  max.distance = 0.05, useBytes = TRUE)   
	x <- paste0(x,"") 
	acled3_wider$Matched_Postos[i] <- x 
} 


# currently did not get Mbau (93:99), Imbuo (120), Muidumbe (137:142) and Mocimboa da Praia (100:107), M'Tamba (146:153), Mahate (200:202)

# OLD did not get Mbau (63:66), Imbuo (81), Muidumbe (91:94) and Mocimboa da Praia (67:70), M'Tamba (98:101), Mahate (122:123)

data.frame(acled3_wider$Postos, acled3_wider$Matched_Postos)

acled3_wider$Matched_Postos[93:99] <- "Mbau"
acled3_wider$Matched_Postos[120] <- "Imbuho"
acled3_wider$Matched_Postos[146:153] <- "Ntamba"
acled3_wider$Matched_Postos[137:142] <- "Muambula"
acled3_wider$Matched_Postos[200:202] <- "Quissanga-Sede"
acled3_wider$Matched_Postos[100:107] <- "Mocimboa-Sede"



matched_data2 <- merge(matched_data, acled3_wider, by.x = c('Postos', 'Year'), by.y = c("Matched_Postos", "Year"), all.x = TRUE)

head(matched_data2)

matched_data2 <- matched_data2 %>%
  dplyr::select(-Postos.y) 

matched_data2 <- matched_data2 %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))

head(matched_data2)

unique(matched_data2$District)

write.csv(matched_data2, "Moz_10_30_24.csv")





###################################################
## write.csv(matched_data2, "Moz_10_21_24.csv") ###
##																						  ###
###################################################


matched_data2 <- read.csv("Moz_10_21_24.csv")


head(matched_data2)

sum(matched_data2$event_count)


mean_ec <- matched_data2 %>%
	group_by(Postos) %>%
	summarise(average_evnt = mean(event_count))

mean(mean_ec$average_evnt)



### DiD estimation #######

matched_data2 <- matched_data2 %>%
	mutate(treated_adi = ifelse(Year > 2018 & ADI == 1, 1, 0))

matched_data2 <- matched_data2 %>%
	mutate(treated_joa = ifelse(Year > 2018 & Area_1 == 1, 1, 0))

event_count_m <- felm(event_count ~ treated_adi | Postos + Year | 0 | District, data = matched_data2)

event_count_m2 <- felm(event_count ~ treated_joa | Postos + Year | 0 | District, data = matched_data2)


stargazer(event_count_m, event_count_m2, 
	column.labels = c("ADI", "JOA"),
  title = "Discovery of LNG on Insurgent Violence",
  type = "latex")



names(matched_data2)

event_count_loot <- felm(looting ~ treated_adi | Postos + Year | 0 | District, data = matched_data2)

event_count_loot2 <- felm(looting ~ treated_joa | Postos + Year | 0 | District, data = matched_data2)


stargazer(event_count_loot, event_count_loot2, 
	column.labels = c("ADI", "JOA"),
  title = "Discovery of LNG on Insurgent Violence",
  type = "latex")


stargazer(event_count_m, event_count_m2, event_count_loot, event_count_loot2,
          column.labels = c("ADI", "JOA"),
          column.separate = c(2, 2), 
          title = "Discovery of LNG on Insurgent Violence",
          type = "latex")



summary(event_count_m)

Battles_m <- felm(Battles ~ treated | Postos + Year | 0 | District, data = matched_data2)
summary(Battles_m)

Riots_m <- felm(Riots ~ treated | Postos + Year | 0 | District, data = matched_data2)
summary(Riots_m)

viol_against_civilians_m <- felm(viol_against_civilians ~ treated | Postos + Year | 0 | District, data = matched_data2)
summary(viol_against_civilians_m)

strategic_dev_m <- felm(strategic_dev ~ treated | Postos + Year | 0 | District, data = matched_data2)
summary(strategic_dev_m)

Protests_m <- felm(Protests ~ treated | Postos + Year | 0 | District, data = matched_data2)
summary(Protests_m)

explosions_remote_viol_m <- felm(explosions_remote_viol ~ treated | Postos + Year | 0 | District, data = matched_data2)
summary(explosions_remote_viol_m)


stargazer(event_count_m, Battles_m, Riots_m, viol_against_civilians_m, 
	column.labels = c("Conflicts", "Battles", "Riots", "Violence against Civilians"),
  title = "Discovery of LNG on Insurgent Violence",
  type = "latex")

stargazer(strategic_dev_m,  Protests_m, explosions_remote_viol_m,
	column.labels = c("Strategic Developments", "Protests", "Remote Violence"),
  title = "Discovery of LNG on Insurgent Violence",
  type = "latex")


