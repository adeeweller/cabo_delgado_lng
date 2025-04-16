
#####################################
########## Adee Weller ##############
## Cabo Delgado Survey, Mozambique
#####################################

# Clear workspace
rm(list = ls())

# Load or install necessary packages
pkgs <- c("tmap", "dplyr", "plm", "fixest", "kableExtra", "stargazer", "ggplot2", "modelsummary", "ggthemes", "did", "fwildclusterboot", "bacondecomp", "spdep", "xtable", "broom", "purrr", "lfe", "did2s", "didimputation", "glue", "MASS", "sjPlot", "lmtest", "gridExtra", "rddtools", "rdrobust", "rdd", "rddensity", "stringr", "haven", "DescTools", "broom", "stringdist", "fuzzyjoin", "readxl", "haven")

for (pkg in pkgs) {
  if (!pkg %in% rownames(installed.packages())) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# Set working directory (update if needed)
setwd("C:\\Users\\adeew\\OneDrive\\Documents\\Mozambique")

list.files()

# === Load Data === #
cabo <- read_dta("24-011823-01-01 - Emory Baseline_Cabo Delegado_Final.dta")

head(cabo)
names(cabo)
dim(cabo)

# are there any people who chose not to complete the interview?
cabo[which(cabo$Q2 == 2),]

# rename
cabo <- cabo %>%
	rename(setting = Q1,
		   province = P1,
		   district = P2,
		   posto = P3,
		   trust_friends = P13_1,
		   trust_private = P13_2,
		   trust_trad = P13_3,
		   trust_relig = P13_4,
		   trust_com_org = P13_5,
		   trust_mun = P13_6,
		   trust_cent = P13_7,
		   trust_FADM = P13_8,
		   trust_intl = P13_9,
		   viol_elect = P15_1,
		   viol_insurg = P15_2,
		   viol_crim = P15_3,
		   no_viol = P15_4,
		   dont_know_viol = P15_5,
		   other_viol = P15_6,
		   not_say_viol = P15_98,
		   change_viol = P16,
		   cg_cent = P18,
		   cg_dist = P19,
		   group_strengthening = P21,
		   sunnah_strengthening = P21_1,
		   bandidos_strengthening = P21_2,
		   shabaab_strengthening = P21_3,
		   IS_strengthening = P21_4,
		   None_strengthening = P21_5,
		   name_IS = P22,
		   seen_crim = P23_1,
		   seen_money = P23_2,
		   seen_services = P23_3,
		   seen_paying = P23_4,
		   seen_food = P23_5,
		   seen_flag = P23_6,
		   seen_farm = P23_7,
		   seen_justice = P23_8,
		   dont_know_seen = P23_10,
		   other_seen = P23_11,
		   none_seen = P23_12,
		   pref_not_say_seen = P23_13,
		   harm = P24,
		   FADM_sec = P25_1,
		   intl_sec = P25_2,
		   insurg_sec = P25_3,
		   naparamas_sec = P25_4,
		   TE_sec = P25_5,
		   dont_know_sec = P25_6,
		   other_sec = P25_7,
		   neighbor_sec = P25_8,
		   none_sec = P25_99,
		   pref_not_say_sec = P25_98,
		   best_sec = P26,
		   dispute_neighbor = P26b,
		   why_sec = P27,
		   spread = P28,
		   lynch_thief = P30,
		   lynch_justice = P31,
		   xitiki = P32_1, # participated in a xitiki
		   church = P32_2, # participated in a church group
		   savings = P32_3, # participated in a savings group
		   assist_neigh = P32_4, # assisted a neighbor
		   volunteer_com = P32_5, # volunteered for a community group
		   participate_com = P32_6, # volunteered for a community meeting
		   voted = P32_7,
		   protest = P32_8,
		   sports = P32_9,
		   campgained = P32_10,
		   dont_know_com = P32_11,
		   other_com = P32_12,
		   pref_not_say_com = P32_98,
		   inf_improve = P34,
		   inf_improve_loc = P35 )

# Verify P23_1-P23_13?

names(cabo)


# get only current CD residents
unique(cabo$P1)

cur_cabo <- subset(cabo, cabo$province == 2)

dim(cur_cabo)

# get only people who have left CD (save these for now)
not_cabo <- subset(cabo, cabo$province != 2)

unique(not_cabo$P1a)

# look at distribution by posto and district (from labeled data)
extract_present_labels <- function(labeled_var) {
  unique_values <- unique(labeled_var)
  labels <- attr(labeled_var, "labels")
  label_values <- as.numeric(labels)
  filtered_labels <- labels[label_values %in% unique_values]
  
  result <- data.frame(value = as.numeric(filtered_labels), 
                       label = names(filtered_labels), 
                       stringsAsFactors = FALSE)
  
  return(result)
}

# Example usage
cd_postos <- unique(cur_cabo$posto)
cd_postos <- extract_present_labels(cd_postos)
cd_postos

cd_districts <- unique(cur_cabo$district)
cd_districts <- extract_present_labels(cd_districts)
cd_districts




cur_cabo <- cur_cabo %>%
  mutate(district = as_factor(district),  
    posto = as_factor(posto))

nrow(cur_cabo)

summary_table <- cur_cabo %>%
  group_by(district, posto) %>%
  summarise(num_respondents = n()) %>%
  arrange(district, posto)

print(summary_table)

sum(summary_table$num_respondents)

cur_cabo$district <- as.numeric(cur_cabo$district)
cur_cabo$posto <- as.numeric(cur_cabo$posto)

# only had 18 responses from Palma out of 933
adi <- subset(cur_cabo, cur_cabo$district == 31)
aii <- subset(cur_cabo, cur_cabo$district != 31)

dim(adi)
dim(aii)

unique(adi$posto)

cur_cabo$adi <- ifelse(cur_cabo$district == 31, 1, 0)


# save labeled posto names as characters
joa_postos <- c(63, 84, 71, 80, 72, 93, 64, 78, 95, 94, 97, 65)

adi_joa <- c(63, 84, 71, 80, 72, 93, 64, 78, 65)

cur_cabo$joa <- ifelse(cur_cabo$posto %in% joa_postos, 1, 0)

joa <- cur_cabo %>%
	dplyr::filter(cur_cabo$posto %in% joa_postos)

dim(joa)

not_joa <- cur_cabo %>%
	dplyr::filter(! cur_cabo$posto %in% joa_postos)

dim(not_joa)

cur_cabo$joa <- ifelse(cur_cabo$posto %in% joa_postos, 1, 0)

cur_cabo$not_adi_joa <- ifelse(cur_cabo$posto %in% adi_joa, 1, 0)

print(cur_cabo, n = 40)


unique(cur_cabo$trust_private)

mean(cur_cabo$trust_private)
mean(adi$trust_private)
mean(aii$trust_private)


## Responses in Palma compared to out of it

names(cur_cabo)

# do people have different expectations about infrastructure improvements?
hep <- t.test(adi$inf_improve, aii$inf_improve)
t.test(joa$inf_improve, not_joa$inf_improve)

hep$stderr

# write function
t_test_viz_combined <- function(vec1, vec2, vec3, vec4, 
                                group1 = "Group 1", group2 = "Group 2", 
                                group3 = "Group 3", group4 = "Group 4", 
                                test_name1 = "Test 1", test_name2 = "Test 2",
                                title) {
  
  # Helper function
  run_t_test <- function(vec_a, vec_b, g1, g2, test_name) {
    t_test_result <- t.test(vec_a, vec_b, conf.level = 0.95)
    
    mean1 <- mean(vec_a, na.rm = TRUE)
    mean2 <- mean(vec_b, na.rm = TRUE)
    
    stderr_95 <- t_test_result$stderr
    t_test_90 <- t.test(vec_a, vec_b, conf.level = 0.90)
    stderr_90 <- t_test_90$stderr
    
    data.frame(
      group = c(g1, g2),
      estimate = c(mean1, mean2),
      lower_90 = c(mean1 - 1.645*stderr_90, mean2 - 1.645*stderr_90),
      upper_90 = c(mean1 + 1.645*stderr_90, mean2 + 1.645*stderr_90),
      lower_95 = c(mean1 - 1.96*stderr_90, mean2 - 1.96*stderr_90),
      upper_95 = c(mean1 + 1.96*stderr_90, mean2 + 1.96*stderr_90),
      test = test_name  # Identify which test this belongs to
    )
  }
  
  # Run both t-tests
  df1 <- run_t_test(vec1, vec2, group1, group2, test_name1)
  df2 <- run_t_test(vec3, vec4, group3, group4, test_name2)
  
  # Combine data
  results_df <- bind_rows(df1, df2)
  
  # Reorder the groups by estimate
  results_df <- results_df %>% 
    arrange(estimate) %>% 
    mutate(group = factor(group, levels = unique(group)))

  results_df <- results_df %>% 
		mutate(group = factor(group, levels = c("Not JOA", "AII","JOA",  "ADI"))) %>% 
  arrange(group)

  
  return(list(results_df = results_df, plot = plot))
}


unique(cur_cabo$inf_improve)

# for joa, not_joa, adi, and aii, make responses of c(5, 6, 7) into NA
rm_answers <- c(5, 6, 7)

clean_unknowns <- function(vec, rm_answers = c(5, 6, 7)) {
	vec <- ifelse(vec %in% rm_answers, NA, vec)
}

joa$inf_improve <- clean_unknowns(joa$inf_improve)
not_joa$inf_improve <- clean_unknowns(not_joa$inf_improve)
adi$inf_improve <- clean_unknowns(adi$inf_improve)
aii$inf_improve <- clean_unknowns(aii$inf_improve)

cur_cabo$inf_improve <- clean_unknowns(cur_cabo$inf_improve)

result <- t_test_viz_combined(joa$inf_improve, not_joa$inf_improve, 
                              adi$inf_improve, aii$inf_improve, 
                              "JOA", "Not JOA", "ADI", "AII", 
                              "JOA vs Not JOA", "ADI vs AII")

result$results_df

inf_improve_adi <- lm(inf_improve ~ adi, data = cur_cabo)
summary(inf_improve_adi)

inf_improve_joa <- lm(inf_improve ~ joa, data = cur_cabo)
summary(inf_improve_joa)

inf_improve_lm <- list(inf_improve_adi, inf_improve_joa)





ggplot(result$results_df, aes(x = estimate, y = factor(group, levels = c("Not JOA", "JOA", "AII", "ADI")), color = test)) +    geom_segment(aes(x = lower_95, xend = upper_95, y = group, yend = group), linewidth = 0.5) +
  geom_segment(aes(x = lower_90, xend = upper_90, y = group, yend = group), linewidth = 1.2) +
  geom_point(size = 3) +  
  labs(x = "Mean Estimate", y = "Group", title = "Likelihood of Infrastructure Improvements") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red")) +
  guides(color = guide_legend(title = NULL)) + 
  theme(
    text = element_text(family = "serif"), 
    axis.text.y = element_text(family = "serif", face = "bold"))

plot_results <- function(results_df, title = title) {
  ggplot(results_df, aes(x = estimate, y = factor(group, levels = c("Not JOA", "JOA", "AII", "ADI")), color = test)) + 
    geom_segment(aes(x = lower_95, xend = upper_95, y = group, yend = group), linewidth = 0.5) +
    geom_segment(aes(x = lower_90, xend = upper_90, y = group, yend = group), linewidth = 1.2) +
    geom_point(size = 3) +  
    labs(x = "Mean Estimate", y = "Group", title = title) + 
    theme_minimal() +
    scale_color_manual(values = c("blue", "red")) +
    guides(color = guide_legend(title = NULL)) + 
    theme(
      text = element_text(family = "serif"), 
      axis.text.y = element_text(family = "serif", face = "bold"))
}








# why do they have different views?
unique(cur_cabo$inf_improve_loc)

dfs <- list(joa, not_joa, adi, aii)

cleaned_dfs <- lapply(dfs, function(df) {
  df$inf_improve_loc <- clean_unknowns(df$inf_improve_loc)
  return(df)
})

Mode(adi$inf_improve_loc) # It would be near schools and hospitals
Mode(aii$inf_improve_loc) # In places that most need improvements

Mode(joa$inf_improve_loc) # It would be near schools and hospitals
Mode(not_joa$inf_improve_loc) # In places that most need improvements


# political engagements
unique(joa$voted)

voted <- t_test_viz_combined(joa$voted, not_joa$voted, 
                              adi$voted, aii$voted, 
                              "JOA", "Not JOA", "ADI", "AII", 
                              "JOA vs Not JOA", "ADI vs AII")

voted$results_df

plot_results(voted$results_df, "Voting History")


cur_cabo$voted <- clean_unknowns(cur_cabo$voted)


voted_adi <- lm(voted ~ adi, data = cur_cabo)
summary(voted_adi)

voted_joa <- lm(voted ~ joa, data = cur_cabo)
summary(voted_joa)

voted_lm <- list(voted_adi, voted_joa)


unique(joa$campgained)

campgained <- t_test_viz_combined(joa$campgained, not_joa$campgained, 
                              adi$campgained, aii$campgained, 
                              "JOA", "Not JOA", "ADI", "AII", 
                              "JOA vs Not JOA", "ADI vs AII")

campgained$results_df

plot_results($results_df, "Campaigning History")


cur_cabo$campgained <- clean_unknowns(cur_cabo$campgained)

head(cur_cabo$campgained)

campgained_adi <- lm(campgained ~ adi, data = cur_cabo)
summary(campgained_adi)

campgained_joa <- lm(campgained ~ joa, data = cur_cabo)
summary(campgained_joa)

campgained_lm <- list(campgained_adi, campgained_joa)



unique(joa$protest)

protest <- t_test_viz_combined(joa$protest, not_joa$protest, 
                              adi$protest, aii$protest, 
                              "JOA", "Not JOA", "ADI", "AII", 
                              "JOA vs Not JOA", "ADI vs AII")

protest$results_df

plot_results(protest$results_df, "History of Protesting the Government")


cur_cabo$protest <- clean_unknowns(cur_cabo$protest)

protest_adi <- lm(protest ~ adi, data = cur_cabo)
summary(protest_adi)

protest_joa <- lm(protest ~ joa, data = cur_cabo)
summary(protest_joa)

protest_lm <- list(protest_adi, protest_joa)





all_adi <- list(voted_adi, campgained_adi, protest_adi)

footnote_text <- "Significance levels: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001."


modelsummary(inf_improve_lm, 
	estimate = "{estimate}{stars}",
	output = "latex", 
	gof_map = c("nobs", "r.squared"),
	coef_rename = c('adi' = 'ADI', 'joa' = 'JOA'),
	title = 'Survey Responses on Infrastructure Improvements in Directly Influenced Areas (ADI) Compared to Responses in Indirectly Influenced Areas (AII) in Cabo Delgado',
	col.names = c('', 'Infrastructure Improvements (ADI)', 'Infrastructure Improvements (JOA)'),
	notes = footnote_text
	)

modelsummary(all_adi, 
	estimate = "{estimate}{stars}",
	output = "latex", 
	gof_map = c("nobs", "r.squared"),
	coef_rename = c('adi' = 'ADI'),
	title = 'Survey Responses in Directly Influenced Areas (ADI) Compared to Responses in Indirectly Influenced Areas (AII) in Cabo Delgado',
	col.names = c('', 'Voting History', 'Campaigning', 'Protest Attendance'),
	notes = footnote_text
	)


all_joa <- list(voted_joa, campgained_joa, protest_joa)

modelsummary(all_joa, 
	estimate = "{estimate}{stars}",
	output = "latex", 
	coef_rename = c('joa' = 'JOA'),
	gof_map = c("nobs", "r.squared"),
	title = 'Survey Responses in Jointly Operated Areas (JOA) Compared to Responses in non-LNG Operated Postos in Cabo Delgado',
	col.names = c('', 'Voting History', 'Campaigning', 'Protest Attendance'),
	notes = footnote_text
	)


all_models <- list(voted_adi, campgained_adi, protest_adi, voted_joa, campgained_joa, protest_joa)


modelsummary(all_models, 
	estimate = "{estimate}{stars}",
	output = "latex", 
	coef_rename = c('adi' = 'ADI', 'joa' = 'JOA'),
	gof_map = c("nobs", "r.squared"),
	title = 'Survey Responses in ADI, AII, JOA and non-JOA Postos in Cabo Delgado',
	col.names = c('', 'Voting History (ADI)', 'Campaigning (ADI)', 'Protest Attendance (ADI)', 'Voting History (JOA)', 'Campaigning (JOA)', 'Protest Attendance (JOA)'),
	notes = footnote_text
	)



# trust
unique(joa$trust_private)


trust_private <- t_test_viz_combined(joa$trust_private, not_joa$trust_private, adi$trust_private, aii$trust_private, 
                              "JOA", "Not JOA", "ADI", "AII", 
                              "JOA vs Not JOA", "ADI vs AII")

trust_private$results_df

plot_results(trust_private$results_df, "Trust in Private Enterprises")


unique(joa$trust_cent)

trust_cent <- t_test_viz_combined(joa$trust_cent, not_joa$trust_cent, adi$trust_cent, aii$trust_cent, 
                              "JOA", "Not JOA", "ADI", "AII", 
                              "JOA vs Not JOA", "ADI vs AII")

trust_cent$results_df

plot_results(trust_cent$results_df, "Trust in Central Government")



unique(joa$trust_mun)

trust_mun <- t_test_viz_combined(joa$trust_mun, not_joa$trust_mun, adi$trust_mun, aii$trust_mun, 
                              "JOA", "Not JOA", "ADI", "AII", 
                              "JOA vs Not JOA", "ADI vs AII")

trust_mun$results_df

plot_results(trust_mun$results_df, "Trust in Municipal Government")


unique(joa$trust_FADM)

trust_FADM <- t_test_viz_combined(joa$trust_FADM, not_joa$trust_FADM, adi$trust_FADM, aii$trust_FADM, 
                              "JOA", "Not JOA", "ADI", "AII", 
                              "JOA vs Not JOA", "ADI vs AII")

trust_FADM$results_df

plot_results(trust_FADM$results_df, "Trust in FADM")



unique(joa$trust_intl)

trust_intl <- t_test_viz_combined(joa$trust_intl, not_joa$trust_intl, adi$trust_intl, aii$trust_intl, 
                              "JOA", "Not JOA", "ADI", "AII", 
                              "JOA vs Not JOA", "ADI vs AII")

trust_intl$results_df

plot_results(trust_intl$results_df, "Trust in International Organizations")



#### Other political behaviors

names(adi)

# xitiki
# church
# savings
# assist_neigh
# volunteer_com 
# participate_com 


xitiki_adi <- lm(xitiki ~ adi, data = cur_cabo)
summary(xitiki_adi)

xitiki_joa <- lm(xitiki ~ joa, data = cur_cabo)
summary(xitiki_joa)

xitiki_lm <- list(xitiki_adi, xitiki_joa)



church_adi <- lm(church ~ adi, data = cur_cabo)
summary(church_adi)

church_joa <- lm(church ~ joa, data = cur_cabo)
summary(church_joa)

church_lm <- list(church_adi, church_joa)


savings_adi <- lm(savings ~ adi, data = cur_cabo)
summary(savings_adi)

savings_joa <- lm(savings ~ joa, data = cur_cabo)
summary(savings_joa)

savings_lm <- list(savings_adi, savings_joa)


volunteer_com_adi <- lm(volunteer_com ~ adi, data = cur_cabo)
summary(volunteer_com_adi)

volunteer_com_joa <- lm(volunteer_com ~ joa, data = cur_cabo)
summary(volunteer_com_joa)

volunteer_com_lm <- list(volunteer_com_adi, volunteer_com_joa)


participate_com_adi <- lm(participate_com ~ adi, data = cur_cabo)
summary(participate_com_adi)

participate_com_joa <- lm(participate_com ~ joa, data = cur_cabo)
summary(participate_com_joa)

participate_com_lm <- list(participate_com_adi, participate_com_joa)

alt_pol_adi <- list("Xitiki" = xitiki_adi, "Church" = church_adi, "Savings" = savings_adi, "Community Group" = volunteer_com_adi, "Community Meeting" = participate_com_adi)

alt_pol_joa <- list("Xitiki" = xitiki_joa, "Church" = church_joa, "Savings" = savings_joa, "Community Group" = volunteer_com_joa, "Community Meeting" = participate_com_joa)


modelsummary(alt_pol_adi, 
	estimate = "{estimate}{stars}",
	output = "latex", 
	coef_rename = c('adi' = 'ADI'),
	gof_map = c("nobs", "r.squared"),
	title = 'Survey Responses in Jointly Operated Areas (JOA) Compared to Responses in non-LNG Operated Postos in Cabo Delgado',
	notes = footnote_text
	)

modelsummary(alt_pol_joa, 
	estimate = "{estimate}{stars}",
	output = "latex", 
	coef_rename = c('joa' = 'JOA'),
	gof_map = c("nobs", "r.squared"),
	title = 'Survey Responses in Jointly Operated Areas (JOA) Compared to Responses in non-LNG Operated Postos in Cabo Delgado',
	notes = footnote_text
	)





