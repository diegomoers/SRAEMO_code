library(haven)
library(readxl)
library(dplyr)
library(MASS)
library(tidyr)
library(ggplot2)
library(broom)
library(margins)
library(marginaleffects)
library(effects)
library(ordinal)
library(scales)   

background_data_path <- "/Users/diegomoers/Desktop/SRAEMO/SRAEMO_code/DATA/background_data_062020.dta"
automation_data_path <- "/Users/diegomoers/Desktop/SRAEMO/SRAEMO_code/DATA/SW_automation_2020.dta"
lmi_index_path <- "/Users/diegomoers/Desktop/SRAEMO/SRAEMO_code/DATA/LMII-AE Index-2019.xlsx"
occupation_data_path <- "/Users/diegomoers/Desktop/SRAEMO/SRAEMO_code/DATA/occupation_w13_WaS.dta"
background_data <- read_dta(background_data_path)
automation_data <- read_dta(automation_data_path)
occupation_data <- read_dta(occupation_data_path)
lmi_index <- read_excel(lmi_index_path, sheet = 3)

#1. Merging survey datasets by ID
merged_surveys <- background_data %>%
  inner_join(automation_data, by = "nomem_encr") %>%
  inner_join(occupation_data, by = "nomem_encr")

#2. Filter for employed people only
employed_data <- merged_surveys %>%
  filter(sh20a001 >= 1 & sh20a001 <= 5)

#3. Add in LMI index
lmi_index_clean <- lmi_index %>%
  dplyr::select(1:2) %>%  # Select first two columns
  rename(ISCO = 1, automation_risk = 2)
final_data <- employed_data %>%
  left_join(lmi_index_clean, by = c("cw20m611" = "ISCO"))

#5. Remove people who do not have matched LMI score
final_clean_data <- final_data %>%
  filter(!is.na(automation_risk))

#6a. Calculate job tenure
final_clean_data$job_tenure_years <- 2020 - final_clean_data$cw20m134

#6b. Clean up false tenure + missing education
final_clean_data <- final_clean_data %>%
  filter(!nomem_encr %in% c(835558, 836486, 849852, 852774, 856915, 863510))
sum(final_clean_data$nomem_encr %in% c(835558, 836486, 849852, 852774, 856915, 863510))  # Should return 0

#7. Make variables for analysis
analysis_data <- final_clean_data %>%
  mutate(
    automation_risk_orig = automation_risk,
    automation_risk_std = scale(automation_risk)[,1],
  
    subjective_risk_std = scale(sh20a005)[,1],
    subjective_risk = factor(sh20a005, 
                             levels = 1:5, 
                             ordered = TRUE,
                             labels = c("Completely Disagree", "Disagree", 
                                        "Neutral", 
                                        "Agree", "Completely Agree")),
    
    education = factor(oplcat,
                       levels = 1:6,
                       labels = c("Primary", "VMBO", "HAVO/VWO", "MBO", "HBO", "WO")),
    education_group = case_when(
      oplcat %in% c(1, 2) ~ "Lower Education",
      oplcat %in% c(3, 4) ~ "Intermediate Education",
      oplcat %in% c(5, 6) ~ "Higher Education",
      TRUE ~ NA_character_
    ) %>%
      factor(levels = c("Lower Education", 
                        "Intermediate Education", 
                        "Higher Education")),
    
    sector = factor(case_when(
      sh20a001 == 1 ~ "Public",
      sh20a001 %in% c(2,3,4,5) ~ "Private",
      TRUE ~ NA_character_
    )),
    
    age = leeftijd,
    
    gender = factor(geslacht,
                    levels = c(1, 2),
                    labels = c("Male", "Female")),
    
    job_tenure = job_tenure_years
  )

#8. Descriptive statstics (Table 1)
age_summary <- analysis_data %>%
  summarise(
    mean_age = mean(leeftijd, na.rm = TRUE),
    sd_age = sd(leeftijd, na.rm = TRUE)
  ) %>%
  mutate(across(everything(), ~round(., 2)))
print(age_summary)

education_dist <- analysis_data %>%
  group_by(education_group) %>%
  summarise(
    n = n(),
    percentage = n() / nrow(analysis_data) * 100,
    .groups = "drop"
  ) %>%
  mutate(percentage = round(percentage, 2))
print(education_dist)

sector_dist <- analysis_data %>%
  group_by(sector) %>%
  summarise(
    n = n(),
    percentage = n() / nrow(analysis_data) * 100,
    .groups = "drop"
  ) %>%
  mutate(percentage = round(percentage, 2))
print(sector_dist)

gender_dist <- analysis_data %>%
  group_by(geslacht) %>%
  summarise(
    n = n(),
    percentage = n() / nrow(analysis_data) * 100,
    .groups = "drop"
  ) %>%
  mutate(percentage = round(percentage, 2))  
print(gender_dist)

tenure_summary <- analysis_data %>%
  summarise(
    mean_tenure = mean(job_tenure_years, na.rm = TRUE),
    sd_tenure = sd(job_tenure_years, na.rm = TRUE)
  ) %>%
  mutate(across(everything(), ~round(., 2)))
print(tenure_summary)

# ---------- VISUALIZATION OF RELATIONSHIP ----------

#9. Pooled relationship obj and subj risk (Figure 1)
p1 <- ggplot(analysis_data, aes(x = automation_risk_orig, y = as.numeric(subjective_risk))) +
  geom_jitter(alpha = 0.3, width = 0.2, height = 0.2) +
  geom_smooth(method = "lm", color = "black") +
  labs(
    title = "Relationship Between Objective and Subjective Automation Risk",
    x = "Objective Automation Risk (LMI Index, scale 1-10)",
    y = "Subjective Risk Assessment (scale 1-5)"
  ) +
  theme_minimal()
ggsave("obj_subj_risk_relationship.png", p1, width = 8, height = 6)

#10. Relationship obj and subj risk per education group (Figure 2)
p2 <- ggplot(analysis_data, aes(x = automation_risk_orig, y = as.numeric(subjective_risk), shape = education_group)) +
  geom_jitter(alpha = 0.3, width = 0.2, height = 0.2) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  labs(
    title = "Relationship Between Subjective and Objective Risks by Education Level",
    x = "Objective Automation Risk (LMI Index, scale 1-10)",
    y = "Subjective Risk Assessment (LISS, scale 1-5)"
  ) +
  theme_minimal() +
  facet_wrap(~education_group) +
  scale_shape_manual(values = c(16, 17, 15)) + 
  theme(legend.position = "none")
print(p2)
ggsave("obj_subj_risk_by_education.png", p2, width = 10, height = 4)

#11. Stats for Figure 2 (Table 2)
lower_corr <- cor(subset(analysis_data, education_group == "Lower Education")$automation_risk_orig, 
                  as.numeric(subset(analysis_data, education_group == "Lower Education")$subjective_risk))
intermediate_corr <- cor(subset(analysis_data, education_group == "Intermediate Education")$automation_risk_orig, 
                         as.numeric(subset(analysis_data, education_group == "Intermediate Education")$subjective_risk))
higher_corr <- cor(subset(analysis_data, education_group == "Higher Education")$automation_risk_orig, 
                   as.numeric(subset(analysis_data, education_group == "Higher Education")$subjective_risk))
corr_results <- data.frame(
  Education_Group = c("Lower", "Intermediate", "Higher"),
  Correlation = c(lower_corr, intermediate_corr, higher_corr)
)
print(corr_results)

#12. Ordered Logit Regressions (Table 4)
#12a. No controls
OLM1 <- polr(subjective_risk ~ automation_risk_std, 
                   data = analysis_data, 
                   Hess = TRUE)
me_OLM1 <- margins(OLM1)
summary(me_OLM1)
pred_effects1 <- Effect("automation_risk_std", OLM1, 
                       xlevels = list(automation_risk_std = seq(-2, 2, by = 1)))
print(pred_effects1)

#12b. With education control
OLM2 <- polr(subjective_risk ~ automation_risk_std + 
                     education_group,
                   data = analysis_data, 
                   Hess = TRUE)
pred_effects2 <- Effect("automation_risk_std", OLM2, 
                        xlevels = list(automation_risk_std = seq(-2, 2, by = 1)))
print(pred_effects2)

#12b. With all controls
OLM3 <- polr(subjective_risk ~ automation_risk_std + 
                     education_group + age + gender + sector + job_tenure,
                   data = analysis_data, 
                   Hess = TRUE)
pred_effects3 <- Effect("automation_risk_std", OLM3, 
                        xlevels = list(automation_risk_std = seq(-2, 2, by = 1)))
print(pred_effects3)

#13. Plotting the marginal effect of Obj Risk on Subj Risk with all controls (figure 3)
plot(pred_effects3, 
     main = "Effect of Objective Automation Risk on Subjective Risk Perception",
     xlab = "Standardized Automation Risk Score",
     ylab = "Probability",
     colors = "black")
png("risk_perception_plot.png", width = 600, height = 500, res = 60)
plot(pred_effects3, 
     main = "Effect of Objective Automation Risk on Subjective Risk Perception",
     xlab = "Standardized Automation Risk Score",
     ylab = "Probability",
     colors = "black",
     xlim = c(-1.5, 1.5))  # Adjust x-axis range here
dev.off()

#14. OLS regressions (Table 3)
#14a. No controls
OLS1 <- lm(subjective_risk_std ~ automation_risk_std, 
                 data = analysis_data)
summary(OLS1)

#14b. with education control
OLS2 <- lm(subjective_risk_std ~ automation_risk_std + 
                   education_group,
                 data = analysis_data)
summary(OLS2)

#14c. with all controls
OLS3 <- lm(subjective_risk_std ~ automation_risk_std + 
                   education_group + age + gender + sector + job_tenure,
                 data = analysis_data)
summary(OLS3)
