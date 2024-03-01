# Code for The Causal Effects of a Trump Endorsement on Voter Perceptions in a General Election Scenario
# Last updated 1/25 by Danielle. Did the last to update close the document? Yes. 

# Read in data. Ensure your current path is set to the Source File location for this script.

poll <- read.csv("../input/poll_2022_weighted_final_topost.csv")

# Load in packages

library(mediation)
library(ggplot2)
library(coefplot)
library(stargazer)
library(dplyr)
library(tidyverse)
library(pwr)
library(survey)
library(car)
library(aod)
library(pwrss)

#### IV Re-codes ####

table(poll$endorsement)

poll$endorsed <- "Control"

poll$endorsed[poll$endorsement=='Former President Trump has given Terry Mitchell his “complete and total endorsement!"'] <- "trumpYes"

poll$endorsed[poll$endorsement=='Former President Trump has said, “I will NOT be endorsing Terry Mitchell for Congress!”'] <- "trumpNo"

poll$endorsed <- factor(poll$endorsed, levels=c("trumpYes","trumpNo","Control"))

poll$endorsed <- relevel(poll$endorsed,ref="Control")

table(poll$endorsed)

class(poll$endorsed)

poll$stance[poll$conventional=='Terry supports lowering taxes on wealthy individuals and large corporations, wants to limit the role of the federal government in the health insurance marketplace, and opposes a path to citizenship for undocumented immigrants.'] <- "convenR"

poll$stance[poll$conventional=='Terry supports raising taxes on wealthy individuals and large corporations, wants to expand the role of the federal government in the health insurance marketplace, and believes in a path to citizenship for undocumented immigrants.'] <- "unconvenR"

poll$stance <- factor(poll$stance, levels=c("unconvenR","convenR"))

table(poll$stance)

class(poll$stance)

poll$partyID <- NA
poll$partyID[poll$pid3==1|poll$ind_lean==1] <- "Democrat"
poll$partyID[poll$pid3==2|poll$ind_lean==2] <- "Republican"
poll$partyID[poll$ind_lean==3|poll$ind_lean==4] <- "Independent/Other"

poll$partyID <- factor(poll$partyID, levels=c("Democrat","Republican","Independent/Other"))

poll$partyID <- relevel(poll$partyID,ref="Independent/Other")

table(poll$partyID)

# Create new variable for six conditions 

poll$condition<- interaction(poll$stance, poll$endorsed, sep = "_")

table(poll$condition)

class(poll$condition)

poll$condition <- relevel(poll$condition,ref="unconvenR_Control")

summary(poll$mitchell_vote ~ poll$partyID)

#### DV Re-codes ####

table(poll$mitchell_vote_scaled)
poll$mitchell_vote_scaled <- car::recode(poll$mitchell_vote, "1=0; 2=0.25; 3=0.5; 4=0.75; 5=1")

# Calculate mean for each party ID 

partyid_mean <- poll %>%
  group_by(partyID) %>%
  summarise_at(vars(mitchell_vote_scaled), list(mean_mitchell_vote_scaled = mean))


#### Figure 1: Distribution of responses by party ID ####
vote_table <- prop.table(table(poll$partyID, poll$mitchell_vote_scaled), 1)
All <- prop.table(table(poll$mitchell_vote_scaled))
vote_table <- data.frame(rbind(vote_table, All))
vote_table$Party <- c("Independents", "Democrats", "Republicans", "All Respondents")
l <- reshape(vote_table, 
             varying = c("X0", "X0.25", "X0.5", "X0.75", "X1"), 
             v.names = "count",
             timevar = "category", 
             times = c("X0", "X0.25", "X0.5", "X0.75", "X1"), 
             direction = "long")

voting_histogram <- ggplot(data = l, aes(category, count, fill=Party)) +
  scale_x_discrete(name="Re-scaled Likelihood to Vote", labels=c("0", "0.25", "0.5", "0.75", "1")) +
  scale_y_continuous(name="Proportion of Group Giving Response") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "identity") + 
  scale_fill_manual(values=c("Orange", "Blue", "Dark Green", "Red")) + 
  theme_bw()
ggsave(filename = "../output/figure1.png", plot = voting_histogram, width = 10, height = 6, units = "in", dpi = 300)

#### Likelihood of voting regressions ####

#### "Additive" Model ####
table(poll$partyID)
poll$partyID <- relevel(poll$partyID, "Independent/Other", "Republican", "Democrat")

#### Table 1 #### 

# Regress stance and endorsement on vote for entire sample. Table 1 Column 1.
stance_and_endorsement_table <- lm(mitchell_vote_scaled ~ stance + endorsed, data = poll)
summary(stance_and_endorsement_table)

# Regress stance and endorsement on vote for Democrats. Table 1 Column 2.
stance_and_endorsement_table_d <- lm(mitchell_vote_scaled ~ stance + endorsed, data = subset(poll, partyID == "Democrat"))
summary(stance_and_endorsement_table_d)

# Regress stance and endorsement on vote for Republicans. Table 1 Column 3.
stance_and_endorsement_table_r <- lm(mitchell_vote_scaled ~ stance + endorsed, data = subset(poll, partyID == "Republican"))
summary(stance_and_endorsement_table_r)

# Regress stance and endorsement on vote for Independents. Table 1 Column 4
stance_and_endorsement_table_i <- lm(mitchell_vote_scaled ~ stance + endorsed, data = subset(poll, partyID == "Independent/Other"))
summary(stance_and_endorsement_table_i)

# Combine stance and partyID and endorsement and partyID. Table 1 Column 5
combined_table <- lm(mitchell_vote_scaled ~ stance*partyID + endorsed*partyID, data=poll)
summary(combined_table)

# Test of effects against eachother
linearHypothesis(combined_table, c("stanceconvenR:partyIDDemocrat - partyIDDemocrat:endorsedtrumpYes"), vcov. = vcov(combined_table))
linearHypothesis(combined_table, c("stanceconvenR:partyIDRepublican - partyIDRepublican:endorsedtrumpYes"), vcov. = vcov(combined_table))

# Test of dem v repub

linearHypothesis(combined_table, c("partyIDDemocrat:endorsedtrumpYes + partyIDRepublican:endorsedtrumpYes = 0"))

# p value is 0.73

#### Interactive Model ####
# Regress six conditions on vote. 

# Democrats. Table 2 Column 1. 
interactive_table_d <- lm(mitchell_vote_scaled ~ condition, data=subset(poll, partyID=="Democrat"))
summary(interactive_table_d)

# Republicans. Table 2 Column 2.
interactive_table_r <- lm(mitchell_vote_scaled ~ condition, data=subset(poll, partyID=="Republican"))
summary(interactive_table_r)

# Independents. Table 2 Column 3. 
interactive_table_i <- lm(mitchell_vote_scaled ~ condition, data=subset(poll, partyID=="Independent/Other"))
summary(interactive_table_i)

#### Tables for regressions #### 

# Table 1: 

table_1_list <- list(stance_and_endorsement_table, stance_and_endorsement_table_d, stance_and_endorsement_table_r, stance_and_endorsement_table_i, combined_table)
  
summary(table_1_list)

stargazer(table_1_list, title="Table 1", align = T, dep.var.labels = c("Likelihood of Voting for Mitchell"), covariate.labels = c("Conventional Policy", "Democrat", "Republican", "Endorsement", "Anti-Endorsement", "Conventional Policy x Democrat", "Conventional Policy x Republican", "Democrat x Endorsement", "Republican x Endorsement", "Democrat x Anti-Endorsement", "Republican x Anti-Endorsement"), digits = 2,column.labels = c("All Participants", "Democrats", "Republicans", "Independents", "All Participants"), star.cutoffs = c(0.1, 0.05, 0.01), out = "../output/table1.htm") 

# Table 2: 
table_2_list <- list(interactive_table_d, interactive_table_r, interactive_table_i)

summary(table_2_list)

stargazer(table_2_list, title="Table 2", align = T, dep.var.labels = c("Likelihood of Voting for Mitchell"), covariate.labels = c("Conventional Policy x Control", "Unconventional Policy x  Endorsement", "Conventional Policy x Endorsement", "Unconventional Policy x Anti-Endorsement", "Conventional Policy x Anti-Endorsement"), digits = 2, column.labels = c("Democrats", "Republicans", "Independents"), star.cutoffs = c(0.1, 0.05, 0.01), out = "../output/table2.htm") 

#### Power Analysis (Post-Hoc) ####. 

# This is modeled from this website: https://cran.r-project.org/web/packages/pwrss/vignettes/examples.html#4_Logistic_Regression_(Wald%E2%80%99s_z_Test) the section called 2.1.1 Independent Samples t Test. 

# Overall effect

# grab sample means, sample standard deviations, and sample size
aggregate(poll$mitchell_vote_scaled, list(poll$endorsed), FUN=mean)
aggregate(poll$mitchell_vote_scaled, list(poll$endorsed), FUN=sd)
table(poll$endorsed)

pwrss.t.2means(mu1 = .435, mu2 = .398, sd1 = .350, sd2 = 0.347, kappa = 1.049, n2 = 448, alpha = 0.05, alternative = "not equal")

# power: 0.362

# in each party, comparing who saw endorsement and who saw control

# grab mean, std, and sample size
aggregate(poll$mitchell_vote, list(poll$partyID, poll$endorsed), FUN=mean)
aggregate(poll$mitchell_vote, list(poll$partyID, poll$endorsed), FUN=sd)
table(poll$endorsed, poll$partyID)

# dem saw endorsement mean: 2.0303, sd: 1.2978
# dem control mean: 2.448113, sd: 1.418

198/212

pwrss.t.2means(mu1 = 2.0303, mu2 = 2.448113, sd1 = 1.2978, sd2 = 1.418, kappa = 0.9339623, n2 = 212, alpha = 0.05, alternative = "not equal")

# power: .874

# rep saw endorsement mean: 3.402878, sd: 1.2494
# rep control mean: 3.179, sd: 1.3322

139/167

pwrss.t.2means(mu1 = 3.402878, mu2 = 3.179, sd1 = 1.2494, sd2 = 1.3322, kappa = 0.8323353, n2 = 167, alpha = 0.05, alternative = "not equal")

# power: 0.326

# indp mean saw endorsement: 2.5667, sd: 1.1519
# indp mean control: 2.5797, sd: 1.2532

90/69

pwrss.t.2means(mu1 = 2.5667, mu2 = 2.5797, sd1 = 1.1519, sd2 = 1.2532, kappa = 1.304348, n2 = 69, alpha = 0.05, alternative = "not equal")

# power: 0.051

#### Appendix Code ####

#### Collapsed Likelihood to Vote for Appendix ####

poll$mitchellVote_appendix[poll$mitchell_vote>3] <- 1
poll$mitchellVote_appendix[poll$mitchell_vote<=3] <- 0
table(poll$mitchellVote_appendix)

# All respondents. Column 1. 
vote_all_appendix <- lm(mitchellVote_appendix ~ stance + endorsed, data = poll)
summary(vote_all_appendix)

# Dems. Column 2. 
vote_d_appendix <- lm(mitchellVote_appendix ~ stance + endorsed, data = subset(poll, partyID == "Democrat"))
summary(vote_d_appendix)

# Reps. Column 3. 
vote_r_appendix <- lm(mitchellVote_appendix ~ stance + endorsed, data = subset(poll, partyID == "Republican"))
summary(vote_r_appendix)

# Indps. Column 4. 
vote_i_appendix <- lm(mitchellVote_appendix ~ stance + endorsed, data = subset(poll, partyID == "Independent/Other"))
summary(vote_i_appendix)

# Combine. Column 5
vote_combined_appendix <- lm(mitchellVote_appendix ~ stance*partyID + endorsed*partyID, data=poll)
summary(vote_combined_appendix)

# Create table 

tablea1 <- list(vote_all_appendix, vote_d_appendix, vote_r_appendix, vote_i_appendix, vote_combined_appendix)

summary(tablea1)

stargazer(tablea1, title="Table A.1", align = T, dep.var.labels = c("Likelihood of Voting for Mitchell"), covariate.labels = c("Conventional Policy", "Democrat", "Republican", "Endorsement", "Anti-Endorsement", "Conventional Policy x Democrat", "Conventional Policy x Republican", "Democrat x Endorsement", "Republican x Endorsement", "Democrat x Anti-Endorsement", "Republican x Anti-Endorsement"), digits = 2,column.labels = c("All Participants", "Democrats", "Republicans", "Independents", "All Participants"), star.cutoffs = c(0.1, 0.05, 0.01), out = "../output/tablea1.htm") 

#### Favorability ####

# Mimics Table 1 from paper

overallFav_endorse <- lm(mitchell_fave_1 ~ endorsed + stance, data = poll)
summary(overallFav_endorse)

fav_party_endorse_d <- lm(mitchell_fave_1 ~ endorsed + stance, data = subset(poll, partyID == "Democrat"))
summary(fav_party_endorse_d)

fav_party_endorse_r <- lm(mitchell_fave_1 ~ endorsed + stance, data = subset(poll, partyID == "Republican"))
summary(fav_party_endorse_r)

fav_party_endorse_i <- lm(mitchell_fave_1 ~ endorsed + stance, data = subset(poll, partyID == "Independent/Other"))
summary(fav_party_endorse_i)

fav_party_endorse <- lm(mitchell_fave_1 ~ endorsed*partyID + stance*partyID, data = poll)
summary(fav_party_endorse)

tablea2 <- list(overallFav_endorse, fav_party_endorse_d, fav_party_endorse_r, fav_party_endorse_i, fav_party_endorse)

stargazer(tablea2, title="Table A.2", align=T, dep.var.labels = c("Terry Mitchell Favorability"), covariate.labels = c("Endorsement", "Anti-Endorsement","Democrat","Republican", "Conventional Policy", "Endorsement x Democrat","Anti-Endorsement x Democrat", "Endorsement x Republican", "Anti-Endorsement x Republican", "Democrat x Conventional Policy", "Republican x Conventional Policy"), digits = 2,column.labels = c("All Participants", "Democrats", "Republicans", "Independents", "All Participants"),out="../output/tablea2.htm")

# Mimics Table 2 from paper 

interaction_fav_d <- lm(mitchell_fave_1 ~ condition, data = subset(poll, partyID == "Democrat"))
summary(interaction_fav_d)

interaction_fav_r <- lm(mitchell_fave_1 ~ condition, data = subset(poll, partyID == "Republican"))
summary(interaction_fav_r)

interaction_fav_i <- lm(mitchell_fave_1 ~ condition, data = subset(poll, partyID == "Independent/Other"))
summary(interaction_fav_i)

tablea3 <- list(interaction_fav_d, interaction_fav_r, interaction_fav_i)

stargazer(tablea3, title="Table A.3", align = T, dep.var.labels = c("Terry Mitchell Favorability"), covariate.labels = c("Conventional Policy Control", "Unconventional Policy x Endorsement", "Conventional Policy x Endorsement", "Unconventional Policy x Anti-Endorsement", "Conventional Policy x Anti-Endorsement"), digits = 2, column.labels = c("Democrats", "Republicans", "Independents"), star.cutoffs = c(0.1, 0.05, 0.01), out = "../output/tablea3.htm") 

#### Donation ####

# Mimics Table 1 from Paper 

overalldonate_endorse <- lm(mitchell_donation_1 ~ endorsed + stance, data = poll)
summary(overalldonate_endorse)

donate_party_endorse_d <- lm(mitchell_donation_1 ~ endorsed + stance, data = subset(poll, partyID == "Democrat"))
summary(donate_party_endorse_d)

donate_party_endorse_r <- lm(mitchell_donation_1 ~ endorsed + stance, data = subset(poll, partyID == "Republican"))
summary(donate_party_endorse_r)

donate_party_endorse_i <- lm(mitchell_donation_1 ~ endorsed + stance, data = subset(poll, partyID == "Independent/Other"))
summary(donate_party_endorse_i)

donate_party_endorse <- lm(mitchell_donation_1 ~ endorsed*partyID + stance*partyID, data = poll)
summary(donate_party_endorse)

tablea4 <- list(overalldonate_endorse, donate_party_endorse_d, donate_party_endorse_r, donate_party_endorse_i, donate_party_endorse)

stargazer(tablea4, title="Table A.4", align=T, dep.var.labels = c("Donations to Terry Mitchell"), covariate.labels = c("Endorsement", "Anti-Endorsement","Democrat","Republican", "Conventional Policy", "Endorsement x Democrat","Anti-Endorsement x Democrat", "Endorsement x Republican", "Anti-Endorsement x Republican", "Democrat x Conventional Policy", "Republican x Conventional Policy"), digits = 2,column.labels = c("All Participants", "Democrats", "Republicans", "Independents", "All Participants"),out="../output/tablea4.htm")

# Mimics Table 2 from Paper

interaction_donate_d <- lm(mitchell_donation_1 ~ condition, data = subset(poll, partyID == "Democrat"))
summary(interaction_donate_d)

interaction_donate_r <- lm(mitchell_donation_1 ~ condition, data = subset(poll, partyID == "Republican"))
summary(interaction_donate_r)

interaction_donate_i <- lm(mitchell_donation_1 ~ condition, data = subset(poll, partyID == "Independent/Other"))
summary(interaction_donate_i)

tablea5 <- list(interaction_donate_d, interaction_donate_r, interaction_donate_i )

stargazer(tablea5, title="Table A.5", align = T, dep.var.labels = c("Donations to Terry Mitchell"), covariate.labels = c("Conventional Policy x Control", "Unconventional Policy x Endorsement", "Conventional Policy x Endorsement", "Unconventional Policy x Anti-Endorsement", "Conventional Policy x Anti-Endorsement"), digits = 2, column.labels = c("Democrats", "Republicans", "Independents"), star.cutoffs = c(0.1, 0.05, 0.01), out = "../output/tablea5.htm") 

## power for appendix ##

aggregate(poll$mitchell_fave_1, list(poll$partyID, poll$endorsed), FUN=mean)
aggregate(poll$mitchell_fave_1, list(poll$partyID, poll$endorsed), FUN=sd)
table(poll$endorsed, poll$partyID)

# dem saw endorsement mean: 33.035, sd: 33.15
# dem saw control mean: 49.132, sd: 36.11
# rep saw endorsement mean: 62.48 sd: 26.4
# rep saw control mean:  56.13 31.24

139/167
198/212

pwrss.t.2means(mu1 = 62.48, mu2 = 56.13, sd1 = 26.4, sd2 = 31.24, kappa = 0.8323353, n2 = 167, alpha = 0.05, alternative = "not equal")

# power: 0.485

pwrss.t.2means(mu1 = 33.035, mu2 = 49.132, sd1 = 33.15, sd2 = 36.11, kappa = 0.9339623, n2 = 212, alpha = 0.05, alternative = "not equal")

# power: 0.997











