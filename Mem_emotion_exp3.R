

##Load packages
library(rstatix)
library(Rmisc)

##Import data
me3_data <- read.csv(file = "Mem_emotion_exp3.csv", header = TRUE, stringsAsFactors = FALSE)



#Check for baseline differences in feeling ratings across groups/conditions
k <- select(me3_data, Subject, Group, P_AMQ_Feel, C_AMQ_Feel)
l <- k %>% convert_as_factor(Subject, Group)

m <- gather(l, condition, base_feel, P_AMQ_Feel:C_AMQ_Feel, factor_key=TRUE)

levels(m$condition)[levels(m$condition)=="P_AMQ_Feel"] <- "Positive"
levels(m$condition)[levels(m$condition)=="C_AMQ_Feel"] <- "Control"

me3_basef_long <- m %>% convert_as_factor(Subject, Group, condition)

#2x2 anova
me3_anova_fc <- lm(base_feel~Group*condition,data=me3_basef_long)
me3_anova_fc_res <- Anova(me3_anova_fc,type=3)
Anova(me3_anova_fc,type=3)
eta_squared(me3_anova_fc_res)

#Calculate avg baseline feeling for each group
me3_data$avg_base_feel <- (me3_data$P_AMQ_Feel+me3_data$C_AMQ_Feel)/2
me3_basefeel <- select(me3_data, Subject, Group, avg_base_feel)
me3_basefeel_wide <- spread(me3_basefeel, Group, avg_base_feel)

#Compare avg baseline feeling between groups
t.test(me3_basefeel_wide$`Delayed-test`, me3_basefeel_wide$`Immediate-test`, paired=FALSE, var.equal=TRUE)
t.test(me3_basefeel_wide$`Delayed-test`, me3_basefeel_wide$`No-reminder`, paired=FALSE, var.equal=TRUE)
t.test(me3_basefeel_wide$`Immediate-test`, me3_basefeel_wide$`No-reminder`, paired=FALSE, var.equal=TRUE)
cohens_d(me3_basefeel, avg_base_feel~Group, paired=FALSE)



#Feeling change
k <- select(me3_data, Subject, Group, P_feelchange, C_feelchange)
l <- k %>% convert_as_factor(Subject, Group)

m <- gather(l, condition, feeling_change, P_feelchange:C_feelchange, factor_key=TRUE)

levels(m$condition)[levels(m$condition)=="P_feelchange"] <- "Positive"
levels(m$condition)[levels(m$condition)=="C_feelchange"] <- "Control"

me3_feel_long <- m %>% convert_as_factor(Subject, Group, condition)

#Combine feeling change & baseline feeling variables
me3_feel_combo <- merge(me3_feel_long, me3_basef_long, by = c("Subject", "condition", "Group"), all = TRUE)

##Anova comparing feeling change by group/condition, controlling for baseline feeling ratings
model1 <- lm(feeling_change ~ base_feel + Group*condition, data = me3_feel_combo)
modelout <- Anova(model1,type=3)
Anova(model1,type=3)
eta_squared(modelout)

#Pairwise t-tests for feeling change, controlling for baseline feeling ratings
#Combine differential feeling change and baseline feeling variables
me3_differfeel <- select(me3_data, Subject, Group, Differ_FeelChange)
me3_avgfeel_combo <- merge(me3_differfeel, me3_basefeel, by = c("Subject", "Group"), all = TRUE)

#Select Immediate-test & No-reminder
a <- me3_avgfeel_combo
a <- a %>% filter(Group != 'Delayed-test')

model2 <- lm(Differ_FeelChange ~ avg_base_feel + Group, data = a)
modelout2 <- Anova(model2,type=3)
Anova(model2,type=3)
eta_squared(modelout2)
summary(model2)

#Select Delayed-test & Immediate-test
a <- me3_avgfeel_combo
a <- a %>% filter(Group != 'No-reminder')

model2 <- lm(Differ_FeelChange ~ avg_base_feel + Group, data = a)
modelout2 <- Anova(model2,type=3)
Anova(model2,type=3)
eta_squared(modelout2)
summary(model2)

#Select Delayed-test & No-reminder
a <- me3_avgfeel_combo
a <- a %>% filter(Group != 'Immediate-test')

model2 <- lm(Differ_FeelChange ~ avg_base_feel + Group, data = a)
modelout2 <- Anova(model2,type=3)
Anova(model2,type=3)
eta_squared(modelout2)
summary(model2)




#Feeling change without controlling for baseline feeling ratings
#2x2 anova
me3_anova_fc <- lm(feeling_change~Group*condition,data=me3_feel_long)
me3_anova_fc_res <- Anova(me3_anova_fc,type=3)
Anova(me3_anova_fc,type=3)
eta_squared(me3_anova_fc_res)

#t-tests
me3_differfeel <- select(me3_data, Subject, Group, Differ_FeelChange)
me3_differfeel_wide <- spread(me3_differfeel, Group, Differ_FeelChange)
t.test(me3_differfeel_wide$`Delayed-test`, me3_differfeel_wide$`Immediate-test`, paired=FALSE, var.equal=TRUE)
t.test(me3_differfeel_wide$`Delayed-test`, me3_differfeel_wide$`No-reminder`, paired=FALSE, var.equal=TRUE)
t.test(me3_differfeel_wide$`Immediate-test`, me3_differfeel_wide$`No-reminder`, paired=FALSE, var.equal=TRUE)
cohens_d(me3_differfeel, Differ_FeelChange~Group, paired=FALSE)


