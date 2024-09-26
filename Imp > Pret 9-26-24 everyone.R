#Order of instruction of preterit-imperfect project code
#3-13-23; updated 9-26-24
#Sophia Minnillo

#project about whether order of instruction
#impacts L2 Spanish preterit-imperfect accuracy and suppliance

#load packages
library(tidyverse)
library(stringr)
library(broom)
library(lme4)
library(psych)
library(afex)
#install.packages('sjPlot')
library(sjPlot)
library(effects)
library(sjstats)
library(ngramr)
library(Rmisc) #for SummarySE
library(plyr)
library(dplyr)
library(rstatix)
library(emmeans)
library(MuMIn)
#install.packages('lmerTest')
library(lmerTest)
#install.packages('effectsize')
library(effectsize)

## Load CSV ####

#read metadata
meta <- read_csv("impfct first_all data - combined_3.csv") |>
  #have to drop NAs of people who don't have review quiz scores
  #N = 106
  drop_na(review_quiz)
#view(meta)

#summarize metadata
meta_summary <- meta %>%
  # dplyr::group_by(Participant, gender)%>%
  # dplyr::summarise(count = n())
  dplyr::summarize(mean_age = mean(age))
#view(meta_summary)

#load csv with just people who had some past obligatory contexts
past_ppl <- read_csv('imp_1st_some_past_ppl_073123.csv')%>%
  select(-count)
#view(past_ppl)

#load csv with all data
csv <- read_csv("SPA_2_3_Control_Experimental_07_25_23 - Sheet1.csv")%>%
  dplyr::mutate(Form = tolower(Form),
                `Corrected Form` = tolower(`Corrected Form`))%>%
  #replace NAs in Corrected Form column with the forms in Form
  dplyr::mutate(`Corrected Form` = coalesce(`Corrected Form`,Form))
#View(csv)

## Clean CSV ####
#turn NAs to 0s
csv$Appropriate[is.na(csv$Appropriate)] <- 0
csv$Ambiguous[is.na(csv$Ambiguous)] <- 0
csv$Inappropriate[is.na(csv$Inappropriate)] <- 0

#case when for form: turning different forms into numbers
#sometimes annotators spelled differently
csv1 <- csv %>%
  dplyr::mutate(Form_num = case_when(
    Form == 'preterit' | 
      Form == 'preterite' |
      Form == 'PRETERIT' |
      Form == 'pretérito' ~ 1,
    Form == 'imperfect' |
      Form == 'IMPERFECT' |
      Form == 'imperfecto' ~ 2,
    Form == 'present' |
      Form == 'PRESENT' |
      Form == 'presente' ~ 3,
    Form == 'infinitive' |
      Form == 'INFINITIVE' |
      Form == 'infinitivo' |
      Form == 'infintive'|
      Form == 'infinative'~ 4,
    Form == 'gerund' |
      Form == 'GERUND' |
      Form == 'Gerund' |
      Form == 'gerundio' ~ 5,
    Form == 'pluscuamperfecto' |
      Form == 'PLUSCUAMPERFECTO' |
      Form == 'PLUPERFECT' |
      Form == 'past perfect' |
      Form == 'PAST PERFECT' |
      Form == 'pluperfect' ~ 6, #added 8-15
    Form == 'imperfect subjunctive' |
      Form == 'IMPERFECT SUBJUNCTIVE' |
      Form == 'PAST SUBJUNCTIVE' |
      Form == 'past subjunctive' ~ 7,
    Form == 'present subjunctive' |
      Form == 'PRESENT SUBJUNCTIVE' |
      `Form` == 'subjunctive' ~ 8, #added 7-29
    Form == 'command' |
      Form == 'COMMAND' |
      Form == 'IMPERATIVE' |
      Form == 'imperative' |
      Form == 'imperativo' ~ 9, #added 7-29
    Form == 'conditional' |
      Form == 'CONDITIONAL' |
      Form == 'condicional' ~ 10, #added 7-29
    Form == 'future' |
      Form == 'FUTURE' |
      Form == 'FUTURO' |
      `Form` == 'Future' |
      Form == 'futuro' ~ 11, #added 7-29
    Form == 'AMBIGUOUS' |
      Form == 'ambiguous' |
      Form == 'ambiguo' ~ 12, #added 7-30
    Form == 'past participle' |
      Form == 'PAST PARTICIPLE' |
      Form == 'participle'|
      Form == 'participio pasado' ~ 13, #added 8-15
    Form == 'present perfect' |
      Form == 'PRESENT PERFECT'~ 14, #added 8-15
    Form == 'pluperfect subjunctive' |
      Form == 'PLUPERFECT SUBJUNCTIVE' ~ 15,
    Form == 'English' ~ 16,
    TRUE ~ 1000))%>% #anything else gets a large # to draw our attention
  dplyr::mutate(Corrected_num = case_when(
    `Corrected Form` == 'preterit' |
      `Corrected Form` == 'preterite' |
      `Corrected Form` == 'PRETERIT' |
      `Corrected Form` == 'preterito' ~ 1,
    `Corrected Form` == 'imperfect' |
      `Corrected Form` == 'IMPERFECT' |
      `Corrected Form` == 'imperfecto' ~ 2,
    `Corrected Form` == 'present' |
      `Corrected Form` == 'PRESENT' |
      `Corrected Form` == 'presente' ~ 3,
    `Corrected Form` == 'infinitive' |
      `Corrected Form` == 'INFINITIVE' |
      `Corrected Form` == 'infinitivo' |
      `Corrected Form` == 'infintive'|
      `Corrected Form` == 'infinative'~ 4,
    `Corrected Form` == 'gerund' |
      `Corrected Form` == 'GERUND' |
      `Corrected Form` == 'Gerund' |
      `Corrected Form` == 'gerundio' ~ 5,
    `Corrected Form` == 'pluscuamperfecto' |
      `Corrected Form` == 'PLUSCUAMPERFECTO' |
      `Corrected Form` == 'PLUPERFECT' |
      `Corrected Form` == 'past perfect' |
      `Corrected Form` == 'PAST PERFECT' |
      `Corrected Form` == 'pluscuamperfect'|
      `Corrected Form` == 'pluperfect'~ 6,
    `Corrected Form` == 'imperfect subjunctive' |
      `Corrected Form` == 'IMPERFECT SUBJUNCTIVE' |
      `Corrected Form` == 'PAST SUBJUNCTIVE' |
      `Corrected Form` == 'past subjunctive' ~ 7,
    `Corrected Form` == 'present subjunctive' |
      `Corrected Form` == 'PRESENT SUBJUNCTIVE' |
      `Corrected Form` == 'subjunctive' |
      `Corrected Form` == 'subjuntive' ~ 8, #added 7-29
    `Corrected Form` == 'command' |
      `Corrected Form` == 'COMMAND' |
      `Corrected Form` == 'IMPERATIVE' |
      `Corrected Form` == 'imperative' |
      `Corrected Form` == 'imperativo' ~ 9, #added 7-29
    `Corrected Form` == 'conditional' |
      `Corrected Form` == 'CONDITIONAL' |
      `Corrected Form` == 'condicional' ~ 10, #added 7-29
    `Corrected Form` == 'future' |
      `Corrected Form` == 'FUTURE' |
      `Corrected Form` == 'FUTURO' |
      `Corrected Form` == 'Future' |
      `Corrected Form` == 'futuro' ~ 11, #added 7-29
    `Corrected Form` == 'AMBIGUOUS' |
      `Corrected Form` == 'ambiguous' |
      `Corrected Form` == 'ambigous' |
      `Corrected Form` == 'Ambiguous' |
      `Corrected Form` == 'Ambigous' |
      `Corrected Form` == 'ambiguo' ~ 12,
    `Corrected Form` == 'past participle' |
      `Corrected Form` == 'PAST PARTICIPLE' |
      `Corrected Form` == 'participio pasado' ~ 13, #added 8-15
    `Corrected Form` == 'present perfect' |
      `Corrected Form` == 'PRESENT PERFECT' ~ 14, #added 8-15
    `Corrected Form` == 'pluperfect subjunctive' |
      `Corrected Form` == 'PLUPERFECT SUBJUNCTIVE' ~ 15, #Added 9-14
    `Corrected Form` == 'English' ~ 16,
    TRUE ~ 1000))

#View(csv1)

#just making sure that orthography of tenses
#is standardized
csv18 <- csv1 %>%
  dplyr::mutate(Corrected_Form1 = case_when(
    Corrected_num == 1 ~ 'preterit',
    Corrected_num == 2 ~ 'imperfect',
    Corrected_num == 3 ~ 'present',
    TRUE ~ 'other'))%>%
  dplyr::mutate(Form_num1 = case_when(
    Form_num == 1 ~ 'preterit',
    Form_num == 2 ~ 'imperfect',
    Form_num == 3 ~ 'present',
    TRUE ~ 'other'))

#View(csv18)

#cleaned
csv18_clean <- csv18 %>%
  filter(Ambiguous != 1 & Form != 'english')
#view(csv18_clean)

#how many participants used the present b/w 1-50% of the time
csv18_pre <- csv18 %>%
  dplyr::group_by(level, group, ID, Corrected_Form1)%>%
  dplyr::summarize(count = n())%>%
  pivot_wider(names_from = Corrected_Form1, values_from = count)%>%
  replace(is.na(.), 0)%>%
  dplyr::rowwise()%>%
  dplyr::mutate(
    total = sum(other, present, preterit, imperfect),
    Preterit = preterit / total * 100,
    Imperfect = imperfect / total * 100,
    Present = present / total * 100,
    Other = other / total * 100)%>%
  dplyr::filter(Present == 0)%>%
  dplyr::group_by(level, group)%>%
  dplyr::summarize(count = n())
#view(csv18_pre)

#remove ambiguous and use of English cases
csv18_token_cde_clean <- csv18 %>%
  filter(Ambiguous != 1 & Form != 'english')%>%
  #2,710 tokens
  #only cases where preterit or imperfect are accurate
  filter(Corrected_Form1 == 'preterit'| Corrected_Form1 == 'imperfect')%>%
  left_join(meta)
  # reduces down to 983 tokens
#view(csv18_token_cde_clean)

#write_csv(csv18_token_cde_clean, "SPA2_3_Control_intervention_7_28_23_cleaned.csv")

#how many participants now?
csv18_token_cde_clean_sum <- csv18_token_cde_clean %>%
  dplyr::group_by(level, group, ID)%>%
  dplyr::summarise(count = n())

# %>%
#   dplyr::select(-count)%>%
#   dplyr::group_by(level, group)%>%
#   dplyr::summarise(num_participants = n())

#view(csv18_token_cde_clean_sum)
#matches what had seen before

#export to csv-- people who had min 1 obligatory context
#write_csv(csv18_token_cde_clean_sum, 'imp_1st_some_past_ppl_073123.csv')


#just preterit
csv18_token_cde_clean_SPA_2 <- csv18_token_cde_clean %>%
  dplyr::filter(level == 'SPA 2')
#view(csv18_token_cde_clean_SPA_2)

#just imperfect

csv18_token_cde_clean_SPA_3 <- csv18_token_cde_clean %>%
  dplyr::filter(level == 'SPA 3')
#view(csv18_token_cde_clean_SPA_3)

#preterit only
csv18_token_cde_clean_pret <- csv18_token_cde_clean %>%
  dplyr::filter(Corrected_Form1 == 'preterit')

#imperfect only
csv18_token_cde_clean_imp <- csv18_token_cde_clean %>%
  dplyr::filter(Corrected_Form1 == 'imperfect')

##  Differences b/w groups? ####

### 1) text length ####
#Are there differences between control & intervention at
#each level in terms of their text length?

#summarize data
meta_sum <- meta %>%
  dplyr::group_by(level, group)%>%
  dplyr::summarise(mean_wl = mean(`word length`),
                   sd_wl = sd(`word length`),
                   min_wl = min(`word length`),
                   max_wl = max(`word length`))

#view(meta_sum)

#visualize the data
ggplot(meta, aes(x=`word length`)) + 
  geom_histogram()+
  facet_wrap(level~group)

#now, let's run a t-test to see if the difference exists in SPA2
#using wilcox.test because it's non-parametric and can account for lack
#of normal distribution in data
#same as Mann–Whitney U test

#separate into only SPA 2
meta2 <- meta %>%
  dplyr::filter(level == 'SPA 2')
#view(meta2)

#control
meta2_c <- meta2 %>%
  dplyr::filter(group == 'control')

#intervention
meta2_e <- meta2 %>%
  dplyr::filter(group == 'intervention')

#wilcox.test(x, y, alternative = "two.sided")
wilcox.test(meta2_c$`word length`, meta2_e$`word length`, alternative = "two.sided")
#stat sig

#only SPA 3
#control
meta3_c <- meta %>%
  dplyr::filter(level == 'SPA 3')%>%
  dplyr::filter(group == 'control')

#intervention
meta3_e <- meta%>%
  dplyr::filter(level == 'SPA 3')%>%
  dplyr::filter(group == 'intervention')

#wilcox.test(x, y, alternative = "two.sided")
wilcox.test(meta3_c$`word length`, meta3_e$`word length`, alternative = "two.sided")
#NOT stat sig

### 2) pre-test scores ####
#Are there differences b/w the groups at each level in terms of their
#pre-test scores

# csv18_clean_cleanest3 <- csv18_clean %>%
#   dplyr::filter(level == 'SPA 3')
# #view(csv18_clean_cleanest3)

#summarize data
meta_sum_pre_test <- meta %>%
  dplyr::group_by(level, group)%>%
  drop_na(review_quiz)%>%
  dplyr::summarise(mean = mean(`review_quiz`),
                   sd = sd(`review_quiz`),
                   min = min(`review_quiz`),
                   max = max(`review_quiz`))

#view(meta_sum_pre_test)

#let's plot the scores
ggplot(meta, aes(x=`review_quiz`)) + 
  geom_histogram()+
  facet_wrap(level~group)

#SPA 2: control
past_ppl_meta_no_na_2c <- meta %>%
  dplyr::filter(level == 'SPA 2')%>%
  dplyr::filter(group == 'control')
#view(past_ppl_meta_no_na_2c)

#SPA 2: intervention
past_ppl_meta_no_na_2e <- meta %>%
  dplyr::filter(level == 'SPA 2')%>%
  dplyr::filter(group == 'intervention')
#view(past_ppl_meta_no_na_2e)

#SPA 2: both are non-parametric, so let's use the 
#same test
wilcox.test(past_ppl_meta_no_na_2c$`review_quiz`, past_ppl_meta_no_na_2e$`review_quiz`, alternative = "two.sided")


#SPA 3: control
past_ppl_meta_no_na_3c <- meta %>%
  dplyr::filter(level == 'SPA 3')%>%
  dplyr::filter(group == 'control')
#view(past_ppl_meta_no_na_3c)

#SPA 3: intervention
past_ppl_meta_no_na_3e <- meta %>%
  dplyr::filter(level == 'SPA 3')%>%
  dplyr::filter(group == 'intervention')
#view(past_ppl_meta_no_na_3e)

#SPA 3: both are non-parametric, so let's use the 
#same test
wilcox.test(past_ppl_meta_no_na_3c$`review_quiz`, past_ppl_meta_no_na_3e$`review_quiz`, alternative = "two.sided")
#not statistically significantly different

### 3) verbal contexts per 100 tokens ####

#3.1 How many times did a student produce a context where a verb should have occurred?

#use summarize to calculate
csv19 <- csv18_clean %>%
  dplyr::group_by(ID, level, group)%>%
  dplyr::summarise(num_verb_opp = n())%>%
  left_join(meta)%>%
  drop_na(review_quiz)

#view(csv19)


#3.2 Divide that number by the word length

csv21 <- csv19 %>%
  dplyr::rowwise()%>%
  dplyr::mutate(num_verb_opp_per_100 = num_verb_opp / `word length` * 100)
#view(csv21)

#3.3 graph the distribution

#visualize the data
ggplot(csv21, aes(x=num_verb_opp_per_100)) + 
  geom_histogram()+
  facet_wrap(level~group)

#seems like relatively similar distributions across groups

#3.4 summarize diff
#summarize data
csv21_sum <- csv21 %>%
  dplyr::group_by(level, group)%>%
  dplyr::summarise(mean = mean(num_verb_opp_per_100),
                   sd = sd(num_verb_opp_per_100),
                   min = min(num_verb_opp_per_100),
                   max = max(num_verb_opp_per_100))
#view(csv21_sum)

#3.5 run tests of difference by group

#SPA 2
#control
csv21_2_c <- csv21 %>%
  dplyr::filter(level == 'SPA 2')%>%
  dplyr::filter(group == 'control')

#intervention
csv21_2_e <- csv21%>%
  dplyr::filter(level == 'SPA 2')%>%
  dplyr::filter(group == 'intervention')

#wilcox.test(x, y, alternative = "two.sided")
wilcox.test(csv21_2_c$num_verb_opp_per_100, csv21_2_e$num_verb_opp_per_100, alternative = "two.sided")
#NOT stat sig

#SPA 3
#control
csv21_3_c <- csv21 %>%
  dplyr::filter(level == 'SPA 3')%>%
  dplyr::filter(group == 'control')

#intervention
csv21_3_e <- csv21%>%
  dplyr::filter(level == 'SPA 3')%>%
  dplyr::filter(group == 'intervention')

#wilcox.test(x, y, alternative = "two.sided")
wilcox.test(csv21_3_c$num_verb_opp_per_100, csv21_3_e$num_verb_opp_per_100, alternative = "two.sided")
#stat sig

#3.6 difference in word length b/w groups?
csv21_sum_wl <- csv21 %>%
  dplyr::group_by(level, group)%>%
  dplyr::summarise(mean_wl = mean(`word length`),
                   sd_wl = sd(`word length`),
                   min_wl = min(`word length`),
                   max_wl = max(`word length`))
#view(csv21_sum_wl)


## SUPPLIANCE ####

#cleaned dataset only looking at suppliance
#remove ambiguous and use of English cases
csv_18_suppliance <- csv18_clean %>%
  left_join(meta)

view(csv_18_suppliance)

#summarize by # marked per essay
csv18_suppliance_sum <- csv_18_suppliance %>%
  dplyr::group_by(ID, level, group, Form_num1)%>%
  dplyr::summarise(count = n())
#view(csv18_suppliance_sum)

#pivot out
csv18_suppliance_sum1 <- csv18_suppliance_sum %>%
  pivot_wider(names_from = Form_num1, values_from = count)

#view(csv18_suppliance_sum1)

#replace NAs with 0s
csv18_suppliance_sum1$imperfect[is.na(csv18_suppliance_sum1$imperfect)] <- 0
csv18_suppliance_sum1$other[is.na(csv18_suppliance_sum1$other)] <- 0
csv18_suppliance_sum1$present[is.na(csv18_suppliance_sum1$present)] <- 0
csv18_suppliance_sum1$preterit[is.na(csv18_suppliance_sum1$preterit)] <- 0

view(csv18_suppliance_sum1)
#view(meta)

#now pivot longer
csv18_suppliance_sum1 <-csv18_suppliance_sum1 %>%
  left_join(meta)%>%
  drop_na(review_quiz) |> 
  pivot_longer(cols= (4:7), names_to = 'tense', values_to = 'count')
  
#view(csv18_suppliance_sum1)

#now add metadata about text length-- already have loaded (meta)
#view(meta)
#calculating mean use per 100 words to account for text length
csv18_suppliance_sum1 <-csv18_suppliance_sum1 %>%
  dplyr::mutate(count_by_length = count / `word length` *100)

#view(csv18_suppliance_sum1)

#summary SE for use
pret_imp_supl_se <- summarySE(data = csv18_suppliance_sum1, 
                    measurevar = 'count_by_length', 
                    groupvars = c('level', 'group', 'tense'),
                    na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
#view(pret_imp_supl_se)


#graph
ggplot(pret_imp_supl_se, aes(x=group, y=count_by_length, fill=level)) + 
  facet_wrap(~tense)+
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=count_by_length-ci, ymax=count_by_length+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  labs(x = "Group", y = "Suppliance per 100 words")+
  #ylim(0,7.5)+
  scale_fill_brewer(palette="Accent")+ 
  theme(text = element_text(size = 15))

### *anova- does suppliance differ by group and appropriate tense-aspect? ####
csv18_suppliance_sum1_anova <- csv18_suppliance_sum1 
# %>%
#   filter(tense == 'imperfect'| tense == 'preterit')
#view(csv18_suppliance_sum1_anova)

#SPA 2
csv18_suppliance_sum1_anova_SPA_2 <- csv18_suppliance_sum1_anova %>%
  dplyr::filter(level == 'SPA 2')
#view(csv18_suppliance_sum1_anova_SPA_2)

#SPA 3
csv18_suppliance_sum1_anova_SPA_3 <- csv18_suppliance_sum1_anova %>%
  dplyr::filter(level == 'SPA 3')
#view(csv18_suppliance_sum1_anova_SPA_3)

#square root data
csv18_suppliance_sum1_anova_SPA_3 <- csv18_suppliance_sum1_anova_SPA_3%>%
  mutate(count_by_length_sqrt = sqrt(count_by_length))

#let's center & scale
#now let's center and scale the frequency data
center_scale <- function(x) {
  scale(x, scale = TRUE) #changed 9-28-22
  #scale = TRUE to center and scale
}

#center scale data
center_scaled_data <- center_scale(csv18_suppliance_sum1_anova_SPA_3$count_by_length_sqrt)
view(center_scaled_data)

#now add back to the tibble
csv18_suppliance_sum1_anova_SPA_3 <- csv18_suppliance_sum1_anova_SPA_3 %>%
  cbind(center_scaled_data)

#naming of columns odd
csv18_suppliance_sum1_anova_SPA_3 <- csv18_suppliance_sum1_anova_SPA_3 %>%
  dplyr::rename(count_by_length_sqrt_cs = `...14`)

view(csv18_suppliance_sum1_anova_SPA_3)

#normal distribution?
histogram(csv18_suppliance_sum1_anova_SPA_3$count_by_length_sqrt_cs)


#anova for SPA 2
aov_supl <- aov(count_by_length ~ group * tense, csv18_suppliance_sum1_anova_SPA_2)
aov_supl
summary(aov_supl)
TukeyHSD(aov_supl)
#significant interaction in group and tense effect
# intervention supplying more imperfect, control supplying more preterit

#anova for SPA 3
aov_supl3 <- aov(count_by_length ~ group * tense, csv18_suppliance_sum1_anova_SPA_3)
aov_supl3
summary(aov_supl3)
TukeyHSD(aov_supl3)

#tense effect but nothing else

#view(csv18_token_cde_clean_imp)

#effect size
#eta_squared(aov_supl3)


### linear mixed-effects model ####

#run only for SPA 3

#response variable:
# mean suppliance per 100 words

#independent variables:
# group: control vs. intervention
# form: preterit vs. imperfect

#random intercepts:
# participant

#histogram of distribution by group
ggplot(csv18_suppliance_sum1_anova_SPA_3, aes(x=`count_by_length`)) + 
  geom_histogram()+
  facet_wrap(tense~group)

#square root of data
csv18_suppliance_sum1_anova_SPA_3 <- csv18_suppliance_sum1_anova_SPA_3 %>%
  mutate(count_by_length_sqrt = sqrt(count_by_length))
#view(csv18_suppliance_sum1_anova_SPA_3)

#histogram of distribution by group
ggplot(csv18_suppliance_sum1_anova_SPA_3, aes(x=`count_by_length_sqrt`)) + 
  geom_histogram()+
  facet_wrap(tense~group)

#looks normal now!

#let's center & scale
#now let's center and scale the frequency data
center_scale <- function(x) {
  scale(x, scale = TRUE) #changed 9-28-22
  #scale = TRUE to center and scale
}

#center scale data
center_scaled_data <- center_scale(csv18_suppliance_sum1_anova_SPA_3$count_by_length_sqrt)
#view(center_scaled_data)

#now add back to the tibble
csv18_suppliance_sum1_anova_SPA_3 <- csv18_suppliance_sum1_anova_SPA_3 %>%
  cbind(center_scaled_data)

# csv18_suppliance_sum1_anova_SPA_3 <- csv18_suppliance_sum1_anova_SPA_3 %>%
#   dplyr::rename(count_by_length_sqrt_cs = `...15`)
view(csv18_suppliance_sum1_anova_SPA_3)

ggplot(csv18_suppliance_sum1_anova_SPA_3, aes(x=`count_by_length_sqrt`)) + 
  geom_histogram()+
  facet_wrap(tense~group)
#good: this is centered and scaled data

#now run lmer model
#maximal model
lmer_model_1 <- lmer(count_by_length_sqrt_cs ~ group * tense
     + (1|ID), data=csv18_suppliance_sum1_anova_SPA_3, 
     control=lmerControl(optimizer="bobyqa",
                         optCtrl=list(maxfun=2e5)))
summary(lmer_model_1)
anova(lmer_model_1)
#lmerTest::anova(lmer_model_1)
emmeans(lmer_model_1,  ~ tense | group)
#R2
r.squaredGLMM(lmer_model_1)
#try again with sjPlot
performance::r2(lmer_model_1, tolerance = 1e-10)

performance::check_singularity(lmer_model_1)

#install.packages("sos")
library(sos)

plot(lmer_model_1)

#no interaction
lmer_model_2 <- lmer(count_by_length_sqrt_cs ~ group + tense
                     + (1|ID), data=csv18_suppliance_sum1_anova_SPA_3, 
                     control=lmerControl(optimizer="bobyqa",
                                         optCtrl=list(maxfun=2e5)))
summary(lmer_model_2)
anova(lmer_model_2)

performance::check_singularity(lmer_model_2)
#also singular

#R2
r.squaredGLMM(lmer_model_2)
#0.1864444 0.1864444

#just the imperfect
csv18_suppliance_sum1_anova_SPA_3_imp <- csv18_suppliance_sum1_anova_SPA_3 %>%
  dplyr::filter(tense == 'imperfect')

#view(csv18_suppliance_sum1_anova_SPA_3_imp)

lmer_model_1imp <- lmer(count_by_length_sqrt_cs ~ group
                     + (1|ID), data=csv18_suppliance_sum1_anova_SPA_3_imp, 
                     control=lmerControl(optimizer="bobyqa",
                                         optCtrl=list(maxfun=2e5)))
summary(lmer_model_1imp)
#doesn't work because too few observations


## Longitudinal data ####
#upload participant #s
long_parts <- read_csv('Checking duplicate participants 7-28-23 - Sheet4.csv') |>
  left_join(meta)|> #add metadata
  left_join(csv18_clean) #add verbal data
#view(long_parts)

#how many participants had some obligatory past cases?
long_parts_past <- long_parts |>
  dplyr::filter(Corrected_Form1 == 'preterit'|Corrected_Form1 == 'imperfect')
#view(long_parts_past)

long_parts_past_sum <- long_parts_past |>
  dplyr::group_by(Participant, level, group)|>
  dplyr::summarise(count = n())
#view(long_parts_past_sum)

### suppliance long ####
long_parts_past_sup <- long_parts_past |>
  group_by(level, group, Form_num1)%>%
  dplyr::summarize(count = n())%>%
  pivot_wider(names_from = Form_num1, values_from = count)%>%
  replace(is.na(.), 0)%>%
  dplyr::rowwise()%>%
  dplyr::mutate(
    total = sum(present, preterit, imperfect), #other not produced
    Preterit = preterit / total * 100,
    Imperfect = imperfect / total * 100,
    Present = present / total * 100,
    Other = other / total * 100
  )%>%
  select(group, level, Preterit, Imperfect, Present, Other)%>%
  pivot_longer(
    cols = c(Preterit, Imperfect, Present, Other),
    names_to = 'Tense-aspect form',
    values_to = 'Percent use'
  )
#view(long_parts_past_sup)

#graph
ggplot(long_parts_past_sup, aes(x=group, y=`Percent use`, fill=level)) + 
  facet_wrap(~`Tense-aspect form`)+
  geom_bar(position=position_dodge(), stat="identity") +
  # geom_errorbar(aes(ymin=count_by_length-ci, ymax=count_by_length+ci),
  #               width=.2,                    # Width of the error bars
  #               position=position_dodge(.9))+
  labs(x = "Group", y = "Percent Suppliance")+
  #ylim(0,7.5)+
  scale_fill_brewer(palette="Accent")+ 
  theme(text = element_text(size = 15))

### obligatory past ####

#only P43 & P50 had obligatory past contexts in both SPA 2 and 3

#let's look at P43
long_parts_past_P43<- long_parts_past |>
  dplyr::filter(Participant == 'P43')

#suppliance
long_parts_past_P43_sup <- long_parts_past |>
  dplyr::filter(Participant == 'P43')%>%
  group_by(level, group, Form_num1)%>%
  dplyr::summarize(count = n())%>%
  pivot_wider(names_from = Form_num1, values_from = count)%>%
  replace(is.na(.), 0)%>%
  dplyr::rowwise()%>%
  dplyr::mutate(
    total = sum(present, preterit, imperfect), #other not produced
    Preterit = preterit / total * 100,
    Imperfect = imperfect / total * 100,
    Present = present / total * 100
    # ,
    # Other = other / total * 100
  )%>%
  select(group, Preterit, Imperfect, Present)%>% #, Other
  pivot_longer(
    cols = c(Preterit, Imperfect, Present), #, Other
    names_to = 'Tense-aspect form',
    values_to = 'Percent use'
  )

#view(long_parts_past_P43_sup)

#use summary se
long_parts_past_P43_se <- summarySE(data = long_parts_past_P43, 
                              measurevar = 'Appropriate', 
                              groupvars = c('level', 'group', 'Corrected_Form1'),
                              na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
#view(long_parts_past_P43_se)

ggplot(long_parts_past_P43_se, aes(x=group, y=Appropriate, fill = Corrected_Form1)) + 
  facet_wrap(~level)+
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Appropriate-ci, ymax=Appropriate+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  ylim(0,1)+
  ylab('Percent accuracy')+ 
  scale_fill_brewer(palette="Accent")+ 
  theme(text = element_text(size = 15))

#let's look at P50
long_parts_past_P50<- long_parts_past |>
  dplyr::filter(Participant == 'P50')

view(long_parts_past_P50)

#suppliance
long_parts_past_P50_sup <- long_parts_past |>
  dplyr::filter(Participant == 'P50')%>%
  group_by(level, group, Form_num1)%>%
  dplyr::summarize(count = n())%>%
  pivot_wider(names_from = Form_num1, values_from = count)%>%
  replace(is.na(.), 0)%>%
  dplyr::rowwise()%>%
  dplyr::mutate(
    total = sum(present, preterit, imperfect), #other not produced
    Preterit = preterit / total * 100,
    Imperfect = imperfect / total * 100,
    Present = present / total * 100
    # ,
    # Other = other / total * 100
  )%>%
  select(group, Preterit, Imperfect, Present)%>% #, Other
  pivot_longer(
    cols = c(Preterit, Imperfect, Present), #, Other
    names_to = 'Tense-aspect form',
    values_to = 'Percent use'
  )

#view(long_parts_past_P50_sup)

#use summary se
long_parts_past_P50_se <- summarySE(data = long_parts_past_P50, 
                                    measurevar = 'Appropriate', 
                                    groupvars = c('level', 'group', 'Corrected_Form1'),
                                    na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
view(long_parts_past_P50_se)
#write_csv(long_parts_past_P50, 'long_parts_past_P50_sup_073123.csv')

ggplot(long_parts_past_P50_se, aes(x=group, y=Appropriate, fill = Corrected_Form1)) + 
  facet_wrap(~level)+
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Appropriate-ci, ymax=Appropriate+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  ylim(0,1)+
  ylab('Percent accuracy')+ 
  scale_fill_brewer(palette="Accent")+ 
  theme(text = element_text(size = 15))

#view(meta)

## num of participants ####
#10-12-23: seeing how many duplicate participants there were in survey completion
num_p <- read_csv('experimental_SPA2_WQ_SPA3_101223.csv') |>
  dplyr::mutate(name = str_to_lower(Name)) |>
  dplyr::group_by(name)|>
  dplyr::summarize(count = n())
#view(num_p)

#write to csv
#write_csv(num_p, 'experimental_SPA2_WQ_SPA3_duplicates_101323.csv')

#same, but with all the experimental data
num_p1 <- read_csv('all_experimental_101323.csv') |>
  dplyr::mutate(name = str_to_lower(Name)) |>
  dplyr::group_by(name)|>
  dplyr::summarize(count = n())
view(num_p1)

## Non-past group (N=39) ####
#view(csv18_clean)

#Need to filter so it's only these people who had no SOC
csv18_clean_39 <- csv18_clean |>
  dplyr::group_by(ID, level, group, Corrected_Form1) |>
  dplyr::summarize(num_tense = n())|>
  pivot_wider(names_from = Corrected_Form1, values_from = num_tense)|>
  left_join(meta) |>
  drop_na(review_quiz)
#view(csv18_clean_39)

#now change NAs to 0
#turn NAs to 0s
csv18_clean_39$other[is.na(csv18_clean_39$other)] <- 0
csv18_clean_39$present[is.na(csv18_clean_39$present)] <- 0
csv18_clean_39$preterit[is.na(csv18_clean_39$preterit)] <- 0
csv18_clean_39$imperfect[is.na(csv18_clean_39$imperfect)] <- 0

#view(csv18_clean_39)

#now filter to only include people with 0 pret and imp
csv18_clean_39_a <- csv18_clean_39 |>
  dplyr::filter(preterit == 0 & imperfect == 0)|>
  mutate(Type = 'non_past_group')
#view(csv18_clean_39_a)
#now, we have their metadata information

#let's filter so it's only the ones in SPA 2
csv18_clean_39_b <- csv18_clean_39_a |>
  dplyr::filter(level == 'SPA 2') #now 33

#view(csv18_clean_39_b)

#just meta info
csv18_clean_39_c <- csv18_clean_39_b |>
  dplyr::select(1:3)
#view(csv18_clean_39_c)


#now combine this with the original dataframe
csv18_clean_39_d <- csv18_clean_39_c |>
  left_join(csv18_clean)
#view(csv18_clean_39_d)

### suppliance ####
csv18_clean_39_d_s <-csv18_clean_39_d |>
  dplyr::group_by(ID, level, group, Form_num1) |>
  dplyr::summarize(count = n())%>%
  pivot_wider(names_from = Form_num1, values_from = count)%>%
  replace(is.na(.), 0)%>%
  dplyr::rowwise()%>%
  dplyr::mutate(
    total = sum(other, present, preterit, imperfect))%>%
  left_join(meta)%>%
  dplyr::mutate(
    Other = other / `word length` *100,
    Present = present / `word length` *100,
    Preterit = preterit / `word length` *100,
    Imperfect = imperfect / `word length` *100) |>
  pivot_longer(cols = Other:Imperfect, names_to = 'tense', values_to = 'count_by_length')

#view(csv18_clean_39_d_s)

#now summarize with summary SE
csv18_clean_39_d_s_se <- summarySE(data = csv18_clean_39_d_s, 
                              measurevar = 'count_by_length', 
                              groupvars = c('group', 'tense'),
                              na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
#view(csv18_clean_39_d_s_se)

#write to csv
#write_csv(csv18_clean_39_a,'non_past_group_103123.csv')

## Past group (N=68) ####
#now filter to only include people with 0 pret and imp
csv18_clean_39_a68 <- csv18_clean_39 |>
  dplyr::filter(preterit > 0 | imperfect > 0)|>
  mutate(Type = 'past_group')
#view(csv18_clean_39_a68)

#write to csv
#write_csv(csv18_clean_39_a68,'past_group_103123.csv')

## names
names <- read_csv('impfct first_all data - combined_2.csv')
view(names)

#rowbind the non-past and past groups
larger_sample <- rbind(csv18_clean_39_a, csv18_clean_39_a68)
view(larger_sample)

#join with names
larger_sample_names <- larger_sample |>
  left_join(names, by = join_by(ID, level, Participant))
#view(larger_sample_names)

#make more compact
larger_sample_names1 <- larger_sample_names |>
  select(ID, Participant, level, group.x, Type, name, instructor)|>
  mutate(name = tolower(name))
#view(larger_sample_names1)

#now I need to join this with the csv with their characteristics
combo <- read_csv('combination_imp_first_10_31_23 - all combined.csv') |>
  mutate(name = tolower(Name1))

#view(combo)

#combine the two
larger_sample_names2 <- larger_sample_names1 |>
  left_join(combo, by = c('name', 'group.x', 'level'))
#view(larger_sample_names2)

#write to csv
#write_csv(larger_sample_names2, 'names_combo_imp_first_10_31_23.csv')

## Difference b/w groups? ####

#read in cleaned csv
combo2 <- read_csv('names_combo_imp_first_10_31_23 - clean clean likert scale.csv')
view(combo2)

#just SPA 2
combo2_2 <- combo2 |>
  filter(level == 'SPA 2')
#view(combo2_2)

#any difference by instructor?
combo2_prof <- combo2 |>
  dplyr::group_by(instructor, Type)|>
  #dplyr::group_by(Type, instructor, level, group)|>
  dplyr::summarize(count = n())
view(combo2_prof)

#not really seeing anything

#any difference by reader adapted to proficiency level?
combo2_adapted <- combo2 |>
  dplyr::group_by(Type)|>
  #dplyr::group_by(Type, level, group)|>
  dplyr::summarize(mean_adapted_prof = mean(adapted_prof),
                   sd_adapted_prof = sd(adapted_prof))
#view(combo2_adapted)

#the past group actually said that it was closer to being
#challenging but still manageable than the non-past group

#check distribution
ggplot(combo2_2, aes(x=adapted_prof)) + 
  geom_histogram()+
  facet_wrap(~Type)

#non-past group
combo2_2_np_adapted <- combo2_2 |>
  filter(Type == 'non_past_group')|>
  select(adapted_prof)
#view(combo2_2_np_adapted)

#past group
combo2_2_p_adapted <- combo2_2 |>
  filter(Type == 'past_group')|>
  select(adapted_prof)
#view(combo2_2_p_adapted)

#Check the equality of variances
# combo2_2 |> 
#   levene_test(adapted_prof ~ Type)
#no significant difference in variance b.w groups

#t-test
t.test(combo2_2_np_adapted, combo2_2_p_adapted)

#any difference by like book?
combo2_like <- combo2 |>
  dplyr::group_by(Type)|>
  #dplyr::group_by(Type, level, group)|>
  dplyr::summarize(mean_like_book = mean(like_book),
                   sd_like_book = sd(like_book))

#same on average across groups, differs a bit when break down by level

#check distribution
ggplot(combo2_2, aes(x=like_book)) + 
  geom_histogram()+
  facet_wrap(~Type)

#non-past group
combo2_2_np_like <- combo2_2 |>
  filter(Type == 'non_past_group')|>
  select(like_book)
#view(combo2_2_np_like)

#past group
combo2_2_p_like <- combo2_2 |>
  filter(Type == 'past_group')|>
  select(like_book)
#view(combo2_2_p_like)

#t-test
t.test(combo2_2_np_like, combo2_2_p_like)

### Review quiz diff? ####
larger_sample_SPA2 <- larger_sample |>
  filter(level == 'SPA 2')
#view(larger_sample_SPA2)

#histogram
ggplot(larger_sample_SPA2, aes(x=review_quiz)) + 
  geom_histogram()+
  facet_wrap(~Type)

#distribution doesn't seem to be different b/w the two

#non-past group
larger_sample_SPA2_np_rq <- larger_sample_SPA2 |>
  dplyr::filter(Type == 'non_past_group')|>
  ungroup() %>%
  dplyr::select('review_quiz')
#view(larger_sample_SPA2_np_rq)

#past group
larger_sample_SPA2_p_rq <- larger_sample_SPA2 |>
  dplyr::filter(Type == 'past_group')|>
  ungroup() %>%
  dplyr::select('review_quiz')
#view(larger_sample_SPA2_p_rq)

#t-test
t.test(larger_sample_SPA2_np_rq, larger_sample_SPA2_p_rq)

#mean SD pre-test for non-past group review quiz
larger_sample_np <- larger_sample |>
  filter(Type == 'non_past_group')%>%
  group_by(level, group)%>%
  dplyr::summarise(mean = mean(`review_quiz`),
                   sd = sd(`review_quiz`),
                   min = min(`review_quiz`),
                   max = max(`review_quiz`))
view(larger_sample_np)

#Thanks for reading!