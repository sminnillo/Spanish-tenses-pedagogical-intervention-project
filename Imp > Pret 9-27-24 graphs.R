#Order of instruction of preterit-imperfect project code
#3-13-23; updated 7-25-23
#Sophia Minnillo

#project about whether order of instruction
#impacts L2 Spanish preterit-imperfect accuracy and suppliance
#this is the SUBSET of particpants that had any past tense obligatory contexts


#load packages
library(tidyverse)
library(stringr)
library(broom)
library(lme4)
library(psych)
library(afex)
library(sjPlot)
library(effects)
library(sjstats)
library(ngramr)
library(Rmisc) #for SummarySE
library(plyr); library(dplyr)
library(rstatix)
library(effectsize)
#install.packages("piecewiseSEM")
#library(piecewiseSEM)
library(MuMIn)

## Load CSV ####

#read metadata
meta <- read_csv("impfct first_all data - combined_3.csv")
#view(meta)

#summarize metadata
meta_summary <- meta %>%
  dplyr::group_by(level,group)%>%
  dplyr::summarise(count = n())
#view(meta_summary)

#load csv with just people who had some past obligatory contexts
past_ppl <- read_csv('imp_1st_some_past_ppl_073123.csv')%>%
  select(-count)
#view(past_ppl)

#load csv with data
csv <- read_csv("SPA_2_3_Control_experimental_07_25_23 - Sheet1.csv")%>%
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
view(csv18_clean)

#past people csv18_clean
csv18_clean_past_ppl <- csv18_clean |>
  semi_join(past_ppl)|>
  left_join(meta)|>
  drop_na(review_quiz)
#view(csv18_clean_past_ppl)

#only SPA 3
csv18_clean_past_ppl3 <- csv18_clean_past_ppl |>
  dplyr::filter(level == 'SPA 3')
#view(csv18_clean_past_ppl3)

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

#left join with 'past_people' to only include students who produced
#past tenses
past_ppl_meta <- past_ppl %>%
  dplyr::left_join(meta)

#view(past_ppl_meta)
#7 NAs out of 75 students

#if you drop these participants, there are 68 left
past_ppl_meta_no_na <- past_ppl_meta %>%
  drop_na(review_quiz)

#view(past_ppl_meta_no_na)
#write to csv
#write_csv(past_ppl_meta_no_na, 'metadata_cleaned_imp_1st_080323.csv')

#how many people?
#summarize metadata
past_ppl_meta_no_na_summary <- past_ppl_meta_no_na %>%
  dplyr::group_by(gender)%>%
  dplyr::summarise(count = n())
#view(past_ppl_meta_no_na_summary)

#merge with data
csv18_clean_cleanest <- csv18_clean %>%
  semi_join(past_ppl_meta_no_na)
#view(csv18_clean_cleanest)

csv18_clean_cleanest3 <- csv18_clean_cleanest %>%
  dplyr::filter(level == 'SPA 3')
#view(csv18_clean_cleanest3)

csv18_clean_cleanest3_past <- csv18_clean_cleanest3 %>%
  dplyr::filter(Corrected_Form1 == 'preterit'|Corrected_Form1 == 'imperfect')
#view(csv18_clean_cleanest3_past)
#write to csv and then use this as baseline



#summarize data
meta_sum_pre_test <- past_ppl_meta %>%
  dplyr::group_by(level, group)%>%
  drop_na(review_quiz)%>%
  dplyr::summarise(mean = mean(`review_quiz`),
                   sd = sd(`review_quiz`),
                   min = min(`review_quiz`),
                   max = max(`review_quiz`))

#view(meta_sum_pre_test)

#SPA 2: control
past_ppl_meta_no_na_2c <- past_ppl_meta %>%
  dplyr::filter(level == 'SPA 2')%>%
  dplyr::filter(group == 'control')
#view(past_ppl_meta_no_na_2c)

#SPA 2: intervention
past_ppl_meta_no_na_2e <- past_ppl_meta %>%
  dplyr::filter(level == 'SPA 2')%>%
  dplyr::filter(group == 'intervention')
#view(past_ppl_meta_no_na_2e)

#SPA 2: both are non-parametric, so let's use the 
#same test
wilcox.test(past_ppl_meta_no_na_2c$`review_quiz`, past_ppl_meta_no_na_2e$`review_quiz`, alternative = "two.sided")


#let's plot the scores
ggplot(past_ppl_meta_no_na, aes(x=`review_quiz`)) + 
  geom_histogram()+
  facet_wrap(level~group)

#SPA 3: control
past_ppl_meta_no_na_3c <- past_ppl_meta_no_na %>%
  dplyr::filter(level == 'SPA 3')%>%
  dplyr::filter(group == 'control')
#view(past_ppl_meta_no_na_3c)
#24

#SPA 3: intervention
past_ppl_meta_no_na_3e <- past_ppl_meta_no_na %>%
  dplyr::filter(level == 'SPA 3')%>%
  dplyr::filter(group == 'intervention')
#view(past_ppl_meta_no_na_3e)
#23  

#SPA 3: both are non-parametric, so let's use the 
#same test
wilcox.test(past_ppl_meta_no_na_3c$`review_quiz`, past_ppl_meta_no_na_3e$`review_quiz`, alternative = "two.sided")
#not statistically significantly different

### 3) obligatory contexts per 100 tokens ####

#3.1 How many times did a student produce a token that should have been
#in the preterit or imperfect? Calculate based on ratings (corrected form).

#read in smaller dataset


#use summarize to calculate
csv19 <- csv18_clean_cleanest3_past %>%
  dplyr::group_by(ID, level, group)%>%
  dplyr::summarise(num_past_opp = n())

#view(csv19)

#only 75 of 114 total texts have some use of the past
#47 here

#merge with meta csv so you can include back the participants who didn't produce any
csv20 <- csv19 |> left_join(meta, by = c('ID', 'level', 'group'))
#view(csv20)

#turn NAs to 0s
csv20$num_past_opp[is.na(csv20$num_past_opp)] <- 0

#3.2 Divide that number by the word length

csv21 <- csv20 %>%
  dplyr::rowwise()%>%
  dplyr::mutate(num_past_opp_per_100 = num_past_opp / `word length` * 100)%>%
  #what if eliminated people who didn't use the past at all
  dplyr::filter(num_past_opp_per_100 > 0)
#view(csv21)

# #actually people who didn't use the past at all
# csv21_hp <- csv20 %>%
#   dplyr::rowwise()%>%
#   dplyr::mutate(num_past_opp_per_100 = num_past_opp / `word length` * 100)%>%
#   #what if eliminated people who didn't use the past at all
#   dplyr::filter(num_past_opp_per_100 == 0)

#view(csv21_hp)
#write_csv(csv21_hp, 'historical_past_texts_073023.csv')

# #summarize how many per level / group
# csv21_hp_sum <- csv21_hp %>%
#   dplyr::group_by(level, group)%>%
#   dplyr::summarise(count = n())
# #view(csv21_hp_sum)

#summarize-- count of each
csv21_num <- csv21 %>%
  dplyr::group_by(level, group)%>%
  dplyr::summarise(count = n())

#view(csv21_num)

#3.3 graph the distribution

#visualize the data
ggplot(csv21, aes(x=num_past_opp_per_100)) + 
  geom_histogram()+
  facet_wrap(level~group)

#seems like relatively similar distributions across groups

#3.4 summarize diff
#summarize data
csv21_sum <- csv21 %>%
  dplyr::group_by(level, group)%>%
  dplyr::summarise(mean = mean(num_past_opp_per_100),
                   sd = sd(num_past_opp_per_100),
                   min = min(num_past_opp_per_100),
                   max = max(num_past_opp_per_100))
#view(csv21_sum)

#3.5 run tests of difference by group

# #SPA 2
# #control
# csv21_2_c <- csv21 %>%
#   dplyr::filter(level == 'SPA 2')%>%
#   dplyr::filter(group == 'control')
# 
# #intervention
# csv21_2_e <- csv21%>%
#   dplyr::filter(level == 'SPA 2')%>%
#   dplyr::filter(group == 'intervention')

# #wilcox.test(x, y, alternative = "two.sided")
# wilcox.test(csv21_2_c$num_past_opp_per_100, csv21_2_e$num_past_opp_per_100, alternative = "two.sided")
# #NOT stat sig

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
wilcox.test(csv21_3_c$num_past_opp_per_100, csv21_3_e$num_past_opp_per_100, alternative = "two.sided")
#stat INsig

#3.6 difference in word length b/w groups?
csv21_sum_wl <- csv21 %>%
  dplyr::group_by(level, group)%>%
  dplyr::summarise(mean_wl = mean(`word length`),
                   sd_wl = sd(`word length`),
                   min_wl = min(`word length`),
                   max_wl = max(`word length`))
#view(csv21_sum_wl)

## ACCURACY ####
# The accuracy models are based on whether or not the preterit-imperfect
# are used in cases where they would be appropriate in the context of narration.

#MODEL STRUCTURE

#logistic mixed-effects model

#run separately for SPA 2 vs. 3

#response variable:
# accuracy (0-1)

#independent variables:
# group: control vs. intervention
# appropriate form: preterit vs. imperfect

#random intercepts:
# participant

### GLMM- SPA 2 ####

#just SPA 2

model1 <- glmer(Appropriate ~ group * Corrected_Form1 +
                  (1|Participant),
                csv18_token_cde_clean_SPA_2,
                family = 'binomial', 
                control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=50000)))
summary(model1)

#get rid of interaction term
model1_1 <- glmer(Appropriate ~ group + Corrected_Form1 +
                  (1|Participant),
                  csv18_token_cde_clean_SPA_2,
                family = 'binomial', 
                control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=50000)))
summary(model1_1)
#crazy effects-- essentially groups are opposites of each other
#what you would expect when 1 class hasn't been taught it yet

#just preterit
csv18_token_cde_clean_SPA_2_pret <- csv18_token_cde_clean_SPA_2 %>%
  filter(Corrected_Form1 == 'preterit')

model2 <- glmer(Appropriate ~ group +
                  (1|Participant),
                csv18_token_cde_clean_SPA_2,
                family = 'binomial', 
                control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=50000)))
summary(model2)
#intervention group less accurate with using the preterit

#just imperfect
csv18_token_cde_clean_SPA_2_imp <- csv18_token_cde_clean_SPA_2 %>%
  filter(Corrected_Form1 == 'imperfect')
#view(csv18_token_cde_clean_imp)


model3 <- glmer(Appropriate ~ group +
                  (1|Participant),
                csv18_token_cde_clean_SPA_2_imp,
                family = 'binomial', 
                control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=50000)))
summary(model3)
#control group didn't use the imperfect correctly ever, so 
#model got a little bit crazy

### GLMM- SPA 3 ####

#just SPA 3

#using this: csv18_clean_cleanest3_past

model1 <- glmer(Appropriate ~ group * Corrected_Form1 +
                  (1|ID),
                csv18_clean_cleanest3_past,
                family = 'binomial', 
                control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=50000)))
summary(model1)

#R2
r.squaredGLMM(model1)

#higher accuracy with preterit

#get rid of interaction term
model1_1 <- glmer(Appropriate ~ group + Corrected_Form1 +
                    (1|ID),
                  csv18_clean_cleanest3_past,
                  family = 'binomial', 
                  control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=50000)))
summary(model1_1)
anova(model1_1)
#still higher accuracy with preterit; no group effect

#R2
r.squaredGLMM(model1_1)
#mcfadden's
#piecewiseSEM::rsquared(model1_1, method = "delta")

#divide into groups for Cohen's d

#view(csv18_clean_cleanest3_past)

csv18_clean_cleanest3_past_pret <- csv18_clean_cleanest3_past |>
  dplyr::filter(Corrected_Form1 == 'preterit')
#view(csv18_clean_cleanest3_past_pret)

csv18_clean_cleanest3_past_imp <- csv18_clean_cleanest3_past |>
  dplyr::filter(Corrected_Form1 == 'imperfect')
#view(csv18_clean_cleanest3_past_imp)

#not working 8-22-23
# cohen.d(csv18_clean_cleanest3_past_pret$Appropriate, 
#         csv18_clean_cleanest3_past_imp$Appropriate)

#just preterit
csv18_clean_cleanest3_past_pret <- csv18_clean_cleanest3_past %>%
  filter(Corrected_Form1 == 'preterit')

model2 <- glmer(Appropriate ~ group +
                  (1|ID),
                csv18_clean_cleanest3_past_pret,
                family = 'binomial', 
                control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=50000)))
summary(model2)

#no group effect

#just imperfect
csv18_clean_cleanest3_past_imp <- csv18_clean_cleanest3_past %>%
  filter(Corrected_Form1 == 'imperfect')
#view(csv18_token_cde_clean_imp)


model3 <- glmer(Appropriate ~ group +
                  (1|ID),
                csv18_clean_cleanest3_past_imp,
                family = 'binomial', 
                control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=50000)))
summary(model3)

#no group effect

### Descriptive stats ####
#just cases where the preterit and imperfect are appropriate
pret_imp_appr <- csv18_clean_cleanest3_past %>%
  #read_csv("SPA2_3_Control_intervention_7_28_23_cleaned.csv")%>%
  mutate(Appropriate = Appropriate * 100)
#view(pret_imp_appr)

#use summary se
pret_imp_appr_se <- summarySE(data = pret_imp_appr, 
                               measurevar = 'Appropriate', 
                               groupvars = c('level', 'group', 'Corrected_Form1'),
                               na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)|>
  mutate(
    Group = case_when(
      group == 'control' ~ 'PFG',
      group == 'intervention' ~ 'IFG'
    )
  )
  
#view(pret_imp_appr_se)

##### *Graph in paper ####

#visualize in graph
ggplot(pret_imp_appr_se, aes(x=Group, y=Appropriate, fill = Corrected_Form1)) + 
  #facet_wrap(~level)+
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Appropriate-ci, ymax=Appropriate+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  ylim(0,100)+
  #xlab('Group')+
  ylab('Percent SOC')+
  labs(fill = "Obligatory Form")+
  scale_fill_brewer(palette="Accent")+ 
  theme(text = element_text(size = 20))

### Alternate tenses in cases where pret-imp accurate ####
#preterit appropriate
csv18_clean_cleanest3_past_pret1 <-csv18_clean_cleanest3_past_pret %>%
  group_by(level, group, Form_num1)%>%
  dplyr::summarize(count = n())%>%
  pivot_wider(names_from = Form_num1, values_from = count)%>%
  replace(is.na(.), 0)%>%
  dplyr::rowwise()%>%
  dplyr::mutate(
    total = sum(other, present, preterit, imperfect),
    Preterit = preterit / total * 100,
    Imperfect = imperfect / total * 100,
    Present = present / total * 100,
    Other = other / total * 100
  )%>%
  select(group, Preterit, Imperfect, Present, Other)%>%
  pivot_longer(
    cols = c(Preterit, Imperfect, Present, Other),
    names_to = 'Tense-aspect form',
    values_to = 'Percent use'
  )

#view(csv18_clean_cleanest3_past_pret1)

#create a bar graph with this information
#now graph this with stacked graph
ggplot(csv18_clean_cleanest3_past_pret1, aes(x = group, y = `Percent use`, fill = `Tense-aspect form`))+
  geom_bar(stat = "identity", position = "stack")+
  facet_wrap(~level)+
  theme_minimal()+
  #labs(x = "Course level", y = "Percent use of forms in preterit-appropriate contexts")+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.80, vjust = 0.9),
        text = element_text(size = 15))+
  scale_fill_brewer(palette="Set2")


#imperfect appropriate
csv18_clean_cleanest3_past_imp1 <-csv18_clean_cleanest3_past_imp %>%
  group_by(level, group, Form_num1)%>%
  dplyr::summarize(count = n())%>%
  pivot_wider(names_from = Form_num1, values_from = count)%>%
  replace(is.na(.), 0)%>%
  dplyr::rowwise()%>%
  dplyr::mutate(
    total = sum(other, present, preterit, imperfect),
    Preterit = preterit / total * 100,
    Imperfect = imperfect / total * 100,
    Present = present / total * 100,
    Other = other / total * 100
  )%>%
  select(group, Preterit, Imperfect, Present, Other)%>%
  pivot_longer(
    cols = c(Preterit, Imperfect, Present, Other),
    names_to = 'Tense-aspect form',
    values_to = 'Percent use'
  )%>%
  dplyr::mutate(`Tense-aspect form` = 
                  factor(`Tense-aspect form`, 
                         levels = (c('Other', 'Present', 'Preterit', 'Imperfect'))))

#view(csv18_clean_cleanest3_past_imp1)

#create a bar graph with this information
#now graph this with stacked graph
ggplot(csv18_clean_cleanest3_past_imp1, aes(x = group, y = `Percent use`, fill = `Tense-aspect form`))+
  geom_bar(stat = "identity", position = "stack")+
  facet_wrap(~level)+
  theme_minimal()+
  #labs(x = "Course level", y = "Percent use of forms in preterit-appropriate contexts")+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.80, vjust = 0.9),
        text = element_text(size = 15))+
  scale_fill_brewer(palette="Set2")

## SUPPLIANCE ####

#cleaned dataset only looking at suppliance
#remove ambiguous and use of English cases
# csv18_clean_cleanest3_suppliance <- csv18_clean_cleanest3 %>%
#   filter(Ambiguous != 1 & Form != 'english')

#summarize by # marked per essay
csv18_clean_past_ppl_sum <- csv18_clean_past_ppl %>%
  dplyr::group_by(ID, level, group, Form_num1)%>%
  dplyr::summarise(count = n())
#view(csv18_clean_past_ppl_sum)

#pivot out
csv18_clean_past_ppl_sum1 <- csv18_clean_past_ppl_sum %>%
  pivot_wider(names_from = Form_num1, values_from = count)

#replace NAs with 0s
csv18_clean_past_ppl_sum1$imperfect[is.na(csv18_clean_past_ppl_sum1$imperfect)] <- 0
csv18_clean_past_ppl_sum1$other[is.na(csv18_clean_past_ppl_sum1$other)] <- 0
csv18_clean_past_ppl_sum1$present[is.na(csv18_clean_past_ppl_sum1$present)] <- 0
csv18_clean_past_ppl_sum1$preterit[is.na(csv18_clean_past_ppl_sum1$preterit)] <- 0

#view(csv18_clean_past_ppl_sum1)

#now pivot longer
csv18_clean_past_ppl_sum1 <-csv18_clean_past_ppl_sum1 %>%
  pivot_longer(cols= (4:7), names_to = 'tense', values_to = 'count')
  
#view(csv18_clean_past_ppl_sum1)

#now add metadata about text length-- already have loaded (meta)
#view(meta)
#calculating mean use per 100 words to account for text length
csv18_clean_past_ppl_sum1 <-csv18_clean_past_ppl_sum1 %>%
  left_join(meta)%>%
  dplyr::mutate(count_by_length = count / `word length` *100)

#view(csv18_clean_past_ppl_sum1)

#summary SE for use
pret_imp_supl_se <- summarySE(data = csv18_clean_past_ppl_sum1, 
                    measurevar = 'count_by_length', 
                    groupvars = c('level', 'group', 'tense'),
                    na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
# %>%
#   filter(tense == 'preterit'|tense == 'imperfect')
#view(pret_imp_supl_se)


#graph
ggplot(pret_imp_supl_se, aes(x=group, y=count_by_length, fill=level)) + 
  facet_wrap(~tense)+
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=count_by_length-ci, ymax=count_by_length+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  labs(x = "Group", y = "Suppliance per 100 words")+
  ylim(0,15)+
  scale_fill_brewer(palette="Accent")+ 
  theme(text = element_text(size = 15))

### linear mixed-effects model ####

#run only for SPA 3

#response variable:
# mean suppliance per 100 words

#independent variables:
# group: control vs. intervention
# appropriate form: preterit vs. imperfect

#random intercepts:
# participant

#csv18_clean_past_ppl_sum1 but only for SPA 3
csv18_clean_past_ppl_sum13 <- csv18_clean_past_ppl_sum1 %>%
  filter(level == 'SPA 3')
#view(csv18_clean_past_ppl_sum1)

#histogram of distribution by group
ggplot(csv18_clean_past_ppl_sum13, aes(x=`count_by_length`)) + 
  geom_histogram()+
  facet_wrap(tense~group)

#square root of data
csv18_clean_past_ppl_sum13_anova <- csv18_clean_past_ppl_sum13 %>%
  mutate(count_by_length_sqrt = sqrt(count_by_length))
#view(csv18_clean_past_ppl_sum13_anova)

#histogram of distribution by group
ggplot(csv18_clean_past_ppl_sum13_anova, aes(x=`count_by_length_sqrt`)) + 
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
center_scaled_data <- center_scale(csv18_clean_past_ppl_sum13_anova$count_by_length_sqrt)
#view(center_scaled_data)

#now add back to the tibble
csv18_clean_past_ppl_sum13_anova <- csv18_clean_past_ppl_sum13_anova %>%
  cbind(center_scaled_data)%>%
  dplyr::rename(count_by_length_sqrt_cs = `...14`)
#view(csv18_clean_past_ppl_sum13_anova)

ggplot(csv18_clean_past_ppl_sum13_anova, aes(x=`count_by_length_sqrt_cs`)) + 
  geom_histogram()+
  facet_wrap(tense~group)
#good: this is centered and scaled data

#now run lmer model
#maximal model
lmer_model_1 <- lmer(count_by_length_sqrt_cs ~ group * tense
                     + (1|ID), data=csv18_clean_past_ppl_sum13_anova)
summary(lmer_model_1)
anova(lmer_model_1)

#R2
r.squaredGLMM(lmer_model_1)

#no interaction
lmer_model_2 <- lmer(count_by_length_sqrt_cs ~ group + tense
                     + (1|ID), data=csv18_clean_past_ppl_sum13_anova)
summary(lmer_model_2)
anova(lmer_model_2)

#R2
r.squaredGLMM(lmer_model_2)

# ### *anova- does suppliance differ by group and produced tense-aspect? ####

#square root data
csv18_clean_cleanest3_suppliance_sum1_anova <- csv18_clean_cleanest3_suppliance_sum1%>%
  mutate(count_by_length_sqrt = sqrt(count_by_length))
#view(csv18_clean_cleanest3_suppliance_sum1_anova)


#let's center & scale
#now let's center and scale the frequency data
center_scale <- function(x) {
  scale(x, scale = TRUE) #changed 9-28-22
  #scale = TRUE to center and scale
}

#center scale data
center_scaled_data <- center_scale(csv18_clean_cleanest3_suppliance_sum1_anova$count_by_length_sqrt)
#view(center_scaled_data)

#now add back to the tibble
csv18_clean_cleanest3_suppliance_sum1_anova <- csv18_clean_cleanest3_suppliance_sum1_anova %>%
  cbind(center_scaled_data)%>%
  dplyr::rename(count_by_length_sqrt_cs = `...14`)

#normal distribution?
histogram(csv18_clean_cleanest3_suppliance_sum1_anova$count_by_length_sqrt_cs)

#anova for SPA 3
aov_supl3 <- aov(count_by_length_sqrt_cs ~ group * tense, csv18_clean_cleanest3_suppliance_sum1_anova)
aov_supl3
summary(aov_supl3)
TukeyHSD(aov_supl3)

#tense effect but nothing else

#effect size
eta_squared(aov_supl3)

#view(csv18_token_cde_clean_imp)

# ## SUPPLIANCE 2.0: just ones with obligatory past contexts ####
# 
# #cases where preterit or imperfect is appropriate
# #csv18_token_cde_clean
# 
# #summarize by # marked per essay
# csv18_token_cde_clean_sum <- csv18_token_cde_clean %>%
#   dplyr::group_by(ID, level, group, Form_num1)%>%
#   dplyr::summarise(count = n())
# #view(csv18_token_cde_clean_sum)
# 
# #pivot out
# csv18_token_cde_clean_sum1 <- csv18_token_cde_clean_sum %>%
#   pivot_wider(names_from = Form_num1, values_from = count)
# 
# #replace NAs with 0s
# csv18_token_cde_clean_sum1$imperfect[is.na(csv18_token_cde_clean_sum1$imperfect)] <- 0
# csv18_token_cde_clean_sum1$other[is.na(csv18_token_cde_clean_sum1$other)] <- 0
# csv18_token_cde_clean_sum1$present[is.na(csv18_token_cde_clean_sum1$present)] <- 0
# csv18_token_cde_clean_sum1$preterit[is.na(csv18_token_cde_clean_sum1$preterit)] <- 0
# 
# #view(csv18_token_cde_clean_sum1)
# 
# #now pivot longer
# csv18_token_cde_clean_sum1 <-csv18_token_cde_clean_sum1 %>%
#   pivot_longer(cols= (4:7), names_to = 'tense', values_to = 'count')
# 
# #view(csv18_token_cde_clean_sum1)
# 
# #now add metadata about text length-- already have loaded (meta)
# #view(meta)
# #calculating mean use per 100 words to account for text length
# csv18_token_cde_clean_sum1 <-csv18_token_cde_clean_sum1 %>%
#   left_join(meta)%>%
#   dplyr::mutate(count_by_length = count / `word length` *100)
# 
# #view(csv18_token_cde_clean_sum1)
# 
# #summary SE for use
# pret_imp_supl_se <- summarySE(data = csv18_token_cde_clean_sum1, 
#                               measurevar = 'count_by_length', 
#                               groupvars = c('level', 'group', 'tense'),
#                               na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)%>%
#   filter(tense == 'preterit'|tense == 'imperfect')
# #view(pret_imp_supl_se)
# 
# 
# #graph
# ggplot(pret_imp_supl_se, aes(x=group, y=count_by_length, fill=tense)) + 
#   facet_wrap(~level)+
#   geom_bar(position=position_dodge(), stat="identity") +
#   geom_errorbar(aes(ymin=count_by_length-ci, ymax=count_by_length+ci),
#                 width=.2,                    # Width of the error bars
#                 position=position_dodge(.9))+
#   labs(x = "Group", y = "Suppliance per 100 words")+
#   #ylim(0,7.5)+
#   scale_fill_brewer(palette="Accent")+ 
#   theme(text = element_text(size = 15))
# 
# 
# ### anova- does suppliance differ by group and appropriate tense-aspect? ####
# csv18_token_cde_clean_sum1_anova <- csv18_token_cde_clean_sum1 %>%
#   filter(tense == 'imperfect'| tense == 'preterit')
# #view(csv18_token_cde_clean_sum1_anova)
# 
# #SPA 2
# csv18_token_cde_clean_sum1_anova_SPA_2 <- csv18_token_cde_clean_sum1_anova %>%
#   dplyr::filter(level == 'SPA 2')
# #view(csv18_token_cde_clean_sum1_anova_SPA_2)
# 
# #SPA 3
# csv18_suppliance_sum1_anova_SPA_3 <- csv18_token_cde_clean_sum1_anova %>%
#   dplyr::filter(level == 'SPA 3')
# #view(csv18_suppliance_sum1_anova_SPA_3)
# 
# #anova for SPA 2
# aov_supl <- aov(count_by_length ~ group * tense, csv18_token_cde_clean_sum1_anova_SPA_2)
# aov_supl
# summary(aov_supl)
# #significant interaction in group and tense effect
# # intervention supplying more imperfect, control supplying more preterit
# 
# #anova for SPA 3
# aov_supl3 <- aov(count_by_length ~ group * tense, csv18_suppliance_sum1_anova_SPA_3)
# aov_supl3
# summary(aov_supl3)
# 
# ## SUPPLIANCE 3.0: all for ppl w obligatory past contexts ####
# 
# past_ppl <- read_csv('imp_1st_some_past_ppl_073123.csv')%>%
#   select(-count)
# #view(past_ppl)
# #view(csv18_clean)
# #left join with csv18_clean to get all the data
# past_ppl_sup <- csv18_clean |>
#   semi_join(past_ppl, join_by(ID))
# 
# #view(past_ppl_sup)
# #write a csv with this-- production of ppl who had at least 1 obligatory past context
# #write_csv(past_ppl_sup, 'imp_1st_some_past_ppl_production_073123.csv')
# 
# #summarize based on this to make sure it did it right
# # past_ppl_sup_sum <- past_ppl_sup |>
# #   dplyr::group_by(level, group, ID)|>
# #   dplyr::summarize(count = n())
# #view(past_ppl_sup_sum)
# #yes, only 75
# 
# #summarize by # marked per essay
# past_ppl_sup_sum <- past_ppl_sup %>%
#   dplyr::group_by(ID, level, group, Form_num1)%>%
#   dplyr::summarise(count = n())
# view(past_ppl_sup_sum)
# 
# #pivot out
# past_ppl_sup_sum1 <- past_ppl_sup_sum %>%
#   pivot_wider(names_from = Form_num1, values_from = count)
# 
# #replace NAs with 0s
# past_ppl_sup_sum1$imperfect[is.na(past_ppl_sup_sum1$imperfect)] <- 0
# past_ppl_sup_sum1$other[is.na(past_ppl_sup_sum1$other)] <- 0
# past_ppl_sup_sum1$present[is.na(past_ppl_sup_sum1$present)] <- 0
# past_ppl_sup_sum1$preterit[is.na(past_ppl_sup_sum1$preterit)] <- 0
# 
# #view(past_ppl_sup_sum1)
# 
# #now pivot longer
# past_ppl_sup_sum1 <-past_ppl_sup_sum1 %>%
#   pivot_longer(cols= (4:7), names_to = 'tense', values_to = 'count')
# 
# #view(past_ppl_sup_sum1)
# 
# #now add metadata about text length-- already have loaded (meta)
# #view(meta)
# #calculating mean use per 100 words to account for text length
# past_ppl_sup_sum1 <-past_ppl_sup_sum1 %>%
#   left_join(meta)%>%
#   dplyr::mutate(count_by_length = count / `word length` *100)
# 
# #view(past_ppl_sup_sum1)
# 
# #summary SE for use
# pret_imp_supl_se <- summarySE(data = past_ppl_sup_sum1, 
#                               measurevar = 'count_by_length', 
#                               groupvars = c('level', 'group', 'tense'),
#                               na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)%>%
#   filter(tense == 'preterit'|tense == 'imperfect')
# #view(pret_imp_supl_se)
# 
# 
# #graph
# ggplot(pret_imp_supl_se, aes(x=group, y=count_by_length, fill=tense)) + 
#   facet_wrap(~level)+
#   geom_bar(position=position_dodge(), stat="identity") +
#   geom_errorbar(aes(ymin=count_by_length-ci, ymax=count_by_length+ci),
#                 width=.2,                    # Width of the error bars
#                 position=position_dodge(.9))+
#   labs(x = "Group", y = "Suppliance per 100 words")+
#   #ylim(0,7.5)+
#   scale_fill_brewer(palette="Accent")+ 
#   theme(text = element_text(size = 15))
# 
# 
# ### anova- does suppliance differ by group and appropriate tense-aspect? ####
# past_ppl_sup_sum1_anova <- past_ppl_sup_sum1 %>%
#   filter(tense == 'imperfect'| tense == 'preterit')
# #view(past_ppl_sup_sum1_anova)
# 
# #SPA 3
# past_ppl_sup_sum1_anova_SPA_3 <- past_ppl_sup_sum1_anova %>%
#   dplyr::filter(level == 'SPA 3')
# #view(past_ppl_sup_sum1_anova_SPA_3)
# 
# #anova for SPA 3
# aov_supl3 <- aov(count_by_length ~ group * tense, past_ppl_sup_sum1_anova_SPA_3)
# aov_supl3
# summary(aov_supl3)

### linear mixed-effects model ####

#run only for SPA 3

#response variable:
# mean suppliance per 100 words

#independent variables:
# group: control vs. intervention
# appropriate form: preterit vs. imperfect

#random intercepts:
# participant

#histogram of distribution by group
ggplot(csv18_clean_cleanest3_suppliance_sum1_anova, aes(x=`count_by_length`)) + 
  geom_histogram()+
  facet_wrap(~group)

#square root of data
csv18_clean_cleanest3_suppliance_sum1_anova <- csv18_clean_cleanest3_suppliance_sum1_anova %>%
  mutate(count_by_length_sqrt = sqrt(count_by_length))
#view(past_ppl_sup_sum1_anova_SPA_3)

#histogram of distribution by group
ggplot(csv18_clean_cleanest3_suppliance_sum1_anova, aes(x=`count_by_length_sqrt`)) + 
  geom_histogram()+
  facet_wrap(~group)

#looks normal now!

#let's center & scale
#now let's center and scale the frequency data
center_scale <- function(x) {
  scale(x, scale = TRUE) #changed 9-28-22
  #scale = TRUE to center and scale
}

#center scale data
center_scaled_data <- center_scale(csv18_clean_cleanest3_suppliance_sum1_anova$count_by_length_sqrt)
#view(center_scaled_data)

#now add back to the tibble
csv18_clean_cleanest3_suppliance_sum1_anova <- csv18_clean_cleanest3_suppliance_sum1_anova %>%
  cbind(center_scaled_data)%>%
  dplyr::rename(count_by_length_sqrt_cs = `...14`)
#view(past_ppl_sup_sum1_anova_SPA_3)

ggplot(csv18_clean_cleanest3_suppliance_sum1_anova, aes(x=`count_by_length_sqrt_cs`)) + 
  geom_histogram()+
  facet_wrap(~group)
#good: this is centered and scaled data

#now run lmer model
#maximal model
lmer_model_1 <- lmer(count_by_length_sqrt_cs ~ group * tense
     + (1|ID), data=csv18_clean_cleanest3_suppliance_sum1_anova)
summary(lmer_model_1)
anova(lmer_model_1)

#R2
r.squaredGLMM()

#no interaction
lmer_model_2 <- lmer(count_by_length_sqrt_cs ~ group + tense
                     + (1|ID), data=csv18_clean_cleanest3_suppliance_sum1_anova)
summary(lmer_model_2)
anova(lmer_model_2)

#R2
r.squaredGLMM(lmer_model_2)

# #just the imperfect
# past_ppl_sup_sum1_anova_SPA_3_imp <- past_ppl_sup_sum1_anova_SPA_3 %>%
#   dplyr::filter(tense == 'imperfect')
# 
# lmer_model_1imp <- lmer(count_by_length_sqrt_cs ~ group
#                      + (1|ID), data=past_ppl_sup_sum1_anova_SPA_3_imp)
# summary(lmer_model_1imp)
# #doesn't work because too few observations

## SPA 3 imperfect ####
spa3_imp <- read_csv('imp_1st_some_past_ppl_production_073123.csv') |>
  dplyr::filter(level == 'SPA 3' &
                  Corrected_Form1 == 'imperfect' & #260
                  Form_num1 == 'imperfect') #159
#view(spa3_imp)
#write to csv for lexical aspect analysis
#write_csv(spa3_imp, 'imp_1st_imp_LA_4_analysis_073123.csv')

## Extract: Mostly present ####
#pivot out
past_ppl_sup_sum1_ex <- past_ppl_sup_sum %>%
  pivot_wider(names_from = Form_num1, values_from = count)%>%
  replace(is.na(.), 0)%>%
  dplyr::rowwise()%>%
  dplyr::mutate(
    total = sum(other, present, preterit, imperfect),
    Preterit = preterit / total * 100,
    Imperfect = imperfect / total * 100,
    Present = present / total * 100,
    Other = other / total * 100,
    Pres_or_Other = Present + Other)

#view(past_ppl_sup_sum1_ex)

#write to csv
#write_csv(past_ppl_sup_sum1_ex, 'imp_1st_Claudia_qual_analysis_073123.csv')

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

## Lexical aspect SPA 3 data ####
la <- read_csv('imp_1st_imp_LA_4_analysis_073123 - Copy of SM.csv')%>%
  semi_join(past_ppl_meta_no_na)
#view(la)

#summarize by group
la_sum <- la %>%
  dplyr::group_by(group, LA_category)%>%
  dplyr::summarise(count = n())%>%
  pivot_wider(names_from = LA_category, values_from = count)%>%
  replace(is.na(.), 0)%>%
  dplyr::rowwise()%>%
  dplyr::mutate(
    total = sum(state, activity, accomplishment), #other not produced
    State = state / total * 100,
    Activity = activity / total * 100,
    Accomplishment = accomplishment / total * 100)
#view(la_sum)

#write current participant pool to csv
#write_csv(past_ppl_meta_no_na, 'metadata_cleaned_imp_1st_080323.csv')

#Thanks for reading!