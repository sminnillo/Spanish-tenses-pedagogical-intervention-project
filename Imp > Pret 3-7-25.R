#Order of instruction of preterit-imperfect project code
#3-13-23; updated 9-26-24; updated 3-7-25
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
library(lmerTest)
library(effectsize)
library(forecast)
library(sos)
library(MuMIn)
library(emmeans)
library(DHARMa)

## Load CSV ####

#read metadata ('demographics_written_text_and_pre_course_quiz_030425.csv' on Github)
meta <- read_csv("impfct first_all data - combined_3.csv") |>
  #have to drop NAs of people who don't have review quiz scores
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

#load csv with all data ('written_text_analysis_030425.csv' on Github)
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

#relatively similar distributions across groups

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

#view(csv_18_suppliance)

#summarize by # marked per essay
csv18_suppliance_sum <- csv_18_suppliance %>%
  dplyr::group_by(ID, level, group, Form_num1)%>%
  dplyr::summarise(count = n())
#view(csv18_suppliance_sum)

#3-4-25

#just select level, group, ID
csv_18_suppliance_sim <- csv_18_suppliance|>
  dplyr::select(level, group, ID)|>
  unique()
view(csv_18_suppliance_sim)

#summarize by # verb tokens analyzed by level
csv_18_suppliance_level <- csv_18_suppliance |>
  dplyr::group_by(level, group)%>%
  dplyr::summarise(count = n())
#view(csv_18_suppliance_level)

#num texts by group
csv_18_suppliance_text_group <- csv_18_suppliance |>
  dplyr::group_by(level, group, ID)%>%
  dplyr::summarise(count = n())|>
  dplyr::group_by(level, group)%>%
  dplyr::summarise(count = n())
#view(csv_18_suppliance_text_group)

#pivot out
csv18_suppliance_sum1 <- csv18_suppliance_sum %>%
  pivot_wider(names_from = Form_num1, values_from = count)

#view(csv18_suppliance_sum1)

#replace NAs with 0s
csv18_suppliance_sum1$imperfect[is.na(csv18_suppliance_sum1$imperfect)] <- 0
csv18_suppliance_sum1$other[is.na(csv18_suppliance_sum1$other)] <- 0
csv18_suppliance_sum1$present[is.na(csv18_suppliance_sum1$present)] <- 0
csv18_suppliance_sum1$preterit[is.na(csv18_suppliance_sum1$preterit)] <- 0

#view(csv18_suppliance_sum1)
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

#write to csv
#write_csv(csv18_suppliance_sum1, 'csv18_suppliance_sum1_030725.csv')

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

### linear mixed-effects model ####

#can read in csv & run from here
csv18_suppliance_sum1 <- read_csv('csv18_suppliance_sum1_030725.csv')

csv18_suppliance_sum1_anova_SPA_3 <- csv18_suppliance_sum1|>
  dplyr::filter(level == 'SPA 3')|>
  mutate(count_by_length_sqrt = sqrt(count_by_length))
#view(csv18_suppliance_sum1_anova_SPA_3)

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

#looks more normal now!

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
#view(csv18_suppliance_sum1_anova_SPA_3)

#histogram of distribution by group- center scaled
ggplot(csv18_suppliance_sum1_anova_SPA_3, aes(x=`center_scaled_data`)) + 
  geom_histogram()+
  facet_wrap(tense~group)

# csv18_suppliance_sum1_anova_SPA_3 <- csv18_suppliance_sum1_anova_SPA_3 %>%
#   dplyr::rename(count_by_length_sqrt_cs = `...15`)
#view(csv18_suppliance_sum1_anova_SPA_3)
# 
# ggplot(csv18_suppliance_sum1_anova_SPA_3, aes(x=`count_by_length_sqrt`)) + 
#   geom_histogram()+
#   facet_wrap(tense~group)
# #good: this is centered and scaled data

#now run lmer model
#maximal model
lmer_model_1 <- lmer(center_scaled_data ~ group * tense
                     + (1|ID), data=csv18_suppliance_sum1_anova_SPA_3, 
                     control=lmerControl(optimizer="bobyqa",
                                         optCtrl=list(maxfun=2e5)))
summary(lmer_model_1)
#singularity message
anova(lmer_model_1)
#lmerTest::anova(lmer_model_1)
emmeans(lmer_model_1,  ~ tense | group)
#R2
r.squaredGLMM(lmer_model_1)
#try again with sjPlot
performance::r2(lmer_model_1, tolerance = 1e-10)

performance::check_singularity(lmer_model_1)

#plot
plot(lmer_model_1)

#no interaction
lmer_model_2 <- lmer(center_scaled_data ~ group + tense
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

#filter so only considering preterit & imperfect
csv18_suppliance_sum1_anova_SPA_3_pi <- csv18_suppliance_sum1_anova_SPA_3|>
  filter(tense == 'preterit' | tense == 'imperfect')
#view(csv18_suppliance_sum1_anova_SPA_3_pi)

#try to run models now- just preterit & imperfect

#with interaction
#maximal model
lmer_model_1_1 <- lmer(center_scaled_data ~ group * tense
                       + (1|ID), data=csv18_suppliance_sum1_anova_SPA_3_pi, 
                       control=lmerControl(optimizer="bobyqa",
                                           optCtrl=list(maxfun=2e5)))
summary(lmer_model_1_1)
#no singularity message!
anova(lmer_model_1_1)
#lmerTest::anova(lmer_model_1)
emmeans(lmer_model_1_1,  ~ tense | group)
#R2
r.squaredGLMM(lmer_model_1_1)
#R2 with sjPlot
performance::r2(lmer_model_1_1, tolerance = 1e-10)

performance::check_singularity(lmer_model_1_1)

#plot
plot(lmer_model_1_1)

#gives nice representation of output
tab_model(lmer_model_1_1)

### suppliance LMR- all SPA 3 ####
#no interaction
lmer_model_2_2 <- lmer(center_scaled_data ~ group + tense
                       + (1|ID), data=csv18_suppliance_sum1_anova_SPA_3_pi, 
                       control=lmerControl(optimizer="bobyqa",
                                           optCtrl=list(maxfun=2e5)))
summary(lmer_model_2_2)
anova(lmer_model_2_2)
#no singularity issue!
performance::check_singularity(lmer_model_2_2)
#R2
r.squaredGLMM(lmer_model_2_2)
#slightly better than in plot with interaction

#gives nice representation of output
tab_model(lmer_model_2_2)

effectsize::eta_squared(lmer_model_2_2)

#### check assumptions ####

# 1. Linearity
# 2. Normality of the residuals
# 3. Homogeneity of residual variance (homoscedasticity)
# 4. No autoccorelation and no multicolinearity
# 
# more about checking assumptions here: https://ademos.people.uic.edu/Chapter18.html
# https://bookdown.org/animestina/phd_july_19/testing-the-assumptions.html
# https://library.virginia.edu/data/articles/diagnostic-plots 

#checking model assumptions

plot(lmer_model_2_2)

#1. Checking Linearity Assumption:

#let's try with sjplot
#dev.off()
sjPlot::plot_model(lmer_model_2_2, type='slope')

#pretty straight 

#2. Normality of the residuals
lattice::qqmath(lmer_model_2_2)
qqnorm(residuals(lmer_model_2_2))
#all good

#3. Homogeneity of residual variance (homoscedasticity)

#let's try with sjplot
#dev.off()
sjPlot::plot_model(lmer_model_2_2, type='diag')

library(car) #for Levine test
#by group
car::leveneTest(residuals(lmer_model_2_2) ~ 
                  csv18_suppliance_sum1_anova_SPA_3_pi$group)
#by tense
car::leveneTest(residuals(lmer_model_2_2) ~ 
                  csv18_suppliance_sum1_anova_SPA_3_pi$tense)

#visual representation
boxplot(residuals(lmer_model_2_2) ~ 
          csv18_suppliance_sum1_anova_SPA_3_pi$tense)

#group
boxplot(residuals(lmer_model_2_2) ~ 
          csv18_suppliance_sum1_anova_SPA_3_pi$group)

#4. No autoccorelation and no multicolinearity
performance::check_collinearity(lmer_model_2_2)
#low correlation

### suppliance LMR- past narratives SPA 3 ####

#filter down dataset
lmr_sup_pivot <- csv18_suppliance_sum1_anova_SPA_3_pi|>
  select(1:11)|>
  pivot_wider(names_from = tense, values_from = c(count))|>
  dplyr::rowwise()|>
  dplyr::mutate(total = sum(`preterit`, `imperfect`))
#view(lmr_sup_pivot)

#now, exclude any who have none marked in pret or imperfect
lmr_sup_pivot_past <- lmr_sup_pivot |>
  filter(total > 0) |>
  #pivot back to longer
  pivot_longer(cols = c(`preterit`, `imperfect`),
               names_to ='tense',
               values_to = 'count')|>
  #now re-attach with original dataframe
  left_join(csv18_suppliance_sum1_anova_SPA_3_pi)


#view(lmr_sup_pivot_past)

#no interaction
lmer_model_2_2_2 <- lmer(center_scaled_data ~ group + tense
                         + (1|ID), data=lmr_sup_pivot_past, 
                         control=lmerControl(optimizer="bobyqa",
                                             optCtrl=list(maxfun=2e5)))
summary(lmer_model_2_2_2)
anova(lmer_model_2_2_2)
#no singularity issue!
performance::check_singularity(lmer_model_2_2_2)
#R2
r.squaredGLMM(lmer_model_2_2_2)

#gives nice representation of output
tab_model(lmer_model_2_2_2)

effectsize::eta_squared(lmer_model_2_2_2)

#### check assumptions ####

# 1. Linearity
# 2. Normality of the residuals
# 3. Homogeneity of residual variance (homoscedasticity)
# 4. No autoccorelation and no multicolinearity
# 
# more about checking assumptions here: https://ademos.people.uic.edu/Chapter18.html
# https://bookdown.org/animestina/phd_july_19/testing-the-assumptions.html
# https://library.virginia.edu/data/articles/diagnostic-plots 

#checking model assumptions

plot(lmer_model_2_2_2)

#1. Checking Linearity Assumption:

#let's try with sjplot
#dev.off()
sjPlot::plot_model(lmer_model_2_2_2, type='slope')

#pretty straight 

#2. Normality of the residuals
lattice::qqmath(lmer_model_2_2_2)
qqnorm(residuals(lmer_model_2_2_2))
#all good

#3. Homogeneity of residual variance (homoscedasticity)

#let's try with sjplot
#dev.off()
sjPlot::plot_model(lmer_model_2_2_2, type='diag')

library(car) #for Levine test
#by group
car::leveneTest(residuals(lmer_model_2_2_2) ~ 
                  lmr_sup_pivot_past$group)
#by tense
car::leveneTest(residuals(lmer_model_2_2_2) ~ 
                  lmr_sup_pivot_past$tense)

#visual representation
boxplot(residuals(lmer_model_2_2_2) ~ 
          lmr_sup_pivot_past$tense)

#group
boxplot(residuals(lmer_model_2_2_2) ~ 
          lmr_sup_pivot_past$group)

#4. No autoccorelation and no multicolinearity
performance::check_collinearity(lmer_model_2_2_2)
#low correlation

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

#plot it
#Accuracy by Group
jtools::effect_plot(model1_1, pred = group,
                    x.label = 'Group',
                    y.label = 'Accuracy'
)+
  ylim(0, 1.0)

#divide into groups for Cohen's d

#view(csv18_clean_cleanest3_past)

csv18_clean_cleanest3_past_pret <- csv18_clean_cleanest3_past |>
  dplyr::filter(Corrected_Form1 == 'preterit')
#view(csv18_clean_cleanest3_past_pret)

csv18_clean_cleanest3_past_imp <- csv18_clean_cleanest3_past |>
  dplyr::filter(Corrected_Form1 == 'imperfect')
#view(csv18_clean_cleanest3_past_imp)

#calculate cohen's d: 3-5-25
#cohen's d
cohens_d(Appropriate ~ 
           group, data = csv18_clean_cleanest3_past)

#### check assumptions ####
#1. distribution of data: good
#group
ggplot(csv18_clean_cleanest3_past, aes(x=group, y=Appropriate)) + 
  geom_point()+
  geom_jitter()

#tense
ggplot(csv18_clean_cleanest3_past, aes(x=Corrected_Form1, y=Appropriate)) + 
  geom_point()+
  geom_jitter()

#2. Normality of the residuals: all good
performance::check_model(model1_1)

#3. Homogeneity of Variance (Heteroscedasticity): all good
#group
DHARMa::testCategorical(model1_1, catPred = csv18_clean_cleanest3_past$group)
#tense
DHARMa::testCategorical(model1_1, catPred = csv18_clean_cleanest3_past$`Corrected_Form1`)

#4. multicollinearity: all good
car::vif(model1_1) #low

#just preterit
#write_csv(csv18_clean_cleanest3_past_pret, 'csv18_clean_cleanest3_past_pret.csv')
csv18_clean_cleanest3_past_pret <- csv18_clean_cleanest3_past %>%
  filter(Corrected_Form1 == 'preterit')

model2 <- glmer(Appropriate ~ group +
                  (1|ID),
                csv18_clean_cleanest3_past_pret,
                family = 'binomial', 
                control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=50000)))
summary(model2)
jtools::summ(model2)
r.squaredGLMM(model2)

#no group effect

#Accuracy by Group
jtools::effect_plot(model2, pred = group,
                    x.label = 'Group',
                    y.label = 'Accuracy'
)+
  ylim(0, 1.0)+
  theme(text = element_text(size = 13))

#cohen's d
csv18_clean_cleanest3_past_pret_c <- csv18_clean_cleanest3_past_pret |>
  dplyr::filter(group== 'control')
#view(csv18_clean_cleanest3_past_pret_c)

csv18_clean_cleanest3_past_pret_i <- csv18_clean_cleanest3_past_pret |>
  dplyr::filter(group == 'intervention')
#view(csv18_clean_cleanest3_past_pret_i)

#using this
cohens_d(csv18_clean_cleanest3_past_pret_i$Appropriate,
         csv18_clean_cleanest3_past_pret_c$Appropriate)

#test
#https://easystats.github.io/effectsize/
cohens_d(Appropriate ~ group,
         data = csv18_clean_cleanest3_past_pret,
         pooled_sd = FALSE )

#calculating by hand
#https://www.memoryandlearninglab.it/wp-content/uploads/2024/02/Lmer-and-glm-effect-size2.html
#estimate/SD of random effects
#0.2162 / 1.141


#just imperfect
csv18_clean_cleanest3_past_imp <- csv18_clean_cleanest3_past %>%
  filter(Corrected_Form1 == 'imperfect')
#view(csv18_token_cde_clean_imp)
#write_csv(csv18_clean_cleanest3_past_imp, 'csv18_clean_cleanest3_past_imp.csv')

model3 <- glmer(Appropriate ~ group +
                  (1|ID),
                csv18_clean_cleanest3_past_imp,
                family = 'binomial', 
                control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=50000)))
summary(model3)
r.squaredGLMM(model3)

#no group effect

#Accuracy by Group
jtools::effect_plot(model3, pred = group,
                    x.label = 'Group',
                    y.label = 'Accuracy'
)+
  ylim(0, 1.0)+
  theme(text = element_text(size = 13))

#cohen's d
cohens_d(Appropriate ~ group,
         data = csv18_token_cde_clean_imp,
         pooled_sd = TRUE )

#https://trendingsideways.com/the-cohens-d-formula
#https://rpsychologist.com/cohend/

#emm <- emmeans(model3, "group")
#emmeans::eff_size(emm)

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
  ylab('Percent Appropriate Use')+
  labs(fill = "Tense-Aspect Form")+
  scale_fill_brewer(palette="Accent")+ 
  theme(text = element_text(size = 20))

### Alternate tenses in cases where pret-imp accurate ####
#preterit appropriate
csv18_clean_cleanest3_past_pret1 <-csv18_clean_cleanest3_past_pret %>%
  dplyr::group_by(level, group, Form_num1)%>%
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

## Cloze Task ####
#Is there any difference in unit exam scores (cloze task) b/w groups?

#function for scaling
minmaxnormalise <- function(x){(x- min(x)) /(max(x)-min(x))}

#import data (cloze_task_012325.csv on Github)
Imp1stUG_012325 <- read_csv('Imp1stUG_012325.csv')|>
  dplyr::mutate(
    score_sqr = (score)^2,
    score_cube = (score)^3,
    score_boxcox = forecast::BoxCox(score, lambda = "auto"),
    #center and scale
    score_bx_scale = scale(score_boxcox, center = TRUE, scale = TRUE),
    Group1 = case_when(
      Group == 'PFG'~ 'control',
      Group == 'IFG'~ 'intervention'
    )
  )
#view(Imp1stUG_012325)

#visualization of data
hist(Imp1stUG_012325$score_bx_scale)
#roughly normal

#boxplots
ggplot(Imp1stUG_012325, aes(x=Group1, 
                            y=score
                            ,
                            color = Group
)) + 
  geom_boxplot()+
  labs(x = "Group", y = "Score")+
  geom_jitter(shape=1, width = 0.3)+
  scale_color_brewer(palette="Set1")+
  theme(text = element_text(size = 13))+
  ylim(0, 40)

#what lambda?
forecast::BoxCox(Imp1stUG_012325$score, lambda = "auto")
# (,"lambda") = 1.999927 (basically square)
#https://rpubs.com/frasermyers/627589

#summarize
Imp1stUG_012325_SE <- 
  summarySE(
    Imp1stUG_012325,
    'score',
    groupvars = 'Group',
    na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
view(Imp1stUG_012325_SE)

#PFG
Imp1stUG_012325_p <- Imp1stUG_012325 |>
  dplyr::filter(Group == 'PFG')
#view(Imp1stUG_012325_p)

#IFG
Imp1stUG_012325_i <- Imp1stUG_012325 |>
  dplyr::filter(Group == 'IFG')
#view(Imp1stUG_012325_i)

#histogram: PFG
hist(Imp1stUG_012325_p$score)
#better
hist(Imp1stUG_012325_p$score_sqr)
#worse
hist(Imp1stUG_012325_p$score_cube)
hist(Imp1stUG_012325_p$score_bx_scale)


#histogram: IFG
hist(Imp1stUG_012325_i$score)
hist(Imp1stUG_012325_i$score_sqr)
#worse
hist(Imp1stUG_012325_i$score_cube)
hist(Imp1stUG_012325_i$score_bx_scale)

### LMER1 ####
#group fixed effect, predicting score_bx_scale
#class random effect

M1 <- lmer(score_bx_scale ~ Group +
             (1 | Section),
           Imp1stUG_012325)
summary(M1)

#R2
r.squaredGLMM(M1)
#gives nice representation of output
tab_model(M1)

#Effect size

effectsize::eta_squared(M1)
#partial eta squared is 0.07

#effect plot
jtools::effect_plot(M1, pred = Group,
                    x.label = 'Group',
                    y.label = 'Score'
)

anova(M1)
#include F value, NumDF & DenomDF
F_to_eta2(1.4217, 1, 19.381)
# Eta2 (partial) |       95% CI
#   0.07           | [0.00, 1.00]

F_to_epsilon2(1.4217, 1, 19.381)
#0.02

# example

### check assumptions ####

# 1. Linearity
# 2. Normality of the residuals
# 3. Homogeneity of residual variance (homoscedasticity)
# 4. No autoccorelation and no multicolinearity
# 
# more about checking assumptions here: https://ademos.people.uic.edu/Chapter18.html
# https://bookdown.org/animestina/phd_july_19/testing-the-assumptions.html
# https://library.virginia.edu/data/articles/diagnostic-plots 

#checking model assumptions

#1. Checking Linearity Assumption:

#let's try with sjplot
#dev.off()
sjPlot::plot_model(M1, type='slope')

#pretty straight 

#2. Normality of the residuals
lattice::qqmath(M1)

#kind of


#3. Homogeneity of residual variance (homoscedasticity)

#let's try with sjplot
#dev.off()
sjPlot::plot_model(M1, type='diag')

#looks fine

library(car) #for Levine test
car::leveneTest(residuals(M1) ~ 
                  Imp1stUG_012325$Group)
#Pr(>F) = 0.07511: established Homogeneity of residual variance

#visual representation
boxplot(residuals(M1) ~ 
          Imp1stUG_012325$Group)

#4. No autoccorelation and no multicolinearity
performance::check_collinearity(M1)
#high correlation of primetype and item:primetype, but likely just because of 
#interaction effect


## Demographics for paper 3-3-25 ####
demo_030425 <- read_csv('combined_metadata_imp_1st_030425.csv')
#view(demo_030425)

#group by level, group, & sum the total number of verbs
demo_030425_sum <- demo_030425 |>
  dplyr::group_by(level, group)
# |>
#   summarize(total_verb = sum(`Total verbs`))
#view(demo_030425_sum)

#Thanks for reading!
