library(tidyverse)
library(purrr)
library(broom.mixed)
library(sjPlot)
library(cowplot)
library(lme4)
library(lmerTest)
library(here)
library(Hmisc)
library(effects)
library(viridis)

###
df <- read.csv(here("data_", "lyrics_runs.csv"))
test_df <- df %>% filter(personality=="True"|topic=="True"|linguistic=="True"|liwc=="True"|value=="True"|audio=="True")

#convert "True" and "False" to 1 and 0 respectively, for analysis
TrueFalseToNumbers <- function(x, print=TRUE){
  x <- as.character(x)
  x <- replace(x, x=="True", "1")
  x <- replace(x, x=="False", "0")
  x <- as.numeric(x)
  return(x)
}

test_df$personality <- TrueFalseToNumbers(test_df$personality)
test_df$liwc <- TrueFalseToNumbers(test_df$liwc)
test_df$topic <- TrueFalseToNumbers(test_df$topic)
test_df$value <- TrueFalseToNumbers(test_df$value)
test_df$linguistic <- TrueFalseToNumbers(test_df$linguistic)
test_df$audio <- TrueFalseToNumbers(test_df$audio)

test_df$feature_number <- (test_df$personality+test_df$topic+test_df$linguistic+test_df$liwc+test_df$value+test_df$audio)
test_df$dimension_number <- (test_df$personality*10+test_df$topic*25+test_df$linguistic*9+test_df$liwc*72+test_df$value*49+test_df$audio*240)
lapply(test_df[,c('personality', 'topic', 'value', 'audio', 'linguistic', 'liwc')], as.factor)

####
test_df <- 
  test_df %>%
  group_by(task) %>%
  mutate(score_z = scale(score))

### models from random effect reduction procedure:
stan1 <- lmer(score_z ~ personality + topic + liwc + value + task + audio + liwc:task + 
                (audio|task/model) + (linguistic+topic+liwc+audio|model), 
              REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000, xtol_abs=1e-8, ftol_abs=1e-8)))
stan1.all <- allFit(stan1)
stan1.nmkbw <- update(stan1, control=  lmerControl(optimizer="nmkbw"))
#didn't converge

#without interaction
stan1.1 <- lmer(score_z ~ personality + topic + liwc + value + task + audio +
                  (audio|task/model) + (linguistic+topic+liwc+audio|model), 
                REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000, xtol_abs=1e-8, ftol_abs=1e-8)))
#fails

#without interaction or task
stan1.2 <- lmer(score_z ~ personality + topic + liwc + value + audio +
                  (audio|task/model) + (linguistic+topic+liwc+audio|model), 
                REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000, xtol_abs=1e-8, ftol_abs=1e-8)))
#runs

stan2 <- lmer(score_z ~ personality + topic + liwc + value + task + audio + liwc:task + 
                (audio|model:task) + (audio|task) + (topic+liwc+audio|model), 
              REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000, xtol_abs=1e-8, ftol_abs=1e-8)))
stan2.all <- allFit(stan2)
#didn't converge

#without interaction
stan2.1 <- lmer(score_z ~ personality + topic + liwc + value + task + audio +  
                  (audio|model:task) + (audio|task) + (topic+liwc+audio|model), 
                REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000, xtol_abs=1e-8, ftol_abs=1e-8)))
#runs

#without interaction or task
stan2.2 <- lmer(score_z ~ personality + topic + liwc + value + audio +  
                  (audio|model:task) + (audio|task) + (topic+liwc+audio|model), 
                REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000, xtol_abs=1e-8, ftol_abs=1e-8)))
#runs

stan3 <- lmer(score_z ~ personality + topic + liwc + value + task + audio + liwc:task + 
                (audio|model:task) + (audio|task) + (linguistic+topic+liwc+audio|model),
              REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000, xtol_abs=1e-8, ftol_abs=1e-8)))
stan3.all <- allFit(stan3)
stan3.nmkbw <- update(stan3, control=  lmerControl(optimizer="nmkbw"))
stan3.nloptwrap <- update(stan3, control=  lmerControl(optimizer="nloptwrap"))
#didn't converge

#without interaction
stan3.1 <- lmer(score_z ~ personality + topic + liwc + value + task + audio + 
                  (audio|model:task) + (audio|task) + (linguistic+topic+liwc+audio|model),
                REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000, xtol_abs=1e-8, ftol_abs=1e-8)))
#fails

#without interaction or task
stan3.2 <- lmer(score_z ~ personality + topic + liwc + value + audio + 
                  (audio|model:task) + (audio|task) + (linguistic+topic+liwc+audio|model),
                REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000, xtol_abs=1e-8, ftol_abs=1e-8)))
#runs

stan4 <- lmer(score_z ~ personality + topic + liwc + value + task + audio + liwc:task + 
                (liwc+audio|model:task) + (audio|task) + (linguistic+topic+liwc+audio|model),
              REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000, xtol_abs=1e-8, ftol_abs=1e-8)))
stan4.all <- allFit(stan4)
stan4.NelderMead <- update(stan4, control = lmerControl(optimizer="Nelder_Mead"))
stan4.nloptwrap <- update(stan4, control=  lmerControl(optimizer="nloptwrap"))
#didn't converge

#without interaction
stan4.1 <- lmer(score_z ~ personality + topic + liwc + value + task + audio +  
                  (liwc+audio|model:task) + (audio|task) + (linguistic+topic+liwc+audio|model),
                REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000, xtol_abs=1e-8, ftol_abs=1e-8)))
#fails

#without interaction or task
stan4.2 <- lmer(score_z ~ personality + topic + liwc + value + audio +  
                  (liwc+audio|model:task) + (audio|task) + (linguistic+topic+liwc+audio|model),
                REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000, xtol_abs=1e-8, ftol_abs=1e-8)))
#runs

stan5 <- lmer(score_z ~ personality + topic + liwc + value + task + audio + liwc:task + 
                (1|model:task) + (1|task) + (liwc+audio|model),
              REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000, xtol_abs=1e-8, ftol_abs=1e-8)))
#runs

#without interaction
stan5.1 <- lmer(score_z ~ personality + topic + liwc + value + task + audio + 
                  (1|model:task) + (1|task) + (liwc+audio|model),
                REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000, xtol_abs=1e-8, ftol_abs=1e-8)))
#runs
#without interaction or task
stan5.2 <- lmer(score_z ~ personality + topic + liwc + value + audio + 
                  (1|model:task) + (1|task) + (liwc+audio|model),
                REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000, xtol_abs=1e-8, ftol_abs=1e-8)))
#runs

stan6 <- lmer(score_z ~ personality + topic + liwc + value + task + audio + liwc:task + 
                (1|model:task) + (1|task) + (1|model), 
              REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000, xtol_abs=1e-8, ftol_abs=1e-8)))
#runs
#without interaction
stan6.1 <- lmer(score_z ~ personality + topic + liwc + value + task + audio + 
                  (1|model:task) + (1|task) + (1|model), 
                REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000, xtol_abs=1e-8, ftol_abs=1e-8)))
#runs
#without task or interaction
stan6.2 <- lmer(score_z ~ personality + topic + liwc + value + audio + 
                  (1|model:task) + (1|task) + (1|model), 
                REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000, xtol_abs=1e-8, ftol_abs=1e-8)))
#runs

#hypothesized model #1
stan7 <- lmer(score_z ~ personality + linguistic + topic + liwc + value + task + audio + 
                topic:liwc + topic:task + liwc:task + value:task + 
                (model|task/model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 1000000,xtol_abs=1e-8, ftol_abs=1e-8)))
#doesn't converge
#with only two interactions
stan7.1 <- lmer(score_z ~ personality + linguistic + topic + liwc + value + task + audio + 
                  liwc:task + value:task + 
                  (model|task/model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 1000000,xtol_abs=1e-8, ftol_abs=1e-8)))
#fails
#only fixed effect reduction interaction
stan7.2 <- lmer(score_z ~ personality + linguistic + topic + liwc + value + task + audio + 
                  liwc:task + 
                  (model|task/model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 1000000,xtol_abs=1e-8, ftol_abs=1e-8)))
save(stan7.2, file = "stan7.2.rda") #runs
#without interactions
stan7.3 <- lmer(score_z ~ personality + linguistic + topic +liwc + value + task + audio +
                  (model|task/model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 1000000,xtol_abs=1e-8, ftol_abs=1e-8)))
#doesn't converge

#without task or interactions
stan7.4 <- lmer(score_z ~ personality + linguistic + topic +liwc + value + audio +
                  (model|task/model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 1000000,xtol_abs=1e-8, ftol_abs=1e-8)))
#fails
#hypothesized model #2
stan8 <- lmer(score_z ~ personality + linguistic + topic + liwc + value + task + audio + 
                topic:liwc + topic:task + liwc:task + value:task + 
                (1|task/model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 1000000,xtol_abs=1e-8, ftol_abs=1e-8)))
save(stan8, file = "stan8.rda") #runs
#runs
#with only two interactions
stan8.1 <- lmer(score_z ~ personality +linguistic + topic + liwc + value + task + audio + 
                  liwc:task + value:task + 
                  (1|task/model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 1000000,xtol_abs=1e-8, ftol_abs=1e-8)))
save(stan8.1, file = "stan8.1.rda") #runs
#with only one interaction
stan8.2 <- lmer(score_z ~ personality +linguistic + topic + liwc + value + task + audio + 
                  liwc:task +
                  (1|task/model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 1000000,xtol_abs=1e-8, ftol_abs=1e-8)))
save(stan8.2, file = "stan8.2.rda") #runs
#without interactions
stan8.3 <- lmer(score_z ~ personality +linguistic + topic + liwc + value + task + audio + 
                  (1|task/model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 1000000,xtol_abs=1e-8, ftol_abs=1e-8)))
save(stan8.3, file = "stan8.3.rda") #runs
#without task or interactions
stan8.4 <- lmer(score_z ~ personality +linguistic + topic + liwc + value + audio + 
                  (1|task/model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 1000000,xtol_abs=1e-8, ftol_abs=1e-8)))
save(stan8.4, file = "stan8.4.rda") #runs

#model_fits_stan =list(stan8.4, stan8.3, stan8.2, stan8.1, stan7.2, stan6.2, stan6.1, stan6, stan5.2, stan5.1, stan4.2, stan3.2, stan2.2, stan2.1, stan1.2)
model_fits_stan =list(stan8.4, stan8.3, stan8.2, stan8.1, stan7.2)

model_params_stan <- model_fits_stan %>%
  tibble() %>%
  rename("model" = ".") %>%
  mutate(tidied = purrr::map(model, broom::tidy),
         model_num = row_number()) %>%
  select(model_num, tidied) %>%
  unnest()
model_params_stan$dv = "standardized"

fixed_params <- model_params_stan %>% 
  filter(effect=="fixed")
ran_params <- model_params_stan %>% 
  filter(effect=="ran_pars")
ran_params$term <- paste(ran_params$group, "_", ran_params$term)
all_params <- rbind(ran_params, fixed_params)

model_confints = map(model_fits_stan, ~confint.merMod(.x, level=.95, method="boot", nsim=500)) %>%
  tibble() %>%
  rename("model" = ".") %>%
  mutate(tidied = purrr::map(model, broom::tidy), 
         model_num = row_number()) %>%
  select(model_num, tidied) %>%
  unnest() %>%
  rename("term" = ".rownames", 
         "lower" = "X2.5..", 
         "upper" = "X97.5..")

all_params <- left_join(all_params, model_confints)
save(all_params, file = "all_params_stan.rda")

#plots
top = all_params %>%
  filter(term=="value"|term=="liwc"|term=="audio"|term=="personality"|term=="topic"|term=="linguistic") %>%
  #filter(term=="value"|term=="liwc"|term=="audio"|term=="personality"|term=="topic"|term=="linguistic"|term=="taskgenre_clf"|term=="taskrec_item_cold") %>%
  #filter(term=="value"|term=="liwc"|term=="audio") %>%
  ggplot(aes(model_num, estimate, color = term)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position="dodge") +
  geom_point(position=position_dodge(width=.9)) +
  scale_colour_manual(values = inferno(6, alpha = 1, begin = .1, end = .9, direction = 1), guide = "legend") +
  scale_fill_manual(values = inferno(6, alpha = 1, begin=.1, end=.9, direction=1), guide=FALSE) +
  labs(x = "", y = "parameter estimate\n") + 
  theme_minimal(base_size = 11) +
  scale_x_continuous(breaks = seq(1, 8, by= 1)) +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        axis.text = element_text(color = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "top",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
top


model_fits_all = model_fits_stan %>%
  tibble() %>%
  rename("model" = ".") %>%
  mutate(model_num = row_number(),
         AIC = map_dbl(model, AIC),
         BIC = map_dbl(model, BIC)) %>%
  select(-model)

(models.sca = all_params %>%
    select(model_num, term, estimate) %>%
    spread(term, estimate) %>%
    left_join(., model_fits_all) %>%
    arrange(AIC)%>%
    select(audio, liwc, linguistic, topic, value, personality, everything(), -AIC, -BIC, -contains("Residual"), -contains("_ sd__M"), -contains("cor")))

bottom_plot <- models.sca %>%
  select(-taskrec_item_cold) %>%
  rename(
    fixed_audio=audio, fixed_liwc=liwc, fixed_value=value, 
    fixed_personality=personality, fixed_linguistic=linguistic, 
    fixed_topic=topic, fixed_task=taskgenre_clf, 
    `interaction_liwc:taskgenre_clf`=`liwc:taskgenre_clf`, 
    `interaction_liwc:taskrec_item_cold`=`liwc:taskrec_item_cold`, 
    `interaction_value:taskgenre_clf`=`value:taskgenre_clf`, 
    `interaction_value:taskrec_item_cold`=`value:taskrec_item_cold`, 
    `random_task_intercept` = `task _ sd__(Intercept)`, 
    `random_model:task_intercept` = `model:task _ sd__(Intercept)`)

#variable.names = names(select(models.sca, -model_num, -AIC, -BIC))
#variable.names = colnames(select(models.sca, -model_num))
variable.names = colnames(select(bottom_plot, -model_num))

slope_line <- data.frame(5, "random_model:task_model_slope", "|")
names(slope_line) <- c("model_num", "variable", "value")

#bottom = models.sca %>%
bottom = bottom_plot %>%
  gather(variable, value, eval(variable.names)) %>% 
  mutate(value = ifelse(!is.na(value), "|", "")) %>%
  rbind(slope_line) %>%
  ggplot(aes(model_num, variable)) +
  geom_text(aes(label = value)) +
  labs(x = "\nmodel number", y = "variables\n") + 
  theme_minimal(base_size = 11) +
  scale_x_continuous(breaks = seq(1, 15, by= 1)) +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 20),
        axis.text = element_text(color = "black"),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )
bottom

cowplot::plot_grid(top, bottom, ncol = 1, align = "v", axis="l", labels = c('A', 'B'))


#median plot
medians <- aggregate(all_params[, 5], list(all_params$term), median)
medians <- medians %>%
  filter(Group.1 == "audio"|Group.1=="value"|Group.1=="personality"|Group.1=="topic"|Group.1=="linguistic"|Group.1=="liwc")

x_names <- c("Audio", "Values", "Personality", "Topics", 
             "Linguistic", "LIWC")
x_vars <- c("audio", "value", "personality", "topic", 
            "linguistic", "liwc")
vars <- as.data.frame(cbind(x_vars, x_names))
names(vars) <- c("Group.1", "Names")

medians <- left_join(medians, vars, by = "Group.1" )
names(medians) <- c("Var_name", "x", "Group.1")


medians <- medians %>%
  arrange(desc(x))
medians$Features <- factor(medians$Group.1, ordered=TRUE)


median_plot <-ggplot(data=medians, aes(x = reorder(Features, x), y = x, fill = Features)) +
  coord_flip() +
  geom_bar(stat="identity") +
  scale_x_discrete(position = "top") +
  scale_fill_manual(values = inferno(6, alpha = 0.8, begin=.1, end=.9, direction=1), guide=FALSE) +
  labs(x = "", y = "Median Parameter Estimate") +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 9),
        axis.text = element_text(color = "black"),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
median_plot

#cowplot::plot_grid(top, median_plot, ncol = 2, align = "h", axis="l", labels = c('A', 'B'))
cowplot::plot_grid(median_plot, top, bottom, ncol = 1, align = "v", axis="l", labels = c('A', 'B', 'C'))

##interactions
stan8.1_plot <- plot_model(stan8.1, ci.lvl=0.95)
stan8.1_plot <- stan8.1_plot + theme(text=element_text(size=15)) +
  ggtitle("") +
  labs(y="Parameter Estimates")

eff_liwc_task <- plot_model(stan8.1, type = "eff", terms = c("task", "liwc [0, 1]"), ci.lvl=.95) 
eff_value_task <- plot_model(stan8.1, type = "eff", terms = c("task", "value [0, 1]"), ci.lvl=.95)


right_column_plot <- cowplot::plot_grid(eff_liwc_task, eff_value_task, ncol=1, align = "v", axis="l", labels = c('B', 'C'))
plot2 <- cowplot::plot_grid(stan8.1_plot, right_column_plot, ncol=2, align="h", labels=c('A'))
#int_liwc_task <- plot_model(stan8.1, type="pred", terms=c("task", "liwc [0, 1]"))
#int_value_task <- plot_model(stan8.1, type="pred", terms=c("task", "value [0, 1]"))
