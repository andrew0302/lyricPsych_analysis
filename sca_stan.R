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
test_df$dimension_number <- (test_df$personality*5+test_df$topic*25+test_df$linguistic*9+test_df$liwc*72+test_df$value*49+test_df$audio*240)
lapply(test_df[,c('personality', 'topic', 'value', 'audio', 'linguistic', 'liwc')], as.factor)

####
test_df <- 
  test_df %>%
  group_by(task) %>%
  mutate(score_z = scale(score))



stan1 <- lmer(score_z ~ personality + topic + liwc + value + task + audio + liwc:task + 
                (audio|task/model) + (linguistic+topic+liwc+audio|model), 
              REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000, xtol_abs=1e-8, ftol_abs=1e-8)))
stan1.all <- allFit(stan1)
stan1.nmkbw <- update(stan1, control=  lmerControl(optimizer="nmkbw"))
#didn't converge

stan2 <- lmer(score_z ~ personality + topic + liwc + value + task + audio + liwc:task + 
                (audio|model:task) + (audio|task) + (topic+liwc+audio|model), 
              REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000, xtol_abs=1e-8, ftol_abs=1e-8)))
stan2.all <- allFit(stan2)
#didn't converge

stan3 <- lmer(score_z ~ personality + topic + liwc + value + task + audio + liwc:task + 
                (audio|model:task) + (audio|task) + (linguistic+topic+liwc+audio|model),
              REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000, xtol_abs=1e-8, ftol_abs=1e-8)))
stan3.all <- allFit(stan3)
stan3.nmkbw <- update(stan3, control=  lmerControl(optimizer="nmkbw"))
stan3.nloptwrap <- update(stan3, control=  lmerControl(optimizer="nloptwrap"))
#didn't converge

stan4 <- lmer(score_z ~ personality + topic + liwc + value + task + audio + liwc:task + 
                (liwc+audio|model:task) + (audio|task) + (linguistic+topic+liwc+audio|model),
              REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000, xtol_abs=1e-8, ftol_abs=1e-8)))
stan4.all <- allFit(stan4)
stan4.NelderMead <- update(stan4, control = lmerControl(optimizer="Nelder_Mead"))
stan4.nloptwrap <- update(stan4, control=  lmerControl(optimizer="nloptwrap"))
#didn't converge

stan5 <- lmer(score_z ~ personality + topic + liwc + value + task + audio + liwc:task + 
                (1|model:task) + (1|task) + (liwc+audio|model),
              REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000, xtol_abs=1e-8, ftol_abs=1e-8)))
#runs
stan6 <- lmer(score_z ~ personality + topic + liwc + value + task + audio + liwc:task + 
                (1|model:task) + (1|task) + (1|model), 
              REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000, xtol_abs=1e-8, ftol_abs=1e-8)))
#runs

#stan 5 without interaction
stan7 <- lmer(score_z ~ personality + topic + liwc + value + task + audio + 
                (1|model:task) + (1|task) + (liwc+audio|model),
              REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000, xtol_abs=1e-8, ftol_abs=1e-8)))
#runs

#stan 5 without task or interaction
stan8 <- stan7 <- lmer(score_z ~ personality + topic + liwc + value + audio + 
                         (1|model:task) + (1|task) + (liwc+audio|model),
                       REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000, xtol_abs=1e-8, ftol_abs=1e-8)))
#runs

#stan 6 without interaction
stan9 <- lmer(score_z ~ personality + topic + liwc + value + task + audio +
                (1|model:task) + (1|task) + (1|model), 
              REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000, xtol_abs=1e-8, ftol_abs=1e-8)))
#runs

#stan 6 without task or interaction
stan10 <- lmer(score_z ~ personality + topic + liwc + value + audio +
                 (1|model:task) + (1|task) + (1|model), 
               REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000, xtol_abs=1e-8, ftol_abs=1e-8)))
#runs 

#hypothesized model
stan11 <- lmer(score_z ~ linguistic + topic + liwc + value + task + audio + 
                 topic:liwc + topic:task + liwc:task + value:task + 
                 (model|task/model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 1000000,xtol_abs=1e-8, ftol_abs=1e-8)))
#doesn't converge

#without interactions
stan12 <- lmer(score_z ~ linguistic + topic +liwc + value + task + audio +
                 (model|task/model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 1000000,xtol_abs=1e-8, ftol_abs=1e-8)))
#doesn't converge

#hypothesized model #2
stan13 <- lmer(score_z ~ linguistic + topic + liwc + value + task + audio + 
                 topic:liwc + topic:task + liwc:task + value:task + 
                 (1|task/model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 1000000,xtol_abs=1e-8, ftol_abs=1e-8)))
#runs
#without interactions
stan14 <- lmer(score_z ~ linguistic + topic + liwc + value + task + audio + 
                 (1|task/model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 1000000,xtol_abs=1e-8, ftol_abs=1e-8)))
#runs

model_fits_stan =list(stan6, stan7, stan8, stan9, stan10, stan13, stan14)

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
save(all_params, file = "all_params_raw.rda")

#plots
top = all_params %>%
  filter(term=="value"|term=="liwc"|term=="audio"|term=="personality"|term=="topic"|term=="linguistic") %>%
  #filter(term=="value"|term=="liwc"|term=="audio") %>%
  ggplot(aes(model_num, estimate, color = term)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position="dodge") +
  geom_point(position=position_dodge(width=.9)) +
  scale_colour_manual(values = inferno(6, alpha = 1, begin = .1, end = .9, direction = 1), guide = FALSE) +
  scale_fill_manual(values = inferno(6, alpha = 1, begin=.1, end=.9, direction=1), guide=FALSE) +
  labs(x = "", y = "regression coefficient\n") + 
  theme_minimal(base_size = 11) +
  scale_x_continuous(breaks = seq(1, 10, by= 1)) +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        axis.text = element_text(color = "black"),
        axis.line = element_line(colour = "black"),
        #        legend.position = "none",
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
    select(AIC, BIC, everything()))

variable.names = names(select(models.sca, -model_num, -AIC, -BIC))

bottom = models.sca %>%
  gather(variable, value, eval(variable.names)) %>% 
  mutate(value = ifelse(!is.na(value), "|", "")) %>%
  ggplot(aes(model_num, variable)) +
  geom_text(aes(label = value)) +
  labs(x = "\nspecification number", y = "variables\n") + 
  theme_minimal(base_size = 11) +
  scale_x_continuous(breaks = seq(1, 10, by= 1)) +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 9),
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

cowplot::plot_grid(top, median_plot, ncol = 2, align = "h", axis="l", labels = c('A', 'B'))


eff_liwc_task <- effect("liwc*task", stan13, KR=T)
plot(eff_liwc_task, confint=list(col='red'))
plot_model(stan13, type="int", terms=c(liwc, score_z), ci.lvl=0.95)

eff_liwc_task_plot <- as.data.frame((eff_liwc_task))
eff_liwc_task_plot$task <- as.factor(eff_liwc_task_plot$task)


eff_liwc_task_plot %>%
  filter(liwc == 0.0|liwc == 1.0) %>%
  ggplot(aes(liwc,fit, color=task), group=task) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position="dodge") +
  geom_point(position=position_dodge(width=.9)) +
  theme()

eff_liwc_task_plot <- eff_liwc_task_plot %>%
  filter(liwc == 0.0|liwc == 1.0)
ggplot(data=eff_liwc_task_plot, aes(liwc, fit, color=task), group=task) +
  geom_line() 