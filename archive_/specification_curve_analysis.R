load.lib <- c("tidyverse", "purrr", "broom.mixed", "sjPlot", "lme4", "lmerTest","cowplot", "MuMIn", "here")

install.lib<-load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib,require,character=TRUE)


library(tidyverse)
library(purrr)
library(broom.mixed)
library(sjPlot)
library(cowplot)
library(MuMIn)
library(lme4)
library(lmerTest)
library(here)

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

#log
test_df <-
  test_df %>%
  mutate(score_log = logit(score))


#temp <- confint(mod, method="boot", boot.type = "perc", nsim=100)
#remp <- lmer(score_z ~ personality + topic + liwc + (1|task/model), REML=FALSE, data=test_df)

#specify models
models =list(score_z ~ (1|task/model), 
             score_z ~ personality + topic + liwc + value + task + audio + liwc:task + value:task + (1|task/model),
             score_z ~ personality + topic + liwc + value + task + audio + liwc:task + value:task + (audio|task:model) + (audio|task) + (topic+liwc+audio|model),
             score_z ~ personality + topic + liwc + value + task + audio + liwc:task + value:task + (audio|task:model) + (audio|task) + (linguistic+topic+liwc+audio|model),
             score_z ~ personality + topic + liwc + value + task + audio + liwc:task + value:task + (liwc+audio|task:model) + (audio|task) + (linguistic+topic+liwc+audio|model),
             score_z ~ personality + topic + liwc + value + task + audio + liwc:task + value:task + (1|model:task) + (1|task) + (liwc+audio|model)
)


# run models and extract parameter estimates and stats
model_params = map(models, ~lmer(.x, REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))) %>%
  tibble() %>%
  rename("model" = ".") %>%
  mutate(tidied = purrr::map(model, broom::tidy),
         model_num = row_number()) %>%
  select(model_num, tidied) %>%
  unnest()
  
#model_params <- model_params %>% 
#  filter(effect=="fixed")

fixed_params <- model_params %>% 
  filter(effect=="fixed")

ran_pars <- model_params %>% 
  filter(effect=="ran_pars")

ran_pars$term <- paste(ran_pars$group, "_", ran_pars$term)

all_params <- rbind(ran_pars, fixed_params)


# run models and extract model fits
model_fits = purrr::map(models, ~lmer(.x, REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))) %>%
  tibble() %>%
  rename("model" = ".") %>%
  mutate(model_num = row_number(),
         AIC = map_dbl(model, AIC),
         BIC = map_dbl(model, BIC)) %>%
  select(-model)

# join dataframes and select model fits and parameter estimates
(models.sca = all_params %>%
    select(model_num, term, estimate) %>%
    spread(term, estimate) %>%
    left_join(., model_fits) %>%
    arrange(AIC)%>%
    select(AIC, BIC, everything()))

# specify null model
null.df = models.sca %>% 
  filter(model_num == 1)

# tidy for plotting by AIC
plot.data = models.sca %>%
  arrange(AIC) %>%
  mutate(specification = row_number(),
         better.fit = ifelse(AIC == null.df$AIC, "equal", 
                             ifelse(AIC < null.df$AIC, "yes","no")))


# get names of variables included in model
variable.names = names(select(plot.data, -model_num, -starts_with("better"), -specification, -AIC, -BIC))

# plot top panel
top = plot.data %>%
  ggplot(aes(specification, AIC, color = better.fit)) +
  geom_point(shape = "|", size = 4) +
  geom_hline(yintercept = null.df$AIC, linetype = "dashed", color = "lightblue")+
#  scale_color_manual(values = c("lightblue","black", "red")) +
  labs(x = "", y = "AIC\n") + 
  theme_dark(base_size = 11) +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        axis.text = element_text(color = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

# plot bottom panel
bottom = plot.data %>%
  gather(variable, value, eval(variable.names)) %>% 
  mutate(value = ifelse(!is.na(value), "|", "")) %>%
  ggplot(aes(specification, variable, color = better.fit)) +
  geom_text(aes(label = value)) +
  scale_color_manual(values = c("lightblue", "black", "red")) +
  labs(x = "\nspecification number", y = "variables\n") + 
  theme_dark(base_size = 8) +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 9),
        axis.text = element_text(color = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
)

# join panels
cowplot::plot_grid(top, bottom, ncol = 1, align = "v", labels = c('A', 'B'))

#full.model = lmer(score_z ~ personality + topic + liwc + value + task + audio + liwc:task + value:task + liwc:value + liwc:value:task + (1|task/model), data=test_df)
#models.1 = MuMIn::dredge(full.model, rank = "AIC", extra = "BIC")

#plot by parameter estimate
plot.params = all_params %>%
  arrange(estimate) %>%
  mutate(specification = row_number(),
         significant.p = ifelse(p.value < .05, "yes", "no")) %>%
  gather(variable, value, -estimate, -specification, -model_num, -std.error, -p.value, -significant.p) %>% 
  mutate(variable = gsub("[()]", "", variable),
         variable = gsub("Intercept", "intercept", variable),
         variable = gsub("as.factor(vs)1", "vs", variable)) %>%
  spread(variable, value)  


#get variable names 
variable.names = names(select(plot.params, -estimate, -specification, -model_num, -std.error, -p.value, -significant.p))

top = plot.params %>%
  ggplot(aes(specification, estimate, color = significant.p)) +
  geom_point(shape = "|", size = 4) +
  #geom_hline(yintercept = null.df$AIC, linetype = "dashed", color = "lightblue") +
  scale_color_manual(values = c("black", "red")) +
  labs(x = "", y = "regression coefficient\n") + 
  theme_minimal(base_size = 11) +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        axis.text = element_text(color = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

order = plot.params %>%
  arrange(estimate) %>%
  mutate(significant.p.num = ifelse(significant.p == "yes", 1, 0)) %>%
  gather(variable, value, eval(variable.names)) %>% 
  filter(!is.na(value)) %>%
  group_by(variable) %>%
  mutate(order = sum(significant.p.num)) %>%
  select(variable, order) %>%
  unique()

bottom = plot.params %>%
  gather(variable, value, eval(variable.names)) %>% 
  mutate(value = ifelse(!is.na(value), "|", ""),
         variable = ifelse(variable == "(Intercept)", "intercept",
                           ifelse(variable == "as.factor(vs)1", "vs", variable))) %>%
  left_join(., order, by = "variable") %>%
  ggplot(aes(specification, reorder(variable, order), color = significant.p)) +
  geom_text(aes(label = value)) +
  scale_color_manual(values = c("black", "red")) +
  labs(x = "\nspecification number", y = "variables\n") + 
  theme_minimal(base_size = 11) +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        axis.text = element_text(color = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

(wt = cowplot::plot_grid(top, bottom, ncol = 1, align = "v", labels = c('A', 'B')))
