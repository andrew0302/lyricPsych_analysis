library(tidyverse)
library(purrr)
library(broom.mixed)
library(sjPlot)
library(cowplot)
library(lme4)
library(lmerTest)
library(here)
library(Hmisc)

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


#specify models
models_stan =list(
  #with interaction
  score_z ~ personality + topic + liwc + value + task + audio + liwc:task + (audio|task/model) + (linguistic+topic+liwc+audio|model),
  score_z ~ personality + topic + liwc + value + task + audio + liwc:task + (audio|model:task) + (audio|task) + (topic+liwc+audio|model),
  score_z ~ personality + topic + liwc + value + task + audio + liwc:task + (audio|model:task) + (audio|task) + (linguistic+topic+liwc+audio|model),
  score_z ~ personality + topic + liwc + value + task + audio + liwc:task + (liwc+audio|model:task) + (audio|task) + (linguistic+topic+liwc+audio|model),
  score_z ~ personality + topic + liwc + value + task + audio + liwc:task + (1|model:task) + (1|task) + (liwc+audio|model)
)

models_raw  =list(score ~ personality + topic + liwc + value + task + audio + liwc:task + value:task + (1|task/model),
                   score ~ personality + topic + liwc + value + task + audio + liwc:task + value:task + (audio|task:model) + (audio|task) + (topic+liwc+audio|model),
                   score ~ personality + topic + liwc + value + task + audio + liwc:task + value:task + (audio|task:model) + (audio|task) + (linguistic+topic+liwc+audio|model),
                   score ~ personality + topic + liwc + value + task + audio + liwc:task + value:task + (liwc+audio|task:model) + (audio|task) + (linguistic+topic+liwc+audio|model),
                   score ~ personality + topic + liwc + value + task + audio + liwc:task + value:task + (1|model:task) + (1|task) + (liwc+audio|model)
)


# run models and extract parameter estimates and stats
model_params_stan = map(models_stan, ~lmer(.x, REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))) %>%
  tibble() %>%
  rename("model" = ".") %>%
  mutate(tidied = purrr::map(model, broom::tidy),
         model_num = row_number()) %>%
  select(model_num, tidied) %>%
  unnest()
model_params_stan$dv = "standardized"

model_params_raw = map(models_raw, ~lmer(.x, REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))) %>%
  tibble() %>%
  rename("model" = ".") %>%
  mutate(tidied = purrr::map(model, broom::tidy),
         model_num = length(models_stan)+ row_number()) %>%
  select(model_num, tidied) %>%
  unnest()
model_params_raw$dv = "raw"

#merge dfs
model_params = rbind(model_params_raw, model_params_stan)

#create df with fixed and random parameter estimates
fixed_params <- model_params %>% 
  filter(effect=="fixed")
ran_params <- model_params %>% 
  filter(effect=="ran_pars")
ran_params$term <- paste(ran_params$group, "_", ran_params$term)
all_params <- rbind(ran_params, fixed_params)


top = all_params %>%
#  filter(term=="value"|term=="liwc"|term=="audio"|term=="personality"|term=="topic"|term=="linguistic") %>%
  filter(term=="value"|term=="liwc"|term=="audio") %>%
  ggplot(aes(model_num, estimate, color = term)) +
  geom_point(shape = "|", size = 4) +
  labs(x = "", y = "regression coefficient\n") + 
  theme_minimal(base_size = 11) +
  scale_x_continuous(breaks = seq(1, 10, by= 1)) +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        axis.text = element_text(color = "black"),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
top


model_fits_stan = purrr::map(models_stan, ~lmer(.x, REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))) %>%
  tibble() %>%
  rename("model" = ".") %>%
  mutate(model_num = row_number(),
         AIC = map_dbl(model, AIC),
         BIC = map_dbl(model, BIC)) %>%
  select(-model)

model_fits_raw = purrr::map(models_raw, ~lmer(.x, REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))) %>%
  tibble() %>%
  rename("model" = ".") %>%
  mutate(model_num = (length(models_stan)+ row_number()),
         AIC = map_dbl(model, AIC),
         BIC = map_dbl(model, BIC)) %>%
  select(-model)

model_fits_all <- rbind(model_fits_stan, model_fits_raw)

# join dataframes and select model fits and parameter estimates
(models.sca = all_params %>%
    select(model_num, term, estimate) %>%
    spread(term, estimate) %>%
    left_join(., model_fits_all) %>%
    arrange(AIC)%>%
    select(AIC, BIC, everything()))

#models.sca$dv <- as.factor(models.sca$dv)

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


#median bar plot
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

median_plot <- medians %>% 
  filter(Group.1 %in% c("Audio", "Values", "Personality", "Topics", "Linguistic", "LIWC")) %>%
  arrange(x)
median_plot$group_factor <- factor(median_plot$Group.1, ordered = TRUE)
median_plot$group_factor <- factor(median_plot$group_factor, levels(median_plot$group_factor)[c(2,5,3,4,1)])
bar1 <- ggplot(data = median_plot, aes(x = group_factor, y = x, fill = group_factor)) +
  coord_flip() +
  geom_bar(stat="identity") + 
  scale_x_discrete(position = "top") +
  labs(x = "Feature Set", y = "Median Parameter Estimate") +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 9),
        axis.text = element_text(color = "black"),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()
  )        
bar1




