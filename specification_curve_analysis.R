if (!require(tidyverse)) {
  install.packages('tidyverse')
}
if (!require(purrr)) {
  install.packages('purrr')
}
if (!require(broom)) {
  install.packages('broom')
}
if (!require(cowplot)) {
  install.packages('cowplot')
}
if (!require(sjPlot)) {
  install.packages('sjPlot')
}
if (!require(specr)) {
  install.packages('specr')
}


library(tidyverse)
library(purrr)
library(broom.mixed)
library(sjPlot)
library(cowplot)
library(specr)
library(MuMIn)


m8.4 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (audio|model:task) + 
               (audio|task) + 
               (topic+liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

m8.3 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (audio|model:task) + 
               (audio|task) + 
               (linguistic+topic+liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

m7.2 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc+audio|model:task) + 
               (audio|task) + 
               (linguistic+topic+liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

m8.1 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (1|model:task) + 
               (1|task) + 
               (liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))


# attempt 1
# specify models
models =list(score ~ (1|task/model), 
             score_z ~ personality + topic + liwc + value + task + audio + liwc:task + value:task + (1|task/model),
             score_z ~ personality + topic + liwc + value + task + audio + liwc:task + value:task + (audio|model:task) + (audio|task) + (topic+liwc+audio|model),
             score_z ~ personality + topic + liwc + value + task + audio + liwc:task + value:task + (audio|model:task) + (audio|task) + (linguistic+topic+liwc+audio|model),
             score_z ~ personality + topic + liwc + value + task + audio + liwc:task + value:task + (liwc+audio|model:task) + (audio|task) + (linguistic+topic+liwc+audio|model),
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

model_params <- model_params %>% 
  filter(effect=="fixed")

# run models and extract model fits
model_fits = purrr::map(models, ~lmer(.x, REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))) %>%
  tibble() %>%
  rename("model" = ".") %>%
  mutate(model_num = row_number(),
         AIC = map_dbl(model, AIC),
         BIC = map_dbl(model, BIC)) %>%
  select(-model)

# join dataframes and select model fits and parameter estimates
(models.sca = model_params %>%
    select(model_num, term, estimate) %>%
    spread(term, estimate) %>%
    left_join(., model_fits) %>%
    arrange(AIC)%>%
    select(AIC, BIC, everything()))

# specify null model
null.df = models.sca %>% 
  filter(model_num == 1)

# tidy for plotting
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
  scale_color_manual(values = c("lightblue","black", "red")) +
  labs(x = "", y = "AIC\n") + 
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

# plot bottom panel
bottom = plot.data %>%
  gather(variable, value, eval(variable.names)) %>% 
  mutate(value = ifelse(!is.na(value), "|", "")) %>%
  ggplot(aes(specification, variable, color = better.fit)) +
  geom_text(aes(label = value)) +
  scale_color_manual(values = c("lightblue", "black", "red")) +
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

# join panels
cowplot::plot_grid(top, bottom, ncol = 1, align = "v", labels = c('A', 'B'))

#full.model = lmer(score_z ~ personality + topic + liwc + value + task + audio + liwc:task + value:task + liwc:value + liwc:value:task + (1|task/model), data=test_df)
#models.1 = MuMIn::dredge(full.model, rank = "AIC", extra = "BIC")

