#logit score
load("l0.1.rda")

#1st round of removal: model and task within slopes
l1.1 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (personality+linguistic+topic+liwc+value+task+audio+model|model:task) +
               (task+audio+model|task) +
               (personality+linguistic+topic+liwc+value+task+audio+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l1.1, file = "l1.1.rda")

#2nd round of removal: model:task: personality, value, task:audio 
l2.1 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (linguistic+topic+liwc+task+audio+model|model:task) +
               (task+model|task) +
               (personality+linguistic+topic+liwc+value+task+audio+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l2.1, file = "l2.1.rda")

#3rd round of removal: all within task
l3.1 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (linguistic+topic+liwc+task+audio+model|model:task) +
               (1|task) +
               (personality+linguistic+topic+liwc+value+task+audio+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l3.1, file = "l3.1.rda")

#4th round of removal: model:task: linguistic, audio
l4.1 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+task+model|model:task) +
               (1|task) +
               (personality+linguistic+topic+liwc+value+task+audio+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l4.1, file = "l4.1.rda")

#5th round of removal: model:task: topic, model: personality, linguistic, topic, value
l5.1 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc+task+model|model:task) +
               (1|task) +
               (liwc+value+task+audio+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l5.1, file = "l5.1.rda")

#6th round of removal: model:task: task, model: task
l6.1 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc+model|model:task) +
               (1|task) +
               (liwc+value+audio+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l6.1, file = "l6.1.rda")

#7th round: model: value
l7.1 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc+model|model:task) +
               (1|task) +
               (liwc+audio+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l7.1, file = "l7.1.rda")

#8th round: task: model
l8.1 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc|model:task) +
               (1|task) +
               (liwc+audio+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l8.1, file = "l8.1.rda")

#9th round: model: model
l9.1 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc|model:task) +
               (1|task) +
               (liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l9.1, file = "l9.1.rda")
#converges with boundary error

#10th round: model: model:task: liwc
l10.1 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (1|model:task) +
               (1|task) +
               (liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l10.1, file = "l10.1.rda")
#converges with boundary error, fits worse than l9.1

#fits the best
l11.1 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
                (liwc|model:task) +
                (1|task) +
                (audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l11.1, file = "l11.1.rda")
#converges with boundary error, fits about the same as l9.1

l12.1 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
                (liwc|model:task) +
                (1|task) +
                (1|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l12.1, file = "l12.1.rda")
#converges with boundary error, fits worse than 11.1

###
###

#with task, without model in slopes
l0.2 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (personality+linguistic+topic+liwc+value+task+audio|model:task) + 
               (personality+linguistic+topic+liwc+value+task+audio|task) + 
               (personality+linguistic+topic+liwc+value+task+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

save(l0.2, file = "l0.2.rda")

#1st round of removal: model: personality, topic, value
l1.2 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (personality+linguistic+topic+liwc+value+task+audio|model:task) + 
               (personality+linguistic+topic+liwc+value+task+audio|task) + 
               (linguistic+liwc+task+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

save(l1.2, file = "l1.2.rda")

#2nd round of removal: all slopes within task
l2.2 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (personality+linguistic+topic+liwc+value+task+audio|model:task) + 
               (1|task) + 
               (linguistic+liwc+task+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

save(l2.2, file = "l2.2.rda")

#3rd round of removal: model:task: personality, linguistic
l3.2 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+value+task+audio|model:task) + 
               (1|task) + 
               (linguistic+liwc+task+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l3.2, file = "l3.2.rda")

#4th round of removal: model:task: topic, value, model: linguistic
l4.2 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc+task+audio|model:task) + 
               (1|task) + 
               (liwc+task+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l4.2, file = "l4.2.rda")

#5th round of removal: model:task: audio
l5.2 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc+task|model:task) + 
               (1|task) + 
               (liwc+task+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l5.2, file = "l5.2.rda")

#6th round of removal: model:task: liwc, model:liwc, audio
l6.2 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (task|model:task) + 
               (1|task) + 
               (task|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l6.2, file = "l6.2.rda")
#worse fit than l5.2

#return liwc, remove task:task 
l7.2 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
                (liwc|model:task) + 
                (1|task) + 
                (liwc+task+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l7.2, file = "l7.2.rda")

#return liwc, remove model:task  #best fit
l8.2 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc|model:task) + 
               (1|task) + 
               (liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l8.2, file = "l8.2.rda")

#return liwc, remove model:task 
l9.2 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc|model:task) + 
               (1|task) + 
               (liwc|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l9.2, file = "l9.2.rda")
#l8.2 fits better

###
###


#with model, without task in the slopes
l0.3 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               + (personality+linguistic+topic+liwc+value+model+audio|model:task) + 
               + (personality+linguistic+topic+liwc+value+model+audio|task) + 
               + (personality+linguistic+topic+liwc+value+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l0.3, file = "l0.3.rda")

#1st round of removal: model:task: personality, task: personality, topic, value
l1.3 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               + (linguistic+topic+liwc+value+model+audio|model:task) + 
               + (personality+linguistic+topic+liwc+value+model+audio|task) + 
               + (linguistic+liwc+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l1.3, file = "l1.3.rda")


#2nd round of removal: task:personality, topic, value, linguistic, audio
l2.3 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               + (linguistic+topic+liwc+value+model+audio|model:task) + 
               + (liwc+model|task) + 
               + (linguistic+liwc+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l2.3, file = "l2.3.rda")

#3rd round of removal: task: model, liwc
l3.3 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               + (linguistic+topic+liwc+value+model+audio|model:task) + 
               + (1|task) + 
               + (linguistic+liwc+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l3.3, file = "l3.3.rda")

#4th round of removal: model:task: linguistic, topic, value
l4.3 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc+model+audio|model:task) + 
               (1|task) + 
               (linguistic+liwc+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l4.3, file = "l4.3.rda")

#5th round of removal: model:linguistic
l5.3 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
              (liwc+model+audio|model:task) + 
              (1|task) + 
              (liwc+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l5.3, file = "l5.3.rda")

#6th round of removal: model:linguistic
l6.3 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc+model|model:task) + 
               (1|task) + 
               (liwc+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l6.3, file = "l6.3.rda")


#7th round of removal: liwc
l7.3 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (model|model:task) + 
               (1|task) + 
               (model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l7.3, file = "l7.3.rda")
#fits worse than l6.3

#8th round: model:task, model
l8.3 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc|model:task) + 
               (1|task) + 
               (liwc+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l8.3, file = "l8.3.rda")

#9th round: model:audio
l9.3 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc|model:task) + 
               (1|task) + 
               (liwc+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l9.3, file = "l9.3.rda")
#worse fit than l8.3

#10th round: model:audio ##best fit
l10.3 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc|model:task) + 
               (1|task) + 
               (liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l10.3, file = "l10.3.rda")

###
###


#without without task or model in the slopes
l0.4 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               + (personality+linguistic+topic+liwc+value+audio|model:task) + 
               + (personality+linguistic+topic+liwc+value+audio|task) + 
               + (personality+linguistic+topic+liwc+value+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l0.4, file = "l0.4.rda")

#1st round of removal: task: personality, value, topic
l1.4 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               + (personality+linguistic+topic+liwc+value+audio|model:task) + 
               + (personality+linguistic+topic+liwc+value+audio|task) + 
               + (linguistic+liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l1.4, file = "l1.4.rda")

#2nd round of removal: task:remaining feature sets
l2.4 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               + (personality+linguistic+topic+liwc+value+audio|model:task) + 
               + (1|task) + 
               + (linguistic+liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l2.4, file = "l2.4.rda")

#3rd round of removal:model:task, personality, linguistic
l3.4 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+value+audio|model:task) + 
               (1|task) + 
               (linguistic+liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l3.4, file = "l3.4.rda")
#converged

#4th round of removal: model:task: topic, value, audio, model:, linguistic
l4.4 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc|model:task) + 
               (1|task) + 
               (liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l4.4, file = "l4.4.rda")
#fits worse than l3.4

#5th round of removal:model:task: value
l5.4 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+audio|model:task) + 
               (1|task) + 
               (linguistic+liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l5.4, file = "l5.4.rda")

#6th round of removal:model:task: topic
l6.4 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc+audio|model:task) + 
               (1|task) + 
               (linguistic+liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l6.4, file = "l6.4.rda")
#fits worse than l5.4

#7th round of removal: model:linguistic
l7.4 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+audio|model:task) + 
               (1|task) + 
               (liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l7.4, file = "l7.4.rda")
#fits worse than l5.4

#8th round: model:task: audio ##best fit
l8.4 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc|model:task) + 
               (1|task) + 
               (linguistic+liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l8.4, file = "l8.4.rda")

#9th round: model: audio
l9.4 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc|model:task) + 
               (1|task) + 
               (linguistic+liwc|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l9.4, file = "l9.4.rda")
#fits worse than l8.4


###

#Best fits:

anova(l8.4, l10.3, l8.2, l11.1)
#l8.4 is overall best fit

#8th round: model:task: audio ##best fit
l8.4 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc|model:task) + 
               (1|task) + 
               (linguistic+liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l8.4, file = "l8.4.rda")

#10th round: model:audio ##best fit
l10.3 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
                (liwc|model:task) + 
                (1|task) + 
                (liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l10.3, file = "l10.3.rda")

#return liwc, remove model:task  #best fit
l8.2 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc|model:task) + 
               (1|task) + 
               (liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l8.2, file = "l8.2.rda")

#fits the best
l11.1 <- lmer(score_log ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
                (liwc|model:task) +
                (1|task) +
                (audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(l11.1, file = "l11.1.rda")
#converges with boundary error, fits about the same as l9.1