#standardized scores

#maximal model
m0.1 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (personality+linguistic+topic+liwc+value+task+audio+model|task/model) + 
               (personality+linguistic+topic+liwc+value+task+audio+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n0.1 <- m0.1
save(n0.1, file = "n0.1.rda")

#1st round: model:task: personality, model: personality, task: personality
m1.1 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (linguistic+topic+liwc+value+task+audio+model|model:task) + 
               (linguistic+topic+liwc+value+task+audio|task) + 
               (linguistic+topic+liwc+value+task+audio+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n1.1 <- m1.1
save(n1.1, file = "n1.1.rda")

#2nd round: task: linguistic, topic, liwc, value, task
m2.1 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (linguistic+topic+liwc+value+task+audio+model|model:task) + 
               (audio|task) + 
               (linguistic+topic+liwc+value+task+audio+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n2.1 <- m2.1
save(n2.1, file = "n2.1.rda")

#3rd round: model:task: linguistic, task, model: value
m3.1 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+value+audio+model|model:task) + 
               (audio|task) + 
               (linguistic+topic+liwc+task+audio+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n3.1 <- m3.1
save(n3.1, file = "n3.1.rda")

#4th round: model:task: topic, liwc, value, audio, model, model:linguistic, topic
m4.1 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (1|model:task) + 
               (audio|task) + 
               (liwc+task+audio+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n4.1 <- m4.1
save(n4.1, file = "n4.1.rda")

#5th round: task: audio, model: liwc, model
m5.1 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (1|model:task) + 
               (audio|task) + 
               (task+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n5.1 <- m5.1
save(n5.1, file = "n5.1.rda")
#this one fits worse than m4.1

#6th round, task:audio from m4.1 instead
m6.1 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (1|model:task) + 
               (1|task) + 
               (liwc+task+audio+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n6.1 <- m6.1
save(n6.1, file = "n6.1.rda")

#7th round, model:task
m7.1 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (1|model:task) + 
               (1|task) + 
               (liwc+audio+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n7.1 <- m7.1
save(n7.1, file = "n7.1.rda")

#8th round, model:model: fits the best
m8.1 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (1|model:task) + 
               (1|task) + 
               (liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n8.1 <- m8.1
save(n8.1, file = "n8.1.rda")

#9th round, model:liwc
m9.1 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (1|model:task) + 
               (1|task) + 
               (audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n9.1 <- m9.1
save(n9.1, file = "n9.1.rda")
#converges with boundary error, fits worse than m8.1

#10th round, replace audio with liwc
m10.1 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (1|model:task) + 
               (1|task) + 
               (liwc|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n10.1 <- m10.1
save(n10.1, file = "n10.1.rda")
#converges with boundary error, also fits worse than m8.1

m11.1 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
                (1|model:task) + 
                (1|task) + 
                (1|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n11.1 <- m11.1
save(n11.1, file = "n11.1.rda")
#converges with boundary error, also fits worse than m8.1

###
###

#with task, without model in the slopes
m0.2 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (personality+linguistic+topic+liwc+value+task+audio|model:task) + 
               (personality+linguistic+topic+liwc+value+task+audio|task) + 
               (personality+linguistic+topic+liwc+value+task+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n0.2 <- m0.2
save(n0.2, file = "n0.2.rda")

#1st round: model:task: personality, model: personality, linguistic, task: personality 
m1.2 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (linguistic+topic+liwc+value+task+audio|model:task) + 
               (topic+liwc+value+task+audio|task) + 
               (linguistic+topic+liwc+value+task+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n1.2 <- m1.2
save(n1.2, file = "n1.2.rda")

#2nd round: model:task: linguistic, model: value, linguistic
m2.2 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+value+task+audio|model:task) + 
               (topic+liwc+value+task+audio|task) + 
               (linguistic+topic+liwc+task+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n2.2 <- m2.2
save(n2.2, file = "n2.2.rda")


#3rd round: task: topic, liwc,
m3.2 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+value+task+audio|model:task) + 
               (task+audio|task) + 
               (linguistic+topic+liwc+task+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n3.2 <- m3.2
save(n3.2, file = "n3.2.rda")


#4th round: task:task
m4.2 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+value+task+audio|model:task) + 
               (audio|task) + 
               (linguistic+topic+liwc+task+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n4.2 <- m4.2
save(n4.2, file = "n4.2.rda")


#5th round, model:task: task
m5.2 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+value+audio|model:task) + 
               (audio|task) + 
               (linguistic+topic+liwc+task+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n5.2 <- m5.2
save(n5.2, file = "n5.2.rda")

#6th round, task:task
m6.2 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+value+audio|model:task) + 
               (audio|task) + 
               (linguistic+topic+liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n6.2 <- m6.2
save(n6.2, file = "n6.2.rda")

#fits worse than 5.2

#7th round, task:topic, value #best fit
m7.2 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc+audio|model:task) + 
               (audio|task) + 
               (linguistic+topic+liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n7.2 <- m7.2
save(n7.2, file = "n7.2.rda")

#8th round, task: audio, model:audio
m8.2 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc|model:task) + 
               (audio|task) + 
               (linguistic+topic+liwc|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n8.2 <- m8.2
save(n8.2, file = "n8.2.rda")

#fits worse than 7.2

#9th round, task:audio
m9.2 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc+audio|model:task) + 
               (1|task) + 
               (linguistic+topic+liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n9.2 <- m9.2
save(n9.2, file = "n9.2.rda")

#7.2 fits better

#10th round, model:linguistic, topic
m10.2 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc+audio|model:task) + 
               (1|task) + 
               (liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n10.2 <- m10.2
save(n10.2, file = "n10.2.rda")

#7.2 fits better

#11th round, model:linguistic, topic
m11.2 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
                (liwc+audio|model:task) + 
                (1|task) + 
                (topic+liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n11.2 <- m11.2
save(n11.2, file = "n11.2.rda")
#7.2fits better

###
###

#with model, without task in the slopes
m0.3 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               + (personality+linguistic+topic+liwc+value+model+audio|model:task) + 
               + (personality+linguistic+topic+liwc+value+model+audio|task) + 
               + (personality+linguistic+topic+liwc+value+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

n0.3 <- m0.3
save(n0.3, file = "n0.3.rda")

#1st round: model:task: personality, task: personality 
m1.3 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (linguistic+topic+liwc+value+model+audio|model:task) + 
               (personality+linguistic+topic+liwc+value+model+audio|task) + 
               (linguistic+topic+liwc+value+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n1.3 <- m1.3
save(n1.3, file = "n1.3.rda")

#2nd round: task: personality, linguistic, topic, liwc, value
m2.3 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (linguistic+topic+liwc+value+model+audio|model:task) + 
               (model+audio|task) + 
               (linguistic+topic+liwc+value+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n2.3 <- m2.3
save(n2.3, file = "n2.3.rda")

#3rd round: task: model, model: value, model:task: linguistic
m3.3 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+value+model+audio|model:task) + 
               (audio|task) + 
               (linguistic+topic+liwc+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n3.3 <- m3.3
save(n3.3, file = "n3.3.rda")

#4th round: model:task: model
m4.3 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+value+audio|model:task) + 
               (audio|task) + 
               (linguistic+topic+liwc+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n4.3 <- m4.3
save(n4.3, file = "n4.3.rda")

#5th round: model:task: liwc
m5.3 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+value+audio|model:task) + 
               (audio|task) + 
               (linguistic+topic+liwc+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n5.3 <- m5.3
save(n5.3, file = "n5.3.rda")

#6th round: model:task: topic, value, model:linguistic, topic
m6.3 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (audio|model:task) + 
               (audio|task) + 
               (linguistic+topic+liwc+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n6.3 <- m6.3
save(n6.3, file = "n6.3.rda")
#5.3 fits a tiny bit better

#7th round: model:task: topic, value, model:linguistic, topic
m7.3 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (audio|model:task) + 
               (audio|task) + 
               (topic+liwc+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n7.3 <- m7.3
save(n7.3, file = "n7.3.rda")
#5.3 fits better

#8th round: replace linguistic, remove model #fits best
m8.3 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (audio|model:task) + 
               (audio|task) + 
               (linguistic+topic+liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n8.3 <- m8.3
save(n8.3, file = "n8.3.rda")

#9th round: model:task:audio
m9.3 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (1|model:task) + 
               (audio|task) + 
               (linguistic+topic+liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n9.3 <- m9.3
save(n9.3, file = "n9.3.rda")
#fits worse than 8.3

#10th round: try removing task:audio
m10.3 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (audio|model:task) + 
               (1|task) + 
               (linguistic+topic+liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n10.3 <- m10.3
save(n10.3, file = "n10.3.rda")
#fits woorse than m8.3

m11.3 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
                (audio|model:task) + 
                (audio|task) + 
                (liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n11.3 <- m11.3
save(n11.3, file = "n11.3.rda")
#fits worse than m8.3

###
###

#without task or model
m0.4 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               + (personality+linguistic+topic+liwc+value+audio|model:task) + 
               + (personality+linguistic+topic+liwc+value+audio|task) + 
               + (personality+linguistic+topic+liwc+value+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n0.4 <- m0.4
save(n0.4, file = "n0.4.rda")

#1st round: model: linguistic, task: personality, value 
m1.4 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (linguistic+topic+liwc+value+model+audio|model:task) + 
               (linguistic+topic+liwc+model+audio|task) + 
               (linguistic+topic+liwc+value+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n1.4 <- m1.4
save(n1.4, file = "n1.4.rda")


#2nd round: model:task: linguistic, value, model: value 
m2.4 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+model+audio|model:task) + 
               (linguistic+topic+liwc+model+audio|task) + 
               (linguistic+topic+liwc+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n2.4 <- m2.4
save(n2.4, file = "n2.4.rda")

#3rd round: model:task: topic, liwc, model: linguistic, topic, task:topic
m3.4 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (model+audio|model:task) + 
               (linguistic+liwc+model+audio|task) + 
               (topic+liwc+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n3.4 <- m3.4
save(n3.4, file = "n3.4.rda")

#4th round: model:task: model
m4.4 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (audio|model:task) + 
               (linguistic+liwc+model+audio|task) + 
               (topic+liwc+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n4.4 <- m4.4
save(n4.4, file = "n4.4.rda")

#5th round: task:linguistic
m5.4 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (audio|model:task) + 
               (liwc+model+audio|task) + 
               (topic+liwc+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n5.4 <- m5.4
save(n5.4, file = "n5.4.rda")

#6th round: task:liwc
m6.4 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (audio|model:task) + 
               (model+audio|task) + 
               (topic+liwc+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n6.4 <- m6.4
save(n6.4, file = "n6.4.rda")

#7th round: task:model
m7.4 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (audio|model:task) + 
               (audio|task) + 
               (topic+liwc+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n7.4 <- m7.4
save(n7.4, file = "n7.4.rda")

#8th round: model:model
m8.4 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (audio|model:task) + 
               (audio|task) + 
               (topic+liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n8.4 <- m8.4
save(n8.4, file = "n8.4.rda")

#9th round: model:topic
m9.4 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (audio|model:task) + 
               (audio|task) + 
               (liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n9.4 <- m9.4
save(n9.4, file = "n9.4.rda")
#8.4 fits better

#10th round: model:audio
m10.4 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (audio|model:task) + 
               (audio|task) + 
               (topic+liwc|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n10.4 <- m10.4
save(n10.4, file = "n10.4.rda")
#8.4 fits better

#11th round: task:audio
m11.4 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
                (audio|model:task) + 
                (1|task) + 
                (topic+audio+liwc|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n11.4 <- m11.4
save(n11.4, file = "n11.4.rda")
#8.4 fits better

#12th round: model:liwc
m12.4 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (audio|model:task) + 
               (audio|task) + 
               (topic+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
n12.4 <- m12.4
save(n12.4, file = "n12.4.rda")
#8.4 fits better

###
###

#best fits
anova(n8.4, n8.3, n7.2, n8.1)


n8.4 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (audio|model:task) + 
               (audio|task) + 
               (topic+liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

n8.3 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (audio|model:task) + 
               (audio|task) + 
               (linguistic+topic+liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

n7.2 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc+audio|model:task) + 
               (audio|task) + 
               (linguistic+topic+liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

n8.1 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (1|model:task) + 
               (1|task) + 
               (liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))



