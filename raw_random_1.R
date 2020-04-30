#maximal model with raw score
#maximal model
r0.1 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (personality+linguistic+topic+liwc+value+task+audio+model|task/model) + 
               (personality+linguistic+topic+liwc+value+task+audio+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r0.1, file = "r0.1.rda")

#1st round: task:model: linguistic, task: personality, linguistic, topic
r1.0 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (personality+topic+liwc+value+task+audio+model|task:model) +   
               (liwc+task+audio+model|task) + 
               (personality+linguistic+topic+liwc+value+task+audio+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
r1.1 <- r1.0
save(r1.1, file = "r1.1.rda")

#2nd round: task:model: personality, task: liwc, model: personality, linguistic
r2.0 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+value+task+audio+model|task:model) +   
               (task+audio+model|task) + 
               (liwc+value+task+audio+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 1000000)))
r2.1 <- r2.0
save(r2.1, file = "r2.1.rda")

#3rd round: model:task
r3.1 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+value+task+audio+model|task:model) +   
               (task+audio+model|task) + 
               (liwc+value+audio+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 1000000)))
save(r3.1, file = "r3.1.rda")

#4th round: task:task
r4.1 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+value+task+audio+model|task:model) +   
               (audio+model|task) + 
               (liwc+value+audio+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 1000000)))
save(r4.1, file = "r4.1.rda")

#5th round: task:model: topic, value, model:value
r5.1 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc+task+audio+model|task:model) +   
               (audio+model|task) + 
               (liwc+audio+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 1000000)))
save(r5.1, file = "r5.1.rda")

#6th round: task:model: task
r6.1 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc+audio+model|task:model) +   
               (audio+model|task) + 
               (liwc+audio+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 1000000)))
save(r6.1, file = "r6.1.rda")

#7th round: task:model
r7.1 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc+audio+model|task:model) +   
               (audio|task) + 
               (liwc+audio+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 1000000)))
save(r7.1, file = "r7.1.rda")

#8th round: task:model: model
r8.1 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc+audio|task:model) +   
               (audio|task) + 
               (liwc+audio+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 1000000)))
save(r8.1, file = "r8.1.rda")

#9th round: task:model: model
r9.1 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc+audio|task:model) +   
               (audio|task) + 
               (liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 1000000)))
save(r9.1, file = "r9.1.rda")

r10.1 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (1|task:model) +   
               (audio|task) + 
               (liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 1000000)))
save(r10.1, file = "r10.1.rda")
#converges iwth boundary error 

#with task, without model in the slopes
r0.2 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (personality+linguistic+topic+liwc+value+task+audio|model:task) + 
               (personality+linguistic+topic+liwc+value+task+audio|task) + 
               (personality+linguistic+topic+liwc+value+task+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r0.2, file = "r0.2.rda")

#round 1: model:task: personality, model:personality, linguistic, task: personality
r1.2 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (linguistic+topic+liwc+value+task+audio|model:task) + 
               (linguistic+topic+liwc+value+task+audio|task) + 
               (topic+liwc+value+task+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r1.2, file = "r1.2.rda")

#round 2: model: topic, task: topic, linguistic, value
r2.2 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (linguistic+topic+liwc+value+task+audio|model:task) + 
               (liwc+task+audio|task) + 
               (liwc+value+task+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r2.2, file = "r2.2.rda")

#round 3: task:task
r3.2 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (linguistic+topic+liwc+value+task+audio|model:task) + 
               (liwc+audio|task) + 
               (liwc+value+task+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r3.2, file = "r3.2.rda")

#round 4: model:task: linguistic
r4.2 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+value+task+audio|model:task) + 
               (liwc+audio|task) + 
               (liwc+value+task+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r4.2, file = "r4.2.rda")

#round 5: task:liwc
r5.2 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+value+task+audio|model:task) + 
               (audio|task) + 
               (liwc+value+task+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r5.2, file = "r5.2.rda")

#round 6: model:task:task
r6.2 <- lme4::lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+value+audio|model:task) + 
               (audio|task) + 
               (liwc+value+task+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r6.2, file = "r6.2.rda")

#round 7: model:task: topic, value, model: value
r7.2 <- lme4::lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
                     (liwc+audio|model:task) + 
                     (audio|task) + 
                     (liwc+task+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r7.2, file = "r7.2.rda")
#6.2 fits better

#round 8: model: value
r8.2 <- lme4::lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
                     (topic+liwc+value+audio|model:task) + 
                     (audio|task) + 
                     (liwc+task+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r8.2, file = "r8.2.rda")

#round 9: model:audio
r9.2 <- lme4::lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
                     (topic+liwc+value+audio|model:task) + 
                     (audio|task) + 
                     (liwc+task|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r9.2, file = "r9.2.rda")

#round 10: model:task: topic
r10.2 <- lme4::lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
                     (liwc+value+audio|model:task) + 
                     (audio|task) + 
                     (liwc+task|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r10.2, file = "r10.2.rda")
#r9.2 fits better

#round 11: model:task:, value
r11.2 <- lme4::lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
                     (topic+liwc+audio|model:task) + 
                     (audio|task) + 
                     (liwc+task|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r11.2, file = "r11.2.rda")
#r9.2 fits better

#12: task:audio
r12.2 <- lme4::lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
                              (topic+liwc+value+audio|model:task) + 
                              (1|task) + 
                              (liwc+task|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r12.2, file = "r12.2.rda")

#13: model: liwc, task ##best fit
r13.2 <- lme4::lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
                      (topic+liwc+value+audio|model:task) + 
                      (1|task) + 
                      (1|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r13.2, file = "r13.2.rda")

#with model, without task in the slopes
r0.3 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (personality+linguistic+topic+liwc+value+model+audio|model:task) + 
               (personality+linguistic+topic+liwc+value+model+audio|task) + 
               (personality+linguistic+topic+liwc+value+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r0.3, file = "r0.3.rda")

#1: task:linguistic, personality
r1.3 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (personality+linguistic+topic+liwc+value+model+audio|model:task) + 
               (topic+liwc+value+model+audio|task) + 
               (personality+linguistic+topic+liwc+value+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r1.3, file = "r1.3.rda")

#2: model:linguistic
r2.3 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (personality+topic+liwc+value+model+audio|model:task) + 
               (topic+liwc+value+model+audio|task) + 
               (personality+linguistic+topic+liwc+value+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r2.3, file = "r2.3.rda")

#3: task:topic, value
r3.3 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (personality+topic+liwc+value+model+audio|model:task) + 
               (liwc+model+audio|task) + 
               (personality+linguistic+topic+liwc+value+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r3.3, file = "r3.3.rda")

#4: task:model
r4.3 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (personality+topic+liwc+value+model+audio|model:task) + 
               (liwc+audio|task) + 
               (personality+linguistic+topic+liwc+value+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r4.3, file = "r4.3.rda")

#5: task:liwc
r5.3 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (personality+topic+liwc+value+model+audio|model:task) + 
               (audio|task) + 
               (personality+linguistic+topic+liwc+value+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r5.3, file = "r5.3.rda")
#4.3 fit better

#6: model:personality, linguistic
r6.3 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (personality+topic+liwc+value+model+audio|model:task) + 
               (liwc+audio|task) + 
               (topic+liwc+value+model+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r6.3, file = "r6.3.rda")

#7: model:audio
r7.3 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (personality+topic+liwc+value+model+audio|model:task) + 
               (liwc+audio|task) + 
               (topic+liwc+value+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r7.3, file = "r7.3.rda")

#8: model:value, model:task: personality
r8.3 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+value+model+audio|model:task) + 
               (liwc+audio|task) + 
               (topic+liwc+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r8.3, file = "r8.3.rda")

#9: model:task: topic
r9.3 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc+value+model+audio|model:task) + 
               (liwc+audio|task) + 
               (topic+liwc+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r9.3, file = "r9.3.rda")

#10: task: liwc
r10.3 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc+value+model+audio|model:task) + 
               (audio|task) + 
               (topic+liwc+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r10.3, file = "r10.3.rda")

#11: model:topic
r11.3 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
                (liwc+value+model+audio|model:task) + 
                (audio|task) + 
                (liwc+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r11.3, file = "r11.3.rda")

#12: model:model
r12.3 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
                (liwc+value+model+audio|model:task) + 
                (audio|task) + 
                (liwc|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r12.3, file = "r12.3.rda")

#13: model:task: model
r13.3 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
                (liwc+value+audio|model:task) + 
                (audio|task) + 
                (liwc|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r13.3, file = "r13.3.rda")

#14: task:audio ##parsimonious fit
r14.3 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
                (liwc+value+audio|model:task) + 
                (1|task) + 
                (liwc|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r14.3, file = "r14.3.rda")

#without task or model
r0.4 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (personality+linguistic+topic+liwc+value+audio|task/model) + 
               (personality+linguistic+topic+liwc+value+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r0.4, file = "r0.4.rda")

#1: linguistic in model:task, personality in task
r1.4 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (personality+topic+liwc+value+audio|model:task) + 
               (linguistic+topic+liwc+value+audio|task) + 
               (personality+linguistic+topic+liwc+value+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r1.4, file = "r1.4.rda")

#2: personality in model:task, personality in model, topic, liwc, linguistic in task
r2.4 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+value+audio|model:task) + 
               (value+audio|task) + 
               (linguistic+topic+liwc+value+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r2.4, file = "r2.4.rda")

#3: linguistic and topic in model
r3.4 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+value+audio|model:task) + 
               (value+audio|task) + 
               (liwc+value+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r3.4, file = "r3.4.rda")

#4: value in task
r4.4 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+value+audio|model:task) + 
               (audio|task) + 
               (liwc+value+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r4.4, file = "r4.4.rda")

#5:audio, value in model
r5.4 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+value+audio|model:task) + 
               (audio|task) + 
               (liwc|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r5.4, file = "r5.4.rda")

#6:audio in task, liwc in model #best fit
r6.4 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+value+audio|model:task) + 
               (1|task) + 
               (1|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r6.4, file = "r6.4.rda")

#7:topic in model:task
r7.4 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (liwc+value+audio|model:task) + 
               (1|task) + 
               (1|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r7.4, file = "r7.4.rda")
#6.4 fits better

#liwc in model:task
r8.4 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+value+audio|model:task) + 
               (1|task) + 
               (1|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r8.4, file = "r8.4.rda")
#6.4 fits better

#value in model:task
r9.4 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+audio|model:task) + 
               (1|task) + 
               (1|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))
save(r9.4, file = "r9.4.rda")


###
#best fits

r9.4 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+audio|model:task) + 
               (1|task) + 
               (1|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

#14: task:audio ##parsimonious fit
r14.3 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
                (liwc+value+audio|model:task) + 
                (1|task) + 
                (liwc|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

r13.2 <- lme4::lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
                      (topic+liwc+value+audio|model:task) + 
                      (1|task) + 
                      (1|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

r10.1 <- lmer(score ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
                (1|task:model) +   
                (audio|task) + 
                (liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 1000000)))
save(r10.1, file = "r10.1.rda")