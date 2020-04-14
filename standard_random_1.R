#models
#https://stats.stackexchange.com/questions/31569/questions-about-how-random-effects-are-specified-in-lmer

#maximal model with REML
m0 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
             (personality+linguistic+topic+liwc+value+task+audio+model|task/model) + 
             (personality+linguistic+topic+liwc+value+task+audio+model|task) + 
             (personality+linguistic+topic+liwc+value+task+audio+model|model), REML=TRUE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

print(summary(m0))
m0_pca <- rePCA(m0)
summary(m0_pca)


#maximal model 
m0.1 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
             (personality+linguistic+topic+liwc+value+task+audio+model|task/model) + 
             (personality+linguistic+topic+liwc+value+task+audio+model|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

m0.1_pca <- rePCA(m0.1)
summary(m0.1_pca)


#save.image(file='session.RData')
#load('session.RData')

#maximal model with no correlation paramters
#m0.2 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
#             (personality+linguistic+topic+liwc+value+task + audio||task/model) + 
#             (personality+linguistic+topic+liwc+value+task + audio||task) + 
#             (personality+linguistic+topic+liwc+value+task+ audio||model), REML=TRUE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))


#m0.2_pca <- rePCA(m0.2)
#summary(m0.2_pca)

m0.3 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (personality+linguistic+topic+liwc+value+task+audio+model||task/model) + 
               (personality+linguistic+topic+liwc+value+task+audio+model||model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

m0.3_pca <- rePCA(m0.3)
summary(m0.3_pca)

#iterate through models, removing sets of random effects variance components

#original run, with task but without model in the slopes
m1.0 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (linguistic+topic+liwc+value+task + audio|task/model) + 
               (personality+linguistic+task|task) + 
               (personality+linguistic+topic+liwc+value+task+ audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

#second run, with task and model in the slopes
m1.1 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6+audio+
               (topic+liwc+value+task+audio+model|task/model) +
               (linguistic+liwc+task+audio+model|task) +
               (personality+linguistic+topic+liwc+value+task+ audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

#third run, without task or model in the slopes
m1.2 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6+audio+
               (topic+liwc+value+audio|task/model) +
               (linguistic+liwc+audio|task) +
               (personality+linguistic+topic+liwc+value+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

m2.0 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (linguistic+topic+liwc+value+task + audio|task/model) + 
               (1|task) + 
               (personality+linguistic+topic+liwc+value+task+ audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

m3.0 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+value+task + audio|task/model) + 
               (1|task) + 
               (linguistic+topic+liwc+value+task+ audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

m3.1 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+value+task + audio||task/model) + 
               (1|task) + 
               (linguistic+topic+liwc+value+task+ audio||model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

m4.0 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+task+audio||task/model) + 
               (1|task) + 
               (linguistic+topic+liwc+task+audio||model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

m4.1 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+task+audio|task/model) + 
               (1|task) + 
               (linguistic+topic+liwc+task+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

m5.0 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+liwc+audio|task/model) + 
               (1|task) + 
               (linguistic+topic+liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

m6.0 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (audio|task/model) + 
               (1|task) + 
               (liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

m6.1 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+audio|task/model) + 
               (1|task) + 
               (linguistic+topic+liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

m7.0 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+audio|task/model) + 
               (linguistic+topic+liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

m7.1 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+audio||task/model) + 
               (linguistic+topic+liwc+audio||model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

m8.0 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (topic+audio|task/model) + 
               (topic+liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

m8.1 <- lmer(score_z ~ (personality+linguistic+topic+liwc+value+task)^6 + audio +
               (audio|task/model) + 
               (linguistic+topic+liwc+audio|model), REML=FALSE, data=test_df, control = lmerControl(optCtrl = list(maxfun = 100000)))

