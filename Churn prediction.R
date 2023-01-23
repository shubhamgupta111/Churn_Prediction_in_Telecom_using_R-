#load libraries
library(tidyverse)
library(caret)
library(reshape2)
library(broom)
library(randomForest)
library(performanceEstimation)
library(regclass)
library(GGally)
library(pROC)
library(plotROC)
library(cowplot)
library(grid)
library(gridExtra)
library(formattable)
library(scales)
library(dplyr)
telco <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv", sep=",", na.strings="?")
glimpse(telco)
telco <- telco %>% mutate(SeniorCitizen=as.factor(SeniorCitizen)) %>% na.omit()
summary(telco)
telco <- telco %>% dplyr::select(-customerID) %>%
  mutate_at(7,~as.factor(case_when(. =="No phone service"~"No",.=="No"~"No",.=="Yes"~"Yes"))) %>%
  mutate_at(c(9:14),~as.factor(case_when(.=="No internet service"~"No", .=="No"~"No", .=="Yes"~"Yes")))
summary(telco)
telco %>% 
  group_by(gender) %>% 
  rename("Gender" = gender) %>% 
  summarise("Number of Observations" = n(),
            "Average Tenure, in months" = round(mean(tenure), 0),
            "Monthly Charges" = round(mean(MonthlyCharges), 2))
t2 <- telco %>% 
  mutate(Churn2 = as.factor(ifelse(Churn == "Yes", "Former Customers", "Current Customers"))) 

g1 <- ggplot(t2, aes(x = fct_rev(Churn2), y = tenure, fill = fct_rev(Churn2))) +
  geom_bar(stat = "summary", fun = "mean", alpha = 0.6, color = "grey20", show.legend = F) +
  stat_summary(aes(label = paste(round(..y.., 0), "months")), fun = mean, 
               geom = "text", size = 3.5, vjust = -0.5) +
  labs(title = "Average Customer Tenure \n", x = "", y = "Customer Tenure\n") +
  theme(plot.title = element_text(hjust = 0.5))

g2 <- ggplot(t2, aes(x = fct_rev(Churn2), y = MonthlyCharges, fill = fct_rev(Churn2))) +
  geom_bar(stat = "summary", fun = "mean", alpha = 0.6, color = "grey20", show.legend = F) +
  stat_summary(aes(label = dollar(..y..)), fun = mean, 
               geom = "text", size = 3.5, vjust = -0.5) +
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "Average Monthly Charges \n", x = "", y = "Monthly Charges \n") +
  theme(plot.title = element_text(hjust = 0.5))

g3 <- ggplot(t2, aes(x = Contract, y = MonthlyCharges, fill = fct_rev(Churn2))) +
  geom_bar(position = "dodge", stat = "summary", fun = "mean", alpha = 0.6, color = "grey20") +
  stat_summary(aes(label = dollar(..y..)), fun = mean, 
               geom = "text", size = 3.5, vjust = -0.5,
               position = position_dodge(width = 0.9)) +
  coord_cartesian(ylim = c(0, 95)) +
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "\nAverage Monthly Charges by Contract Type", x = "\n Contract Type", 
       y = "Monthly Charges \n", fill = "") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "top", legend.justification = "left")

options(repr.plot.width=10, repr.plot.height=14)
grid.arrange(g1, g2, g3, ncol = 2, nrow = 2, layout_matrix = rbind(c(1,2), c(3,3)))
g1 <- ggplot(t2, aes(x = Contract, group = fct_rev(Churn2))) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count",
           alpha = 0.6, color = "grey20", show.legend = F) +
  geom_text(aes(label = percent(..prop..), y = ..prop.. ), 
            size = 4, stat = "count", vjust = -0.5) +
  facet_grid(~fct_rev(Churn2)) +
  scale_y_continuous(labels = percent_format()) +
  coord_cartesian(ylim = c(0, .95)) +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Customer Churn by Contract Type\n", x = "\n Contract Type", y = "") +
  theme(plot.title = element_text(hjust = 0.5))

g2 <- ggplot(t2, aes(x = InternetService, group = fct_rev(Churn2))) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count",
           alpha = 0.6, color = "grey20", show.legend = F) +
  geom_text(aes(label = percent(..prop..), y = ..prop.. ), 
            size = 4, stat = "count", vjust = -0.5) +
  facet_grid(~fct_rev(Churn2)) +
  scale_y_continuous(labels = percent_format()) +
  coord_cartesian(ylim = c(0, .9)) +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "\n Customer Churn by Internet Service \n", x = "\n Internet Service", y = "") +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(g1, g2, ncol = 1)
g1 <- ggplot(t2, aes(x = fct_rev(ifelse(SeniorCitizen==1, "Yes", "No")), group = Churn2)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count",
           alpha = 0.6, color = "grey20", show.legend = F) +
  geom_text(aes(label = percent(..prop.., accuracy = 0.1), y = ..prop..), 
            size = 4, stat = "count", position = position_stack(vjust = 0.5)) +
  facet_grid(~fct_rev(Churn2)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0, .9)) +
  labs(x = "\n Senior Citizen", y = "")

g2 <- ggplot(t2, aes(x = gender, group = Churn2)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count",
           alpha = 0.6, color = "grey20", show.legend = F) +
  geom_text(aes(label = percent(..prop.., accuracy = 0.1), y = ..prop..), 
            size = 4, stat = "count", position = position_stack(vjust = 0.5)) +
  facet_grid(~fct_rev(Churn2)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0, .6)) +
  labs(x = "\n Gender", y = "")

options(repr.plot.width=12, repr.plot.height=7)
grid.arrange(g1, g2, nrow = 1, top = textGrob("Customer Attrition Demographics \n",
                                              gp = gpar(fontsize = 14)))
options(repr.plot.width=12, repr.plot.height=10)
telco %>% 
  select(tenure, MonthlyCharges, TotalCharges, Churn) %>%
  ggpairs(aes(color = fct_rev(Churn)), title = "Customer Account Distributions and Correlations \n",
          columnLabels = c("Tenure", "Monthly Charges", "Total Charges", "Churn"),
          upper = list(combo = wrap("box_no_facet", alpha = 0.7)),
          diag = list(continuous = wrap("densityDiag", alpha = 0.6), 
                      discrete = wrap("barDiag", alpha = 0.7, color = "grey30")),
          lower = list(combo = wrap("box_no_facet", alpha = 0.7), continuous = wrap("smooth", alpha = 0.15))) 
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

norm <- as.data.frame(lapply(telco[,c(5,18)], min_max_norm))
summary(norm)

df_normed <- telco %>% dplyr::select(-c(5,18)) %>% cbind(norm)
glimpse(df_normed)

set.seed(1)
train = sample(nrow(df_normed),nrow(df_normed)*0.7,replace=FALSE)
df_train = df_normed[train,]
df_test = df_normed[-train,]
dim(df_train)
dim(df_test)

model_logit_train <- glm(as.factor(Churn) ~., df_train, family="binomial")
summary(model_logit_train)

df_test$logit_pred_prob<-predict(model_logit_train, df_test, type="response")
df_test$logit_pred_class<-ifelse(df_test$logit_pred_prob>0.5,"Yes","No")
glimpse(df_test)

mean(df_test$logit_pred_class==df_test$Churn)

logit_ct <- table(df_test$logit_pred_class, df_test$Churn)
logit_ct
logit_recall <- logit_ct[2,2]/(logit_ct[2,2]+logit_ct[1,2])
logit_recall

model_rf <- randomForest(as.factor(Churn) ~., df_train, ntree=500, mtry=2)
df_test$rf_vote <- predict(model_rf,df_test,type="class")

mean(df_test$rf_vote==df_test$Churn)

rf_ct <- table(df_test$rf_vote, df_test$Churn)
rf_ct
rf_recall <- rf_ct[2,2]/(rf_ct[2,2]+rf_ct[1,2])
rf_recall

model_svm <- svm(as.factor(Churn) ~., df_train, kernel="linear", cost=0.1)
model_svm
predicted_svm<-predict(model_svm,df_test,decision.values = TRUE)
mean(predicted_svm==df_test$Churn)
svm_ct <- table(predicted_svm, df_test$Churn)
svm_ct
svm_recall <- svm_ct[2,2]/(svm_ct[2,2]+svm_ct[1,2])
svm_recall
Logistic <- predict(model_logit_train, df_test, type = "response")
SVM <- predict(model_svm, df_test, type = "response")
RandomForest <- predict(model_rf, df_test, type = "response")
l
roc.data <- cbind(df_test, Logistic, SVM, RandomForest)
roc.long <- melt_roc(roc.data, d = "Churn", m = c("Logistic", "SVM", "RandomForest"))

rocplot <- ggplot(roc.long, aes(d = ifelse(D == "Yes", 1, 0), m = M, color = name)) +
  geom_roc(n.cuts = 0) + 
  style_roc(xlab = "\nFalse Positive Rate (1 - Specificity)", 
            ylab = "True Positive Rate (Sensitivity)\n") +
  labs(title = "ROC Curve Comparison on the Test Set", color = "Model") +
  theme(plot.title = element_text(hjust = 0.5))

rocplot +
  geom_abline(size = 0.5, color = "grey30") +
  annotate("text",x=.75,y=.28,label=paste("AUC of Logistic =", round(calc_auc(rocplot)$AUC[1],3))) +
  annotate("text",x=.75,y=.21,label=paste("AUC of SVM =", round(calc_auc(rocplot)$AUC[3],3))) +
  annotate("text",x=.75,y=.14,label=paste("AUC of Random Forest =", round(calc_auc(rocplot)$AUC[2],3))) +
  scale_color_discrete(breaks = c("Logistic", "SVM", "RandomForest"))

model_logit_train <- train(Churn ~ tenure + MonthlyCharges + InternetService + PaymentMethod + 
                   Contract + OnlineSecurity + TechSupport + PaperlessBilling, 
                 data = telco, method = "glm", 
                 preProcess = c("center", "scale"), 
                 trControl = trainControl(method = "cv", number = 10))
summary(model_logit_train$finalModel)

OR <- coef(model_logit_train$finalModel) %>% exp() %>% round(digits = 2) %>% as.data.frame() %>% slice(-c(1,6,8))
data.frame(Predictor = c("Tenure", "MonthlyCharges", "InternetServiceFiberOptic", 
                         "InternetServiceNo", "PaymentMethodECheck", "ContractOneYear", "ContractTwoYear",
                         "OnlineSecurityYes", "TechSupportYes", "PaperlessBillingYes"),
           OddsRatio = OR[,1],
           Interpretation = c("A one month increase in tenure decreases the risk of churning by about 53%.",
                              "For every $1 increase in monthly charges, we expect to see an increase in 
                              the odds of churning by a factor of 1.39 or by 39%.",
                              "Customers with fiber optic internet are 31% more likely to churn than those 
                              with DSL.", "Those without internet are 28% less likely to churn than 
                              customers with DSL internet.", "Customers who pay with electronic checks are 
                              more likely to churn by a factor of 1.19 or by 19% compared to customers who use 
                              automatic bank transfers.", "Customers on one-year contracts are 25% less likely 
                              to churn than customers on month-to-month contracts.", "Customers 
                              on two-year contracts are 44% less likely to churn compared to those on 
                              month-to-month contracts.", "Customers with online security are 19% less likely 
                              to churn than customers without online security.", "Customers with tech support 
                              are about 17% less likely to churn than customers without tech support.", 
                              "Customers with paperless billing are 21% more likely to churn than customers 
                              without paperless billing.")) %>% 
  arrange(desc(OddsRatio))
