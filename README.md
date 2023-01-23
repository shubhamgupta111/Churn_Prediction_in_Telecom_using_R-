
# Churn_Prediction_in_Telecom_using_R

## Overview: 
Customer churn occurs when customers or subscribers stop doing business with a company or service, also known as customer attrition. It is also referred to as loss of clients or customers. One industry in which churn rates are particularly useful is the telecommunications industry, because most customers have multiple options from which to choose within a geographic location. Objective: Telecom observed a set of customers for a certain period of time and provided the information whether the customer churned or not. Along with this information, Telecom provided several other information that could affect the churn. The objective was to look for general patterns in the provided data that would help to identify customers that are more likely to churn, before they do that, so the company could address them and prevent their churn. We will predict behaviour to retain customers at a home phone and internet service provider called Telco. We'll first use exploratory data analysis to understand the relationships between the features and the target variable and identify factors that are influential in predicting customer attrition. Using these features, We'll develop a predictive model to help the company proactively reduce their churn rate and use insights from the model to strengthen their customer retention strategies. 


## Approach:

●	Exploratory Data Analysis \
●	Data Preprocessing\
●	Feature Selection\
●	Predicting Customer Churn\
●	Model Evaluation and ROC Curves



## Objective:

Telecom observed a set of customers for a certain period of time and provided the information whether the customer churned or not. Along with this information, Telecom provided several other information that could affect the churn.

The objective was to look for general patterns in the provided data that would help to identify customers that are more likely to churn, before they do that, so the company could address them and prevent their churn.

We will predict behaviour to retain customers at a home phone and internet service provider called Telco. 

We'll first use exploratory data analysis to understand the relationships between the features and the target variable and identify factors that are influential in predicting customer attrition. 

Using these features, We'll develop a predictive model to help the company proactively reduce their churn rate and use insights from the model to strengthen their customer retention strategies.





## Data Overview:

The dataset from Telco consists of 7,043 records with twenty attributes divided into two categories: customer demographic data and information related to their wireless accounts. The demographic features include the customer's gender, whether they have a partner, dependents, and are 65 years or older. The features related to their account information include how long the customer has been with Telco, their monthly and total charges, the contract each customer carries (month-to-month, one year, or two years), and the type of phone, internet, and TV services they have. Our target variable for this study is Churn, a binary indicator that represents whether or not the customer left within the last month. 

There were 11 customers missing from TotalCharges. Since it is a fairly small amount, these observations will be removed prior to beginning the analysis, leaving 7,032 customers in the data set. In addition, several of the Yes/No categorical variables contained an additional group indicating that the customer had no phone or internet service. These were recorded and combined with the value No.


## Conclusion:

In predicting customer attrition, logistic regression produced the highest Area Under the Curve, accuracy. Some of the most important predictors of customer attrition include 𝐓𝐞𝐧𝐮𝐫𝐞, 𝐌𝐨𝐧𝐭𝐡𝐥𝐲𝐂𝐡𝐚𝐫𝐠𝐞𝐬, 𝐈𝐧𝐭𝐞𝐫𝐧𝐞𝐭𝐒𝐞𝐫𝐯𝐢𝐜𝐞, 𝐏𝐚𝐲𝐦𝐞𝐧𝐭𝐌𝐞𝐭𝐡𝐨𝐝, 𝐂𝐨𝐧𝐭𝐫𝐚𝐜𝐭, 𝐎𝐧𝐥𝐢𝐧𝐞𝐒𝐞𝐜𝐮𝐫𝐢𝐭𝐲, 𝐓𝐞𝐜𝐡𝐒𝐮𝐩𝐩𝐨𝐫𝐭, 𝐚𝐧𝐝 𝐏𝐚𝐩𝐞𝐫𝐥𝐞𝐬𝐬𝐁𝐢𝐥𝐥𝐢𝐧𝐠 We also found that the most significant relationships from our logistic model are the customer’s monthly charges, the type of internet service and contract they have, and the length of time they have been customers with Telco. To proactively reduce their churn rate, Telco could target customers who are on month-to-month contracts, use fibre optic internet, have higher monthly charges on average, and who have a shorter tenure of less than 18 months, which is the average tenure of their former customers.
