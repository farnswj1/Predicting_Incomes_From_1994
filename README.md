# Predicting Incomes From 1994
In this project, a sample of the US population, originally from the 1994 Census, was taken and analyzed in an effort to generate an algorithm that could accurately predict whether an individual made over \$50,000 or not. The variables that were used to predict income included but were not limited to age, race, sex, education, occupation, hours per week, and marital status.

Before generating numerous algorithms, an exploration of the dataset was conducted to identify patterns that could be useful when predicting income. We identified groups that were most likely to make over \$50,000 based on the data.

A total of six machine learning algorithms were used to predict income. Five of the models were supervised learning models and the final model was an ensemble of the supervised learning models. It was determined that the **stochastic gradient boosting (GBM)** model performed the best, with an **accuracy of 86.18%**. The ensemble also did comparatively well as it had an accuracy of 86.10%. Across all models, they were all capable of correctly predicting those who make \$50,000 or less most of the time. However, they all struggled with correctly predicting those who made more than \$50,000.

Each section has their methods and models explained, followed by their respective results. 

The dataset can be accessed here:
<https://www.kaggle.com/uciml/adult-census-income/data>