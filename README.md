# DataScience-FinalProject

Mike Ogrysko

DS795/796 Data Science Final Project - Loyola Maryland MS Data Science program

Title: Predicting MLB Player Performance

Research areas:
- Estimated peak player age at each position
- Binary classification to determine players that will exceed their 2022 performance
- Evaluation of two regression models to predict future performance

Data and Research Platform
- Data compiled from Stathead Baseball (https://stathead.com/baseball/) and FanGraphs (https://www.fangraphs.com/) from 1980 onwards
- Exported as a series of CSV files -> merged -> cleaned -> transformed using Python in Jupyter Notebooks
- Python used for identifying peak age at each position and binary classification
  - Libraries used included: Pandas, Numpy, Matplotlib, Seaborn, and Sklearn
- R used for evaluation of multiple linear and lasso regressions

Summary
- Peak ages differ by position; range from 26 to 30 with DH as the outlier
- Binary classification – 20/45 batters and 21/36 pitchers predicted to exceed 2022 WAR in 2023
  - Random Forest algorithm 97% & 98% accuracies on classification
- Lasso regression R2 exceeded Multiple Linear Regression for predicting 2023 WAR
  - Explaining 50.99058% of the variation of the response values of the training data
- Lasso regression could be a good starting point for building a model to predict WAR

<i><a href="https://mcogrysko.github.io">Back to Mike Ogrysko's portfolio</a></i>
