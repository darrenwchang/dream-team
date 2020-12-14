# ORIE-4741

### Project Name
Dream Team - Predicting Fantasy Football Weekly Projections

### Team Member Names
Darren Chang (dwc236)
Zach Schaffer (zjs25)
Jack Vaughan (jcv67)

### Summary
In this project, we have developed a method to predict fantasy football points for a specific player with a set of features for each game. We use low rank models for missing data imputation (for week 1 data), a plethora of linear models for predictive purposes, and methods such as test/training set splits and cross-validation to reduce the effect of overfitting. In addition, we compare our model results to those used by ESPN and find that while our models do not outperform professional models, they show a promising start to open-source fantasy football calculations.

### Links
* [Final Report](Final_Report.pdf)

* [Data Collection](data%20collection/scraping.R)
* [Missing Data Imputation](data%20collection/538_predictions.R)
* [Lasso Regression](base%20models/lm_base.ipynb)
* [Linear Regression](base%20models/lm_sklearn.ipynb)
* [Quantile Regression](base%models/quantile_regression.ipynb)
* [Backtest versus ESPN projections](projections_2019/2019_projections.R)
