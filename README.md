Agricultural Yield Prediction (Linear Regression in R)
📌 Overview

This project predicts crop yield (tons/ha) using environmental and agricultural factors such as rainfall, temperature, humidity, fertilizer usage, and soil type. A Linear Regression model is used to analyze relationships and make predictions.

🎯 Objectives
Perform Exploratory Data Analysis (EDA)
Build a Linear Regression model
Evaluate using R², RMSE, MAE
Identify key factors affecting yield
📊 Dataset
2000 observations (2010–2024)
Features: Rainfall, Temperature, Humidity, Fertilizer, Soil Type
Target: Crop Yield (tons/ha)
⚙️ Model
Algorithm: Linear Regression (lm() in R)
Train-Test Split: 80/20
Seed: 42
📈 Performance
Metric	Value
R²	0.8014
RMSE	0.5172
MAE	0.4170

✅ Model explains ~80% of yield variation

🔑 Key Insights
🌧️ Rainfall → strongest positive factor
🌡️ Temperature → negative impact
💊 Fertilizer → increases yield
🌱 Best soils: Silt Loam & Sandy Loam
🛠️ Tech Stack
R
ggplot2, dplyr, caret, corrplot
🚀 Future Work
Try Random Forest / Boosting
Add interaction terms
Build Shiny dashboard
👩‍💻 Authors

Stephy Ann Biju
Sreyanandha Santhosh
Steve Thomas Shylu
