# NBA Outcome Predictor (R Package)

This project builds a reproducible R-based pipeline to generate features and train a logistic regression model for predicting NBA game outcomes using Elo ratings, box score statistics, and rolling performance metrics. It was developed as part of a data science learning journey and packaged for reuse.

> 🏀 Predicts home win/loss for NBA games with data from 2002–2025  
> 📦 Fully modular R package with custom functions and preprocessing logic  
> 📉 Final model accuracy: ~68% on 2025 holdout season

---

## 📌 Features

- **Custom Elo Rating Calculation** (`compute_elo.R`, `compute_alltime_elo.R`)  
  Calculates both per-season and continuous all-time Elo ratings

- **Feature Engineering Pipeline** (`build_features_pipeline.R`)  
  Aggregates schedules, joins box scores, computes stat differentials

- **Rolling Metrics** (`compute_recent_stats.R`)  
  Adds 5-game rolling net rating and eFG% differentials

- **Train/Test Split Utility** (`prepare_train_test_data.R`)  
  One-liner to build clean train/test datasets

- **Model Training & Evaluation** (`model_utils.R`)  
  Loads pre-trained model, prints metrics, draws ROC curve

---

## 🧪 Model Performance (2025 Holdout)

- **Accuracy**: 68.2%  
- **AUC**: ~0.74  
- **Features used**:
  - Elo difference  
  - All-time Elo difference  
  - 5-game net rating diff  
  - 5-game eFG% diff

---

## 📦 Installation

To load the package locally:

```r
# From root project folder
devtools::load_all()
```

Install required packages:

```r
install.packages(c("dplyr", "ggplot2", "tidyr", "pROC", "caret", "slider", "hoopR", "devtools"))
```

---

## 🔧 Usage

```r
# Load all functions
library(devtools); load_all()

# Prepare features
data <- prepare_train_test()
train <- data$train_feats
test  <- data$test_feats

# Load pretrained model
model <- load_model()

# Evaluate
present_model(model, train, test)
```

---

## 🧠 Lessons Learned

- Packaging R code makes testing and reuse easier  
- Elo rating systems are interpretable but can underperform on noisy sports data  
- Real-world modeling often involves more data wrangling than modeling itself

---

## 🚧 Limitations & Future Work

- Current model only uses logistic regression  
- No modeling of player-level injuries or travel  
- Could explore tree-based models (e.g., XGBoost) or Bayesian GLMs for better calibration

---

## 📁 Project Structure
.
├── R/
│   ├── build_features_pipeline.R
│   ├── compute_elo.R
│   ├── compute_alltime_elo.R
│   ├── compute_recent_stats.R
│   ├── prepare_train_test_data.R
│   ├── model_utils.R
├── data/
│   └── final_model.rda
├── DESCRIPTION
├── NAMESPACE
└── NBAMODEL.Rproj

---
## 📬 Contact

Created by **Kaleb Coleman** — [GitHub](https://github.com/kalebcoleman)
