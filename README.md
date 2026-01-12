# Fourth Down Decision Analysis

**Measuring Coach Aggression Over Expectation (AOE) in College Football**

A machine learning pipeline that analyzes fourth down decision-making in college football, training classification models to predict go-for-it decisions and calculating how aggressive each coach is compared to expected behavior.

---

## Project Overview

This project answers a simple question: **Which college football coaches go for it on fourth down more (or less) than expected?**

By training ML models on situational features (field position, score, time remaining, etc.), we establish what an "average" decision would be in any given scenario. Coaches who go for it more often than predicted are considered aggressive; those who punt/kick more often are conservative.

### Key Metric: Aggression Over Expectation (AOE)

```
AOE = Actual Go-For-It Rate - Expected Go-For-It Rate
```

- **Positive AOE** → Coach goes for it more than expected (aggressive)
- **Negative AOE** → Coach punts/kicks more than expected (conservative)

---

## Project Structure

```
├── Fourth_Down_Data.R              # Data acquisition & feature engineering
├── Fourth_Down_Clean_FinalData.R   # Data cleaning for misclassified plays
├── fourth_down_ml_pipeline.ipynb   # ML training & AOE calculation
├── Fourth_Down_Visualization.R     # Professional tables with team logos
├── fourth_down_model_data.csv      # Raw processed data
├── Fourth_Down_Model_Data_v2.csv   # Cleaned final dataset
├── coach_aggression_rankings.csv   # Final coach rankings output
└── README.md
```

---

## Requirements

### R Dependencies
```r
install.packages(c("tidyverse", "data.table", "cfbfastR", "lubridate", 
                   "gt", "gtExtras", "cfbplotR", "rvest", "httr", 
                   "glue", "scales", "webshot2"))
```

### Python Dependencies
```bash
pip install pandas numpy scikit-learn xgboost lightgbm joblib
```

### API Key
The project uses the College Football Data API. 

---

## Pipeline Workflow

### Step 1: Data Acquisition (`Fourth_Down_Data.R`)

Downloads and processes data from cfbfastR:
- **Game Information** (2015-2025)
- **Betting Lines** (spread data)
- **Team Talent Composites**
- **Coach Records**
- **Play-by-Play Data** (filtered to 4th downs)

**Key Features Engineered:**
| Category | Features |
|----------|----------|
| Situation | `distance`, `yards_to_goal`, `period` |
| Time | `seconds_remaining_game`, `two_minute_drill` |
| Score | `point_differential`, `is_trailing`, `late_and_close` |
| Field Position | `in_red_zone`, `in_opponent_territory`, `goal_to_go` |
| Betting | `pos_team_spread`, `pos_team_is_underdog` |
| Talent | `talent_gap`, `scaled_talent_gap` |
| Historical | `prev_4th_down_success_rate`, `cumulative_epa` |

### Step 2: Data Cleaning (`Fourth_Down_Clean_FinalData.R`)

Fixes misclassified plays where special teams plays (punts, blocked kicks) were incorrectly labeled as "went for it":

### Step 3: ML Pipeline (`fourth_down_ml_pipeline.ipynb`)

**Models Evaluated:**
- Logistic Regression
- Random Forest (Best)
- Gradient Boosting
- XGBoost
- LightGBM
- Extra Trees
- AdaBoost
- Decision Tree
- K-Nearest Neighbors
- Naive Bayes

**Model Comparison Results:**

| Model | Accuracy | F1-Score | ROC-AUC |
|-------|----------|----------|---------|
| Random Forest | 0.8967 | 0.7470 | 0.9372 |
| XGBoost | 0.8903 | 0.7331 | 0.9319 |
| LightGBM | 0.8873 | 0.7261 | 0.9301 |
| Gradient Boosting | 0.8770 | 0.6829 | 0.9190 |

### Step 4: Visualization (`Fourth_Down_Visualization.R`)

Creates professional `gt` tables with:
- Team logos
- Color-coded AOE values
- Conference breakdowns
- Exportable PNG/HTML formats

---

## Key Results

### Most Aggressive Coaches (2015-2025)

| Rank | Coach | Team | Opportunities | Actual % | Expected % | AOE |
|------|-------|------|---------------|----------|------------|-----|
| 1 | Willie Simmons | Florida International | 118 | 25.4% | 20.3% | +5.1% |
| 2 | Joe Harasymiak | Massachusetts | 125 | 22.4% | 17.6% | +4.8% |
| 3 | Jason Eck | New Mexico | 95 | 33.7% | 29.5% | +4.2% |
| 4 | Joey McGuire | Texas Tech | 412 | 30.3% | 26.5% | +3.9% |
| 5 | Lane Kiffin | Ole Miss | 850 | 32.1% | 28.5% | +3.6% |

### Most Conservative Coaches

| Rank | Coach | Team | Opportunities | Actual % | Expected % | AOE |
|------|-------|------|---------------|----------|------------|-----|
| 367 | Scott Shafer | Syracuse | 107 | 13.1% | 15.9% | -2.8% |
| 366 | David Bailiff | Rice | 291 | 16.2% | 18.6% | -2.4% |
| 365 | Ryan Beard | Missouri State | 93 | 14.0% | 16.1% | -2.2% |
| 364 | Urban Meyer | Ohio State | 362 | 21.3% | 23.2% | -1.9% |
| 363 | Nick Saban | Alabama | 712 | 17.0% | 18.3% | -1.3% |

---

## Dataset Statistics

- **Total 4th Down Plays:** 200,056
- **Unique Coaches Ranked:** 367 (min. 20 opportunities)
- **Years Covered:** 2015-2025
- **Overall Go-For-It Rate:** 21.57%

---

## Target Variable Definition

A play is classified as "went for it" if it was NOT:
- A punt (including blocked punts)
- A field goal attempt (including misses/blocks)
- A penalty

---

## Output Files

| File | Description |
|------|-------------|
| `coach_aggression_rankings.csv` | Full coach rankings with AOE |
| `play_level_predictions.csv` | Individual play predictions |
| `model_comparison_results.csv` | All model metrics |
| `final_fourth_down_model.joblib` | Trained model for inference |
| `top_25_aggressive_coaches.png` | Visualization table |
| `top_25_conservative_coaches.png` | Visualization table |
| `conference_breakdown.png` | Conference-level analysis |

---

## Methodology Notes

1. **Stratified Split:** 80/20 train/test with stratification on target variable
2. **Hyperparameter Tuning:** GridSearchCV with 3-fold stratified CV
3. **Minimum Sample Size:** Coaches need 20+ fourth down opportunities to be ranked
4. **Feature Selection:** 30 features covering situation, time, score, field position, betting, and historical context

---
## Author

Campbell Taylor

---

## License

This project is for educational and analytical purposes. Data sourced from [cfbfastR](https://cfbfastr.sportsdataverse.org/) and the College Football Data API.

---

## Acknowledgments

- [cfbfastR](https://cfbfastr.sportsdataverse.org/) for the R package
- [College Football Data](https://collegefootballdata.com/) for the API
