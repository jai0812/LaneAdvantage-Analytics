# 🏃 Lane Advantage in Track & Field Sprint Races
### A Data Science & Statistical Analysis Project

---

## 📌 Overview
There has long been debate in professional athletics about whether lane assignments provide an advantage or disadvantage in sprint races. Although all athletes run the same distance, factors such as track geometry, curve radius, staggered starts, and visibility may influence race outcomes.

This project applies **data science and statistical analysis** to evaluate whether **lane assignment affects win probability and performance** in elite sprint events using Olympic and World Championship data.

---

## 🎯 Research Questions
- Does lane number significantly affect the probability of winning a sprint race?
- Do average finishing times differ across lanes?
- Do grouped lane positions (Inside / Middle / Outside) show different performance or win rates?
- Does lane advantage vary across events (100m, 200m, 400m)?

---

## 📊 Dataset
**Source**
- Olympic Games (2008, 2012, 2016, 2020)
- World Athletics Championships (2009–2023)
- Publicly available Wikipedia race archives

**Dataset Size (after cleaning)**
- ~866 race entries
- Events: 100m, 200m, 400m
- Competitions: Olympics & World Championships
- Gender: Men and Women

---

## 🧾 Key Variables
- `event` – 100m / 200m / 400m  
- `lane` – Lane number (1–9)  
- `lane_group` – Inside (1–2), Middle (3–6), Outside (7–9)  
- `position` – Finishing position  
- `win` – Binary indicator (1 = win, 0 = not win)  
- `time` – Finishing time (seconds)  
- `reaction_time` – Start reaction time  
- `gender`, `year`, `competition`, `round`

---

## 🧹 Data Cleaning & Preprocessing
- Removed DQ, DNS, and DNF entries
- Merged duplicated scraped columns
- Converted variables to correct numeric and categorical types
- Filtered only valid sprint events
- Created lane group classifications for interpretability

---

## 🧠 Methods Used
- Descriptive Statistics
- Chi-Square Test of Independence
- Logistic Regression (Binary GLM)
- One-Way ANOVA
- Two-Way ANOVA (Lane Group × Event)
- Tukey HSD Post-Hoc Tests
- Data Visualization (bar plots, boxplots, interaction plots)

---

## 🧪 Results Summary

### 🥇 Win Probability by Lane
- Lane assignment significantly affects win probability
- Middle lanes (3–6) produce the highest number of winners
- Inside lanes (1–2) rarely produce winners

### ⏱️ Finishing Time Analysis
- No statistically significant difference in average finishing times across lanes
- Athletes across all lanes run statistically identical times

### 📐 Lane Group Comparison

| Lane Group | Win Rate |
|-----------|----------|
| Inside (1–2) | ~0% |
| Middle (3–6) | ~8.2% |
| Outside (7–9) | ~2.9% |

- Middle lanes offer a structural advantage for winning
- Advantage affects race outcome, not raw speed

### 🏟️ Event-Wise Consistency
- Lane advantage is consistent across 100m, 200m, and 400m events
- No significant interaction between lane group and event type

---

## ⚠️ Limitations
- Lane assignment is not random (faster qualifiers are often placed in middle lanes)
- Dataset includes only elite competitions
- Results may differ at lower competition levels

---

## ✅ Conclusion
Lane assignment significantly influences **who wins** a sprint race but does **not** affect how fast athletes run. Middle lanes consistently provide the highest probability of winning due to structural and geometric factors rather than athlete ability. This pattern holds across all major sprint events.

---

## 🛠 Tools & Technologies
- R
- tidyverse
- ggplot2
- dplyr
- broom
- stats

---

## 👤 Author

**Jai Sharma**  
Data Science & Analytics  
Boston University
