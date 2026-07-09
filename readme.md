# Reintroducing Choice: Social media app-entry interventions

This repository contains the analysis code for a field experiment testing whether brief decision points at app entry change adolescents' social media behaviour. The study followed Danish adolescents aged 13--17 years over a two-week baseline period and a four-week intervention period. Participants were assigned to one of three conditions: **Reflection**, **Planning**, or **Waiting**.

The analyses estimate changes in three behavioural outcomes:

1. total daily social media use,
2. daily app opens, and
3. average session duration.

They also examine dismissed app-opening attempts, burst-like access episodes, platform-specific effects, moderation by baseline use and individual differences, and pre-post survey outcomes.

> **Data access note**  
> The raw individual-level behavioural logs are not included in this repository. They contain detailed digital activity records from adolescent participants and are subject to privacy, consent, and data protection restrictions. The scripts assume that authorised users have access to the required raw and processed data files in the folder structure described below.

## Repository structure

```text
.
├── 01_preprocessing/        # Builds cleaned survey, demographic, app-log, baseline, and intervention datasets
├── 02_analysis/             # Fits main, moderation, platform-specific, burst, and survey models
├── 03_figures/              # Recreates main manuscript figures from model outputs
├── 04_supplementary/        # Recreates supplementary analyses, figures, and tables
├── README.md                # Project-level guide
└── 02_analysis/README.md    # More detailed analysis-pipeline notes
```

The code expects the following working folders to exist at the project root when the full pipeline is run:

```text
00_data/      # raw input data; not tracked in the public repository
data/         # processed analysis datasets
models/       # fitted model objects
figures/      # generated figures
tables/       # generated tables
reports/      # preprocessing and diagnostic reports
```

## Software requirements

The project is written in R. Analyses were developed for R 4.4.x.

Required R packages include:

```r
install.packages(c(
  "tidyverse", "lubridate", "zoo", "readxl", "writexl",
  "janitor", "psych", "lme4", "lmerTest", "glmmTMB",
  "broom.mixed", "emmeans", "ggeffects", "splines",
  "knitr", "kableExtra", "patchwork", "scales",
  "showtext", "sysfonts"
))
```

All analysis and figure scripts source `02_analysis/shared_utils.R`, which loads packages, defines shared plotting settings, and provides helper functions for saving model tables.

## Quick start

From the project root, start R and set the working directory to the repository root:

```r
setwd("/path/to/repository")
```

Then run the pipeline in order.

### 1. Preprocess data

```r
source("01_preprocessing/01_survey_preprocessing.R")
source("01_preprocessing/02_demographic_preprocessing.R")
source("01_preprocessing/03_app_data_preprocessing.R")
source("01_preprocessing/04_baseline_preprocessing.R")
source("01_preprocessing/05_intervention_period_preprocessing.R")
source("01_preprocessing/06_regret_model.R")
source("01_preprocessing/06a_reintervention_preprocessing.R")
source("01_preprocessing/07_individual_differences.R")
```

These scripts read raw survey, demographic, and behavioural log files from `00_data/` and write cleaned datasets to `data/`.

### 2. Fit main behavioural models

```r
source("02_analysis/main_effects/01_main_daily_time.R")
source("02_analysis/main_effects/02_main_daily_opens.R")
source("02_analysis/main_effects/03_main_session_duration.R")
```

These scripts estimate intervention-period versus baseline-period changes in total daily social media use, app opens, and average session duration.

### 3. Fit moderation models

```r
source("02_analysis/moderation/04a_moderation_baseline_time_use_daily_time.R")
source("02_analysis/moderation/04b_moderation_baseline_opens_daily_opens.R")
source("02_analysis/moderation/05a_moderation_daily_time_addiction_selfcontrol.R")
source("02_analysis/moderation/05b_moderation_daily_opens_addiction_selfcontrol.R")
source("02_analysis/moderation/05c_moderation_dismissal_addiction_selfcontrol.R")
```

These scripts test whether intervention responses vary by baseline social media use, baseline app opens, self-control, and social media addiction.

### 4. Fit platform-specific, burst, and survey models

```r
source("02_analysis/app_specific/06a_app_specific_daily_time.R")
source("02_analysis/app_specific/06b_app_specific_daily_opens.R")
source("02_analysis/app_specific/06c_app_specific_session_duration.R")
source("02_analysis/app_specific/06d_app_specific_dismissal_probability.R")
source("02_analysis/burst/07_burst_analysis.R")
source("02_analysis/survey/08_survey_wellbeing_updated.R")
```

These scripts estimate app-specific effects, changes in burst-like access episodes, and pre-post survey changes in well-being and related subjective outcomes.

### 5. Recreate main figures

```r
source("03_figures/figure1_main_figure.R")
source("03_figures/figure2_burst_figure.R")
source("03_figures/figure3_moderation_daily_time.R")
source("03_figures/figure2_moderation_with_dismissal.R")
source("03_figures/figure4_app_specific.R")
```

Figures are written to `figures/main/`, with supporting data and tables written to `tables/main/` or the relevant analysis subfolder.

### 6. Recreate supplementary analyses

```r
source("04_supplementary/supp_01_sample_characteristics/sample_characteristics.R")
source("04_supplementary/supp_02_baseline_descriptives/A_baseline_platform_use.R")
source("04_supplementary/supp_03_activity_patterns/B_activity_heatmap.R")
source("04_supplementary/supp_04_reinterventions/C_reintervention_analysis.R")
source("04_supplementary/supp_05_opens_session_trajectories/D_opens_session_its.R")
source("04_supplementary/supp_09/E_wellbeing_outcomes.R")
source("04_supplementary/supp_10/F_retention_analysis.R")
```

Supplementary outputs are written to `figures/supplementary/` and `tables/supplementary/`.

## Main outputs

The pipeline produces:

- cleaned behavioural and survey datasets in `data/`,
- fitted model objects in `models/`,
- manuscript-ready figures in `figures/`,
- `.csv`, `.xlsx`, and `.tex` tables in `tables/`, and
- preprocessing diagnostics in `reports/`.

Model tables are exported in multiple formats. The `.tex` files use `booktabs` formatting and are intended for direct inclusion in a LaTeX manuscript.

## Analysis overview

The main estimands are within-participant changes from the baseline period to the intervention period, estimated separately by intervention condition. The core behavioural hierarchy is:

1. **total daily social media use**, the primary behavioural outcome;
2. **daily app opens**, indicating whether change reflects fewer entries;
3. **average session duration**, indicating whether remaining sessions become shorter or longer; and
4. **dismissed app-opening attempts**, indicating whether access attempts stop at the app-entry decision point.

Related outcomes, including burst-like access episodes and platform-specific estimates, should be read as converging evidence about the behavioural pattern rather than as independent discoveries.

## Reproducibility notes

- All scripts assume the project root as the working directory.
- Timestamps are handled in the `Europe/Copenhagen` time zone.
- The baseline period and intervention period are treated consistently across preprocessing and analysis scripts.
- Fall break is coded from `2024-10-14` where relevant.
- Generated files are not required to be tracked in Git; they can be recreated from the scripts when the authorised input data are available.

## Suggested `.gitignore`

Because the repository contains analysis code for sensitive data, generated data and outputs should usually be excluded from version control:

```gitignore
.DS_Store
.Rhistory
.RData
.Rproj.user/
00_data/
data/
models/
figures/
tables/
reports/
*.html
*.pdf
```

## Citation

If you use this repository, please cite the associated manuscript:

> Hansen, L. H., Grüning, D. J., Jespersen, A. M., Riedel, F., & Normann, C. Reintroducing choice reduces and reshapes social media use: evidence from a 1.2 million-interaction field experiment.

## Contact

For questions about the analyses or access to non-public replication materials, please contact the corresponding author listed in the manuscript.
