READ FIRST: This project is a personal project and for demonstrating skills and analytical abilities only. Thus, any analysis is my own and documented in an informal way. It is currently still in progress with more to come.

This project is to showcase my abilities in Python. It will be organized via a Makefile for easy documentation and future work.

=============================================================================

Initial EDA data is from https://www.kaggle.com/datasets/jayjoshi37/sleep-screen-time-and-stress-analysis/code

Data description: 15000 synthetic observations of 13 variables. Most variable names are self-explanatory, but a few are multi-class categorical variables on scales. The official data card info will be included in the final report. For now, however, the variables that are rated on scales are included below for convenience.

- sleep_quality_score: A score representing sleep quality, typically ranging from 1 (poor) to 10 (excellent)
- stress_level: Measured stress level of the individual on a scale from 1 (low stress) to 10 (high stress).
- mental_fatigue_score: A score representing mental fatigue levels ranging from 1 (low fatigue) to 10 (high fatigue).

***** Important Note: while the scales are this range, the observations themselves are continuous within those intervals.

=============================================================================

Sleep_health_and_lifestyle_dataset.csv: https://www.kaggle.com/datasets/orvile/health-and-sleep-relation-2024

This dataset remains in the same topic area as the previous dataset, but it claims to be produced with funding from the University of Oxford. 

Data Description: 374 observations of 13 variables. Most variables are self explanatory, but two are rated on scales that are not explicitly defined. Thus, while in the final report, the full data description will be given, these scales and my assumptions are provided while analysis is continuing.

- Quality of Sleep: Individuals gave quality of sleep scores between 4 and 9. However, the assumption is that the scale that researchers provided was 1-10, where 1 is extremely poor and 10 is extremely well.

- Stress Level: Individuals provided ratings ranging between 3 and 8. The assumption, like with Quality of Sleep, is that the true scale was between 1-10, where 1 is not stressed and 10 is extremely stressed. 


=============================================================================

How this project is organized (continuously updated):

- Initial brainstorming and informal EDA is contained within the Jupyter Notebook 'Initial_EDA_and_Brainstorming' with the data described above. At the end of the notebook, there was a note made about how the data might be a little too clean / manufactured. Thus, the analysis for this project onwards will be on the new data: "Sleep_health_and_lifestyle_dataset.csv"

- Second EDA: Contains the formalized EDA for the "Sleep_health_and_lifestyle_dataset.csv"; finished as of 4/25/2026
