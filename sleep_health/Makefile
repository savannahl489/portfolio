.PHONY: clean all
all:\
data/modified_sleep_health_and_lifestyle_dataset.csv\


clean:\
	rm -rf data/modified_sleep_health_and_lifestyle_dataset.csv


data/modified_sleep_health_and_lifestyle_dataset.csv: Second_EDA.ipynb data/Sleep_health_and_lifestyle_dataset.csv\
	python Second_EDA.ipynb
