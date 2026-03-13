.PHONY: clean all

all:\
derived_data/user_cleaned.csv\
derived_data/ads_cleaned.csv\
derived_data/composite_data1.csv\
derived_data/composite_data2.csv\
figures/interaction_count_histogram.png\
figures/interaction_count_boxplot.png\
figures/overinteract_age.png\
figures/underinteract_age.png\
figures/overinteract_country.png\
figures/underinteract_country.png\
figures/underinteract_interests.png\
figures/interact_interests_total.png\
figures/user_age_group_distr.png\
figures/targetvsuser_gender_table.png\
figures/gender_table.png\
figures/target_gender_vs_user_gender.png\
figures/health_varimpplot.png\
figures/interest_ROC.png\
figures/confusion_matrix_grid.png\
figures/average_interest_confusion_matrix_grid.png\
figures/average_interest_ROC.png\
figures/travel_varimpplot.png\
figures/interactions_vs_budget.png\
figures/campaign_ad_count.png\
figures/entropy_plot.png\
figures/tsne_plot.png\
figures/silhouette_width.png\
figures/tsne_aggclus.png\
Report.html\


clean:
	rm -rf derived_data
	rm -rf figures
	rm -rf Report.html

derived_data:
	mkdir -p derived_data

figures:
	mkdir -p figures

derived_data/user_cleaned.csv: derived_data source_data/users.csv DataClean1.R
	Rscript DataClean1.R

derived_data/ads_cleaned.csv: derived_data source_data/ads.csv DataClean1.R
	Rscript DataClean1.R
	
derived_data/composite_data1.csv: derived_data \
    source_data/ads.csv \
    source_data/campaigns.csv \
    source_data/ad_events.csv \
    DataClean1.R
	Rscript DataClean1.R
	
derived_data/composite_data2.csv: derived_data \
   source_data/ads.csv \
    source_data/campaigns.csv \
    source_data/ad_events.csv \
    DataClean1.R
	Rscript DataClean1.R
	
figures/interaction_count_histogram.png: derived_data/composite_data2.csv figures EDA_Outliers_Analysis.R
	Rscript EDA_Outliers_Analysis.R
	
figures/interaction_count_boxplot.png: derived_data/composite_data2.csv figures EDA_Outliers_Analysis.R
	Rscript EDA_Outliers_Analysis.R

figures/overinteract_age.png: derived_data/composite_data2.csv figures EDA_Outliers_Analysis.R
	Rscript EDA_Outliers_Analysis.R
	
figures/underinteract_age.png: derived_data/composite_data2.csv figures EDA_Outliers_Analysis.R
	Rscript EDA_Outliers_Analysis.R
	
figures/overinteract_country.png: derived_data/composite_data2.csv figures EDA_Outliers_Analysis.R
	Rscript EDA_Outliers_Analysis.R
	
figures/underinteract_country.png: derived_data/composite_data2.csv figures EDA_Outliers_Analysis.R
	Rscript EDA_Outliers_Analysis.R
	
figures/underinteract_interests.png: derived_data/composite_data2.csv figures EDA_Outliers_Analysis.R
	Rscript EDA_Outliers_Analysis.R
	
figures/overinteract_interests.png: derived_data/composite_data2.csv figures EDA_Outliers_Analysis.R
	Rscript EDA_Outliers_Analysis.R
	
figures/interact_interests_total.png: derived_data/composite_data2.csv figures EDA_Distributions.R 
	Rscript EDA_Distributions.R
	
figures/user_age_group_distr.png: derived_data/user_cleaned.csv figures EDA_Distributions.R 
	Rscript EDA_Distributions.R

figures/targetvsuser_gender_table.png: derived_data/composite_data2.csv figures Target_Gender_Analysis.R derived_data
	Rscript Target_Gender_Analysis.R

figures/gender_table.png: derived_data figures derived_data/user_cleaned.csv Target_Gender_Analysis.R
	Rscript Target_Gender_Analysis.R
	
figures/target_gender_vs_user_gender.png: derived_data figures derived_data/composite_data2.csv Target_Gender_Analysis.R
	Rscript Target_Gender_Analysis.R

figures/health_varimpplot.png: derived_data figures derived_data/composite_data2.csv Target_Interests_Analysis.R
	Rscript Target_Interests_Analysis.R
	
figures/interest_ROC.png: derived_data figures derived_data/composite_data2.csv Target_Interests_Analysis.R
	Rscript Target_Interests_Analysis.R

figures/confusion_matrix_grid.png: derived_data figures derived_data/composite_data2.csv Target_Interests_Analysis.R
	Rscript Target_Interests_Analysis.R

figures/average_interest_confusion_matrix_grid.png: derived_data figures derived_data/composite_data2.csv Target_Interests_Analysis2.R
	Rscript Target_Interests_Analysis2.R

figures/average_interest_ROC.png: derived_data figures derived_data/composite_data2.csv Target_Interests_Analysis2.R
	Rscript Target_Interests_Analysis2.R

figures/travel_varimpplot.png: derived_data figures derived_data/composite_data2.csv Target_Interests_Analysis2.R
	Rscript Target_Interests_Analysis2.R

figures/interactions_vs_budget.png: derived_data figures derived_data/composite_data2.csv Interactions_vs_Budget.R
	Rscript Interactions_vs_Budget.R

figures/campaign_ad_count.png: derived_data figures derived_data/composite_data2.csv Interactions_vs_Budget.R
	Rscript Interactions_vs_Budget.R

figures/entropy_plot.png: derived_data figures derived_data/user_cleaned.csv User_Clustering.R
	Rscript User_Clustering.R
	
figures/tsne_plot.png:  derived_data figures derived_data/user_cleaned.csv User_Clustering.R
	Rscript User_Clustering.R

figures/tsne_aggclus.png: derived_data figures derived_data/user_cleaned.csv Users_Hierarchical_Clusters.R
	Rscript Users_Hierarchical_Clusters.R
	
figures/silhouette_width.png: derived_data figures derived_data/user_cleaned.csv Users_Hierarchical_Clusters.R
	Rscript Users_Hierarchical_Clusters.R
	
Report.html: derived_data figures \
     Report.Rmd \
     derived_data/user_cleaned.csv\
     derived_data/ads_cleaned.csv\
     derived_data/composite_data1.csv\
     derived_data/composite_data2.csv\
     figures/interaction_count_histogram.png\
     figures/interaction_count_boxplot.png\
     figures/overinteract_age.png\
     figures/underinteract_age.png\
     figures/overinteract_country.png\
     figures/underinteract_country.png\
     figures/underinteract_interests.png\
     figures/interact_interests_total.png\
     figures/user_age_group_distr.png\
     figures/targetvsuser_gender_table.png\
     figures/gender_table.png\
     figures/target_gender_vs_user_gender.png\
     figures/health_varimpplot.png\
     figures/interest_ROC.png\
     figures/confusion_matrix_grid.png\
     figures/average_interest_confusion_matrix_grid.png\
     figures/average_interest_ROC.png\
     figures/travel_varimpplot.png\
     figures/interactions_vs_budget.png\
     figures/campaign_ad_count.png\
     figures/entropy_plot.png\
     figures/tsne_plot.png\
     figures/silhouette_width.png\
     figures/tsne_aggclus.png
	Rscript -e "rmarkdown::render('Report.Rmd')"