Maximizing Impact: Social Media Campaign Effectiveness
======================================================

This project serves to explore a collection of datasets on various social media campaigns. The datasets are sourced from Kaggle and are presented in the "source_data" folder. The goal of this project is three-fold: to find out whether ads within campaigns are effective in reaching their target audiences, whether budget of a campaign always makes for a better reward, and whether users are truly unique or can be grouped into categories/ clusters. The "Report.html" presents the full analysis done as well as an in depth description of the data and an EDA exploring the initial properties of the datasets.

This project is run within a docker container. the following instructions will tell you how to build/run the container and create the report.

To Build And Run the Container:
-----------------------------------------------------
1) Before cloning the repository, ensure that you have Git LFS as this is needed to load the datasets in source_data properly. Then clone this folder onto your local computer.
2) Make sure the present working directory is set to the folder you just cloned and ensure that the datasets in the folder source_data show up as datasets. If they do not:
    - Ensure that git lfs is installed by running "git lfs install"
    - Run "git lfs pull"
3) Run "./start.sh" to build and run the container.
4) Navigate to "localhost:8797" in your web browser. Your username will be "rstudio" and password "1234".

To Create the Report:
-----------------------------------------------------
3) Set the working directory within the container as the "work" folder
4) run "make Report.html"

Instructions for Developers/ Future Contributors:
-----------------------------------------------------
This project is organized via a Makefile. The artifacts within the folders "derived_data" and "figures" are automated via the Makefile, as is the "Report.html" file.

