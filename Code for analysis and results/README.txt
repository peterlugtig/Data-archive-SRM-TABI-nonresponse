README
by: Katalin Roth  & Peter Lugtig
Last Update: 21 August 2021

---------------------------------------------------------------------------------

The files in this folder were used to create the findings mentioned oil the article 
"nonresponse analysis in a longitudinal smartphone-based travel survey". 
The datafiles loaded in the code are property of CBS (Statistics Netherlands) and cannot be openly shared. 
Some results are already pre-compiled and can be viewed as pdf files in the subfolder "Documents". 


The following files are available in the map:
*Files in the folder:
- README.txt: the current file
- Creating_Analysis_File.Rmd: with this file you can recreate the analysis file that I used for my analyses. The original data files were substantial in size, therefore I only selected the variables needed for analysis.
- Descriptive_Statistics.Rmd: in my thesis you can find some descriptive statistics, those can be recreated in this file. 
- Stage_124_Analysis.Rmd: with this file you can recreate my analyses for stage 1, 2, 4 (called stage 3 in the paper). You can find the logistic regressions mentioned in my thesis here. 
- Stage_3_Analysis.Rmd: with this file you can explore results for intermediate dropout (not reported in paper). We perform a cluster analysis to understand what patterns of completion people show between registration (stage 2 ) and completion (stage 3), or where they drop out.
- Documents: this folder contains the .pdf version of the markdown files

If you want to recreate the findings, first you need to get permission to access the data. 
Permission to access can be handled via Barry Schouten (jg.schouten@cbs.nl). As the data we are working with contains micro-data from the population register, you will need to go through several steps to get acces to the data on location (at Statistics Netherlands), and additional steps if you want to get access to the data outside of the country.

Once you have access to the data, you have to add the datafiles to the "Documents" folder. 
The code calls upon the various datasets and it is important that each time you use the correct data set. 

