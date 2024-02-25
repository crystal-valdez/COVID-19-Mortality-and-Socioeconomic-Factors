# Final-Papiha-Alana-Crystal-Deweena
Final Project with Papiha, Alana, Crystal and Deweena 

Note on tmap: 
- The tmap worked for some of us, but others needed to do additional steps for it to work, if faced with an error these steps may help:
- The only reason it didnt work for some of us was because downloading the sf package had some issues. 
- Things that can help if the tmap doesnt work:
1) Updating brew on terminal, then downloading stars, sf, rgeos and tmap
2) There can be an error that says R needs sf 1.0.8 for stars and tmap to load, in that case when installing stars some of us selected the 4th option to only download the sf related packages and not the CRAN - which we think worked.

Task 1 - Get data from Novel Coronavirus COVID-19 API 

Task 2 – Use “wbstats" package to obtain data. 

Task 3 – Combine data to calculate the ratio of all types of COVID-19 cases (confirmed, recovered, death/mortality-rate) to the population (mortality rate = # of deaths in population per unit of time) 

Task 4 -  Create categories of countries based on their geolocation and socioeconomic indicators (e.g. by geographical region, by income, by economic inequality, by GINI index score, by education etc.). Use pre-defined thresholds to define categories whenever possible. - Deweena 

Task 5 - Compare the confirmed cases and mortality rates by select country categories. 

Task 6 - Pick strongest country level indicator of COVID mortality rate. 

Task 7 - Define country categories based on strongest country level indicator.

Task 8 - ANOVA to test whether the mean mortality rate between the different country categories based on your selected indicator is different. Use faceted plots/color groupings. Can use post-hoc tests if ANOVA null hypothesis is rejected i.e. Tukey's test. 
