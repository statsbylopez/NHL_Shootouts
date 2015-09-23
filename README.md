# NHL_Shootouts

This file contains R code for an analysis of NHL shootouts by Lopez and Schuckers. The paper was presented at the 2015 New England Symposium on Statistics in Sports. For link to those slides, click [here](www.slides.com). 

There are two sets of code, each of which links to public csv files to extract the data. 

The [team level code](https://github.com/statsbylopez/NHL_Shootouts/blob/master/TeamCode.R) provides our team level analysis. There are a few parts to the team level code.

1. Year-to-year variability in team shootout win percentage
2. Does the team that shoots first win the shootout?
3. Visiting team win percentage look over time

The [individual level code](https://github.com/statsbylopez/NHL_Shootouts/blob/master/IndividualCode.R) provides individual level analysis. There are many parts to the individual level code

1. Descriptive statistics, including funnel plots for shooters and goalies
2. Generalized linear models, and generalized linear mixed models, of goal outcomes (Yes/No), along with coefficient plots
3. A resampling strategy to try and better understand the decision making processes of NHL coaches
4. Out of sample testing on shooter and goalie characteristics
5. Estimation of shooter and goalie worth using past seasons as guidelines

If you are unfamiliar with R and would just like to access the data, team level data is found [here](https://docs.google.com/spreadsheets/d/1oXhcXE4N5MUqv9VifxTXvPjnfhwPGtZTZxvinzyn6OM/pub?output=csv), and shot level data is found [here](https://docs.google.com/spreadsheets/d/1SAByAftxLi8ozisTwERn-IOmHxmksWNqV1tAv2KJlYo/pub?output=csv). 
