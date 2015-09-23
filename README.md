# NHL_Shootouts

This file contains Rcode for an analysis of NHL shootouts by Lopez and Schuckers. The paper was presented at the 2015 New England Symposium on Statistics in Sports. For link to those slides, clink [here](www.slides.com). 

There are two sets of code, each of which links to public csv files to extract the data. 

The [team level code](https://github.com/statsbylopez/NHL_Shootouts/blob/master/TeamCode.R) provides our team level analysis. There are a few parts to the team level code.

1. Year-to-year variability in team shootout win percentage
2. Does the team that shoots first win the shootout?
3. How does visiting team win percentage look over time.

The individual level code provides individual level analysis. There are many parts to the individual level code

1. Descriptive statistics, including funnel plots for shooters and goalies
2. Generalized linear models, and generalized linear mixed models, of goalie outcomes, along with coefficient plots.
3. A resampling strategy to try and better understand the decision making processes of NHL coaches. 
4. Out of sample testing on shooter and goalie characteristics
5. Estimation of shooter and goalie worth using past seasons as guidelines.
