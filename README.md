# Sentiment Analysis of LegCo Members' Speech on Chief Executive
Sentiment analysis on lawmakers' speeches in Chief Executive-attended Hong Kong Legislative Council meetings

## What it does
A sentiment analysis on the words used by lawmakers in LegCo meetings that the Chief Executive attendeds.
The analysis examines data from the Fifth LegCo (2012-2016) and the Sixth LegCo (2016-2019) up until May 2019,
which stretches across the CE terms of Leung Chun-ying and Carrie Lam.

## What they are
* **fetch_data.R:** Scripts to fetch a lawmaker list and the speech text from the LegCo API and and export as
*cy_legco_text.rds*, *cl_legco_text.rds* and *members_name.rds*
* **preprocessing.R:** Functions to compute the sentiment of the text
* **sentiment_member.R:** Scripts to compute sentiment of each lawmaker's speech and plot bar graphs as *cy.png*
and *cl.png*
* **sentiment_timetrend.R:** Scripts to compute sentiment the speech the overall sentiment of each meeting and
plot time trend as *timetrend.png*
* **pro_est5.rds** & **pro_est6.rds**: Lists of pro-establishment lawmakers in the Fifth and Sixth LegCo

## How it works
The Chief Executive attends LegCo during CE's Question and Answer Sessions on the Policy Address, 
CE's Question and Answer Sessions and CE's Question Time. When putting questions to the CE, lawmakers
often take this opportunity to express their views about the administration and/or the CE.

This analysis matches sentiment terms from the Bing lexicon (for English text) and the National Taiwan
University Semantic Dictionary (for Chinese text) with the speech text and caluclate the proportion of positive and
negative terms used. This can provide a glimpse of the relationship between the legislature and 
the executive body.

## What you need
You need the R packages [elgarteo/legco](https://github.com/elgarteo/legco) and 
[elgarteo/legcoplus](https://github.com/elgarteo/legcoplus) to fetch data from the LegCo API.

All other packages are obtainable from CRAN.

## Reference
The code to share a legend within mulitple graphs is taking reference to this 
[ggplot2 wiki page](https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs).
