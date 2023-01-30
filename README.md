# CTIS mental health
<p align="center">
<img src="shiny/ctis-app/www/icon.png" width="100" height="100" class="center">
</p>

## The CTIS Survey
"The University of Maryland Social Data Science Center Global COVID-19 Trends and Impact Survey, in partnership with Facebook" [UMD Global CTIS](https://covidmap.umd.edu/) serves as the data source for this study. During the survey period, a representative sample of Facebook users aged 18 and over were invited to report on symptoms, social distancing behavior, mental health issues, and financial constraints on a daily basis.

## Data used
* CTIS macrodata (public access) and microdata (restricted access - only for researchers)
* Our world in data policy response datasets

## Our findings and remarks

### Microdata
Our micro data analysis suggests three major risk factors: 
* age
* education level and
* gender.  

The age variable seems to have an tremendous impact on the mental health. We observed younger people to report a higher level of mental health problems than seniors. These findings are also backed by other [general mental health studies](https://pubmed.ncbi.nlm.nih.gov/27561149/) that propose older age groups are more resilient than younger ones. Our findings show that this paradox trend for mental health did continue during Covid-19 and older people experienced less mental problems than young adults. We want to stress that the younger generation seems to have had major mental health problems during the pandemic and therefore suggest targeted mental health care for this group. 

### Macrodata analysis
The macrodata shows how the anxiety during the pandemic was quite different in every country. Peak of anxiety were reached in Europe on February 2022, while trends in America and Asia were quite stable. Oceania had the lowest anxiety levels. 
Moreover, very high correlation were found in the data between the finance and food security variables and retail and recreation and grocery and pharmacy variables. 
We then observe how all variables are relevant to the regression model, with the finance one being the last one disappering from the model and therefore having a large weight on it. 

## Paper Link
[Paperlink]

## Shiny App Link
[Applink]

## Acknowledgements
- [Prof. Dr. Frauke Kreuter](https://www.soda.statistik.uni-muenchen.de/people/professors/kreuter1/index.html)
- [Dr. Carolin Haensch](https://www.soda.statistik.uni-muenchen.de/people/employees/haensch/index.html) 
- [University of Maryland Global CTIS](https://covidmap.umd.edu/)
- [Our world in data (Policy Responses)](https://ourworldindata.org/policy-responses-covid)

## How to contribute
Clone (or fork) this repository by calling the command below or by using e.g the R Studio GIT IDE.
```
git clone https://github.com/christian-hobelsberger/CTIS-Seminar.git
```

Feel free to open an issue or a pull request to our current repository.

## Authors

- [@christian-hobelsberger](https://www.github.com/christian-hobelsberger)
- [@MaxMLang](https://www.github.com/MaxMLang)
- @elisa-shima



