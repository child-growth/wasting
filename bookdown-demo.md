--- 
title: "Supplement to Longitudinal analysis of child wasting and concurrence with stunting in low-resource settings"
author: "Andrew Mertens et al."
date: "2020-06-10"
site: bookdown::bookdown_site
output: bookdown::gitbook
documentclass: book
bibliography: [book.bib, packages.bib]
biblio-style: apalike
link-citations: yes
github-repo: rstudio/bookdown-demo
description: "This is supplementary information to Longitudinal analysis of child wasting and concurrence with stunting in low-resource settings"
---

# Overview

**Recommended citation:** Mertens A N, et al. 2020. Longitudinal analysis of child wasting and concurrence with stunting in low-resource settings. *Journal Name*. doi. 

This site contains supplementary information to the *Longitudinal analysis of child wasting and concurrence with stunting in low-resource settings*. 




<!--chapter:end:index.Rmd-->

# Sensitivity analysis using fixed effects {#fixed-effects}

---
output:
  pdf_document:
    keep_tex: yes
fontfamily: mathpazo
fontsize: 9pt
---

\raggedright


The primary analyses presented in this manuscript pooled across individual studies using random effects. Inferences about estimates from fixed effects models are restricted to only the included studies.[^1] The random effects approach is more conservative in the presence of study heterogeneity and has larger confidence intervals around each point estimate unless all cohort-specific estimates are very similiar. Overall, the inference from results produced by each method did not greatly differ. 



## Age-specific prevalence

### Random effects

\includegraphics[width=58.33in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-wast-2-prev-overall_region--allage-primary} 

### Fixed effects




## Age-specific incidence

### Random effects

\includegraphics[width=58.33in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-wast-2-cuminc-overall_region--allage-primary} 

### Fixed effects


\includegraphics[width=58.33in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/FE/fig-wast-2-cuminc-overall_region--allage-primary_FE} 

## Age-specific incidence rate

### Random effects

\includegraphics[width=58.33in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-wast-2-ir-overall_region--allage-primary} 

### Fixed effects


\includegraphics[width=58.33in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/FE/fig-wast-2-ir-overall_region--allage-primary_FE} 

## Age-specific recovery

### Random effects

\includegraphics[width=58.33in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-wast-2-rec-overall_region--allage-primary} 

### Fixed effects


\includegraphics[width=58.33in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/FE/fig-wast-2-rec-overall_region--allage-primary_FE} 

## Age-specific prevalence of severe wasting

### Random effects

\includegraphics[width=58.33in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-wast-3-prev-overall_region--allage-primary} 

### Fixed effects


\includegraphics[width=58.33in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/FE/fig-wast-3-prev-overall_region--allage-primary_FE} 

## Age-specific longitudinal prevalence of persistent wasting

### Random effects


### Fixed effects


\includegraphics[width=25in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/FE/fig-perswast_plot_FE} 

## Age-specific prevalence of concurrent wasting and stunting

### Random effects

\includegraphics[width=58.33in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-wast-2-co-overall_region--allage-primary} 

### Fixed effects


\includegraphics[width=58.33in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/FE/fig-wast-2-co-overall_region--allage-primary_FE} 

<!-- ## Age-specific prevalence of underweight (weight-for-age Z-score < -2) -->

<!-- ```{r, echo = FALSE} -->
<!-- include_graphics("figure-copies/fig-wast-2-uwt-overall_region--allage-primary_FE.png") -->

<!-- ``` -->




[^1]: Hedges, L. V. & Vevea, J. L. Fixed- and random-effects models in meta-analysis. Psychol. Methods 3, 486–504 (1998).

<!--chapter:end:01-fixed-effects.Rmd-->

# Cohort-specific estimates {#cohort}

---
output:
  pdf_document:
    keep_tex: yes
fontfamily: mathpazo
fontsize: 9pt
---

\raggedright

Below are the cohort-specific estimates for the age-specific prevalences of wasting, severe wasting, persistent wasting, underweight, and concurrent wasting and stunting.





## Age-specific prevalence


\includegraphics[width=41.67in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-prev_plot_africa} 
\includegraphics[width=41.67in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-prev_plot_lam} 
\includegraphics[width=41.67in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-prev_plot_sasia} 



<!-- ## Age-specific incidence -->

<!-- ```{r, echo = FALSE} -->
<!-- include_graphics("figure-copies/fig-wast-ci-no-Kenaba-BW.png") -->

<!-- ``` -->

<!-- ## Age-specific incidence rate -->

<!-- ```{r, echo = FALSE} -->
<!-- include_graphics("figure-copies/fig-wast-ir-no-Kenaba-BW.png") -->

<!-- ``` -->

<!-- ## Age-specific recovery -->

<!-- ```{r, echo = FALSE} -->
<!-- include_graphics("figure-copies/fig-wast-rec-no-Kenaba-BW.png") -->

<!-- ``` -->

## Age-specific prevalence of severe wasting


\includegraphics[width=41.67in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-sevwast_plot_africa} 
\includegraphics[width=41.67in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-sevwast_plot_lam} 
\includegraphics[width=41.67in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-sevwast_plot_sasia} 

## Age-specific longitudinal prevalence of persistent wasting


\includegraphics[width=41.67in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-perswast_plot_africa} 
\includegraphics[width=41.67in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-perswast_plot_lam} 
\includegraphics[width=41.67in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-perswast_plot_sasia} 

## Age-specific prevalence of concurrent wasting and stunting


\includegraphics[width=41.67in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-co_plot_africa} 
\includegraphics[width=41.67in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-co_plot_lam} 
\includegraphics[width=41.67in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-co_plot_sasia} 

## Age-specific prevalence of underweight (weight-for-age Z-score < -2)


\includegraphics[width=41.67in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-underweight_plot_africa} 
\includegraphics[width=41.67in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-underweight_plot_lam} 
\includegraphics[width=41.67in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-underweight_plot_sasia} 



<!--chapter:end:02-cohort-estimates.Rmd-->

# Sensitivity analysis dropping at-Birth measures in Kenaba {#no-kenaba}

---
output:
  pdf_document:
    keep_tex: yes
fontfamily: mathpazo
fontsize: 9pt
---

\raggedright

Here, we re-estimate primary results after dropping the observations of children at birth within the MRC Kenaba cohort, which used a different team to measure child anthropometry at birth from the trained anthropometrists used in follow-up measurements. While other cohort data from the Kenaba area show that children tend to experience decreased LAZ after birth, in the MRC Kenaba data used in this analysis, we see a high birth LAZ (approximately 0) and a rapid drop in LAZ in followup measurements (approximately -1 at one month).
we see that the at birth measurements LAZ is ~0, and then for the follow-up measurements after the mean LAZ is ~ -1 (and the mean LAZ of just the subsequent follow-up visit at one month is ~ -1). We calculated 30% of measurements taken within two weeks of birth are lower than the at birth measurement beyond the technical error of measurement, and so are unrealistic decreases in child length, and 43% of follow-up measurements within two weeks of birth are less than the at-birth measurement by any amount. 




## Mean WLZ by region

![](C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/WLZ_by_region-no-Kenaba-BW.png)<!-- --> 

## Age-specific prevalence

![](C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-wast-prev-no-Kenaba-BW.png)<!-- --> 



## Age-specific incidence

![](C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-wast-ci-no-Kenaba-BW.png)<!-- --> 

## Age-specific incidence rate

![](C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-wast-ir-no-Kenaba-BW.png)<!-- --> 

## Age-specific recovery

![](C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-wast-rec-no-Kenaba-BW.png)<!-- --> 

## Age-specific prevalence of severe wasting

![](C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-sev-wast-no-Kenaba-BW.png)<!-- --> 

## Age-specific longitudinal prevalence of persistent wasting

![](C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-pers-wast-no-Kenaba-BW.png)<!-- --> 

## Age-specific prevalence of concurrent wasting and stunting

![](C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-co-prev-no-Kenaba-BW.png)<!-- --> 

## Age-specific prevalence of underweight (weight-for-age Z-score < -2)

![](C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-uw-prev-no-Kenaba-BW.png)<!-- --> 



<!--chapter:end:03-no-kenaba.Rmd-->

# Sensitivity analysis comparing wasting defined via weight-for-length versus middle-upper arm circumference {#muac}

---
output:
  pdf_document:
    keep_tex: yes
fontfamily: mathpazo
fontsize: 9pt
---

\raggedright





\includegraphics[width=58.33in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/wasting/fig-wast-2-muac-overall_region--allage-primary} 


<!--chapter:end:04-MUAC.Rmd-->

# Anthropometry measurement quality {#anthro}

---
output:
  pdf_document:
    keep_tex: yes
fontfamily: mathpazo
fontsize: 9pt
---

\raggedright





## Anthropometry measuresments compared to WHO standards

To check for outliers in length measurements, We plotted the distribution of raw length and weight measurements by age and sex against bands marking the first, second, and third standard deviations of the World Health Organization child growth standard distribution. The majority of observations fell within 3 standard deviations of the mean of the standard for males and females. 


\includegraphics[width=33.33in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/shared/waz_QA_monthly} 
\includegraphics[width=104.17in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/shared/waz_QA_cohort_monthly} 
\includegraphics[width=33.33in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/shared/laz_QA_monthly} 
\includegraphics[width=104.17in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/shared/laz_QA_cohort_monthly} 
\includegraphics[width=104.17in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/shared/wlz_QA_monthly} 
\includegraphics[width=104.17in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/shared/wlz_QA_cohort_monthly} 


## Age-specific incidence

This study included cohorts that measured child growth from 1987 to 2014. To assess potential secular trends, we plotted the mean LAZ, WAZ, and WLZ over time. The plot below shows the individual observations from included studies over this range of years. There does not appear to be a secular trend in LAZ, WAZ, or WLZ. 


\includegraphics[width=33.33in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/shared/waz_secular_trend_monthly} 
\includegraphics[width=33.33in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/shared/laz_secular_trend_monthly} 
\includegraphics[width=33.33in]{C:/Users/andre/Documents/HBGDki/wasting/ki-longitudinal-manuscripts/figures/shared/wlz_secular_trend_monthly} 




<!--chapter:end:05-anthro-QC.Rmd-->

