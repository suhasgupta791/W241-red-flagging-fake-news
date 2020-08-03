Red Flagging Fake News
================
Suhas Gupta, Kevin Drever, Imran Manji





``` r
# load packages
library(data.table)
library(foreign)
library(sandwich)
```

    ## Warning: package 'sandwich' was built under R version 4.0.2

``` r
library(lmtest)
```

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
library(stargazer)
```

    ## Warning: package 'stargazer' was built under R version 4.0.2

    ## 
    ## Please cite as:

    ##  Hlavac, Marek (2018). stargazer: Well-Formatted Regression and Summary Statistics Tables.

    ##  R package version 5.2.2. https://CRAN.R-project.org/package=stargazer

``` r
library(lfe)
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'lfe'

    ## The following object is masked from 'package:lmtest':
    ## 
    ##     waldtest

``` r
library(car)
```

    ## Loading required package: carData

``` r
library(ggplot2)
library(data.table)
library(knitr)
# options(kableExtra.latex.load_packages = FALSE)
library(kableExtra)
```

    ## Warning: package 'kableExtra' was built under R version 4.0.2

``` r
library(rgeolocate)
```

    ## Warning: package 'rgeolocate' was built under R version 4.0.2

``` r
library(data.table)
library(knitr)
library(lmtest)
library(ri2)
```

    ## Loading required package: randomizr

    ## Warning: package 'randomizr' was built under R version 4.0.2

    ## Loading required package: estimatr

    ## Warning: package 'estimatr' was built under R version 4.0.2

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following object is masked from 'package:kableExtra':
    ## 
    ##     group_rows

    ## The following object is masked from 'package:car':
    ## 
    ##     recode

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(forcats)
```

## Null & Alternate Hypothesis

  - *NULL Hypothesis* : **Make people aware of the prevalence of fake
    news has no effect on its believabiliy**
  - *Alternate Hypothesis* : **General flags about fake news reduce its
    believability**

## Calculating the sample size

In this section, we calculate the minimum required sample size for our
experiment.

The statistical power of an experiment is the experiment’s abilitiy to
reject the NULL hypothesis when a specific alternate hypothesis is true.

\[ \alpha = P(\text{reject}\ H_0 | H_0) \]

where \(\alpha\) is the significance level. We select a significance
level of \(\alpha = 0.05\) as a tolerance for Type I errors in our
experiment.

Now that we have chosen our significance level, we would like to
minimize the probability of Type II error. i.e. we would like to
**maximize** the power of our test against the relevan alternative.
Mathematically, power
is

\[ power = 1 - P_r(\text{Type II Error}) = P_r(\text{reject}\ H_0 | H_1 \text{is true}) \]

  - We would set the required power of our experiment to be **80%** for
    this study as a reasonable expectation.
  - To calculate the power for the test, we need to conjecture an
    expected ATE and the standard deviation for the outcome in the
    experiment.
  - The outcome is a rating on a scale of 0-10 on how successfull the
    red flag was in reducing the believability of the fake/misleading
    social media post. We would like our experiment to be able detect a
    difference in means of minimum 2 points on this scale.
  - We do expect the measured values for this rating to vary
    significantly as we poll subjects with different political opinions,
    life experiences and political affiliations. To be on the
    conservative side, we would like to have enough power in our
    experiment to minimize Type II errors when the std. deviation is at
    least 2.5 times the minimum detectable treatment
effect.

<!-- end list -->

``` r
power_sim <- function(ate,sig_level=0.05,power=0.8,alternate_hyp="two.sided", sd = 1){
    result <- NA
    sims <- seq(1e-5,sd,by=0.1)
    for(i in seq_along(sims)){
        result[i] <- power.t.test(d=ate, 
                               sig.level=sig_level, 
                               power=power,
                               sd=sims[i],
                               alternative=alternate_hyp)$n
        }
    return(result)
    }
sd <- 3
expected_ate <- 0.5
x <- seq(1e-5,sd, by = 0.1)
samples <- power_sim(ate=expected_ate,sd = sd)
plot(x = x, y=samples,col = 'blue',
     xlab="Std. Dev",
     ylab = 'Number of subjects',
     main = "Sample size vs. expected variance in outcome (Power = 0.8)")
abline(v=1.0,col='black',lwd=1)
```

![](Final_project_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

The above plot shows that we need a minimum sample size of 100 to
achieve a power of 0.8 when the outcome variable has a standard
deviation 1.0 times the treatment effect. The plot below, validates that
the absolute value of minimum treatment effect doesn’t change the sample
size requirement significantly and that this is determined mostly by the
expected variance in the measurement
data.

``` r
power_sim_by_ate <- function(ate_vector,sig_level=0.05,power=0.8,alternate_hyp="two.sided",sd = 1){
    result <- NA
    for(i in seq_along(ate_vector)){
        result[i] <- power.t.test(d=ate_vector[i], 
                               sig.level=sig_level, 
                               power=power,
                               sd=sd,
                               alternative=alternate_hyp)$n
        }
    return(result)
    }
sd <- 1
expected_ate <- 3
x <- seq(1e-5,expected_ate,by=0.01)
samples <- power_sim_by_ate(ate=x,sd = sd)

plot(x = x, y=samples,col = 'blue',
     xlab= "Desired treatment effect",
     ylab = 'Number of samples',ylim=c(0,100),
     main = "Sample size vs. minimum detectable treatment effect (Power = 0.8)")
abline(v=0.5,col='black',lwd=1)
```

![](Final_project_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## Covariate questions in the survey

  - Age
  - Political affiliation
  - Registered Voter / non-voter
  - race
  - are you active on social media?  
  - education (\< high school, high school , undergrad, grad)

## Covariates in regression (not in survey)

  - Mturk subject
  - location of subject

## Experimental Design

### 2 x 2

  - treatment :
      - banner or no banner
      - tweet is false or true
  - block by party affiliation and gender

### Treatment & control assginment

  - how to randomly assign while blocking for
above

## Regression Models

### Outcome: Score on how many headlines were correctly identified by subjects (equally balanced True and False posts)

1.  Baseline model

outcome ~ general\_flag on survey page

2.  Model with co-variates

outcome ~ red\_flag \* gender + red\_flag \* political\_affiliation +
factor(age\_group) + factor(education) + red\_flag \* location +
registered\_voter + race + social\_media\_active

3.  Model with treatment-covariate interactions
    
      - Test if fake news red flagging affects democrats and republicans
        differently
      - Test if fake news red flagging affects different age groups
        differently
      - Test if fake news red flagging affects voters and non voters
        differently

### Hypothesis

The primary hypothesis and effect that we have set out to test is the
following:

**H1: Reminding subjects about possiblity of misleading tweets using a
genral warning will reduce the perceived accuracy of false headlines
relative to a no-warning scenario.**

We also want to check the spillover effect of this general warning about
fake news on people’s trust in true news/headlines.

**H2: Reminding subjects about possiblity of misleading tweets using a
genral warning will also reduce their trust in true headlines/news
relative to a no-warning scenario.**

### Experimental Method

#### Participants

The study was conducted online through survey forms created using the
Qualtric Survey service provided to us by the Unversity of California,
Berkeley. There were two types of pariticipants recruited for this study
:

1.  Participants recruited using the Amazon Mechanical Turk service

<!-- end list -->

  - Although samples from Mturk are not nationally respresentative,
    results from the study closely match those obtained from other
    samples(e.g., Berinsky et al. 2012; Coppock 2016; Horton et
    al. 2011; Mullinix et al. 2015)
  - Non-US residents, were not allowed to participate.

<!-- end list -->

2.  Participants recruited through experimenters’ personal and
    professional network using personal contacts, direct messages,
    social media network and email.

***\< Fill in the text about sample distribution from the data
analysis\>***

#### Procedure

The experiment design used individual random assignment to place
subjects in treatment and control. Table 1 shows the distribution of
subject randomly assigned to treatment and control for each of the
participant group described above. The survey would display warning
before presenting the test questions to a subjects if the subject was
placed in the treatment group. If the subject was placed in the control
group, then no warning would be displayed on the questions. We focused
on posts made on the social media platform *Twitter* as the source for
all headlines used in the survey. The tweets were mainly from three
broad categories 1) US politics 2) Climate change and general belief in
science 3) Random facts about US. After each headline, the question
asked the participant whether they believe the information in the
headline is true or not. The scoring was based on the total number of
correct responses (responses that match the group truth about each
headline).

``` r
col1 <- c("Treatment","Control")
col2 <- c("General Warning","No Warning")
col3 <- c("X","Y")
dt<- data.table(col1,col2,col3)

colnames(dt) <- c("Assignment Group", "Flag", "N")
kable(dt,"latex",booktabs=T)
```

### Pilot data analysis

``` r
pilot_dataset <- fread('./data/pilot/pilot_data_07262020.csv')
#head(pilot_dataset)
#names(pilot_dataset)
```

``` r
data_pruned <- pilot_dataset[ 3:nrow(pilot_dataset),]
data_pruned[, c(6,7)] <- lapply(data_pruned[, c(6,7)], as.numeric)
```

    ## Warning in lapply(data_pruned[, c(6, 7)], as.numeric): NAs introduced by
    ## coercion

``` r
question_col_names <- c('8B','9B','10B','11B','12B','13B','14B','15B','16B','17B',
                        '8A','9A','10A','11A','12A','13A','14A','15A','16A','17A')


# replace all empty strings with NA
for(i in c(26:length(names(data_pruned)))){
    data_pruned[[i]][data_pruned[[i]]==''] <- NA
}

# Check the data
head(data_pruned[, 26:length(names(data_pruned))])
```

    ##      8B   9B  10B  11B  12B  13B  14B  15B  16B  17B   8A   9A  10A  11A  12A
    ## 1: <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>  Yes  Yes   No  Yes  Yes
    ## 2:  Yes  Yes   No  Yes  Yes   No  Yes   No   No   No <NA> <NA> <NA> <NA> <NA>
    ## 3: <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>  Yes  Yes   No  Yes   No
    ## 4:   No  Yes   No  Yes   No   No  Yes  Yes   No   No <NA> <NA> <NA> <NA> <NA>
    ## 5: <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>  Yes  Yes   No  Yes   No
    ## 6: <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>   No  Yes   No  Yes   No
    ##     13A  14A  15A  16A  17A
    ## 1:   No  Yes  Yes   No   No
    ## 2: <NA> <NA> <NA> <NA> <NA>
    ## 3:   No   No  Yes   No   No
    ## 4: <NA> <NA> <NA> <NA> <NA>
    ## 5:   No  Yes  Yes   No   No
    ## 6:   No   No   No   No   No

``` r
# Set assignment group variable (treatment = 1 , conrol = 0)
data_pruned[, assignment := ifelse(is.na(data_pruned[,'8B']),0,1)]
```

    ## Warning in `[.data.table`(data_pruned, , `:=`(assignment,
    ## ifelse(is.na(data_pruned[, : Invalid .internal.selfref detected and fixed by
    ## taking a (shallow) copy of the data.table so that := can add this new column
    ## by reference. At an earlier point, this data.table has been copied by R (or
    ## was created manually using structure() or similar). Avoid names<- and attr<-
    ## which in R currently (and oddly) may copy the whole data.table. Use set* syntax
    ## instead to avoid copying: ?set, ?setnames and ?setattr. If this message doesn't
    ## help, please report your use case to the data.table issue tracker so the root
    ## cause can be fixed or this message improved.

``` r
head(data_pruned[, 26:length(names(data_pruned))])
```

    ##      8B   9B  10B  11B  12B  13B  14B  15B  16B  17B   8A   9A  10A  11A  12A
    ## 1: <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>  Yes  Yes   No  Yes  Yes
    ## 2:  Yes  Yes   No  Yes  Yes   No  Yes   No   No   No <NA> <NA> <NA> <NA> <NA>
    ## 3: <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>  Yes  Yes   No  Yes   No
    ## 4:   No  Yes   No  Yes   No   No  Yes  Yes   No   No <NA> <NA> <NA> <NA> <NA>
    ## 5: <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>  Yes  Yes   No  Yes   No
    ## 6: <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>   No  Yes   No  Yes   No
    ##     13A  14A  15A  16A  17A assignment
    ## 1:   No  Yes  Yes   No   No          0
    ## 2: <NA> <NA> <NA> <NA> <NA>          1
    ## 3:   No   No  Yes   No   No          0
    ## 4: <NA> <NA> <NA> <NA> <NA>          1
    ## 5:   No  Yes  Yes   No   No          0
    ## 6:   No   No   No   No   No          0

``` r
head(data_pruned)
```

    ##              StartDate             EndDate     Status      IPAddress Progress
    ## 1: 2020-07-17 18:52:45 2020-07-17 18:59:24 IP Address   73.93.90.157      100
    ## 2: 2020-07-17 19:46:06 2020-07-17 19:48:33 IP Address  98.234.117.52      100
    ## 3: 2020-07-17 19:48:21 2020-07-17 19:59:28 IP Address 71.244.172.196      100
    ## 4: 2020-07-17 20:01:07 2020-07-17 20:04:12 IP Address   173.67.9.152      100
    ## 5: 2020-07-17 19:58:48 2020-07-17 20:13:25 IP Address 174.195.207.41      100
    ## 6: 2020-07-17 20:08:21 2020-07-17 20:17:59 IP Address  67.188.128.89      100
    ##    Duration (in seconds) Finished        RecordedDate        ResponseId
    ## 1:                   399       NA 2020-07-17 18:59:25 R_u8Geu0CykTxNh6h
    ## 2:                   147       NA 2020-07-17 19:48:34 R_2EgXOU7K2nTlgqG
    ## 3:                   666       NA 2020-07-17 19:59:28 R_1NgJqwlFgUpLjT1
    ## 4:                   185       NA 2020-07-17 20:04:13 R_33woiiDhvnhBZZR
    ## 5:                   876       NA 2020-07-17 20:13:25 R_3nAiDFypCLOOxYV
    ## 6:                   578       NA 2020-07-17 20:18:00 R_3QXbtPAqPrxpNRD
    ##    RecipientLastName RecipientFirstName RecipientEmail ExternalReference
    ## 1:                                                                      
    ## 2:                                                                      
    ## 3:                                                                      
    ## 4:                                                                      
    ## 5:                                                                      
    ## 6:                                                                      
    ##       LocationLatitude      LocationLongitude DistributionChannel UserLanguage
    ## 1:     37.777099609375 -122.40599822998046875           anonymous           EN
    ## 2:    36.5802001953125    -121.84429931640625           anonymous           EN
    ## 3: 39.1269073486328125       -76.697998046875           anonymous           EN
    ## 4: 39.1269073486328125       -76.697998046875           anonymous           EN
    ## 5:         33.87890625 -117.53530120849609375           anonymous           EN
    ## 6: 36.6808013916015625 -121.61640167236328125           anonymous           EN
    ##                                           Q_RecaptchaScore     Q1  Q2    Q3
    ## 1: 0.90000000000000002220446049250313080847263336181640625 Female Yes 21-40
    ## 2: 0.90000000000000002220446049250313080847263336181640625 Female Yes 21-40
    ## 3: 0.90000000000000002220446049250313080847263336181640625 Female Yes 21-40
    ## 4: 0.90000000000000002220446049250313080847263336181640625 Female Yes 21-40
    ## 5: 0.90000000000000002220446049250313080847263336181640625 Female Yes 21-40
    ## 6: 0.90000000000000002220446049250313080847263336181640625   Male Yes   61+
    ##            Q4              Q5                Q6  Q7   8B   9B  10B  11B  12B
    ## 1:   Democrat Graduate degree             Asian Yes <NA> <NA> <NA> <NA> <NA>
    ## 2:      Other Graduate degree         Caucasian  No  Yes  Yes   No  Yes  Yes
    ## 3:             College degree         Caucasian  No <NA> <NA> <NA> <NA> <NA>
    ## 4:   Democrat  College degree Hispanic / Latinx  No   No  Yes   No  Yes   No
    ## 5:      Other  College degree         Caucasian Yes <NA> <NA> <NA> <NA> <NA>
    ## 6: Republican Graduate degree         Caucasian  No <NA> <NA> <NA> <NA> <NA>
    ##     13B  14B  15B  16B  17B   8A   9A  10A  11A  12A  13A  14A  15A  16A  17A
    ## 1: <NA> <NA> <NA> <NA> <NA>  Yes  Yes   No  Yes  Yes   No  Yes  Yes   No   No
    ## 2:   No  Yes   No   No   No <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>
    ## 3: <NA> <NA> <NA> <NA> <NA>  Yes  Yes   No  Yes   No   No   No  Yes   No   No
    ## 4:   No  Yes  Yes   No   No <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>
    ## 5: <NA> <NA> <NA> <NA> <NA>  Yes  Yes   No  Yes   No   No  Yes  Yes   No   No
    ## 6: <NA> <NA> <NA> <NA> <NA>   No  Yes   No  Yes   No   No   No   No   No   No
    ##    assignment
    ## 1:          0
    ## 2:          1
    ## 3:          0
    ## 4:          1
    ## 5:          0
    ## 6:          0

``` r
# Compute the score against answer guide
answer_guide <- c('Yes','Yes','No','Yes','No','No','Yes','Yes','No','No',
                  'Yes','Yes','No','Yes','No','No','Yes','Yes','No','No')

compute_scores <- function(dataset,answer_guide){
    for(i in 1:nrow(data_pruned)){
    dataset[i,"score"] <- sum(dataset[i,26:45] == answer_guide,na.rm=TRUE)
    }
    return(dataset)
}


data_w_scores <- compute_scores(data_pruned,answer_guide)
head(data_w_scores)
```

    ##              StartDate             EndDate     Status      IPAddress Progress
    ## 1: 2020-07-17 18:52:45 2020-07-17 18:59:24 IP Address   73.93.90.157      100
    ## 2: 2020-07-17 19:46:06 2020-07-17 19:48:33 IP Address  98.234.117.52      100
    ## 3: 2020-07-17 19:48:21 2020-07-17 19:59:28 IP Address 71.244.172.196      100
    ## 4: 2020-07-17 20:01:07 2020-07-17 20:04:12 IP Address   173.67.9.152      100
    ## 5: 2020-07-17 19:58:48 2020-07-17 20:13:25 IP Address 174.195.207.41      100
    ## 6: 2020-07-17 20:08:21 2020-07-17 20:17:59 IP Address  67.188.128.89      100
    ##    Duration (in seconds) Finished        RecordedDate        ResponseId
    ## 1:                   399       NA 2020-07-17 18:59:25 R_u8Geu0CykTxNh6h
    ## 2:                   147       NA 2020-07-17 19:48:34 R_2EgXOU7K2nTlgqG
    ## 3:                   666       NA 2020-07-17 19:59:28 R_1NgJqwlFgUpLjT1
    ## 4:                   185       NA 2020-07-17 20:04:13 R_33woiiDhvnhBZZR
    ## 5:                   876       NA 2020-07-17 20:13:25 R_3nAiDFypCLOOxYV
    ## 6:                   578       NA 2020-07-17 20:18:00 R_3QXbtPAqPrxpNRD
    ##    RecipientLastName RecipientFirstName RecipientEmail ExternalReference
    ## 1:                                                                      
    ## 2:                                                                      
    ## 3:                                                                      
    ## 4:                                                                      
    ## 5:                                                                      
    ## 6:                                                                      
    ##       LocationLatitude      LocationLongitude DistributionChannel UserLanguage
    ## 1:     37.777099609375 -122.40599822998046875           anonymous           EN
    ## 2:    36.5802001953125    -121.84429931640625           anonymous           EN
    ## 3: 39.1269073486328125       -76.697998046875           anonymous           EN
    ## 4: 39.1269073486328125       -76.697998046875           anonymous           EN
    ## 5:         33.87890625 -117.53530120849609375           anonymous           EN
    ## 6: 36.6808013916015625 -121.61640167236328125           anonymous           EN
    ##                                           Q_RecaptchaScore     Q1  Q2    Q3
    ## 1: 0.90000000000000002220446049250313080847263336181640625 Female Yes 21-40
    ## 2: 0.90000000000000002220446049250313080847263336181640625 Female Yes 21-40
    ## 3: 0.90000000000000002220446049250313080847263336181640625 Female Yes 21-40
    ## 4: 0.90000000000000002220446049250313080847263336181640625 Female Yes 21-40
    ## 5: 0.90000000000000002220446049250313080847263336181640625 Female Yes 21-40
    ## 6: 0.90000000000000002220446049250313080847263336181640625   Male Yes   61+
    ##            Q4              Q5                Q6  Q7   8B   9B  10B  11B  12B
    ## 1:   Democrat Graduate degree             Asian Yes <NA> <NA> <NA> <NA> <NA>
    ## 2:      Other Graduate degree         Caucasian  No  Yes  Yes   No  Yes  Yes
    ## 3:             College degree         Caucasian  No <NA> <NA> <NA> <NA> <NA>
    ## 4:   Democrat  College degree Hispanic / Latinx  No   No  Yes   No  Yes   No
    ## 5:      Other  College degree         Caucasian Yes <NA> <NA> <NA> <NA> <NA>
    ## 6: Republican Graduate degree         Caucasian  No <NA> <NA> <NA> <NA> <NA>
    ##     13B  14B  15B  16B  17B   8A   9A  10A  11A  12A  13A  14A  15A  16A  17A
    ## 1: <NA> <NA> <NA> <NA> <NA>  Yes  Yes   No  Yes  Yes   No  Yes  Yes   No   No
    ## 2:   No  Yes   No   No   No <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>
    ## 3: <NA> <NA> <NA> <NA> <NA>  Yes  Yes   No  Yes   No   No   No  Yes   No   No
    ## 4:   No  Yes  Yes   No   No <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>
    ## 5: <NA> <NA> <NA> <NA> <NA>  Yes  Yes   No  Yes   No   No  Yes  Yes   No   No
    ## 6: <NA> <NA> <NA> <NA> <NA>   No  Yes   No  Yes   No   No   No   No   No   No
    ##    assignment score
    ## 1:          0     9
    ## 2:          1     8
    ## 3:          0     9
    ## 4:          1     9
    ## 5:          0    10
    ## 6:          0     7

``` r
# Compute the SD and point estimates with pilot data
sd_pilot <- data_w_scores[, sd(score)]
sd_pilot
```

    ## [1] 1.3

``` r
d <- data_w_scores[, .(scores=mean(score)), by = assignment]
mod <- lm(score ~ assignment, data_w_scores)
ate <- diff(d$scores)
ate
```

    ## [1] -0.27

``` r
stargazer(mod, type="text")
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                                score           
    ## -----------------------------------------------
    ## assignment                    -0.270           
    ##                               (0.540)          
    ##                                                
    ## Constant                     7.900***          
    ##                               (0.360)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                    26             
    ## R2                             0.011           
    ## Adjusted R2                   -0.030           
    ## Residual Std. Error       1.400 (df = 24)      
    ## F Statistic             0.260 (df = 1; 24)     
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

``` r
## Power calculation 
power.t.test(d=ate,sig.level=0.95,power=0.8,sd=sd_pilot,alternative="two.sided")
```

    ## 
    ##      Two-sample t test power calculation 
    ## 
    ##               n = 39
    ##           delta = 0.27
    ##              sd = 1.3
    ##       sig.level = 0.95
    ##           power = 0.8
    ##     alternative = two.sided
    ## 
    ## NOTE: n is number in *each* group

``` r
# Modify the column names for better readability
data_mod <- rename(data_w_scores, 
       Gender = Q1,
       Reg_Voter = Q2,
       Age_bin = Q3,
       Party = Q4,
       Education = Q5,
       Ethnicity = Q6,
       Soc_Med_Active = Q7
        )
head(data_mod[,19:27])
```

    ##    Gender Reg_Voter Age_bin      Party       Education         Ethnicity
    ## 1: Female       Yes   21-40   Democrat Graduate degree             Asian
    ## 2: Female       Yes   21-40      Other Graduate degree         Caucasian
    ## 3: Female       Yes   21-40             College degree         Caucasian
    ## 4: Female       Yes   21-40   Democrat  College degree Hispanic / Latinx
    ## 5: Female       Yes   21-40      Other  College degree         Caucasian
    ## 6:   Male       Yes     61+ Republican Graduate degree         Caucasian
    ##    Soc_Med_Active   8B   9B
    ## 1:            Yes <NA> <NA>
    ## 2:             No  Yes  Yes
    ## 3:             No <NA> <NA>
    ## 4:             No   No  Yes
    ## 5:            Yes <NA> <NA>
    ## 6:             No <NA> <NA>

## Define functions

``` r
prune_data <- function(dataset){
    data_pruned <- dataset[ 3:nrow(dataset),]
    data_pruned[, c(6,7)] <- lapply(data_pruned[, c(6,7)], as.numeric)
    question_col_names <- c('8B','9B','10B','11B','12B','13B','14B','15B','16B','17B',
                            '8A','9A','10A','11A','12A','13A','14A','15A','16A','17A')
    # Set NA for empty empty strings in question columns (either in treatment or control but not both)
    for(i in c(31:length(names(data_pruned)))){
        data_pruned[[i]][data_pruned[[i]]==''] <- NA
    }
    # Set assignment group variable (treatment = 1 , control = 0)
    data_pruned[, assignment := ifelse(is.na(data_pruned[,'8B']),0,1)]
  
    return(data_pruned)
}

compute_score <- function(dataset,answer_guide){
  # compute full score  
  for(i in 1:nrow(dataset)){
          dataset[i,"score"] <- sum(dataset[i,31:50] == answer_guide,na.rm=TRUE)
          dataset[i, "score_false"] <- sum(dataset[i,c(33,35,36,39,40,43,45,46,49,50)] == answer_guide[c(3,5,6,9,10,13,15,16,19,20)], na.rm=TRUE)
          dataset[i, "score_true"] <- sum(dataset[i,c(31,32,34,37,38,41,42,44,47,48)] == answer_guide[c(1,2,4,7,8,11,12,14,17,18)], na.rm=TRUE)
    }
  return(dataset)
}

rename_cols <- function(dataset){
    dt <- rename(dataset, 
     Gender = Q1,
     Reg_Voter = Q2,
     Age_bin = Q3,
     Party = Q4,
     Education = Q5,
     Ethnicity = Q6,
     Soc_Med_Active = Q7,
     Voted_2012 = Q38,
     Voted_2016 = Q39,
     Marital_status = Q37,
     Income = Q36,
     Language = Q40
    
      )
  
     dt$Gender[dataset$Gender == ''] <-"Unanswered"
     return(dt)
}
```

#### Import data

Study 1: Mturk survey 1 was done with a higher reward and no check for
BOTs. The survey subject count was 104 and responses were obtained
within 24 hours due to the high reward (5-8 min task paid $1.5).

Study 2: Mturk survey 2 was done with a higher reward and no check for
BOTs. The survey subject count was 132 and responses were obtained over
a period of 5 days

Study3: Personal and professional network of the experimenters

``` r
study1_data <- fread('./data/Mturk_nocaptcha/data.csv')
study2_data <- fread('./data/Mturk_captcha/data.csv')
study3_data <- fread("./data/NonMturk_wCaptcha/data.csv")
# head(study1_data)
# head(study2_data)
# head(study3_data)
# names(study1_data)[31:51]
# names(study2_data)[31:51]
# names(study3_data)[31:51]
```

#### Modify data

``` r
study1_data_pruned <- prune_data(study1_data)
```

    ## Warning in lapply(data_pruned[, c(6, 7)], as.numeric): NAs introduced by
    ## coercion

    ## Warning in `[.data.table`(data_pruned, , `:=`(assignment,
    ## ifelse(is.na(data_pruned[, : Invalid .internal.selfref detected and fixed by
    ## taking a (shallow) copy of the data.table so that := can add this new column
    ## by reference. At an earlier point, this data.table has been copied by R (or
    ## was created manually using structure() or similar). Avoid names<- and attr<-
    ## which in R currently (and oddly) may copy the whole data.table. Use set* syntax
    ## instead to avoid copying: ?set, ?setnames and ?setattr. If this message doesn't
    ## help, please report your use case to the data.table issue tracker so the root
    ## cause can be fixed or this message improved.

``` r
study2_data_pruned <- prune_data(study2_data)
```

    ## Warning in lapply(data_pruned[, c(6, 7)], as.numeric): NAs introduced by
    ## coercion
    
    ## Warning in lapply(data_pruned[, c(6, 7)], as.numeric): Invalid .internal.selfref
    ## detected and fixed by taking a (shallow) copy of the data.table so that :=
    ## can add this new column by reference. At an earlier point, this data.table
    ## has been copied by R (or was created manually using structure() or similar).
    ## Avoid names<- and attr<- which in R currently (and oddly) may copy the whole
    ## data.table. Use set* syntax instead to avoid copying: ?set, ?setnames and ?
    ## setattr. If this message doesn't help, please report your use case to the
    ## data.table issue tracker so the root cause can be fixed or this message
    ## improved.

``` r
study3_data_pruned <- prune_data(study3_data)
```

    ## Warning in lapply(data_pruned[, c(6, 7)], as.numeric): NAs introduced by
    ## coercion
    
    ## Warning in lapply(data_pruned[, c(6, 7)], as.numeric): Invalid .internal.selfref
    ## detected and fixed by taking a (shallow) copy of the data.table so that :=
    ## can add this new column by reference. At an earlier point, this data.table
    ## has been copied by R (or was created manually using structure() or similar).
    ## Avoid names<- and attr<- which in R currently (and oddly) may copy the whole
    ## data.table. Use set* syntax instead to avoid copying: ?set, ?setnames and ?
    ## setattr. If this message doesn't help, please report your use case to the
    ## data.table issue tracker so the root cause can be fixed or this message
    ## improved.

``` r
# Rename the covariate columns
study1_data_mod <- rename_cols(study1_data_pruned)
study2_data_mod <- rename_cols(study2_data_pruned)
study3_data_mod <- rename_cols(study3_data_pruned)

# Add score columns 
answer_guide <- c('Yes','Yes','No','Yes','No','No','Yes','Yes','No','No',  
                  'Yes','Yes','No','Yes','No','No','Yes','Yes','No','No') 

study1_data_mod <- compute_score(study1_data_mod, answer_guide = answer_guide )
study2_data_mod <- compute_score(study2_data_mod, answer_guide = answer_guide )
study3_data_mod <- compute_score(study3_data_mod, answer_guide = answer_guide )

# Add indicator variables
study1_data_mod[, Mturk := 1]
study1_data_mod[, captcha := 0]

study2_data_mod[, Mturk := 1]
study2_data_mod[, captcha := 1]


study3_data_mod[, Mturk := 0]
study3_data_mod[, captcha := 1]

# Check the data
# head(study1_data_mod[, 31:length(names(study1_data_mod))])
head(study2_data_mod)
```

    ##              StartDate             EndDate     Status      IPAddress Progress
    ## 1: 2020-07-25 20:18:27 2020-07-25 20:20:31 IP Address 47.196.110.241      100
    ## 2: 2020-07-25 20:20:14 2020-07-25 20:21:01 IP Address   68.191.33.60      100
    ## 3: 2020-07-25 20:21:00 2020-07-25 20:23:03 IP Address 136.24.243.198      100
    ## 4: 2020-07-25 20:23:24 2020-07-25 20:27:34 IP Address   76.91.179.34      100
    ## 5: 2020-07-25 20:30:35 2020-07-25 20:34:52 IP Address 73.189.103.206      100
    ## 6: 2020-07-25 20:42:25 2020-07-25 20:45:25 IP Address 67.188.112.154      100
    ##    Duration (in seconds) Finished        RecordedDate        ResponseId
    ## 1:                   124       NA 2020-07-25 20:20:32 R_2zpQP14fLuuhfoW
    ## 2:                    47       NA 2020-07-25 20:21:01 R_2YGr5mSM9w5R549
    ## 3:                   122       NA 2020-07-25 20:23:03 R_2OGVuLEQeP89B0y
    ## 4:                   250       NA 2020-07-25 20:27:35 R_1IpizIjkU9jHhYL
    ## 5:                   256       NA 2020-07-25 20:34:53 R_2uKyuFbRClKReQG
    ## 6:                   179       NA 2020-07-25 20:45:25 R_1nTTrweksgEQjuh
    ##    RecipientLastName RecipientFirstName RecipientEmail ExternalReference
    ## 1:                                                                      
    ## 2:                                                                      
    ## 3:                                                                      
    ## 4:                                                                      
    ## 5:                                                                      
    ## 6:                                                                      
    ##       LocationLatitude      LocationLongitude DistributionChannel UserLanguage
    ## 1:  27.944793701171875  -82.24089813232421875           anonymous           EN
    ## 2: 41.3318939208984375  -73.23670196533203125           anonymous           EN
    ## 3: 37.7642059326171875 -122.39929962158203125           anonymous           EN
    ## 4: 34.0312957763671875 -118.31240081787109375           anonymous           EN
    ## 5: 37.4774932861328125    -122.45050048828125           anonymous           EN
    ## 6: 37.3896942138671875 -122.08319854736328125           anonymous           EN
    ##                                           Q_RecaptchaScore Gender Reg_Voter
    ## 1: 0.90000000000000002220446049250313080847263336181640625   Male       Yes
    ## 2: 0.90000000000000002220446049250313080847263336181640625   Male       Yes
    ## 3: 0.90000000000000002220446049250313080847263336181640625 Female       Yes
    ## 4: 0.90000000000000002220446049250313080847263336181640625   Male       Yes
    ## 5: 0.90000000000000002220446049250313080847263336181640625   Male       Yes
    ## 6: 0.90000000000000002220446049250313080847263336181640625 Female       Yes
    ##    Voted_2012 Voted_2016 Marital_status Language            Income Age_bin
    ## 1:         No        Yes         Single  English          < $60000   21-40
    ## 2:         No         No         Single  Spanish         > $250000   21-40
    ## 3:        Yes        Yes         Single  English          < $60000   21-40
    ## 4:        Yes        Yes         Single  English  $60000 - $150000   21-40
    ## 5:        Yes        Yes        Married  English  $60000 - $150000   41-60
    ## 6:         No        Yes         Single  English $150000 - $250000   21-40
    ##       Party            Education       Ethnicity Soc_Med_Active   8B   9B  10B
    ## 1: Democrat High school graduate       Caucasian            Yes <NA> <NA> <NA>
    ## 2: Democrat High school graduate Black / African            Yes <NA> <NA> <NA>
    ## 3: Democrat       College degree           Asian             No  Yes  Yes   No
    ## 4: Democrat         Some college Black / African            Yes <NA> <NA> <NA>
    ## 5: Democrat       College degree           Asian            Yes   No  Yes   No
    ## 6: Democrat       College degree       Caucasian            Yes <NA> <NA> <NA>
    ##     11B  12B  13B  14B  15B  16B  17B   8A   9A  10A  11A  12A  13A  14A  15A
    ## 1: <NA> <NA> <NA> <NA> <NA> <NA> <NA>  Yes  Yes  Yes  Yes   No   No  Yes  Yes
    ## 2: <NA> <NA> <NA> <NA> <NA> <NA> <NA>   No   No   No   No   No   No  Yes   No
    ## 3:  Yes   No   No  Yes  Yes   No   No <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>
    ## 4: <NA> <NA> <NA> <NA> <NA> <NA> <NA>   No  Yes   No   No   No   No  Yes  Yes
    ## 5:  Yes   No  Yes  Yes  Yes  Yes   No <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>
    ## 6: <NA> <NA> <NA> <NA> <NA> <NA> <NA>  Yes  Yes   No   No  Yes   No  Yes   No
    ##     16A  17A SC0 assignment score score_false score_true Mturk captcha
    ## 1:   No   No   9          0     9           4          5     1       1
    ## 2:   No  Yes   5          0     5           4          1     1       1
    ## 3: <NA> <NA>  10          1    10           5          5     1       1
    ## 4:   No   No   8          0     8           5          3     1       1
    ## 5: <NA> <NA>   7          1     7           3          4     1       1
    ## 6:   No   No   7          0     7           4          3     1       1

``` r
# head(study3_data_mod)
```

#### Combine data sets from all studies

``` r
# combine data set 
data_full <- rbind(study1_data_mod,study2_data_mod,study3_data_mod)
head(data_full)
```

    ##        StartDate       EndDate     Status       IPAddress Progress
    ## 1: 7/24/20 22:43 7/24/20 22:45 IP Address   68.33.126.140      100
    ## 2: 7/24/20 22:43 7/24/20 22:45 IP Address   98.19.217.229      100
    ## 3: 7/24/20 22:43 7/24/20 22:45 IP Address  174.85.199.139      100
    ## 4: 7/24/20 22:43 7/24/20 22:45 IP Address 209.159.199.248      100
    ## 5: 7/24/20 22:43 7/24/20 22:46 IP Address    68.63.20.188      100
    ## 6: 7/24/20 22:43 7/24/20 22:46 IP Address   24.13.196.131      100
    ##    Duration (in seconds) Finished  RecordedDate        ResponseId
    ## 1:                   112       NA 7/24/20 22:45 R_2wHbTKc7249gZQY
    ## 2:                    99       NA 7/24/20 22:45 R_2Et95GjQbJ9BgaR
    ## 3:                   142       NA 7/24/20 22:45 R_3LimuwbiSdyNO53
    ## 4:                   152       NA 7/24/20 22:45 R_1rwQr9otPszxd5D
    ## 5:                   136       NA 7/24/20 22:46 R_4Jyqc4Ld2c4PwMF
    ## 6:                   172       NA 7/24/20 22:46 R_33jTNIqQiSmFSX3
    ##    RecipientLastName RecipientFirstName RecipientEmail ExternalReference
    ## 1:                                                                      
    ## 2:                                                                      
    ## 3:                                                                      
    ## 4:                                                                      
    ## 5:                                                                      
    ## 6:                                                                      
    ##    LocationLatitude LocationLongitude DistributionChannel UserLanguage
    ## 1:      38.86700439      -76.81729889           anonymous           EN
    ## 2:      34.45120239      -84.15299988           anonymous           EN
    ## 3:      34.34539795      -86.27400208           anonymous           EN
    ## 4:      44.14149475      -103.2052002           anonymous           EN
    ## 5:      30.49079895      -84.31580353           anonymous           EN
    ## 6:      42.18449402      -88.32659912           anonymous           EN
    ##    Q_RecaptchaScore Gender Reg_Voter Voted_2012 Voted_2016 Marital_status
    ## 1:              0.9 Female       Yes        Yes        Yes        Married
    ## 2:              0.7 Female       Yes        Yes        Yes        Married
    ## 3:              0.9 Female       Yes        Yes        Yes        Married
    ## 4:              0.9   Male       Yes        Yes        Yes         Single
    ## 5:              0.9   Male       Yes        Yes        Yes         Single
    ## 6:              0.9   Male       Yes        Yes        Yes        Married
    ##    Language            Income Age_bin      Party            Education
    ## 1:  English $150000 - $250000   21-40   Democrat      Graduate degree
    ## 2:  English  $60000 - $150000   21-40 Republican       College degree
    ## 3:  English          < $60000   21-40      Other         Some college
    ## 4:  English          < $60000   21-40      Other High school graduate
    ## 5:  English          < $60000   21-40   Democrat       College degree
    ## 6:  English  $60000 - $150000   41-60 Republican       College degree
    ##            Ethnicity Soc_Med_Active   8B   9B  10B  11B  12B  13B  14B  15B
    ## 1:         Caucasian            Yes <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>
    ## 2: Hispanic / Latinx            Yes <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>
    ## 3:         Caucasian            Yes <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>
    ## 4:         Caucasian            Yes   No  Yes   No  Yes  Yes   No  Yes   No
    ## 5:         Caucasian            Yes <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>
    ## 6:         Caucasian            Yes   No  Yes   No  Yes  Yes   No   No  Yes
    ##     16B  17B   8A   9A  10A  11A  12A  13A  14A  15A  16A  17A SC0 assignment
    ## 1: <NA> <NA>  Yes   No  Yes  Yes  Yes  Yes   No  Yes  Yes  Yes   3          0
    ## 2: <NA> <NA>  Yes  Yes  Yes   No   No  Yes   No  Yes   No  Yes   5          0
    ## 3: <NA> <NA>   No  Yes   No  Yes  Yes   No  Yes  Yes   No   No   8          0
    ## 4:   No   No <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>   7          1
    ## 5: <NA> <NA>   No  Yes   No   No   No   No   No   No   No   No   6          0
    ## 6:   No  Yes <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>   6          1
    ##    score score_false score_true Mturk captcha
    ## 1:     3           0          3     1       0
    ## 2:     5           2          3     1       0
    ## 3:     8           4          4     1       0
    ## 4:     7           4          3     1       0
    ## 5:     6           5          1     1       0
    ## 6:     6           3          3     1       0

### EDA

Figure 1 summarizes the score of subjects in each assignment group
(treatment/control) and for true and false
tweets.

``` r
dt <- data_full[, .(mean_total_score = mean(score),mean_true_score = mean(score_true),mean_false_score = mean(score_false)), by=assignment]
dt
```

    ##    assignment mean_total_score mean_true_score mean_false_score
    ## 1:          0              7.2             3.6              3.5
    ## 2:          1              7.0             3.4              3.6

``` r
p <- ggplot(dt, aes(x = assignment, y = mean_total_score)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  geom_text(aes(label = mean_total_score), size=3.5, color="white", vjust=1.5) + 
  ggtitle("Average score for all tweets by assignment group") +
  ylab("Average Total Score") + 
  xlab("Assignment Group \n (0 : Control | 1: Treatment)") + 
  theme_minimal()


p + scale_x_discrete(limits=c(0, 1))
```

![](Final_project_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
dfm <- melt(dt[,c('assignment','mean_true_score','mean_false_score')],id.vars = 1)
dfm <- rename(dfm, 
       Assignment = assignment,
       Score_Type = variable,
       Score = value)
p <- ggplot(dfm,aes(x = Assignment,y = Score)) + 
    geom_bar(aes(fill = Score_Type),stat = "identity",position = "dodge")


p <- ggplot(dfm,aes(x = Assignment,y = Score)) + 
    geom_bar(aes(fill = Score_Type),stat = "identity",position = "dodge") + 
  ggtitle("Average score for all tweets by assignment group") +
  ylab("Average Total Score") + 
  xlab("Assignment Group \n (0 : Control | 1: Treatment)") + 
  theme_minimal()


p + scale_x_discrete(limits=c(0, 1))
```

![](Final_project_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

The figure above shows that there is some improvement in scores for
false tweets in the presence of a general flag but there is also a
descrease in the score for true tweets in the presence of this flag.

``` r
sd <- data_full[, sd(score)]
d <- data_full[, .(scores=mean(score)), by = assignment]
mod1 <- lm(score ~ assignment, data_full)
mod2 <- lm(score ~ assignment+Mturk, data_full)
mod3 <- lm(score ~ assignment+Mturk+captcha, data_full)
ate <- diff(d$scores)
stargazer(mod1,mod2,mod3, type="text",ci=TRUE)
```

    ## 
    ## ======================================================================================
    ##                                            Dependent variable:                        
    ##                     ------------------------------------------------------------------
    ##                                                   score                               
    ##                             (1)                  (2)                     (3)          
    ## --------------------------------------------------------------------------------------
    ## assignment                -0.120                -0.120                 -0.120         
    ##                       (-0.480, 0.240)      (-0.480, 0.230)         (-0.440, 0.200)    
    ##                                                                                       
    ## Mturk                                         -0.820***                -0.140         
    ##                                            (-1.200, -0.410)        (-0.540, 0.260)    
    ##                                                                                       
    ## captcha                                                               1.600***        
    ##                                                                    (1.200, 1.900)     
    ##                                                                                       
    ## Constant                 7.200***              7.800***               6.200***        
    ##                       (6.900, 7.400)        (7.400, 8.200)         (5.700, 6.700)     
    ##                                                                                       
    ## --------------------------------------------------------------------------------------
    ## Observations                313                  313                     313          
    ## R2                         0.001                0.049                   0.220         
    ## Adjusted R2               -0.002                0.043                   0.210         
    ## Residual Std. Error  1.600 (df = 311)      1.600 (df = 310)       1.400 (df = 309)    
    ## F Statistic         0.420 (df = 1; 311) 8.100*** (df = 2; 310) 29.000*** (df = 3; 309)
    ## ======================================================================================
    ## Note:                                                      *p<0.1; **p<0.05; ***p<0.01

``` r
## Power calculation 
power.t.test(d=ate,sig.level=0.95,n=nrow(data_full),sd=sd,alternative="two.sided")
```

    ## 
    ##      Two-sample t test power calculation 
    ## 
    ##               n = 313
    ##           delta = 0.12
    ##              sd = 1.6
    ##       sig.level = 0.95
    ##           power = 0.8
    ##     alternative = two.sided
    ## 
    ## NOTE: n is number in *each* group

**A large portion of survey subjects said that they considered
themselves to be active on social media**

``` r
ggplot(data_full) + geom_bar(aes(x = Soc_Med_Active))
```

![](Final_project_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

**Alse, majority of survey subjects said that they were registered as a
voter**

``` r
ggplot(data_full) + geom_bar(aes(x = Reg_Voter))
```

![](Final_project_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

**The randomization worked well in the survey software and we had an
equal allocation to treatment and control groups in the experiment**

``` r
ggplot(data_full) + geom_bar(aes(x = assignment))
```

![](Final_project_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

**There does seem to be a slight skew in the distribution of
participant’s gender towards the Male gender (One subject did not
answer the gender question)**

``` r
ggplot(data_full) + geom_bar(aes(x = Gender))
```

![](Final_project_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

**Within each gender category, we see that the party affiliation is
approximately evenly distributed.**

``` r
data_full[, .N, by=.(Gender,Party)]
```

    ##    Gender      Party  N
    ## 1: Female   Democrat 90
    ## 2: Female Republican 37
    ## 3: Female      Other 21
    ## 4:   Male      Other 34
    ## 5:   Male   Democrat 82
    ## 6:   Male Republican 47
    ## 7:          Democrat  2

``` r
ggplot(data_full) + geom_bar(aes(x = Gender,fill=Party))
```

![](Final_project_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

**Our dataset does appear to consist mostly of people with atleast a
college degree or higher and the participants mostly belong to the 21-40
age
bucket.**

``` r
ggplot(data_full) + geom_bar(aes(x = Education,fill=Age_bin))
```

![](Final_project_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
ggplot(mutate(data_full, Age = fct_infreq(Age_bin))) + geom_bar(aes(x = Age_bin))
```

![](Final_project_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
data_mod[, .N, by=.(Party,Age_bin)]
```

    ##         Party Age_bin N
    ## 1:   Democrat   21-40 7
    ## 2:      Other   21-40 7
    ## 3:              21-40 2
    ## 4: Republican     61+ 1
    ## 5: Republican   21-40 4
    ## 6:   Democrat    0-20 2
    ## 7:      Other     61+ 1
    ## 8: Republican    0-20 1
    ## 9:   Democrat   41-60 1

In terms of ethinicity of the randomly sampled subjects, the majority
were Caucasian followed by approximately equal counts of Hispanic and
Native americans, followed by african americans and asians.

``` r
data_full[, .N, by=Ethnicity]
```

    ##            Ethnicity   N
    ## 1:         Caucasian 192
    ## 2: Hispanic / Latinx  21
    ## 3:   Native American  14
    ## 4:   Black / African  21
    ## 5:             Asian  58
    ## 6:             Other   7

``` r
ggplot(mutate(data_full, Ethnicity = fct_infreq(Ethnicity))) + geom_bar(aes(x = Ethnicity))
```

![](Final_project_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

``` r
compute_robust_ci<- function(mod,type="HC",clustering = FALSE,data=NA) { 
  coefs <- names(mod$coefficients)
  if (clustering){
    # calculate robust clustered standard errors 
    robust_se <- sqrt(diag(vcovCL(mod,cluster = data,type=type))) 
  }
  else{
    # calculate robust standard errors without clustering
    robust_se <- sqrt(diag(vcovHC(mod,type=type)))  
  }
  ci_ll <- NA
  ci_ul <- NA
  for(i in 1:length(coefs)){
    ci_ll[i] <- mod$coefficients[[coefs[i]]] - 1.96 * robust_se[i]
    ci_ul[i] <- mod$coefficients[[coefs[i]]] + 1.96 * robust_se[i]
  }
    ci_custom <- matrix(c(ci_ll,ci_ul), nrow = length(coefs), byrow = FALSE)
    return(ci_custom)
}

compute_robust_se<- function(mod,type="HC",clustering = FALSE,data=NA) { 
  coefs <- names(mod$coefficients)
  if (clustering){
    # calculate robust clustered standard errors 
    robust_se <- sqrt(diag(vcovCL(mod,cluster = data,type=type))) 
  }
  else{
    # calculate robust standard errors without clustering
    robust_se <- sqrt(diag(vcovHC(mod,type=type)))  
  }
  
    return(robust_se)
}
```

``` r
mod1 <- lm(score ~ assignment, data_full)
mod2 <- lm(score ~ assignment+factor(Party)*factor(Gender)+factor(Ethnicity)*factor(Gender)+factor(Age_bin), data_full)
ci_custom1 <- compute_robust_ci(mod1,type="HC3")
ci_custom2 <- compute_robust_ci(mod2,type="HC3")

stargazer(mod1,mod2, type="text",ci.custom=list(ci_custom1,ci_custom2))
```

    ## 
    ## ===================================================================================================
    ##                                                                     Dependent variable:            
    ##                                                         -------------------------------------------
    ##                                                                            score                   
    ##                                                                 (1)                   (2)          
    ## ---------------------------------------------------------------------------------------------------
    ## assignment                                                    -0.120                -0.140         
    ##                                                           (-0.480, 0.240)       (-0.490, 0.210)    
    ##                                                                                                    
    ## factor(Party)Other                                                                  0.760**        
    ##                                                                                 (0.110, 1.400)     
    ##                                                                                                    
    ## factor(Party)Republican                                                            -0.580**        
    ##                                                                                (-1.100, -0.023)    
    ##                                                                                                    
    ## factor(Gender)Female                                                                 0.650         
    ##                                                                                 (-2.400, 3.700)    
    ##                                                                                                    
    ## factor(Gender)Male                                                                   0.130         
    ##                                                                                 (-2.900, 3.200)    
    ##                                                                                                    
    ## factor(Ethnicity)Black / African                                                    -0.430         
    ##                                                                                 (-1.400, 0.580)    
    ##                                                                                                    
    ## factor(Ethnicity)Caucasian                                                          -1.000         
    ##                                                                                 (-5.300, 3.300)    
    ##                                                                                                    
    ## factor(Ethnicity)Hispanic / Latinx                                                  -0.380         
    ##                                                                                 (-1.400, 0.610)    
    ##                                                                                                    
    ## factor(Ethnicity)Native American                                                    -0.770         
    ##                                                                                 (-2.000, 0.450)    
    ##                                                                                                    
    ## factor(Ethnicity)Other                                                              -0.180         
    ##                                                                                 (-2.000, 1.700)    
    ##                                                                                                    
    ## factor(Age_bin)21-40                                                                -0.250         
    ##                                                                                 (-1.400, 0.940)    
    ##                                                                                                    
    ## factor(Age_bin)41-60                                                                -0.280         
    ##                                                                                 (-1.500, 0.940)    
    ##                                                                                                    
    ## factor(Age_bin)61+                                                                  -0.680         
    ##                                                                                 (-2.100, 0.770)    
    ##                                                                                                    
    ## factor(Party)Other:factor(Gender)Female                                             -0.360         
    ##                                                                                 (-1.400, 0.660)    
    ##                                                                                                    
    ## factor(Party)Republican:factor(Gender)Female                                        -0.440         
    ##                                                                                 (-1.300, 0.410)    
    ##                                                                                                    
    ## factor(Party)Other:factor(Gender)Male                                                              
    ##                                                                                                    
    ##                                                                                                    
    ## factor(Party)Republican:factor(Gender)Male                                                         
    ##                                                                                                    
    ##                                                                                                    
    ## factor(Gender)Female:factor(Ethnicity)Black / African                               -0.530         
    ##                                                                                 (-2.100, 1.100)    
    ##                                                                                                    
    ## factor(Gender)Male:factor(Ethnicity)Black / African                                                
    ##                                                                                                    
    ##                                                                                                    
    ## factor(Gender)Female:factor(Ethnicity)Caucasian                                      0.630         
    ##                                                                                 (-3.700, 4.900)    
    ##                                                                                                    
    ## factor(Gender)Male:factor(Ethnicity)Caucasian                                        1.200         
    ##                                                                                 (-3.100, 5.500)    
    ##                                                                                                    
    ## factor(Gender)Female:factor(Ethnicity)Hispanic / Latinx                             -1.000         
    ##                                                                                 (-2.700, 0.610)    
    ##                                                                                                    
    ## factor(Gender)Male:factor(Ethnicity)Hispanic / Latinx                                              
    ##                                                                                                    
    ##                                                                                                    
    ## factor(Gender)Female:factor(Ethnicity)Native American                               -0.710         
    ##                                                                                 (-2.600, 1.200)    
    ##                                                                                                    
    ## factor(Gender)Male:factor(Ethnicity)Native American                                                
    ##                                                                                                    
    ##                                                                                                    
    ## factor(Gender)Female:factor(Ethnicity)Other                                         -1.100         
    ##                                                                                 (-3.600, 1.500)    
    ##                                                                                                    
    ## factor(Gender)Male:factor(Ethnicity)Other                                                          
    ##                                                                                                    
    ##                                                                                                    
    ## Constant                                                     7.200***              7.400***        
    ##                                                           (6.900, 7.400)        (4.100, 11.000)    
    ##                                                                                                    
    ## ---------------------------------------------------------------------------------------------------
    ## Observations                                                    313                   313          
    ## R2                                                             0.001                 0.170         
    ## Adjusted R2                                                   -0.002                 0.110         
    ## Residual Std. Error                                      1.600 (df = 311)      1.500 (df = 291)    
    ## F Statistic                                             0.420 (df = 1; 311) 2.800*** (df = 21; 291)
    ## ===================================================================================================
    ## Note:                                                                   *p<0.1; **p<0.05; ***p<0.01
