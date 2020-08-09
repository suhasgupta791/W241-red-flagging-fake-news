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

create_question_column <- function(dataset){
  dataset[, TestQ1 := ifelse(is.na(dataset$'8B'), dataset$'8A', dataset$'8B')]
  dataset[, TestQ2 := ifelse(is.na(dataset$'9B'), dataset$'9A', dataset$'9B')]
  dataset[, TestQ3 := ifelse(is.na(dataset$'10B'), dataset$'10A', dataset$'10B')]
  dataset[, TestQ4 := ifelse(is.na(dataset$'11B'), dataset$'11A', dataset$'11B')]
  dataset[, TestQ5 := ifelse(is.na(dataset$'12B'), dataset$'12A', dataset$'12B')]
  dataset[, TestQ6 := ifelse(is.na(dataset$'13B'), dataset$'13A', dataset$'13B')]
  dataset[, TestQ7 := ifelse(is.na(dataset$'14B'), dataset$'14A', dataset$'14B')]
  dataset[, TestQ8 := ifelse(is.na(dataset$'15B'), dataset$'15A', dataset$'15B')]
  dataset[, TestQ9 := ifelse(is.na(dataset$'16B'), dataset$'16A', dataset$'16B')]
  dataset[, TestQ10 := ifelse(is.na(dataset$'17B'), dataset$'17A', dataset$'17B')]
  return(dataset)
}


# Color palette for ggplot 
cbPalette <- c("#009999", "#AA9900")

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
study1_data_temp <- rename_cols(study1_data_pruned)
study2_data_temp <- rename_cols(study2_data_pruned)
study3_data_temp <- rename_cols(study3_data_pruned)

# Add score columns 
answer_guide <- c('Yes','Yes','No','Yes','No','No','Yes','Yes','No','No',  
                  'Yes','Yes','No','Yes','No','No','Yes','Yes','No','No') 

study1_data_mod <- compute_score(study1_data_temp, answer_guide = answer_guide )
study2_data_mod <- compute_score(study2_data_temp, answer_guide = answer_guide )
study3_data_mod <- compute_score(study3_data_temp, answer_guide = answer_guide )

# Add indicator variables
study1_data_mod[, Mturk := 1]
study1_data_mod[, captcha := 0]

study2_data_mod[, Mturk := 1]
study2_data_mod[, captcha := 1]


study3_data_mod[, Mturk := 0]
study3_data_mod[, captcha := 1]

# Check the data
# head(study1_data_mod)
# head(study2_data_mod)
# head(study3_data_mod)
```

#### Combine data sets from all studies

``` r
# combine data set 
dt <- rbind(study1_data_mod,study2_data_mod,study3_data_mod)
data_full <- create_question_column(dt)
```

## Hypothesis

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

### check experimental data

#### Randomization

**The randomization worked well in the survey software and we had an
equal allocation to treatment and control groups in the experiment**

``` r
dt <- data_full[, .(count = .N), by=assignment]
ggplot(dt, aes(x = assignment, y = count)) + 
  geom_bar(stat="identity", fill="steelblue") 
```

![](Final_project_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

#### Power

**The data collected has 80% power in detecting any treatment effect
that may exist in this experiment**

``` r
## Power calculation 
d <- data_full[, .(score_false = mean(score_false)), by = assignment]
ate <- diff(d$score_false)
sd <- data_full[, sd(score_false)]
power.t.test(d=ate,sig.level=0.95,n=nrow(data_full),sd=sd,alternative="one.sided")
```

    ## 
    ##      Two-sample t test power calculation 
    ## 
    ##               n = 313
    ##           delta = 0.062
    ##              sd = 1.6
    ##       sig.level = 0.95
    ##           power = 0.98
    ##     alternative = one.sided
    ## 
    ## NOTE: n is number in *each* group

### EDA

**Figure 1** summarizes the score of subjects in each assignment group
(treatment/control) and for true and false tweets. The table and figures
indicate that a general flag slightly decreased the overall score for
the subjects ability to detect whether a tweet was true or false.
Looking at the third and fourth column of Table1, we see that the
detection ability (score) for true tweets decreased slightly in the
presence of the general warning flag while the score for detecting false
tweets improved slightly. This suggests that though the general warning
flag about fake news might improve people’s ability to accurately detect
false information, it also reduces the belief in true information as
well. We will test our hypothesis and research questions more formally
in the upcoming
sections.

``` r
dt <- data_full[, .(mean_total_score = mean(score),mean_true_score = mean(score_true),mean_false_score = mean(score_false)), by=assignment]
dt$Warning_Flag[dt$assignment == 0] <- "No Flag"
dt$Warning_Flag[dt$assignment == 1] <- "Flag"
dfm <- melt(dt[,c('Warning_Flag','mean_total_score')],id.vars = 1)
dfm <- rename(dfm,
       Assignment = Warning_Flag,
       Score_Type = variable,
       Score = value)
ggplot(dfm, aes(x = Assignment, y = Score,label=sprintf("%0.2f", round(Score, digits = 2)))) + 
  geom_bar(stat="identity", fill="steelblue", width = 0.5) + 
  geom_text(size=3.5, color="white", vjust=1.5) + 
  ggtitle("Figure 1. Average score for all tweets by survey assignment group") +
  ylab("Mean Total Score") + 
  xlab("Survey Group")   + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()
```

![](Final_project_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
dt$Warning_Flag[dt$assignment == 0] <- "No"
dt$Warning_Flag[dt$assignment == 1] <- "Yes"

dfm <- melt(dt[,c('Warning_Flag','mean_true_score','mean_false_score')],id.vars = 1)
dfm <- rename(dfm,
       Score_Type = variable,
       Score = value)
levels(dfm$Score_Type) = c("True Tweets", "False Tweets")

p <- ggplot(dfm,aes(x = Score_Type,y = Score)) + 
  geom_bar(aes(fill = Warning_Flag),stat = "identity", width=0.7, position=position_dodge(width=0.8)) +
  ggtitle("Figure2. General warning effect on true and false tweets") +
  ylab("Average score for tweet accuracy detection") + 
  xlab("Type of tweet") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(values = cbPalette)
p
```

![](Final_project_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

We find from figures 3 and 4 that a large majority of users identified
themselves as being active on social media and most of the survewy
participants were registered voters in the united states.

**A large portion of survey subjects said that they considered
themselves to be active on social media**

``` r
data_full$Soc_Med_Active[data_full$Soc_Med_Active == ''] <- "Unanswered"
ggplot(data_full) + geom_bar(aes(x = Soc_Med_Active))
```

![](Final_project_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

**Alse, majority of survey subjects said that they were registered as a
voter**

``` r
data_full$Reg_Voter[data_full$Reg_Voter == ''] <- "Unanswered"
ggplot(data_full) + geom_bar(aes(x = Reg_Voter))
```

![](Final_project_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

There appears to be an increase of 6% in score of participants for
correctly identifying the false tweets with a confidence interval of
(-0.3,0.420) before including control for mechanical turk participants
and BOT checks. We added indicator variables for participants recruited
on mechanical turk as participants on amazon’s mechanical turk may not
be accurate representatives of general US population. Futhermore, we
also added an indicator variable for a BOT check being present in the
survey to control for any malignant activity on mechanical turk (using
BOTs to answer survey and earn monetray rewards). Since the CAPTCHA
verification was added in the latter half of the experiment, we added an
indicator variable in regression to control for errors due to BOT
activity. We see that the 95% confidence intervals shrink slightly when
we control for these covariates. The model in the third column will be
our baseline model. The coefficient for assignment variable in the table
is not statistically significant in this model.

``` r
mod1 <- lm(score_false ~ assignment, data_full)
mod2 <- lm(score_false ~ assignment+Mturk, data_full)
mod3 <- lm(score_false ~ assignment+Mturk+captcha, data_full)
ci_custom1 <- compute_robust_ci(mod1)
ci_custom2 <- compute_robust_ci(mod2)
ci_custom3 <- compute_robust_ci(mod3)
se_custom1 <- compute_robust_se(mod1)
se_custom2 <- compute_robust_se(mod2)
se_custom3 <- compute_robust_se(mod3)
# stargazer(mod1,mod2,mod3, type="text",ci.custom = list(ci_custom1,ci_custom2,ci_custom3))
stargazer(mod1,mod2,mod3, type="latex",se = list(se_custom1,se_custom2,se_custom3))
```

% Table created by stargazer v.5.2.2 by Marek Hlavac, Harvard
University. E-mail: hlavac at fas.harvard.edu % Date and time: Sat, Aug
08, 2020 - 20:48:41

**Figure 5** examines the distribution of the survey participants by
Gender and shows that we have a well balanced data set in terms of
gender distribution.

``` r
dt <- data_full[, .(count = .N), by=Gender]
dt$Gender[dt$Gender == ''] <- "Unanswered"

ggplot(dt, aes(x = Gender, y = count)) +
  geom_bar(aes(fill=count), stat="identity", width = 0.5) + 
  ggtitle("Figure 5. Gender distribution of survey takers") +
  ylab("Count") + xlab("Gender")   + 
  theme(plot.title = element_text(hjust = 0.5))
```

![](Final_project_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

**Figure 6** examines the data distribution based on party affiliation.
We see that the Male and Female genders are well balanced for Democrats
and Republicans parties while there is a slight skew towards males in
*Other* party affiliations. The unanswered or non-conforming genders are
only 2 counts in the data set and hence not represented well in this
experimental data.

``` r
data_full$Party[data_full$Party == ''] <- "Unanswered"

dt <- data_full[, .(count = .N), by=.(Gender,Party)]
dt$Gender[dt$Gender == ''] <- "Unanswered"

ggplot(dt, aes(x = Party, y = count)) +
  geom_bar(aes(fill=Gender), stat="identity", width = 0.5) + 
  ggtitle("Figure 6. Gender distribution by party affiliation") +
  ylab("Count") + xlab("Gender")   + 
  theme(plot.title = element_text(hjust = 0.5)) 
```

![](Final_project_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
dt <- data_full[, .(mean_score_by_gender = mean(score_false)), by=Gender]
dt$Gender[dt$Gender == ''] <- "Other"
ggplot(dt, aes(x = Gender, y = mean_score_by_gender)) +
  geom_bar(aes(Gender), stat="identity", width = 0.5) + 
  ggtitle("Figure X. Mean scores by Gender") + 
  ylab("Score on False Tweets") + xlab("Gender")   + 
  theme(plot.title = element_text(hjust = 0.5)) 
```

![](Final_project_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

The tweets selected for our experiment deal with US politics, climate
change and general US current affair topics. Believability in social
media news/headlines/tweets (and thus ability to score correctly on the
survey) can have large variation between Republicans vs Democrats or
Male vs Female, depending on the type of the tweets selected. Therefore,
we next test the affect of displaying a general warning flag on
participant score by including interaction terms on top of the baseline
regression model. The mean score of different gender identities is
different from each other and we will control for this in the regression
model to get a better estimate of the treatment
effect.

``` r
mod4 <- lm(score_false ~ assignment*factor(Gender)+Mturk+captcha, data_full)
mod5 <- lm(score_false ~ assignment*factor(Party)+Mturk+captcha, data_full)
mod6 <- lm(score_false ~ assignment*factor(Gender)+assignment*factor(Party)+Mturk+captcha, data_full)
ci_custom4 <- compute_robust_ci(mod4)
ci_custom5 <- compute_robust_ci(mod5)
ci_custom6 <- compute_robust_ci(mod6)

se_custom4 <- compute_robust_se(mod4)
se_custom5 <- compute_robust_se(mod5)
se_custom6 <- compute_robust_se(mod6)

stargazer(mod4,mod5,mod6,type="text",se=list(se_custom4,se_custom5,se_custom6))
```

    ## 
    ## ===========================================================================================================
    ##                                                              Dependent variable:                           
    ##                                    ------------------------------------------------------------------------
    ##                                                                  score_false                               
    ##                                              (1)                     (2)                     (3)           
    ## -----------------------------------------------------------------------------------------------------------
    ## assignment                                 -0.038                  -0.037                   -0.160         
    ##                                            (0.190)                 (0.170)                 (0.240)         
    ##                                                                                                            
    ## factor(Gender)Female                      -1.100***                                       -1.000***        
    ##                                            (0.290)                                         (0.280)         
    ##                                                                                                            
    ## factor(Gender)Male                        -0.670***                                       -0.660***        
    ##                                            (0.220)                                         (0.200)         
    ##                                                                                                            
    ## factor(Party)Other                                                  0.280                   0.260          
    ##                                                                    (0.230)                 (0.230)         
    ##                                                                                                            
    ## factor(Party)Republican                                           -0.540**                 -0.540**        
    ##                                                                    (0.260)                 (0.250)         
    ##                                                                                                            
    ## Mturk                                      -0.180                  -0.120                   -0.130         
    ##                                            (0.120)                 (0.120)                 (0.120)         
    ##                                                                                                            
    ## captcha                                   2.300***                2.100***                 2.100***        
    ##                                            (0.190)                 (0.190)                 (0.190)         
    ##                                                                                                            
    ## assignment:factor(Gender)Female             0.180                                           0.230          
    ##                                            (0.280)                                         (0.280)         
    ##                                                                                                            
    ## assignment:factor(Gender)Male                                                                              
    ##                                                                                                            
    ##                                                                                                            
    ## assignment:factor(Party)Other                                       0.250                   0.250          
    ##                                                                    (0.300)                 (0.310)         
    ##                                                                                                            
    ## assignment:factor(Party)Republican                                  0.120                   0.130          
    ##                                                                    (0.350)                 (0.350)         
    ##                                                                                                            
    ## Constant                                  3.000***                2.300***                 3.200***        
    ##                                            (0.290)                 (0.240)                 (0.300)         
    ##                                                                                                            
    ## -----------------------------------------------------------------------------------------------------------
    ## Observations                                 313                     313                     313           
    ## R2                                          0.460                   0.470                   0.480          
    ## Adjusted R2                                 0.450                   0.460                   0.470          
    ## Residual Std. Error                   1.200 (df = 306)        1.200 (df = 305)         1.200 (df = 302)    
    ## F Statistic                        43.000*** (df = 6; 306) 39.000*** (df = 7; 305) 28.000*** (df = 10; 302)
    ## ===========================================================================================================
    ## Note:                                                                           *p<0.1; **p<0.05; ***p<0.01

``` r
mod11a <- lm(score_false ~ assignment+TestQ3+Mturk+captcha, data_full)
mod11b <- lm(score_false ~ assignment+TestQ5+Mturk+captcha, data_full)
mod11c <- lm(score_false ~ assignment+TestQ6+Mturk+captcha, data_full)
mod11d <- lm(score_false ~ assignment+TestQ9+Mturk+captcha, data_full)
mod11e <- lm(score_false ~ assignment+TestQ3+TestQ5+TestQ6+TestQ9+TestQ10+Mturk+captcha, data_full)

stargazer(mod11a,mod11b,mod11c,mod11d,mod11e,type="text")
```

    ## 
    ## ====================================================================================================================================================================================
    ##                                                                                           Dependent variable:                                                                       
    ##                     ----------------------------------------------------------------------------------------------------------------------------------------------------------------
    ##                                                                                               score_false                                                                           
    ##                               (1)                      (2)                      (3)                      (4)                                        (5)                             
    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    ## assignment                   0.087                    0.085                    0.200*                   -0.110                                    -0.000*                           
    ##                             (0.100)                  (0.110)                  (0.100)                  (0.100)                                    (0.000)                           
    ##                                                                                                                                                                                     
    ## TestQ3Yes                  -2.400***                                                                                                             -1.000***                          
    ##                             (0.150)                                                                                                               (0.000)                           
    ##                                                                                                                                                                                     
    ## TestQ5Yes                                           -1.500***                                                                                    -1.000***                          
    ##                                                      (0.120)                                                                                      (0.000)                           
    ##                                                                                                                                                                                     
    ## TestQ6Yes                                                                    -2.300***                                                           -1.000***                          
    ##                                                                               (0.150)                                                             (0.000)                           
    ##                                                                                                                                                                                     
    ## TestQ9Yes                                                                                             -2.200***                                  -1.000***                          
    ##                                                                                                        (0.130)                                    (0.000)                           
    ##                                                                                                                                                                                     
    ## TestQ10Yes                                                                                                                                       -1.000***                          
    ##                                                                                                                                                   (0.000)                           
    ##                                                                                                                                                                                     
    ## Mturk                        -0.200                   -0.130                   -0.120                   -0.078                                     -0.000                           
    ##                             (0.130)                  (0.140)                  (0.130)                  (0.130)                                    (0.000)                           
    ##                                                                                                                                                                                     
    ## captcha                     0.980***                 1.600***                 1.200***                 1.400***                                    0.000                            
    ##                             (0.140)                  (0.140)                  (0.140)                  (0.130)                                    (0.000)                           
    ##                                                                                                                                                                                     
    ## Constant                    3.600***                 3.300***                 3.300***                 3.200***                                   5.000***                          
    ##                             (0.190)                  (0.200)                  (0.180)                  (0.170)                                    (0.000)                           
    ##                                                                                                                                                                                     
    ## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    ## Observations                  313                      313                      313                      313                                        313                             
    ## R2                           0.690                    0.630                    0.680                    0.710                                      1.000                            
    ## Adjusted R2                  0.690                    0.620                    0.680                    0.700                                      1.000                            
    ## Residual Std. Error     0.910 (df = 308)         1.000 (df = 308)         0.930 (df = 308)         0.890 (df = 308)                           0.000 (df = 304)                      
    ## F Statistic         174.000*** (df = 4; 308) 129.000*** (df = 4; 308) 166.000*** (df = 4; 308) 186.000*** (df = 4; 308) 987,437,409,256,563,212,468,824,834,048.000*** (df = 8; 304)
    ## ====================================================================================================================================================================================
    ## Note:                                                                                                                                                    *p<0.1; **p<0.05; ***p<0.01

``` r
mod11a <- lm(score_true ~ assignment+TestQ3+Mturk+captcha, data_full)
mod11b <- lm(score_true ~ assignment+TestQ5+Mturk+captcha, data_full)
mod11c <- lm(score_true ~ assignment+TestQ3+TestQ5+TestQ6+Mturk+captcha, data_full)
mod11d <- lm(score_true ~ assignment+TestQ9+Mturk+captcha, data_full)
mod11e <- lm(score_true ~ assignment+TestQ3+TestQ5+TestQ6+TestQ9+TestQ10+Mturk+captcha, data_full)

stargazer(mod11a,mod11b,mod11c,mod11d,mod11e,type="text",
          add.lines=list(c("Question Fixed Effects", rep("Yes",5))))
```

    ## 
    ## ==========================================================================================================================================
    ##                                                                        Dependent variable:                                                
    ##                        -------------------------------------------------------------------------------------------------------------------
    ##                                                                            score_true                                                     
    ##                                  (1)                    (2)                    (3)                    (4)                    (5)          
    ## ------------------------------------------------------------------------------------------------------------------------------------------
    ## assignment                     -0.190*                 -0.180                -0.200*                 -0.170                -0.210*        
    ##                                (0.110)                (0.110)                (0.110)                (0.110)                (0.110)        
    ##                                                                                                                                           
    ## TestQ3Yes                     0.560***                                       0.450**                                       0.460**        
    ##                                (0.160)                                       (0.180)                                       (0.200)        
    ##                                                                                                                                           
    ## TestQ5Yes                                              0.130                  0.074                                         0.075         
    ##                                                       (0.120)                (0.120)                                       (0.130)        
    ##                                                                                                                                           
    ## TestQ6Yes                                                                     0.220                                         0.240         
    ##                                                                              (0.180)                                       (0.180)        
    ##                                                                                                                                           
    ## TestQ9Yes                                                                                            0.100                  -0.200        
    ##                                                                                                     (0.150)                (0.170)        
    ##                                                                                                                                           
    ## TestQ10Yes                                                                                                                  0.170         
    ##                                                                                                                            (0.170)        
    ##                                                                                                                                           
    ## Mturk                           0.047                  0.037                  0.038                  0.037                  0.032         
    ##                                (0.140)                (0.140)                (0.140)                (0.140)                (0.140)        
    ##                                                                                                                                           
    ## captcha                       -0.400**               -0.640***               -0.320*               -0.650***               -0.300*        
    ##                                (0.160)                (0.140)                (0.170)                (0.140)                (0.170)        
    ##                                                                                                                                           
    ## Constant                      3.700***                4.000***               3.600***               4.000***               3.600***       
    ##                                (0.200)                (0.200)                (0.220)                (0.200)                (0.220)        
    ##                                                                                                                                           
    ## ------------------------------------------------------------------------------------------------------------------------------------------
    ## Question Fixed Effects           Yes                    Yes                    Yes                    Yes                    Yes          
    ## Observations                     313                    313                    313                    313                    313          
    ## R2                              0.140                  0.110                  0.140                  0.110                  0.150         
    ## Adjusted R2                     0.130                  0.098                  0.130                  0.097                  0.130         
    ## Residual Std. Error       0.980 (df = 308)        1.000 (df = 308)       0.980 (df = 306)       1.000 (df = 308)       0.980 (df = 304)   
    ## F Statistic            12.000*** (df = 4; 308) 9.500*** (df = 4; 308) 8.600*** (df = 6; 306) 9.300*** (df = 4; 308) 6.700*** (df = 8; 304)
    ## ==========================================================================================================================================
    ## Note:                                                                                                          *p<0.1; **p<0.05; ***p<0.01

``` r
data_full[, score_TestQ1 := ifelse(TestQ1 == "Yes", 1, 0)]
data_full[, score_TestQ2 := ifelse(TestQ2 == "Yes", 1, 0)]
data_full[, score_TestQ3 := ifelse(TestQ3 == "No", 1, 0)]
data_full[, score_TestQ4 := ifelse(TestQ4 == "Yes", 1, 0)]
data_full[, score_TestQ5 := ifelse(TestQ5 == "No", 1, 0)]
data_full[, score_TestQ6 := ifelse(TestQ6 == "No", 1, 0)]
data_full[, score_TestQ7 := ifelse(TestQ7 == "Yes", 1, 0)]
data_full[, score_TestQ8 := ifelse(TestQ8 == "Yes", 1, 0)]
data_full[, score_TestQ9 := ifelse(TestQ9 == "No", 1, 0)]
data_full[, score_TestQ10 := ifelse(TestQ10 == "No", 1, 0)]

dt <- data_full[, .(countQ1 = sum(score_TestQ1), 
              countQ2 = sum(score_TestQ2),
              countQ3 = sum(score_TestQ3),
              countQ4 = sum(score_TestQ4),
              countQ5 = sum(score_TestQ5),
              countQ6 = sum(score_TestQ6),
              countQ7 = sum(score_TestQ7),
              countQ8 = sum(score_TestQ8),
              countQ9 = sum(score_TestQ9),
              countQ10 = sum(score_TestQ10)
              )]

d2 <- data_full[, .(countQ1 = sum(score_TestQ1), 
              countQ2 = sum(score_TestQ2),
              countQ3 = sum(score_TestQ3),
              countQ4 = sum(score_TestQ4),
              countQ5 = sum(score_TestQ5),
              countQ6 = sum(score_TestQ6),
              countQ7 = sum(score_TestQ7),
              countQ8 = sum(score_TestQ8),
              countQ9 = sum(score_TestQ9),
              countQ10 = sum(score_TestQ10)
              ), by =assignment]

barplot(dt[,c(countQ3,countQ5,countQ6,countQ9,countQ10)],
        col = c('blue','gold'),
        ylab = "Percentage score for correct answer",
        xlab = "Scores for each survey question",
        names.arg= c("Q3","Q5","Q6","Q9","Q10")
      )
```

![](Final_project_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
d2$Warning_Flag[d2$assignment == 0] <- "No"
d2$Warning_Flag[d2$assignment == 1] <- "Yes"

dfm <- melt(d2[,c('Warning_Flag','countQ3','countQ5','countQ6','countQ9','countQ10')],id.vars = 1)
levels(dfm$variable) = c("Q3","Q5","Q6","Q9","Q10")

p <- ggplot(dfm,aes(x = variable,y = value)) + 
  geom_bar(aes(fill = Warning_Flag),stat = "identity", width=0.7, position=position_dodge(width=0.8)) +
  ggtitle("Figure2. General warning effect on scores for each tweet ") +
  ylab("Percentage score for correct answer") + 
  xlab("Question Number") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(values = c("blue","gold"))
p
```

![](Final_project_files/figure-gfm/unnamed-chunk-20-2.png)<!-- -->

  - \*\*Score on qestion 10 is collinear with the scores on other
    questions. Running regression with the fixed effects of question 10
    leads to
singularities.

<!-- end list -->

``` r
mod1 <- lm(score_false ~ assignment+score_TestQ3+score_TestQ5+score_TestQ6, data = data_full)
se_custom1 <- compute_robust_se(mod1)

stargazer(mod1,type="text",se=list(se_custom1))
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                             score_false        
    ## -----------------------------------------------
    ## assignment                   0.190***          
    ##                               (0.057)          
    ##                                                
    ## score_TestQ3                 1.700***          
    ##                               (0.120)          
    ##                                                
    ## score_TestQ5                 1.300***          
    ##                               (0.065)          
    ##                                                
    ## score_TestQ6                 1.500***          
    ##                               (0.120)          
    ##                                                
    ## Constant                     0.340***          
    ##                               (0.080)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                    313            
    ## R2                             0.900           
    ## Adjusted R2                    0.900           
    ## Residual Std. Error      0.520 (df = 308)      
    ## F Statistic          703.000*** (df = 4; 308)  
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

``` r
# mod1 <- lm(score_TestQ1 ~ assignment, data = data_full)
# mod2 <- lm(score_TestQ2 ~ assignment, data = data_full)
# mod3 <- lm(score_TestQ3 ~ assignment, data = data_full)
# mod4 <- lm(score_TestQ4 ~ assignment, data = data_full)
# mod5 <- lm(score_TestQ5 ~ assignment, data = data_full)
# mod6 <- lm(score_TestQ6 ~ assignment, data = data_full)
# mod7 <- lm(score_TestQ7 ~ assignment, data = data_full)
# mod8 <- lm(score_TestQ8 ~ assignment, data = data_full)
# mod9 <- lm(score_TestQ9 ~ assignment, data = data_full)
# mod10 <- lm(score_TestQ10 ~ assignment, data = data_full)
mod11 <- lm(score_false ~ assignment*Ethnicity+TestQ3+TestQ5+TestQ6, data_full)
mod12 <- lm(score_false ~ assignment*Gender+TestQ3+TestQ5+TestQ6, data_full)
mod13 <- lm(score_false ~ assignment*Education+TestQ3+TestQ5+TestQ6, data_full)
mod14 <- lm(score_false ~ assignment*factor(Income)+TestQ3+TestQ5+TestQ6, data_full)

#stargazer(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,mod10,mod11,type="text")
stargazer(mod11,mod12,mod13,mod14,type="text")
```

    ## 
    ## ================================================================================================================================================
    ##                                                                                    Dependent variable:                                          
    ##                                           ------------------------------------------------------------------------------------------------------
    ##                                                                                        score_false                                              
    ##                                                      (1)                      (2)                       (3)                       (4)           
    ## ------------------------------------------------------------------------------------------------------------------------------------------------
    ## assignment                                          0.010                   0.240***                 0.290***                  0.270***         
    ##                                                    (0.140)                  (0.082)                   (0.090)                   (0.091)         
    ##                                                                                                                                                 
    ## EthnicityBlack / African                          -0.610***                                                                                     
    ##                                                    (0.180)                                                                                      
    ##                                                                                                                                                 
    ## EthnicityCaucasian                                 -0.190                                                                                       
    ##                                                    (0.120)                                                                                      
    ##                                                                                                                                                 
    ## EthnicityHispanic / Latinx                         -0.250                                                                                       
    ##                                                    (0.170)                                                                                      
    ##                                                                                                                                                 
    ## EthnicityNative American                           -0.390                                                                                       
    ##                                                    (0.240)                                                                                      
    ##                                                                                                                                                 
    ## EthnicityOther                                      0.120                                                                                       
    ##                                                    (0.380)                                                                                      
    ##                                                                                                                                                 
    ## GenderFemale                                                                 0.470                                                              
    ##                                                                             (0.380)                                                             
    ##                                                                                                                                                 
    ## GenderMale                                                                   0.400                                                              
    ##                                                                             (0.370)                                                             
    ##                                                                                                                                                 
    ## EducationGraduate degree                                                                               0.140                                    
    ##                                                                                                       (0.098)                                   
    ##                                                                                                                                                 
    ## EducationHigh school graduate                                                                          0.001                                    
    ##                                                                                                       (0.180)                                   
    ##                                                                                                                                                 
    ## EducationLess than high school                                                                        -0.140                                    
    ##                                                                                                       (0.370)                                   
    ##                                                                                                                                                 
    ## EducationSome college                                                                                  0.059                                    
    ##                                                                                                       (0.120)                                   
    ##                                                                                                                                                 
    ## 250000                                                                                                                           0.120          
    ##                                                                                                                                 (0.140)         
    ##                                                                                                                                                 
    ## 250000                                                                                                                           0.170          
    ##                                                                                                                                 (0.140)         
    ##                                                                                                                                                 
    ## 150000                                                                                                                          -0.021          
    ##                                                                                                                                 (0.095)         
    ##                                                                                                                                                 
    ## TestQ3Yes                                         -1.700***                -1.700***                 -1.700***                 -1.700***        
    ##                                                    (0.092)                  (0.090)                   (0.092)                   (0.091)         
    ##                                                                                                                                                 
    ## TestQ5Yes                                         -1.300***                -1.300***                 -1.300***                 -1.300***        
    ##                                                    (0.062)                  (0.063)                   (0.063)                   (0.064)         
    ##                                                                                                                                                 
    ## TestQ6Yes                                         -1.500***                -1.500***                 -1.500***                 -1.500***        
    ##                                                    (0.093)                  (0.092)                   (0.093)                   (0.094)         
    ##                                                                                                                                                 
    ## assignment:EthnicityBlack / African               0.820***                                                                                      
    ##                                                    (0.260)                                                                                      
    ##                                                                                                                                                 
    ## assignment:EthnicityCaucasian                       0.130                                                                                       
    ##                                                    (0.150)                                                                                      
    ##                                                                                                                                                 
    ## assignment:EthnicityHispanic / Latinx              0.510*                                                                                       
    ##                                                    (0.280)                                                                                      
    ##                                                                                                                                                 
    ## assignment:EthnicityNative American                 0.280                                                                                       
    ##                                                    (0.310)                                                                                      
    ##                                                                                                                                                 
    ## assignment:EthnicityOther                          -0.170                                                                                       
    ##                                                    (0.450)                                                                                      
    ##                                                                                                                                                 
    ## assignment:GenderFemale                                                      -0.085                                                             
    ##                                                                             (0.120)                                                             
    ##                                                                                                                                                 
    ## assignment:GenderMale                                                                                                                           
    ##                                                                                                                                                 
    ##                                                                                                                                                 
    ## assignment:EducationGraduate degree                                                                   -0.200                                    
    ##                                                                                                       (0.140)                                   
    ##                                                                                                                                                 
    ## assignment:EducationHigh school graduate                                                              -0.140                                    
    ##                                                                                                       (0.240)                                   
    ##                                                                                                                                                 
    ## assignment:EducationLess than high school                                                             -0.190                                    
    ##                                                                                                       (0.530)                                   
    ##                                                                                                                                                 
    ## assignment:EducationSome college                                                                      -0.140                                    
    ##                                                                                                       (0.170)                                   
    ##                                                                                                                                                 
    ## 250000                                                                                                                          -0.250          
    ##                                                                                                                                 (0.210)         
    ##                                                                                                                                                 
    ## 250000                                                                                                                          -0.270          
    ##                                                                                                                                 (0.200)         
    ##                                                                                                                                                 
    ## 150000                                                                                                                          -0.053          
    ##                                                                                                                                 (0.130)         
    ##                                                                                                                                                 
    ## Constant                                          5.000***                  4.400***                 4.800***                  4.800***         
    ##                                                    (0.110)                  (0.380)                   (0.069)                   (0.070)         
    ##                                                                                                                                                 
    ## ------------------------------------------------------------------------------------------------------------------------------------------------
    ## Observations                                         313                      313                       313                       313           
    ## R2                                                  0.910                    0.900                     0.900                     0.900          
    ## Adjusted R2                                         0.900                    0.900                     0.900                     0.900          
    ## Residual Std. Error                           0.510 (df = 298)          0.520 (df = 305)         0.520 (df = 300)          0.520 (df = 302)     
    ## F Statistic                               207.000*** (df = 14; 298) 401.000*** (df = 7; 305) 232.000*** (df = 12; 300) 280.000*** (df = 10; 302)
    ## ================================================================================================================================================
    ## Note:                                                                                                                *p<0.1; **p<0.05; ***p<0.01

  - **The SEs don’t appear to be changing with different question fixed
    effects but the magnitude of the coefficient is changing. We will
    exploring cummulating the fixed effects from false questions.**

<!-- end list -->

``` r
mod11a <- lm(score_true ~ assignment+TestQ3+Mturk+captcha, data_full)
mod11b <- lm(score_true ~ assignment+TestQ3+TestQ5+Mturk+captcha, data_full)
mod11c <- lm(score_true ~ assignment+TestQ3+TestQ5+TestQ6+Mturk+captcha, data_full)
mod11d <- lm(score_true ~ assignment+TestQ3+TestQ5+TestQ6+TestQ9+Mturk+captcha, data_full)
mod11e <- lm(score_true ~ assignment+TestQ3+TestQ5+TestQ6+TestQ9+TestQ10+Mturk+captcha, data_full)

stargazer(mod11a,mod11b,mod11c,mod11d,mod11e,type="text")
```

    ## 
    ## ========================================================================================================================================
    ##                                                                     Dependent variable:                                                 
    ##                     --------------------------------------------------------------------------------------------------------------------
    ##                                                                          score_true                                                     
    ##                               (1)                     (2)                    (3)                    (4)                    (5)          
    ## ----------------------------------------------------------------------------------------------------------------------------------------
    ## assignment                  -0.190*                 -0.190*                -0.200*                -0.220*                -0.210*        
    ##                             (0.110)                 (0.110)                (0.110)                (0.110)                (0.110)        
    ##                                                                                                                                         
    ## TestQ3Yes                  0.560***                0.550***                0.450**                0.500***               0.460**        
    ##                             (0.160)                 (0.170)                (0.180)                (0.190)                (0.200)        
    ##                                                                                                                                         
    ## TestQ5Yes                                            0.090                  0.074                  0.095                  0.075         
    ##                                                     (0.120)                (0.120)                (0.120)                (0.130)        
    ##                                                                                                                                         
    ## TestQ6Yes                                                                   0.220                  0.260                  0.240         
    ##                                                                            (0.180)                (0.180)                (0.180)        
    ##                                                                                                                                         
    ## TestQ9Yes                                                                                          -0.170                 -0.200        
    ##                                                                                                   (0.170)                (0.170)        
    ##                                                                                                                                         
    ## TestQ10Yes                                                                                                                0.170         
    ##                                                                                                                          (0.170)        
    ##                                                                                                                                         
    ## Mturk                        0.047                   0.044                  0.038                  0.044                  0.032         
    ##                             (0.140)                 (0.140)                (0.140)                (0.140)                (0.140)        
    ##                                                                                                                                         
    ## captcha                    -0.400**                -0.360**                -0.320*                -0.330**               -0.300*        
    ##                             (0.160)                 (0.160)                (0.170)                (0.170)                (0.170)        
    ##                                                                                                                                         
    ## Constant                   3.700***                3.700***                3.600***               3.600***               3.600***       
    ##                             (0.200)                 (0.220)                (0.220)                (0.220)                (0.220)        
    ##                                                                                                                                         
    ## ----------------------------------------------------------------------------------------------------------------------------------------
    ## Observations                  313                     313                    313                    313                    313          
    ## R2                           0.140                   0.140                  0.140                  0.150                  0.150         
    ## Adjusted R2                  0.130                   0.130                  0.130                  0.130                  0.130         
    ## Residual Std. Error    0.980 (df = 308)        0.980 (df = 307)        0.980 (df = 306)       0.980 (df = 305)       0.980 (df = 304)   
    ## F Statistic         12.000*** (df = 4; 308) 10.000*** (df = 5; 307) 8.600*** (df = 6; 306) 7.600*** (df = 7; 305) 6.700*** (df = 8; 304)
    ## ========================================================================================================================================
    ## Note:                                                                                                        *p<0.1; **p<0.05; ***p<0.01

**Our dataset does appear to consist mostly of people with atleast a
college degree or higher and the participants mostly belong to the 21-40
age
bucket.**

``` r
ggplot(data_full) + geom_bar(aes(x = Education,fill=Age_bin))
```

![](Final_project_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
mod7 <- lm(score_false ~ assignment*factor(Party)+factor(Gender)+assignment*factor(Age_bin)+Mturk+captcha, data_full)
mod8 <- lm(score_false ~ assignment*factor(Age_bin)+assignment*factor(Party)+Mturk+captcha, data_full)
se_custom7 <- compute_robust_se(mod7)
se_custom8 <- compute_robust_se(mod8)
stargazer(mod7,mod8,type="text",se=list(se_custom7,se_custom8))
```

    ## 
    ## ====================================================================================
    ##                                                   Dependent variable:               
    ##                                    -------------------------------------------------
    ##                                                       score_false                   
    ##                                              (1)                      (2)           
    ## ------------------------------------------------------------------------------------
    ## assignment                                 -0.780**                -0.950***        
    ##                                            (0.350)                  (0.320)         
    ##                                                                                     
    ## factor(Party)Other                          0.280                    0.290          
    ##                                            (0.230)                  (0.230)         
    ##                                                                                     
    ## factor(Party)Republican                    -0.510**                 -0.510**        
    ##                                            (0.260)                  (0.260)         
    ##                                                                                     
    ## factor(Gender)Female                      -0.810***                                 
    ##                                            (0.190)                                  
    ##                                                                                     
    ## factor(Gender)Male                        -0.560***                                 
    ##                                            (0.200)                                  
    ##                                                                                     
    ## factor(Age_bin)21-40                        -0.240                   -0.350         
    ##                                            (0.270)                  (0.280)         
    ##                                                                                     
    ## factor(Age_bin)41-60                        -0.360                   -0.480         
    ##                                            (0.320)                  (0.320)         
    ##                                                                                     
    ## factor(Age_bin)61+                          -0.600                   -0.800         
    ##                                            (0.590)                  (0.590)         
    ##                                                                                     
    ## Mturk                                       -0.085                   -0.086         
    ##                                            (0.130)                  (0.130)         
    ##                                                                                     
    ## captcha                                    2.200***                 2.100***        
    ##                                            (0.190)                  (0.200)         
    ##                                                                                     
    ## assignment:factor(Party)Other               0.160                    0.200          
    ##                                            (0.300)                  (0.300)         
    ##                                                                                     
    ## assignment:factor(Party)Republican          0.068                    0.074          
    ##                                            (0.350)                  (0.350)         
    ##                                                                                     
    ## assignment:factor(Age_bin)21-40            0.780**                  0.950***        
    ##                                            (0.360)                  (0.330)         
    ##                                                                                     
    ## assignment:factor(Age_bin)41-60            0.880**                  1.100***        
    ##                                            (0.400)                  (0.370)         
    ##                                                                                     
    ## assignment:factor(Age_bin)61+               0.480                    0.720          
    ##                                            (0.710)                  (0.700)         
    ##                                                                                     
    ## Constant                                   3.200***                 2.700***        
    ##                                            (0.340)                  (0.330)         
    ##                                                                                     
    ## ------------------------------------------------------------------------------------
    ## Observations                                 313                      313           
    ## R2                                          0.490                    0.480          
    ## Adjusted R2                                 0.460                    0.460          
    ## Residual Std. Error                    1.200 (df = 297)         1.200 (df = 299)    
    ## F Statistic                        19.000*** (df = 15; 297) 22.000*** (df = 13; 299)
    ## ====================================================================================
    ## Note:                                                    *p<0.1; **p<0.05; ***p<0.01

``` r
ggplot(mutate(data_full, Age = fct_infreq(Age_bin))) + geom_bar(aes(x = Age_bin))
```

![](Final_project_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

``` r
data_full[, .N, by=.(Party,Age_bin)]
```

    ##          Party Age_bin   N
    ##  1:   Democrat   21-40 126
    ##  2: Republican   21-40  52
    ##  3:      Other   21-40  31
    ##  4: Republican   41-60  26
    ##  5:      Other   41-60  20
    ##  6:   Democrat   41-60  37
    ##  7:   Democrat     61+   8
    ##  8: Republican     61+   5
    ##  9:   Democrat    0-20   3
    ## 10:      Other     61+   1
    ## 11:      Other    0-20   3
    ## 12: Republican    0-20   1

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

![](Final_project_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

``` r
dt <- data_full[, .(mean_score_by_Ethnicity = mean(score_false)), by=Ethnicity]
dt$Gender[dt$Gender == ''] <- "Other"
ggplot(dt, aes(x = Ethnicity, y = mean_score_by_Ethnicity)) +
  geom_bar(aes(Ethnicity), stat="identity", width = 0.5) + 
  ggtitle("Figure X. Mean scores by Ethnicity") + 
  ylab("Score on False Tweets") + xlab("Ethnicity")   + 
  theme(plot.title = element_text(hjust = 0.5)) 
```

![](Final_project_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

``` r
mod9 <- lm(score_false ~ assignment*Age_bin+Gender+Party+Ethnicity + Mturk+captcha, data_full)
mod10 <- lm(score_false ~ assignment*Ethnicity*Age_bin + Party+Gender+Mturk+captcha, data_full)
se_custom9 <- compute_robust_se(mod9)
se_custom10 <- compute_robust_se(mod10)
stargazer(mod9,mod10,type="text",se=list(se_custom9,se_custom10)
          )
```

    ## 
    ## ===================================================================================================
    ##                                                                  Dependent variable:               
    ##                                                    ------------------------------------------------
    ##                                                                      score_false                   
    ##                                                              (1)                      (2)          
    ## ---------------------------------------------------------------------------------------------------
    ## assignment                                                 -0.660*                 -0.770**        
    ##                                                            (0.350)                  (0.320)        
    ##                                                                                                    
    ## Age_bin21-40                                                -0.180                  -0.490         
    ##                                                            (0.280)                  (0.360)        
    ##                                                                                                    
    ## Age_bin41-60                                                -0.390                 -1.500**        
    ##                                                            (0.320)                  (0.660)        
    ##                                                                                                    
    ## Age_bin61+                                                  -0.420                   0.360         
    ##                                                            (0.530)                  (0.710)        
    ##                                                                                                    
    ## GenderFemale                                              -0.720***                -0.620***       
    ##                                                            (0.160)                  (0.160)        
    ##                                                                                                    
    ## GenderMale                                                 -0.420**                -0.400**        
    ##                                                            (0.170)                  (0.170)        
    ##                                                                                                    
    ## PartyOther                                                 0.340**                  0.390**        
    ##                                                            (0.160)                  (0.150)        
    ##                                                                                                    
    ## PartyRepublican                                            -0.450**                -0.390**        
    ##                                                            (0.190)                  (0.190)        
    ##                                                                                                    
    ## EthnicityBlack / African                                    -0.090                 -0.850**        
    ##                                                            (0.330)                  (0.400)        
    ##                                                                                                    
    ## EthnicityCaucasian                                          0.200                   -0.390         
    ##                                                            (0.160)                  (0.290)        
    ##                                                                                                    
    ## EthnicityHispanic / Latinx                                  -0.360                 -2.600***       
    ##                                                            (0.330)                  (0.820)        
    ##                                                                                                    
    ## EthnicityNative American                                   -0.710*                 -2.600***       
    ##                                                            (0.390)                  (0.820)        
    ##                                                                                                    
    ## EthnicityOther                                              -0.320                  -0.092         
    ##                                                            (0.410)                  (0.360)        
    ##                                                                                                    
    ## Mturk                                                       -0.150                  -0.230         
    ##                                                            (0.140)                  (0.140)        
    ##                                                                                                    
    ## captcha                                                    2.000***                1.900***        
    ##                                                            (0.200)                  (0.210)        
    ##                                                                                                    
    ## assignment:EthnicityBlack / African                                                 0.850**        
    ##                                                                                     (0.400)        
    ##                                                                                                    
    ## assignment:EthnicityCaucasian                                                       -0.230         
    ##                                                                                     (0.320)        
    ##                                                                                                    
    ## assignment:EthnicityHispanic / Latinx                                               -0.500         
    ##                                                                                     (0.690)        
    ##                                                                                                    
    ## assignment:EthnicityNative American                                                 2.200*         
    ##                                                                                     (1.200)        
    ##                                                                                                    
    ## assignment:EthnicityOther                                                           -0.440         
    ##                                                                                     (0.650)        
    ##                                                                                                    
    ## assignment:Age_bin21-40                                     0.700*                  0.880**        
    ##                                                            (0.390)                  (0.420)        
    ##                                                                                                    
    ## assignment:Age_bin41-60                                    0.840**                  1.800**        
    ##                                                            (0.420)                  (0.730)        
    ##                                                                                                    
    ## assignment:Age_bin61+                                       0.380                   -0.360         
    ##                                                            (0.700)                  (0.700)        
    ##                                                                                                    
    ## EthnicityBlack / African:Age_bin21-40                                                0.750         
    ##                                                                                     (0.620)        
    ##                                                                                                    
    ## EthnicityCaucasian:Age_bin21-40                                                      0.510         
    ##                                                                                     (0.410)        
    ##                                                                                                    
    ## EthnicityHispanic / Latinx:Age_bin21-40                                            2.400***        
    ##                                                                                     (0.910)        
    ##                                                                                                    
    ## EthnicityNative American:Age_bin21-40                                                1.600         
    ##                                                                                     (0.980)        
    ##                                                                                                    
    ## EthnicityOther:Age_bin21-40                                                                        
    ##                                                                                                    
    ##                                                                                                    
    ## EthnicityBlack / African:Age_bin41-60                                               -0.071         
    ##                                                                                     (0.710)        
    ##                                                                                                    
    ## EthnicityCaucasian:Age_bin41-60                                                     1.600**        
    ##                                                                                     (0.680)        
    ##                                                                                                    
    ## EthnicityHispanic / Latinx:Age_bin41-60                                             2.100*         
    ##                                                                                     (1.100)        
    ##                                                                                                    
    ## EthnicityNative American:Age_bin41-60                                              2.500***        
    ##                                                                                     (0.970)        
    ##                                                                                                    
    ## EthnicityOther:Age_bin41-60                                                                        
    ##                                                                                                    
    ##                                                                                                    
    ## EthnicityBlack / African:Age_bin61+                                                1.000***        
    ##                                                                                    (0.00000)       
    ##                                                                                                    
    ## EthnicityCaucasian:Age_bin61+                                                        0.074         
    ##                                                                                     (0.560)        
    ##                                                                                                    
    ## EthnicityHispanic / Latinx:Age_bin61+                                                              
    ##                                                                                                    
    ##                                                                                                    
    ## EthnicityNative American:Age_bin61+                                                                
    ##                                                                                                    
    ##                                                                                                    
    ## EthnicityOther:Age_bin61+                                                                          
    ##                                                                                                    
    ##                                                                                                    
    ## assignment:EthnicityBlack / African:Age_bin21-40                                   -2.600***       
    ##                                                                                     (0.690)        
    ##                                                                                                    
    ## assignment:EthnicityCaucasian:Age_bin21-40                                           0.230         
    ##                                                                                     (0.480)        
    ##                                                                                                    
    ## assignment:EthnicityHispanic / Latinx:Age_bin21-40                                                 
    ##                                                                                                    
    ##                                                                                                    
    ## assignment:EthnicityNative American:Age_bin21-40                                    -1.900         
    ##                                                                                     (1.500)        
    ##                                                                                                    
    ## assignment:EthnicityOther:Age_bin21-40                                                             
    ##                                                                                                    
    ##                                                                                                    
    ## assignment:EthnicityBlack / African:Age_bin41-60                                     0.300         
    ##                                                                                     (0.830)        
    ##                                                                                                    
    ## assignment:EthnicityCaucasian:Age_bin41-60                                          -0.900         
    ##                                                                                     (0.770)        
    ##                                                                                                    
    ## assignment:EthnicityHispanic / Latinx:Age_bin41-60                                                 
    ##                                                                                                    
    ##                                                                                                    
    ## assignment:EthnicityNative American:Age_bin41-60                                                   
    ##                                                                                                    
    ##                                                                                                    
    ## assignment:EthnicityOther:Age_bin41-60                                                             
    ##                                                                                                    
    ##                                                                                                    
    ## assignment:EthnicityBlack / African:Age_bin61+                                                     
    ##                                                                                                    
    ##                                                                                                    
    ## assignment:EthnicityCaucasian:Age_bin61+                                                           
    ##                                                                                                    
    ##                                                                                                    
    ## assignment:EthnicityHispanic / Latinx:Age_bin61+                                                   
    ##                                                                                                    
    ##                                                                                                    
    ## assignment:EthnicityNative American:Age_bin61+                                                     
    ##                                                                                                    
    ##                                                                                                    
    ## assignment:EthnicityOther:Age_bin61+                                                               
    ##                                                                                                    
    ##                                                                                                    
    ## Constant                                                   3.100***                3.500***        
    ##                                                            (0.280)                  (0.340)        
    ##                                                                                                    
    ## ---------------------------------------------------------------------------------------------------
    ## Observations                                                 313                      313          
    ## R2                                                          0.510                    0.530         
    ## Adjusted R2                                                 0.480                    0.470         
    ## Residual Std. Error                                    1.200 (df = 294)        1.200 (df = 274)    
    ## F Statistic                                        17.000*** (df = 18; 294) 8.200*** (df = 38; 274)
    ## ===================================================================================================
    ## Note:                                                                   *p<0.1; **p<0.05; ***p<0.01

``` r
mod1a <- lm(score_true ~ assignment, data_full)
mod2a <- lm(score_true ~ assignment+Mturk, data_full)
mod3a <- lm(score_true ~ assignment+Mturk+captcha+Age_bin+Party+Gender+Ethnicity, data_full)
ci_custom1a <- compute_robust_ci(mod1a)
ci_custom2a <- compute_robust_ci(mod2a)
ci_custom3a <- compute_robust_ci(mod3a)
stargazer(mod1a,mod2a,mod3a, type="text",ci.custom = list(ci_custom1a,ci_custom2a,ci_custom3a))
```

    ## 
    ## ============================================================================================
    ##                                                   Dependent variable:                       
    ##                            -----------------------------------------------------------------
    ##                                                       score_true                            
    ##                                    (1)                  (2)                    (3)          
    ## --------------------------------------------------------------------------------------------
    ## assignment                       -0.180               -0.180                 -0.150         
    ##                              (-0.410, 0.051)      (-0.410, 0.050)        (-0.380, 0.075)    
    ##                                                                                             
    ## Mturk                                                 0.340**                 0.050         
    ##                                                   (0.072, 0.610)         (-0.310, 0.410)    
    ##                                                                                             
    ## captcha                                                                     -0.600***       
    ##                                                                         (-0.890, -0.320)    
    ##                                                                                             
    ## Age_bin21-40                                                                 -0.190         
    ##                                                                          (-0.670, 0.280)    
    ##                                                                                             
    ## Age_bin41-60                                                                 -0.300         
    ##                                                                          (-0.800, 0.200)    
    ##                                                                                             
    ## Age_bin61+                                                                   -0.560         
    ##                                                                          (-1.400, 0.260)    
    ##                                                                                             
    ## PartyOther                                                                    0.085         
    ##                                                                          (-0.230, 0.400)    
    ##                                                                                             
    ## PartyRepublican                                                              -0.067         
    ##                                                                          (-0.350, 0.210)    
    ##                                                                                             
    ## GenderFemale                                                                  1.100         
    ##                                                                          (0.750, 1.400)     
    ##                                                                                             
    ## GenderMale                                                                    1.000         
    ##                                                                          (0.740, 1.300)     
    ##                                                                                             
    ## EthnicityBlack / African                                                     -0.084         
    ##                                                                          (-0.640, 0.470)    
    ##                                                                                             
    ## EthnicityCaucasian                                                            0.098         
    ##                                                                          (-0.240, 0.440)    
    ##                                                                                             
    ## EthnicityHispanic / Latinx                                                    0.280         
    ##                                                                          (-0.190, 0.750)    
    ##                                                                                             
    ## EthnicityNative American                                                     0.710**        
    ##                                                                          (0.120, 1.300)     
    ##                                                                                             
    ## EthnicityOther                                                               -0.160         
    ##                                                                          (-0.890, 0.570)    
    ##                                                                                             
    ## Constant                        3.600***             3.400***               3.100***        
    ##                              (3.500, 3.800)       (3.100, 3.600)         (2.500, 3.700)     
    ##                                                                                             
    ## --------------------------------------------------------------------------------------------
    ## Observations                       313                  313                    313          
    ## R2                                0.007                0.028                  0.140         
    ## Adjusted R2                       0.004                0.021                  0.094         
    ## Residual Std. Error         1.100 (df = 311)     1.000 (df = 310)       1.000 (df = 297)    
    ## F Statistic                2.300 (df = 1; 311) 4.400** (df = 2; 310) 3.200*** (df = 15; 297)
    ## ============================================================================================
    ## Note:                                                            *p<0.1; **p<0.05; ***p<0.01

``` r
mod4a <- lm(score_true ~ assignment*factor(Gender)+factor(Party)+Mturk+captcha, data_full)
mod5a <- lm(score_true ~ assignment*factor(Party)+factor(Gender)+Mturk+captcha, data_full)
mod6a <- lm(score_true ~ assignment*factor(Gender)*factor(Party)+Mturk+captcha, data_full)
ci_custom4a <- compute_robust_ci(mod4a)
ci_custom5a <- compute_robust_ci(mod5a)
ci_custom6a <- compute_robust_ci(mod6a)

se_custom4a <- compute_robust_se(mod4a)
se_custom5a <- compute_robust_se(mod5a)
se_custom6a <- compute_robust_se(mod6a)

stargazer(mod4a,mod5a,mod6a,type="text",se=list(se_custom4a,se_custom5a,se_custom6a))
```

    ## 
    ## =============================================================================================================================
    ##                                                                                  Dependent variable:                         
    ##                                                         ---------------------------------------------------------------------
    ##                                                                                      score_true                              
    ##                                                                  (1)                    (2)                     (3)          
    ## -----------------------------------------------------------------------------------------------------------------------------
    ## assignment                                                      -0.150                 0.022                  -0.012         
    ##                                                                (0.160)                (0.150)                 (0.220)        
    ##                                                                                                                              
    ## factor(Gender)Female                                           1.100***               1.200***               1.100***        
    ##                                                                (0.220)                (0.150)                 (0.280)        
    ##                                                                                                                              
    ## factor(Gender)Male                                             1.100***               1.200***               1.100***        
    ##                                                                (0.150)                (0.140)                 (0.160)        
    ##                                                                                                                              
    ## factor(Party)Other                                              0.059                  0.170                   0.140         
    ##                                                                (0.150)                (0.220)                 (0.330)        
    ##                                                                                                                              
    ## factor(Party)Republican                                         -0.051                 0.220                   0.280         
    ##                                                                (0.150)                (0.180)                 (0.260)        
    ##                                                                                                                              
    ## Mturk                                                           0.038                  0.046                   0.071         
    ##                                                                (0.150)                (0.150)                 (0.150)        
    ##                                                                                                                              
    ## captcha                                                       -0.720***              -0.710***               -0.710***       
    ##                                                                (0.130)                (0.130)                 (0.130)        
    ##                                                                                                                              
    ## assignment:factor(Gender)Female                                 -0.045                                         0.060         
    ##                                                                (0.230)                                        (0.300)        
    ##                                                                                                                              
    ## assignment:factor(Gender)Male                                                                                                
    ##                                                                                                                              
    ##                                                                                                                              
    ## assignment:factor(Party)Other                                                          -0.210                  0.100         
    ##                                                                                       (0.310)                 (0.430)        
    ##                                                                                                                              
    ## assignment:factor(Party)Republican                                                    -0.580**                -0.550         
    ##                                                                                       (0.270)                 (0.380)        
    ##                                                                                                                              
    ## factor(Gender)Female:factor(Party)Other                                                                        0.078         
    ##                                                                                                               (0.420)        
    ##                                                                                                                              
    ## factor(Gender)Male:factor(Party)Other                                                                                        
    ##                                                                                                                              
    ##                                                                                                                              
    ## factor(Gender)Female:factor(Party)Republican                                                                  -0.120         
    ##                                                                                                               (0.350)        
    ##                                                                                                                              
    ## factor(Gender)Male:factor(Party)Republican                                                                                   
    ##                                                                                                                              
    ##                                                                                                                              
    ## assignment:factor(Gender)Female:factor(Party)Other                                                            -0.920         
    ##                                                                                                               (0.570)        
    ##                                                                                                                              
    ## assignment:factor(Gender)Male:factor(Party)Other                                                                             
    ##                                                                                                                              
    ##                                                                                                                              
    ## assignment:factor(Gender)Female:factor(Party)Republican                                                       -0.056         
    ##                                                                                                               (0.540)        
    ##                                                                                                                              
    ## assignment:factor(Gender)Male:factor(Party)Republican                                                                        
    ##                                                                                                                              
    ##                                                                                                                              
    ## Constant                                                       3.000***               2.800***               2.800***        
    ##                                                                (0.220)                (0.210)                 (0.270)        
    ##                                                                                                                              
    ## -----------------------------------------------------------------------------------------------------------------------------
    ## Observations                                                     313                    313                     313          
    ## R2                                                              0.120                  0.130                   0.140         
    ## Adjusted R2                                                     0.091                  0.100                   0.097         
    ## Residual Std. Error                                        1.000 (df = 304)       1.000 (df = 303)       1.000 (df = 298)    
    ## F Statistic                                             4.900*** (df = 8; 304) 4.900*** (df = 9; 303) 3.400*** (df = 14; 298)
    ## =============================================================================================================================
    ## Note:                                                                                             *p<0.1; **p<0.05; ***p<0.01
