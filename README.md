# Topic Modeling

## Project summary
  This R script analyzed data from a social psychology experiment about how feelings of power affects perspective-taking. 
### About the study and data:
The study is from [the ManyLabs project](https://osf.io/ct89g/). It has a sample size larger than most psychology studies (> 3,000 in this study).

**Independent Variable:** In this study, participants wrote about a time when they were powerful (vs. powerless), to induce feelings of power (vs. powerlessness).

**Dependent Variable:** Participants compeleted a measure of perspective-taking after the power manipulation. 
### Goal of analyses: 
* We will use topic modeling to capture the themes of the essays. 
* We will explore if the themes of essays can tell us more about the effect of power on perspective-taking.  

## What you need:
* The analysis script in R: Power Text Topic Model_Github.R
* The dataset: ML3AllSitesandmTurk.csv

## Main methods: 
* Topic modeling: 
    I used tidytext to preprocess the text data. 
    Then I used LDA model to build topic models to describe the themes in the essays. 
* Exploratory analyses using the generated topics:
    I used regression analyses to test whether the relationship between the independent (power) and dependent (perspective-taking) variables changed when people wrote about different topics. 

## Results:
* Four topics emerged from the essays, I summarized it with the following labels. 
  1. Close relationships: friends and family
  2. Relationships at work: manager and evaluation
  3. Relationships at school: student and teacher
  4. Relationships in team sports: player and coach 
 
The most frequent words used in each topic: 

<img src="/images/TopWords_4Topics.png" width="700">



