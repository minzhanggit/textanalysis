# Topic Modeling

## Project summary
  This R script analyzed data from a social psychology experiment about how feelings of power affects perspective-taking. 
  ### About the study and data:
    The study is from [the ManyLabs project](https://osf.io/ct89g/). It has a sample size larger than most psychology studies (> 3,000 in this study).
    Independent Variable: In this study, participants wrote about a time when they were powerful (vs. powerless), to induce feelings of power (vs. powerlessness). 
    Dependent Variable: Participants also compeleted a measure of perspective-taking. 
  ### Goal of analyses: 
    We will use topic modeling to capture the themes of the essays. 
    We will also explore if the themes of essays can tell us more about the effect of power on perspective than simplying comparing the powerful and powerless group.  

## What you need:
  1. The analysis script in R: Power Text Topic Model_Github.R
  2. The dataset: ML3AllSitesandmTurk.csv

## Main methods: 
  1. Topic modeling: 
    I used tidytext for the necessary preprocessing of the text data. 
    Then I used LDA model to build topic models to describe the themes in the essays. 
  2. Exploratory analyses using the generated topics:
    I used regression analyses to test whether the relationship between the independent and dependent variables changes when people wrote about different topics

## Results:




