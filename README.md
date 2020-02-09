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

## Findings:
### Topics
Four topics emerged from the essays, the most frequent word-stems used in each topic are shown below: 
<img src="/images/TopWords_4Topics.png" width="700">

To help you understand "what the topics are" better, I used the following labels to summarize the topics. For each topic, you can also find an excerpt from an representative essay below.
* **1. Close relationships: friends and family.** 
"I want a car on campus, even thought I'm a resident it would feel nice to leave campus without being at the mercy of someone driving you. I keep asking my parents if they could help me but they keep saying no..."

* **2 Relationships at work: manager and evaluation** 
"I'm the Human Resources Manager at the Distribution Center for the company I work for.  I'm usually the person that hires and terminates employees.  I evaluate individuals all the time for positions within our company..."  

* **3 Relationships at school: student and teacher**  
"I was a teacher in a private school in summer. I realised the power of my pen. I can pen down future of any student. For e.g  the marks I gave to one of my student allowed him to go to a University he dreamt of..."

* **4 Relationships in team sports: player and coach**

"I played golf for my high school golf team and in my second year the coach decided to take another player to a tournament. At that tournament that player performed poorly and then the coach decided to take that same player to another tournament the next week over me again..."

### The effect of power on perspective-taking could be different if people wrote about power experienced in different contexts
Regression analysis showed that when people wrote primarily about power exeprience in the "School" context, the low power group had better perspective-taking than the high power group (Cohen's D = 0.17). When people wrote primarily about other topics, power did not seem to affect perspective-taking.  
<img src="/images/PowerEffectByTopic.png" width="700"> 

Error bars are 95% Confidence Intervals.
 




