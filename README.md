# Subgroup Identification Toolbox (SIDToolox)

This package provides utilities to identify subgroups in clinical trial datasets. 
Currently the package supports one-arm and two-arm studies with numeric, binary, count, or survival outcomes.
All types of covariates are allowed. Note that covariates must be measured at *baseline*. 

A subgroup is defined as a set of *rules*. Each rule is combined of a *covariate*, a "constraint", and a *cutoff*.
For example in the rule "AGE > 65", "AGE" is the selected covariate, ">" is the constraint, and "65" is the cutoff.
A subgroup is defined as the *AND* of several rules.  A subgroup can further be restricted by its *depth*, which is how many rules it has.
As the depth of a subgroup increases, the number of patients in the subgroup decreases. Number of patients in a subgroup can also be restricted.
Setting *minimum subgroup size* ensures that the subgroup has clinical significance.

Subgroups can be *prognostic* or *predictive*. In the case of prognostic subgroups, the covariates are not predictive. 
This applies to categorial covariates such as SEX, and well as numeric covariates i.e `Y ~ a + RULE * b` where Y is the outcome.
In predictive subgroups, however, the covariate is correlated to the outcome i.e. `Y ~ a + RULE * b * X` where Y is the outcome and X is the covariate.
sidToolbox facilitates finding the informatic covariates for both prognostic and predictive.


## How to use the Shiny app

This app follows a four step workflow:
  1. Load data (real data or simulate data)
  2. Explore the data
  3. Identify subgroups
  4. Analyze the rules

### Setup

Maybe the most difficult part of the app is to load data correctly. 
Once you have uploaded your data, the rest of the steps run automatically.
You can choose to load your custom dataset or simulate a case study for benchmarking purposes.

### 1.A. Load dataset

In the *Setup* tab you can upload a *tab or semicolon separated text file*. 
Upload the data and then press *Load data*. 
The main panel will be populated with the features (columns of the data).
You need to specify:

**1.A.1. Contrast:** If this is a two-arm study, What is the contrast variable and what is the control variable (for example "PSO")? 
If you have a one arm study, leave the contrast field empty.

**1.A.2. Covariates:** Choose the covariates from the list of variables.

**1.A.3. Outcomes and outcome types:** Choose the outcomes from the list of variables. 
For each outcome you will need to specify the type of it. 
Currently we support numeric, binary, count, and survival data.

**1.A.4. Save settings:** Finally, save the setting by pressing *Save feature settings". 
If you do not save the setting, you will not be able to use the rest of the app. This will use the `sidtoolbox::loadDataset() function`

### 1.B. Simulate study

You can also simulate a one-arm or two-arm study. 
A random subgroup will be implanted in the study.
The subgroup can be predictive or prognostic.

**1.B.1. Study size:** 
You can set the number of patients (No. of samples).

**1.B.2. Prognostic vs. predictive:** 
You can choose the type of subgroup to implant. 
Note that the subgroup will be made randomly.

**1.B.3. Background treatment effect:** 
Choose the overall treatment effect.

**1.B.4. Choose the true positive (TP) subgroup enhancement effect:** 
The subgroup will be enhanced by this amount. 
Note that this is the b in the formulas: `Y ~ a + subgroup * b` for prognostic and `Y ~ a + subgroup * b * X` for predictive subgroups.
Here subgroup is an indicator function of 

- 1: if in subgroup,
- 0 if not.

**1.B.5. Choose the false positive (FP) shift effect:** 
For two-arm studies we can place a false positive subgroup in which both arms are enhanced. 
Thus there will be no enhancement between the arms.
In the case of one-arm studies, this will be a random subgroup with no enhancement.

**1.B.6. Subgroup size:**
Here set the approximate subgroup size.

**1.B.7. Ratio of treatment arm:**
For a two-arm study you can specify what presentage of the data will be in the treatment arm vs. the control arm.
To get a one-arm study set the `trt ratio=1`.

**1.B.8. Subgroup depth:**
You specify how deep the subgroup should be.

**1.B.9. Covariates:**

- For each type of covariate enter the parameters of it. 
- For *normal* covariates, specify the *mean and sd*. Each covariate on one line. 
- For *uniform* covariates, specify the *min and max*. Each covariate on one line. 
- For *binomial* covariates, specify the *size and probability*. Each covariate on one line. 
- For *binary* covariates, specify the *probability*. Each covariate on one line. 
- You can add random correlation between normal variables. Enter the amount of each correlation pair on one line. You can see the random correlation in the explore tab.

**1.B.10. Simulate data:** 
Finally press *simulate data*. 
Three outcomes "Y_numeric", "Y_binary", and "Y_count" will be made. 
The main panel will be populated. 
You can see the implanted subgroups in the third tab.

### 2. Explore the data

**2.1. Correlation matrix:** 
Correlation matrix allows you to see one to one correlations between numeric variables.

**2.2. Association rules:**
Association rules help us find high level biases in the data. 
The rules are controled by **support** (how large the corresponding subgroup is) 
and **confidence** (how many of the total subgroup size follow the rule).
Support shows us how frequently the rule (left hand side) is in the data, similar to subgroup size. 
Confidence is the number of times the rule is correct (right hand side | left hand side), 
and thus is a measure of strength for the rule. The minimum and maximum depth of the rule can be set by **min_order** and **max_order**.
By filtering RHS (right hand side) for the outcome variables you can find immediate subgroups with highest support.

**2.3. Conditional inference trees:**
You can find biases using these helpful trees.
see: 
<a href="https://www.rdocumentation.org/packages/party/versions/1.3-3/topics/Conditional%20Inference%20Trees">R party documentation</a>

**2.4. Clustering analysis:** Coming soon...

### 3. Subgroup identification
Three methods are supported:

-  Treatment-Specific Subgroup Detection Tool (TSDT): supports one-arm and two-arm studies with numeric, binary and survival outcome.
We have added count outcomes by log transforming count measures. Use with caution.

    Battioui, C., Shen, L., Ruberg, S., (2014). A Resampling-based Ensemble Tree Method to Identify Patient Subgroups with Enhanced Treatment Effect. JSM proceedings, 2014

    Shen, L., Battioui, C., Ding, Y., (2013). Chapter "A Framework of Statistical methods for Identification of Subgroups with Differential Treatment Effects in Randomized Trials" in the book "Applied Statistics in Biomedicine and Clinical Trials Design"
    
- Virtual Twins
Currently supports only two-arm studies with binary outcomes.

https://rdrr.io/cran/aVirtualTwins/src/R/aVirtualTwins.R

Foster, Jared C., Jeremy MG Taylor, and Stephen J. Ruberg. "Subgroup identification from randomized clinical trial data." Statistics in medicine 30.24 (2011): 2867-2880.

- PSO
Methodology to be added. To get any possible depth, set `depth=0`. 

https://www.rdocumentation.org/packages/metaheuristicOpt/versions/2.0.0/topics/PSO


### 4. Analysis

Each method has a different outcome format. Here all rules are summarized for comparison.
Note that these rules need further analysis by statisticiansto evaluate their significance 
and  and clinicians to evaluate the clinical relevance.

The features in the rule summary table/plot are:

- **N_ratio:** size of subgroup
- **effect_size:** Effect size is the interaction effect size of the 
`Y ~ subgroup + contrast + subgroup*contrast` term in a two arm study 
or the effect size in `Y ~ subgropup` in a one-arm study.
- **p-value:** -log10 transformed of the p-value of the model.
- **method:** the method that the subgroup was obtained from.
Acknowledgement: Thanks to **Xu Shu** for helping me with modeling.
- **outcome:** the outcome that the subgroup was found on. If empty, means all (random subgroup or parato PSO - to be added).

In case of a simulation study, the precision, recall and F1 measure will be shown (compared to the true positive random subgroup).


To draw a **funnel plot ** set x-axis to N_ratio and y-axis to p-value and size to effect_size.

To draw a **volcano plot ** set x-effect_size to N_ratio and y-axis to p-value.

In case of a simulation study, to draw **precision-recall plot** set x-axis to precision and y-axis to recall.

## How to use the R package

You can download the R package from gitlab at:

https://gitlabce.statwb.eu.novartis.net/scc/sidtoolbox

See the two demo Rmarkdowns, `demo_onearm.Rmd` and `demo_twoarm.Rmd` for guidance on how to use the functionalities.
You could also run a user friendly Shiny app which demostrates the functionalities in an interactive fashion.
To run the app, use the `runSidApp()`. 

# Acknowledges
Douglas Robinson, Xiai Ni, Xu Shu
This work was done as my internship project at Novartis with the Scientific Computing and Consulting team in GDD.

# Contact
For further information or suggestions please contact Xiao: 
"NI, XIAO" <xiao.ni@novartis.com>
or author Marzie Rasekh: marzie.rasekh@gmail.com



