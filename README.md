# Project Robyn 3.0 - Continuous & Semi-Automated MMM
### The Open Source Marketing Mix Model Package from Facebook Marketing Science

2021-09-29

### Quick start (R only)

**1. Installing the package**

  * Run `install.packages('devtools')` if you haven't installed devtools yet.
  
  * Run `devtools::install_github('facebookexperimental/Robyn@package_test')` to install the package.
  
  * This package is built on R version 4.1.0 (2021-05-18). It's recommended to update to this version or above. 
  
  * Robyn requires the Python library [Nevergrad](https://facebookresearch.github.io/nevergrad/). If encountering Python-related 
  error during installation, please check out the step-by-step guide to get more info.
  
  * For Windows, if you get openssl error, please see instructions
  [here](https://stackoverflow.com/questions/54558389/how-to-solve-this-error-while-installing-python-packages-in-rstudio/54566647) and
  [here](https://dev.to/danilovieira/installing-openssl-on-windows-and-adding-to-path-3mbf) to install and update openssl
  
**2. Getting started**

  * Use this [demo.R](https://github.com/facebookexperimental/Robyn/blob/package_test/inst/demo.R) script as step-by-step guide that is
  intended to cover most common use-cases. Test the package using simulated dataset provided in the package. 
  
  * Visit our [website](https://facebookexperimental.github.io/Robyn/) to explore more details about Project Robyn.
  
  * Join our [public group](https://www.facebook.com/groups/robynmmm/) to exchange with other users and interact with team Robyn.


### Introduction

  * **What is Robyn**: Robyn is an experimental, semi-automated and open-sourced Marketing Mix Modeling (MMM) package from Facebook 
  Marketing Science. It uses various machine learning techniques (Ridge regression with cross validation, multi-objective evolutionary 
  algorithm for hyperparameter optimisation, time-series decomposition for trend & season, gradient-based optimisation for budget allocation
  etc.) to define media channel efficiency and effectivity, explore adstock rates and saturation curves. It's built for granular datasets 
  with many independent variables and therefore especially suitable for digital and direct response advertisers with rich data sources. 
  
  * **Why are we doing this**: MMM used to be a resource-intensive technique that was only affordable for "big players". As the privacy 
  needs of the measurement landscape evolve, there's a clear trend of increasing demand for modern MMM as a privacy-safe solution. At 
  Facebook Marketing Science, our mission is to help all businesses grow by transforming marketing practices grounded in data and science. 
  It's highly aligned with our mission to democratising MMM and making it accessible for advertisers of all sizes. With Project Robyn, we 
  want to contribute to the measurement landscape, inspire the industry and build a community for exchange and innovation around the future 
  of MMM and Marketing Science in general.
  
### Proof of concept

We're very proud to see that there're already 100+ known users of Project Robyn since it's initial release in November 2020, while we're certain that the unknown number of users will be even higher. Two of the users have achieved remarkable success that we want to share with you.

  * **[Resident](https://www.facebook.com/business/success/resident) from Israel**: 5 days to implement the Robyn model compared to 5 
  working months to implement its in-house model
  * **[Central Retail](https://www.facebook.com/business/success/central-retail-corporation) from Thailand**: 28% increase in revenue 
  possible with reallocated budget
  
### Core capabilities

  * **Semi-automated modelling process**: Robyn automatically returns a set of business-relevant and Pareto-optimum results by optimizing on
  the model fit and business fit over larger iterations. Building MMM manually is a very time-consuming process that involves many subjective decisions, modelling experience and trial and error over hundreds of iterations. Months of effort is common to build MMM from scratch. Robyn is able to automate a large portion of the modelling process (see Resident case above) and thus reduce the "analyst-bias". Pure model run time for recommended 10k iterations on a laptop is <1 hour. Technically speaking, Robyn leverages the multi-objective optimization capacity of Facebook's evolutionary optimization platform Nevergrad to minimize both prediction error (NRMSE, normalized root-mean-square error) and decomposition distance (DECOMP.RSSD, decomposition root-sum-square distance, a major innovation of Robyn) at the same time and eliminates the majority of "bad models" (larger prediction error and/or unrealistic media effect like the smallest channel getting the most effect). 
  
  * **Continuous reporting**: After the initial model is built and selected, the new `robyn_refresh()` function is able to continuously
  build model refresh at any given cadence based on previous model result. This capability enables using MMM as a continuous reporting tool 
  and therefore makes MMM more actionable.
  
  * **Trend & season decomposition**: Robyn leverages Facebook's time-serie-forecast package Prophet to decompose trend, season, holiday and
  weekday as model predictor out of the box. This capability often increases model fit and reduces autoregressive pattern in residuals.
  
  * **Rolling window**: In Robyn 3.0, user can specify `window_start` and `window_end` in the `robyn_inputs()` function to set modelling 
  period to a subset of available data. A, important capability to keep MMM returning up-to-date results frequently. At the same time,
  baseline variables like trend, season, holiday and weekday are still derived from the entire dataset, ensuring higher accuracy for 
  time-serie decomposition. For example, with Robyn's integrated dataset of 208 weeks, user can set 100 weeks as modelling window while 
  trend, season, holiday and weekday are derived from all 208 weeks.
  
  * **Granular dataset**: Robyn is able to deal with larger dataset with multicollinearity. It's common to have similar spending pattern 
  among Marketing channels, for example increasing spend for multiple channels around Christmas. Robyn uses Ridge regression to deal with 
  the inherent multicollinearity in Marketing dataset naturally, selects predictors by penalisation natually and prevents overfitting 
  without doing computationally intensive time-serie cross validation.
  
  * **Organic media**: In Robyn 3.0, user can specify `organic_vars` to model Marketing activities that have no spend. Typically, this 
  includes newsletter, push notification, social media posts etc. Technically speaking, organic variables are expected to have similar 
  carried-over (adstock) and saturating behavior as paid media variables. The respective transformation techniques (Geometric or Weibull 
  transformation for adstock; Hill transformaiton for saturation) as treatment for these behaviours are now also applied for organic 
  variables.
  
  * **Experimental calibration**: We believe integrating experimental results into MMM is the best choice for model selection. As the general aphorism in statistics "all models are wrong but some are useful", there's no reliable way to select final MMM results, even after Robyn has accounted for the business fit with the DECOMP.RSSD as objective function. Experiments (RCT, randomised controlled trials) are causal by nature and thus are seen as ground-truth. Common experimental tools include people-based technique like Facebook [Conversion Lift](https://www.facebook.com/business/m/one-sheeters/conversion-lift) and geo-based technique like Facebook [GeoLift](https://github.com/ArturoEsquerra/GeoLift/), among others. Technically speaking, Robyn drives model results closer to experimental results by using MAPE.LIFT as the third objective function besides NRMSE & DECOMP.RSSD when calibrating and minimizing the error between predicted and experimental results.
  
  * **Custom adstock**: Robyn offers Geometric and Weibull as adstock options to enable more customisation and flexibility in adstock transformation.
  
  * **S-shape saturation**: Robyn uses the Hill function that is able to transform between C- and S-shape to enable more customisation and flexibility in saturation transformation. 
  
  * **Budget allocator**: blabla
  
  * **Automated output**: blabla
  

### Q&A

  * xxx

### Change log

  * xxx
  
  * xxx


## License

FB Robyn MMM R script is MIT licensed, as found in the LICENSE file.

- Terms of Use - https://opensource.facebook.com/legal/terms 
- Privacy Policy - https://opensource.facebook.com/legal/privacy


## Contact

* gufeng@fb.com, Gufeng Zhou, Marketing Science Partner
* leonelsentana@fb.com, Leonel Sentana, Marketing Science Partner
* igorskokan@fb.com, Igor Skokan, Marketing Science Partner



**3. Test run with sample data**
  * Please follow all instructions in fb_robyn.exec.R
  * After above steps, if you select all and run in fb_robyn.exec.R, the script should execute 20k iterations (500 iterations * 40 trials) and save some plots on your selected folder
  * An example model onepager looks like this:
![result](https://user-images.githubusercontent.com/14415136/110111544-c81d1f80-7db0-11eb-9a9f-51249514baae.png)

  * The final function f.budgetAllocator() might throw error "provided ModID is not within the best result". First of all, please read all instructions behind the function. Model IDs are encoded in each onepager .png name and also in the title. Also, execute model_output_collect$allSolutions will output all final model IDs. Please pick one and put it into f.budgetAllocator(). 
  * An example optimised model looks like this:
![result_optimised](https://user-images.githubusercontent.com/14415136/110111552-ceab9700-7db0-11eb-84b5-9f105c49b09b.png)


## Step-by-step Guide Website

* Guidelines on the website: https://facebookexperimental.github.io/Robyn/docs/step-by-step-guide

## Model selection with evolutionary algorithm

Using Facebook AI's open source gradient-free optimisation library [Nevergrad](https://facebookresearch.github.io/nevergrad/), Robyn is able to leverage evolutionary algorithms to perform multi-objective hyperparameter optimisation and output a set of Pareto-optimal solutions. Besides NRMSE as loss function for the optimisation, Robyn also minimises on a business logic "decomposition distance", or DECOMP.RSSD that is aiming to steer the model towards more realistic decomposition results. In case of calibration, a third loss function MAPE.LIFT is added too.

The following plot demonstrates typical Pareto fronts 1-3 on NRMSE and DECOMP.RSSD:
![paretofront](https://user-images.githubusercontent.com/14415136/110000483-a3269f00-7d13-11eb-85de-0bae918f4f5c.png)


## Join the FB Robyn MMM community. **Coming soon**



See the [CONTRIBUTING](CONTRIBUTING.md) file for how to help out.


