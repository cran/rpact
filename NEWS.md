
# rpact 2.04

* Incorrect output of function 'summary' fixed for getSampleSize[...] and getPower[...]
* as.data.frame: default value of argument 'niceColumnNamesEnabled' changed from TRUE to FALSE

# rpact 2.03

## New features

* Plot function for Fisher design implemented
* Generic function 'summary' implemented for getDesign[...], getSampleSize[...], getPower[...], and getSimulation[...] results: a simple boundary summary will be displayed

## Improvements, issues and changes

* Generic function as.data.frame improved for getDesign[...], getSampleSize[...], getPower[...], and getSimulation[...] results
* Ouput of getStageResults() improved
* Improvements for Shiny app compatibility and better Shiny app performance
* Repeated p-values are no longer calculated for typeOfDesign = "WToptimum"
* Piecewise suvival time improved for numeric definition: median and pi will not be calculated and displayed any longer
* Plot: legend title and tick mark positioning improved; optional arguments xlim and ylim implemented
* Sample size/power: usage of argument 'twoSidedPower' optimized
* Performance of function rpwexp/getPiecewiseExponentialRandomNumbers improved (special thanks to Marcel Wolbers for his example code)
* For group sequential designs a warning will be displayed if information rates from design not according to data information
* Format for output of standard deviation optimized

# rpact 2.02

* Minor corrections in the inline help
* Labeling of lower and upper critical values (effect scale) reverted
* Simulation for Fisher's combination test corrected
* Parameter minNumberOfAdditionalEventsPerStage renamed to minNumberOfEventsPerStage
* Parameter maxNumberOfAdditionalEventsPerStage renamed to maxNumberOfEventsPerStage
* Parameter minNumberOfAdditionalSubjectsPerStage renamed to minNumberOfSubjectsPerStage
* Parameter maxNumberOfAdditionalSubjectsPerStage renamed to maxNumberOfSubjectsPerStage
* Output of function getAccrualTime() improved
* Validation of arguments maxNumberOfIterations, allocation1, and allocation2 added: check for positive integer 
* Function getSampleSizeSurvival improved: numeric search for accrualTime if followUpTime is given
* Default value improved for analysis tools: if no effect was specified for conditional power calculation, the observed effect is selected
* Fixed: function getDataset produced an error if only one log-rank value and one event was defined
* Number of subjects per treatment arm are provided in output of simulation survival if allocation ratio != 1
* Function getSimulationSurvival improved: first value of minNumberOfEventsPerStage and maxNumberOfEventsPerStage must be NA or equal to first value of plannedSubjects

# rpact 2.0.1

* Function base::isFALSE replaced to guarantee R 3.4.x compatibility
* C++ compiler warning on r-devel-linux-x86_64-debian-clang system removed 
* C++ compiler error on r-patched-solaris-x86 system fixed 

# rpact 2.0.0

## New features

* Power calculation at given or adapted sample size for means, rates and survival data
* Sample size and power calculation for survival trials with piecewise accrual time and intensity
* Sample size and power calculation for survival trials with exponential survival time, piecewise exponential survival time and survival times that follow a Weibull distribution
* Simulation tool for survival trials; our simulator is very fast because it was implemented with C++. Adaptive event number recalculations based on conditional power can be assessed
* Simulation tool for designs with continuous and binary endpoints. Adaptive sample size recalculations based on conditional power can be assessed
* Comprehensive and unified tool for performing sample size calculation for fixed sample size design
* Enhanced plot functionalities

## Improvements, issues and changes

* Fisher design, analysis of means or rates, conditional rejection probabilities (CRP): calculation issue fixed for stage > 2
* Call of getSampleSize[Means/Rates/Survival] without design argument implemented
* For all 'set.seed' calls 'kind' and 'normal.kind' were specified as follows: kind = "Mersenne-Twister", normal.kind = "Inversion"
* Minor code optimizations, e.g. 'return()' replaced by 'return(invisible())' if reasonable
* Bug in 'readDatasets' fixed: variable names 'group' and 'groups' are now accepted
* "Overall reject per stage" and "Overall futility per stage" renamed to "Overall reject" and "Overall futility", respectively (also variable names).
* Labels "events.." and "..patients.." consistently changed to "# events.." and "# patients...", respectively.
* Output format for 'allocationRatioPlanned' specified
* Method 'show' of class 'ParameterSet' expanded: R Markdown output features implemented
* getSampleSizeSurvival(): argument 'maxNumberOfPatients' was renamed in 'maxNumberOfSubjects'
* Result output, inline help and documentation: the word 'patient' was replaced by 'subject'
* Variables 'numberOfSubjectsGroup1' and 'numberOfSubjectsGroup2' were renamed to 'numberOfSubjects1' and 'numberOfSubjects1'
* Final p-values for two-sided test (group sequential, inverse normal, and Fisher combination test) available
* Upper and lower boundaries on effect scale for testing rates in two samples

# rpact 1.0.0

* First release of rpact
