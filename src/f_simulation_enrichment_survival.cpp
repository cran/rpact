/**
 *
 * -- Simulation of survival data with group sequential and combination test --
 *
 * This file is part of the R package rpact:
 * Confirmatory Adaptive Clinical Trial Design and Analysis
 *
 * Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
 * Licensed under "GNU Lesser General Public License" version 3
 * License text can be found here: https://www.r-project.org/Licenses/LGPL-3
 *
 * RPACT company website: https://www.rpact.com
 * rpact package website: https://www.rpact.org
 *
 * Contact us for information about our services: info@rpact.com
 *
 * File version: $Revision: 5428 $
 * Last changed: $Date: 2021-10-27 20:23:49 +0200 (Mi, 27 Okt 2021) $
 * Last changed by: $Author: pahlke $
 *
 */

#include <Rcpp.h>
#include "f_utilities.h"
#include "f_simulation_survival_utilities.h"
using namespace Rcpp;

Function subset("[.data.frame");


// [[Rcpp::export]]
List getStratifiedLogRankTestCpp(DataFrame survivalDataSet, double time, bool directionUpper, double thetaH0 = 1.0) {

    double numerator = 0, denominator = 0, eventNumber = 0;
    int subjectsNumber = 0;

    NumericVector strata = survivalDataSet["stratum"];
    for (int iStratum = 1; iStratum <= max(strata); iStratum++) {
        NumericVector survivalDataSetStratumVec = survivalDataSet["stratum"];
        LogicalVector stratumEqualsi(survivalDataSetStratumVec.length());
        for (int i = 0; i < stratumEqualsi.length(); i++) {
            stratumEqualsi[i] = survivalDataSetStratumVec[i] == iStratum;
        }
        DataFrame survivalDataSetStratum = subset(survivalDataSet, stratumEqualsi, R_MissingArg);
        NumericVector survivalDataSetStratumAccrualTime = survivalDataSetStratum["accrualTime"];
        int numberOfSubjects = survivalDataSetStratumAccrualTime.length();
        NumericVector survivalDataSetStratumTreatmentGroup = survivalDataSetStratum["treatmentGroup"];
        NumericVector survivalDataSetStratumSurvivalTime = survivalDataSetStratum["survivalTime"];
        LogicalVector survivalDataSetStratumEvent(numberOfSubjects); //needs to be generated
        NumericVector survivalDataSetStratumTimeUnderObservation(numberOfSubjects); //needs to be generated

        double subjectsT1 = 0;
        double subjectsT2 = 0;
        for (int i = 0; i < numberOfSubjects; i++) {
            if (survivalDataSetStratumAccrualTime[i] > time) {
                survivalDataSetStratumTreatmentGroup[i] = 0;
                survivalDataSetStratumEvent[i] = false;
            } else {
                if (survivalDataSetStratumTreatmentGroup[i] == 1) {
                    subjectsT1++;
                } else if (survivalDataSetStratumTreatmentGroup[i] == 2) {
                    subjectsT2++;
                }
            }

            if (survivalDataSetStratumAccrualTime[i] + survivalDataSetStratumSurvivalTime[i] < time &&
            		survivalDataSetStratumTreatmentGroup[i] > 0) {
                survivalDataSetStratumEvent[i] = true;
            } else {
                survivalDataSetStratumEvent[i] = false;
            }
            if (survivalDataSetStratumEvent[i]) {
                survivalDataSetStratumTimeUnderObservation[i] = survivalDataSetStratumSurvivalTime[i];
            } else {
                survivalDataSetStratumTimeUnderObservation[i] = time - survivalDataSetStratumAccrualTime[i];
            }
        }
        survivalDataSetStratum = DataFrame::create(
        	_["accrualTime"] = survivalDataSetStratumAccrualTime,
			_["treatmentGroup"] = survivalDataSetStratumTreatmentGroup,
			_["survivalTime"] = survivalDataSetStratumSurvivalTime,
			_["event"] = survivalDataSetStratumEvent,
			_["timeUnderObservation"] = survivalDataSetStratumTimeUnderObservation);

        IntegerVector sortedIndex = getOrder(survivalDataSetStratumTimeUnderObservation, false);
        DataFrame survivalDataSetSorted = subset(survivalDataSetStratum, sortedIndex, R_MissingArg);
        NumericVector survivalDataSetSortedEvent = survivalDataSetSorted["event"];
        NumericVector survivalDataSetSortedTreatmentGroup = survivalDataSetSorted["treatmentGroup"];

        int events1 = 0, events2 = 0;

        for (int i = 0; i < numberOfSubjects; i++) {
            if (survivalDataSetSortedEvent[i]) {
                if (survivalDataSetSortedTreatmentGroup[i] == 1) {
                    if (subjectsT1 + subjectsT2 > 0) {
                        numerator += -thetaH0 * subjectsT2 / (subjectsT1 + thetaH0 * subjectsT2);
                    }
                    events1++;
                } else if (survivalDataSetSortedTreatmentGroup[i] == 2) {
                    if (subjectsT1 + subjectsT2 > 0) {
                        numerator += 1 - thetaH0 * subjectsT2 / (subjectsT1 + thetaH0 * subjectsT2);
                    }
                    events2++;
                }
                if (subjectsT1 + subjectsT2 > 0) {
                    denominator += thetaH0 * subjectsT1 * subjectsT2 / pow(subjectsT1 + thetaH0 * subjectsT2, 2);
                }
            }

            if (survivalDataSetSortedTreatmentGroup[i] == 1) {
                subjectsT1--;
            } else if (survivalDataSetSortedTreatmentGroup[i] == 2) {
                subjectsT2--;
            }
        }
        eventNumber += events1 + events2;
        subjectsNumber += numberOfSubjects;
    }

    double strLogRank = denominator > 0 ? -numerator / sqrt(denominator) : R_NegInf;
    if (!directionUpper) {
    	strLogRank = -strLogRank;
    }

    return List::create(
		_["strLogRank"] = strLogRank,
		_["thetaH0"] = thetaH0,
		_["directionUpper"] = directionUpper,
		_["eventNumber"] = eventNumber,
		_["subjectsNumber"] = subjectsNumber);
}

// [[Rcpp::export]]
List getSimulationStratifiedLogRankCpp(
		int kMax,
		NumericVector criticalValues,
		NumericVector lambda2,
		NumericVector lambda1,
		NumericVector prevalences,
		bool directionUpper,
		int maxNumberOfSubjects,
		NumericVector accrualTime,
		NumericVector plannedEvents,
		double allocation1,
		double allocation2,
		int maxIterations,
		DataFrame survivalDataSet) {

    NumericVector simulatedAnalysisTime(kMax);
    NumericVector simulatedSubjects(kMax);
    NumericVector simulatedEvents(kMax);
    NumericVector simulatedRejections(kMax);
    NumericVector simulatedEventsNotAchieved(kMax);
    double simulatedDuration = 0;
    NumericVector iterations(kMax);

    IntegerVector survivalDataSetTreatmentGroup = survivalDataSet["treatmentGroup"];
    NumericVector survivalDataSetSurvivalTime(maxNumberOfSubjects);
    NumericVector survivalDataSetStratum = survivalDataSet["stratum"];
	for (int j = 1; j <= maxIterations; j++) {
        for (int iSubject = 0; iSubject < maxNumberOfSubjects; iSubject++) {
        	double lambda;
            if (survivalDataSetTreatmentGroup[iSubject] == 1) {
            	lambda = lambda1[survivalDataSetStratum[iSubject] - 1];
            } else {
            	lambda = lambda2[survivalDataSetStratum[iSubject] - 1];
            }
            //double x = rexp(1.0, lambda)[0];
            double x = getRandomExponentialDistribution(lambda);
            if (Rcpp::traits::is_nan<REALSXP>(x)) {
            	x = R_PosInf;
            }
            survivalDataSetSurvivalTime[iSubject] = x;
        }
        survivalDataSet["survivalTime"] = survivalDataSetSurvivalTime;

        for (int k = 0; k < kMax; k++) {
			double observationTime = findObservationTime(
				survivalDataSet["accrualTime"],
				survivalDataSetSurvivalTime,
				survivalDataSet["dropoutTime"],
				(double) plannedEvents[k]);
			if (R_IsNA(observationTime)) {
				simulatedEventsNotAchieved[k]++;
				break; // trial stop
			}

			List survivalResult = getStratifiedLogRankTestCpp(
				survivalDataSet, observationTime, directionUpper);

			simulatedEvents[k] += (double) survivalResult["eventNumber"];
			simulatedAnalysisTime[k] += observationTime;
			iterations[k]++;

			if ((double) survivalResult["strLogRank"] >= criticalValues[k]) {
				simulatedRejections[k]++;
				simulatedDuration += observationTime;
				simulatedSubjects[k] += (int) survivalResult["subjectsNumber"];
				break; // trial stop
			} else {
				simulatedSubjects[k] += (int) survivalResult["subjectsNumber"];
				if (k == kMax - 1) {
					simulatedDuration += observationTime;
				}
			}
        }
    }

    return List::create(
		_["iterations"] = iterations,
		_["events"] = simulatedEvents / iterations,
		_["eventsNotAchieved"] = simulatedEventsNotAchieved / maxIterations,
		_["rejectPerStage"] = simulatedRejections / maxIterations,
		_["overallReject"] = vectorSum(simulatedRejections / maxIterations),
		_["analysisTime"] = simulatedAnalysisTime / iterations,
		_["duration"] = simulatedDuration / maxIterations,
		_["numberOfSubjects"] = simulatedSubjects / iterations);
}

