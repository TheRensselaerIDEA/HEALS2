## generateStagingData
## author: alexander new

''' 
Generate synthetic cancer records. We generate 7 variables:
T, M, N, Grade, HER2, ER, PR
The frequencies of T,N,M match those found in the slice of SEER Sabbir gave us.
HER2, ER, and PR are independent and uniform, although this could be changed.

Questions:
(1) What is the full list of values T, N, M, and Stage can take on? The SEER 
dataset and slide 16 of the Staging Presentation do not fully agree with each 
other wrt terminology.
For example, 'T0-1' is a valid value for T in the Staging Presentation, but
this value does not appear in the SEER dataset.

(2) Can we get a better idea of the dependency structure between T,N,M and the
other variables? Is getting a better idea of this structure useful for the demo?

(3) Should we also generate a staging variable? If we want it to be consistent
with the T,N,M frequencies, it would have to be the old version.
'''

import numpy as np
import pandas as pd

## TNM tuples and frequenciess
TNMfreqs = pd.read_csv('TNMfreqs.csv')

## grade and biomarker variables and values
GradeVals = ['1', '2', '3']
HER2Vals = ['Pos', 'Neg']
ERVals = ['Pos', 'Neg']
PRVals = ['Pos', 'Neg']

## sample characteristics
N = 1000 # number of samples

## generate TNM samples
np.random.seed(126)
TNMs = TNMfreqs['TNM'][np.random.multinomial(1, TNMfreqs['freq'], N).argmax(axis=1)]
TNMs = [TNM.split(',') for TNM in TNMs]
Ts, Ns, Ms = map(list, zip(*TNMs))

## generate other variables -- assume they are independent and uniform for now
Grades = np.random.choice(GradeVals, N)
HER2s = np.random.choice(HER2Vals, N)
ERs = np.random.choice(ERVals, N)
PRs = np.random.choice(PRVals, N)

## combine everything
samples = pd.DataFrame({'T': Ts, 'N': Ns, 'M': Ms, 'Grade': Grades, 'HER2': HER2s, 'ER': ERs, 'PR': PRs})
## reorder columns for convenience
samples = samples[['T', 'N', 'M', 'Grade', 'HER2', 'ER', 'PR']]
samples.to_csv('stagingSamples.csv', index=False)
