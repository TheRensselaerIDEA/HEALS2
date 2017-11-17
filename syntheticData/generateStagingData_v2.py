## generateStagingData_v2.py
## author: alexander new

''' Generate every possible TNM-Grade-HER2-ER-PR-OncotypeDX value combination,
with one dependency restriction: OncotypeDX is always N/A if T is not 1 or 2.
Set of TNM values taken from SEER.
Other variable values taken from AJCC Staging Rules.

If we want, we can add a dependency structure based on SEER to the frequencies
of TNM-tuples.
'''

import itertools as it
import pandas as pd

## load unique TNM values from SEER
Tvals = pd.read_csv('TValues.csv').values.T.tolist()[0]
Nvals = pd.read_csv('Nvalues.csv').dropna().values.T.tolist()[0]         
Mvals = pd.read_csv('Mvalues.csv').values.T.tolist()[0]

## strip first character from each TNM value
Tvals = [Tval[1:] for Tval in Tvals]
Nvals = [Nval[1:] for Nval in Nvals]
Mvals = [Mval[1:] for Mval in Mvals]

Tvals034 = [Tval for Tval in Tvals if Tval[0] not in ['1', '2']]
Tvals12  = [Tval for Tval in Tvals if Tval[0] in ['1', '2']]

GradeVals = ['1', '2', '3']
HER2vals = ['Pos', 'Neg']
ERvals = ['Pos', 'Neg']
PRvals = ['Pos', 'Neg']
OncotypeVals = ['<11', '>=11', 'N/A']

## values of T for which Oncotype is N/A
perms1 = {'T': [], 'N': [], 'M': [], 'Grade': [], 'HER2': [], 'ER': [], 'PR': [], 'OncotypeDX': []}
for Tval, Nval, Mval, GradeVal, HER2val, ERval, PRval in it.product(Tvals034, Nvals, Mvals, GradeVals, HER2vals, ERvals, PRvals):
    perms1['T'].append(Tval)
    perms1['N'].append(Nval)
    perms1['M'].append(Mval)
    perms1['Grade'].append(GradeVal)
    perms1['HER2'].append(HER2val)
    perms1['ER'].append(ERval)
    perms1['PR'].append(PRval)
    perms1['OncotypeDX'].append('N/A')
        
perms1 = pd.DataFrame(perms1)

## values of T for which Oncotype is < 11 or >= 11
perms2 = {'T': [], 'N': [], 'M': [], 'Grade': [], 'HER2': [], 'ER': [], 'PR': [], 'OncotypeDX': []}
for Tval, Nval, Mval, GradeVal, HER2val, ERval, PRval, OncotypeVal in it.product(Tvals12, Nvals, Mvals, GradeVals, HER2vals, ERvals, PRvals,['<11','>=11']):
    perms2['T'].append(Tval)
    perms2['N'].append(Nval)
    perms2['M'].append(Mval)
    perms2['Grade'].append(GradeVal)
    perms2['HER2'].append(HER2val)
    perms2['ER'].append(ERval)
    perms2['PR'].append(PRval)
    perms2['OncotypeDX'].append(OncotypeVal)
        
perms2 = pd.DataFrame(perms2)

## rowbind two DataFrames
perms = perms1.append(perms2)
## reorder for convenience
perms = perms[['T', 'N', 'M', 'Grade', 'HER2', 'ER', 'PR', 'OncotypeDX']]
perms.to_csv('stagingSamplesFull.csv', index=False)