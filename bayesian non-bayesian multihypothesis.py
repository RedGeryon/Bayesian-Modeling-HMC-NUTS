from math import log10
import matplotlib.pyplot as plt
import numpy as np

#1, 2, 3: under, correct, oversized parts
original_data = [2,2,2,2,2,2,2,2,2,2,2,
		1,3,
		2,2,2,2,2,2,2,2,2,
		1,3,
		2,2,2,2,2,2,2,2,
		1,1,3,
		2,2,2,2,2,2,2,2,2,
		3]

#Prior probabilities for H:
p_H_X = .2

#map to column vector [under, correct, oversized]
data = map(lambda x: [0 if x-1 != i else 1 for i in range(3)], original_data)
data = map(lambda x: np.transpose(np.matrix(x)), data)

#Hypothesis	priors	
H1 = np.matrix ([[.05,.0,.0],
				[.0,.9,.0],
				[.0,.0,.05]])

H2 = np.matrix ([[.1,.0,.0],
				[.0,.8,.0],
				[.0,.0,.1]])

H3 = np.matrix ([[.2,.0,.0],
				[.0,.6,.0],
				[.0,.0,.2]])

H4 = np.matrix([[.0,.2,.8],
				[.05,.9,.05],
				[.8,.2,.0]])
H4 = np.matrix.transpose(H4)

H5 = np.matrix ([[.1,.6,.3],
				[.05,.9,.05],
				[.3,.6,.1]])
H5 = np.matrix.transpose(H5)

#Individual data update
D_H1X = [H1 * outcome for outcome in data]
D_H2X = [H2 * outcome for outcome in data]
D_H3X = [H3 * outcome for outcome in data]
D_H4X = [H4 * outcome for outcome in data]
D_H5X = [H5 * outcome for outcome in data]

for i in range(len(data)):
	outcome = original_data[i] - 1
	D_H1X[i] = D_H1X[i][outcome]
	D_H2X[i] = D_H2X[i][outcome]
	D_H3X[i] = D_H3X[i][outcome]
	D_H4X[i] = D_H4X[i][outcome]
	D_H5X[i] = D_H5X[i][outcome]


#Cumulative Data Update
EH1_X = [D_H1X[0][0]]
EH2_X = [D_H2X[0][0]]
EH3_X = [D_H3X[0][0]]
EH4_X = [D_H4X[0][0]]
EH5_X = [D_H5X[0][0]]


for i in range(1,len(D_H1X)):
	EH1_X.append(EH1_X[i-1] * D_H1X[i])
	EH2_X.append(EH2_X[i-1] * D_H2X[i])
	EH3_X.append(EH3_X[i-1] * D_H3X[i])
	EH4_X.append(EH4_X[i-1] * D_H4X[i])
	EH5_X.append(EH5_X[i-1] * D_H5X[i])

EH1_X = map(lambda x: x * p_H_X, EH1_X)
EH2_X = map(lambda x: x * p_H_X, EH2_X)
EH3_X = map(lambda x: x * p_H_X, EH3_X)
EH4_X = map(lambda x: x * p_H_X, EH4_X)
EH5_X = map(lambda x: x * p_H_X, EH5_X)

#evidence posterior
ev_H1_EX = map(lambda x: 10*log10(x[0]/(x[1]+x[2])) if x[0] > 0 else 0, zip(EH1_X, EH2_X, EH3_X, EH4_X, EH5_X))
ev_H2_EX = map(lambda x: 10*log10(x[0]/(x[1]+x[2])) if x[0] > 0 else 0, zip(EH2_X, EH1_X, EH3_X, EH4_X, EH5_X))
ev_H3_EX = map(lambda x: 10*log10(x[0]/(x[1]+x[2])) if x[0] > 0 else 0, zip(EH3_X, EH1_X, EH2_X, EH4_X, EH5_X))
ev_H4_EX = map(lambda x: 10*log10(x[0]/(x[1]+x[2])) if x[0] > 0 else 0, zip(EH4_X, EH1_X, EH2_X, EH3_X, EH5_X))
ev_H5_EX = map(lambda x: 10*log10(x[0]/(x[1]+x[2])) if x[0] > 0 else 0, zip(EH5_X, EH1_X, EH2_X, EH4_X, EH3_X))


#graphical view of evidence
plt.plot(ev_H1_EX, linewidth=2.0, label= "H1")
plt.plot(ev_H2_EX, linewidth=2.0, label = "H2")
plt.plot(ev_H3_EX, linewidth=2.0, label = "H3")
plt.plot(ev_H4_EX, linewidth=2.0, label = "H4")
plt.plot(ev_H5_EX, linewidth=2.0, label = "H5")
plt.ylabel('ev(H|E)')
plt.legend(bbox_to_anchor=(0., 1.02, 1., .102), loc=3,
           ncol=5, mode="expand", borderaxespad=0.)
plt.show()