from math import log10
import matplotlib.pyplot as plt
import numpy as np

#negation hypothesis
h1 = (5.0,20.0)
h2 = (10.0,20.0)
h3 = (15.0,20.0)

#hypothesis priors
p_H1_X = .8
p_H2_X = .15
p_H3_X = .05

#individual data update
D_H1X = [(h1[0]-i)/(h1[1]-i) if (h1[0]-i)> 0 else 0 for i in range(0,8)]
D_H2X = [(h2[0]-i)/(h2[1]-i) if (h2[0]-i)> 0 else 0 for i in range(0,8)]
D_H3X = [(h3[0]-i)/(h3[1]-i) if (h3[0]-i)> 0 else 0 for i in range(0,8)]

#cumulative data update
EH1_X = [D_H1X[0]]
EH2_X = [D_H2X[0]]
EH3_X = [D_H3X[0]]

for i in range(1,len(D_H1X)):
	EH1_X.append(EH1_X[i-1] * D_H1X[i])
	EH2_X.append(EH2_X[i-1] * D_H2X[i])
	EH3_X.append(EH3_X[i-1] * D_H3X[i])

EH1_X = map(lambda x: x * p_H1_X, EH1_X)
EH2_X = map(lambda x: x * p_H2_X, EH2_X)
EH3_X = map(lambda x: x * p_H3_X, EH3_X)

#evidence posterior
ev_H1_EX = map(lambda x: 10*log10(x[0]/(x[1]+x[2])) if x[0] > 0 else 0, zip(EH1_X, EH2_X, EH3_X))
ev_H2_EX = map(lambda x: 10*log10(x[0]/(x[1]+x[2])) if x[0] > 0 else 0, zip(EH2_X, EH1_X, EH3_X))
ev_H3_EX = map(lambda x: 10*log10(x[0]/(x[1]+x[2])) if x[0] > 0 else 0, zip(EH3_X, EH1_X, EH2_X))

#graphical view of evidence
plt.plot(ev_H1_EX[:-3], linewidth=2.0, label= "H1")
plt.plot(ev_H2_EX, linewidth=2.0, label = "H2")
plt.plot(ev_H3_EX, linewidth=2.0, label = "H3")
plt.ylabel('ev(H|E)')
plt.legend(bbox_to_anchor=(0., 1.02, 1., .102), loc=3,
           ncol=3, mode="expand", borderaxespad=0.)
plt.show()


                                     