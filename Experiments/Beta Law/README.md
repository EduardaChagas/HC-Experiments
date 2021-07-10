The objective of the following experiment is to calculate the ML estimators of a beta law for the white noise Bandt-Pompe histograms.

For this, 100 pure white noise sequences with length M = 50k (data present in the `random_50k.bin` file) were collected and the following steps were applied to each one of the signals:

- Calculate the Bandt-Pompe histogram,
- Order the bins in ascending order,
- Calculate sample mean and variance estimates,
- Solve the system of equations of moments to get alpha and beta,
- Sse Anderson Darling statistics to assess the goodness of fit.

As can be seen in the `beta_adjusting.csv` file, applying a 0.05 significance level, the p-value of the Anderson-Darling test concluded that all Bandt-Pompe distributions of the analyzed white noise sequences follow a beta distribution with specific parameters.
