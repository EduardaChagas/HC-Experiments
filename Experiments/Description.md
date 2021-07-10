The objective of the following experiment is to calculate the ML estimators of a beta law for the white noise Bandt-Pompe histograms.

For this, 100 pure white noise sequences with length M = 50k (data present in the `random_50k.bin` file) were collected and the following steps were applied to each one of the signals:
- We calculate the Bandt-Pompe distribution,
- We order the bins in ascending order,
- We calculate the fit parameters of a beta distribution over our pattern distribution (shape1 and shape2),
- We used Anderson Darling statistics to assess the goodness of fit.

As can be seen in the `beta_adjusting.csv` file, applying a 0.05 significance level, the p-value of the Anderson-Darling test concluded that all Bandt-Pompe distributions of the analyzed white noise sequences follow a beta distribution with specific parameters.
