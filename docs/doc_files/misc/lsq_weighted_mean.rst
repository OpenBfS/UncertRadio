Least-squares calculation of a weighted mean and its standard uncertainty
-------------------------------------------------------------------------

UncertRadio applies a matrix-based procedure described in section :ref:`mathematics of the linear lsq curve fitting with correlated measured values`:
**x** = **A** **y.**

The single activity values :math:`A_{i}\ `\ are taken as elements of the
vector **x**. The design matrix **A** = (1,1,…,1)\ :sup:`T` in this case
has only one column the elements of which all are equal to1; **y**
reduces to a vector consisting of only one value, the desired weighted
mean.

For applying this procedure, the quadratic covariance matrix **U\ x** is
needed. UncertRadio assembles all the data into the corresponding
algebraic elements and calculates also the required elements of the
covariance matrix as already :ref:`calculation of the weighted mean and its standard uncertainty`
The matrix **U\ y** consists of only one element, the variance
associated with weighted mean.