View of the result from calculating a mean with Gamspk1
-------------------------------------------------------

**For the weighted mean of the single line activities** one obtains the
following interim report for the case of a measurement of Co-60 on a
HPGe detector:

.. code-block:: text

   ----------------------------------------------------------------------
   (1 + b/2L) equivalent factor for Compton BG rate: 1.120

   Individual peak data:
         (pgamm*fcoin is a measure for the importance of the line!)

   i E       PNRate    epsPeak pgamm   fatt   fcoin  (pgamm*fcoin)
     keV     cps          %
   ----------------------------------------------------------------------------------
   1 1173.20 5.699E-03 0.7790  0.99850 1.0000 1.0615 1.0599  values
              2.71     1.6789  0.03000 1.0000 1.3810         u_rels in %
   2 1332.50 5.360E-03 0.7030  0.99986 1.0000 1.0641 1.0640  values
              2.76     1.6245  0.00060 1.0000 1.3890         u_rels in %

   Results from individual peak activities:

        A(i) = PeakNetRate(i) * (fatt(i) * fcoin(i)) / (epsPeak(i) * pgamm(i))

   i  E(keV)  Activity (Bq) rel.StdDev (%)
   --------------------------------------------------
   1  1173.20 7.7771E-01    3.61
   2  1332.50 8.1147E-01    3.63

   Evaluation of the weighted mean:
   weighted mean                 = 0.79379
   int. std. dev. of the mean    = 2.23372E-02 ( 2.81 %) (Bayes compliant)
   ext. std. dev. of the mean    = 1.85227E-02 ( 2.33 %) (not Bayes compliant)
   Chi-square = test value T     = 0.68763
   reduced Chi-square            = 0.68763
   significance (Chi-square > T) = 4.06973 %

   Note: only the internal standard deviation will be used hereafter!
   ----------------------------------------------------------------------

In the first table of this report the input data are shown in shortened
form without reproducing their uncertainties. In this example the
corrections for coincidence summing (fcoinsu) are quite significant
because of the well-type counting geometry. The product (pgamm \*
fcoinsu) given in the last column of that table is a measure for the
weighting of the individual gamma lines.

The second table of this report shows the activity values (in Bq) and
their relative standard uncertainties (in %) calculated for the
individual lines. What follows are the data obtained from calculating
the weighted mean.

**For the mean obtained by applying weighted least-squares** (LSQ Mean)
calculated from several peaks one obtains for the lower result-part
shown above for the weighted mean:

(Note: in this case the activity variances of the two gamma lines are
practically identical; thus, there are nearly no deviations between this
method and that of the weighted mean.)

Evaluation of the weighted mean by least-squares:

weighted mean = 0.79358

std. dev. of the mean = 2.23360E-02 ( 2.81 %)

reduced Chi-square = 0.86694
