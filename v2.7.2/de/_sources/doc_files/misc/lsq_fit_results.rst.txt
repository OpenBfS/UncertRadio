Viewing the result from the LSQ fit to the decay curve
------------------------------------------------------------

As a result, one obtains a table displayed by the programs internal
editor (in this case no editing allowed) which has the following
structure.


.. code-block:: text

    Result of decay curve analysis (with covariances) : Method: PLSQ
              LinFit(t) = a1*X1(t) + a2*X2(t) + a3*X3(t)

    i     t    X1(t)   X2(t)   X3(t)   NetRate   rUnc.     LinFit   relDev uTest
          (m)                           (cps)     (%)       (cps)      (%)
    ---------------------------------------------------------------------------
    1   433.00 0.83176 0.00000 0.00000 0.0017278  23.33 |  0.0016381   5.5  0.2
    2  1633.00 0.67000 0.00000 0.00000 0.0013944  28.49 |  0.0013195   5.7  0.2
    3  2833.00 0.53970 0.00000 0.00000 0.0009500  40.99 |  0.0010629 -10.6 -0.3
    4  4033.00 0.43474 0.00000 0.00000 0.0009361  41.57 |  0.0008562   9.3  0.2
    5  5237.00 0.34994 0.00000 0.00000 0.0006306  60.85 |  0.0006892  -8.5 -0.1
    6  6437.00 0.28189 0.00000 0.00000 0.0006722  57.19 |  0.0005552  21.1  0.3
    7  7637.00 0.22707 0.00000 0.00000 0.0006306  60.85 |  0.0004472  41.0  0.5
    8  8837.00 0.18291 0.00000 0.00000 0.0002833 133.18 |  0.0003602 -21.3 -0.2
    9 10037.00 0.15710 0.00000 0.00000 0.0004382 103.13 |  0.0003094  41.6  0.3
    ---------------------------------------------------------------------------

    LinFit: a1= 0.0019694   a2= 0.0000000     a3= 0.0000000 (given in cps !)
           ra1=   16.240   ra2= 0.000        ra3= 0.000     (given in % !)
                                                             CHi2R= 8.481E-02
          Prob= 0.000272   Prob= 0.000000    Prob= 0.000000 (t-test-signific. !)


The table columns are:

-  No. of measurement i

-  time duration between the Y-90/Sr-90 separation and the start time of
   the i-th measurement, is given in this example in m

-  decay factors of the defined components dependent on t; in this
   example, only the decay of Y-90 is considered; 0.000 indicates that
   this decay component has not been defined/used

-  :math:`NetRate`, net counting rates, in 1/s

-  relative standard uncertainty, in %

-  :math:`LinFit`, the value obtained from fitting the model for the net
   counting rate, in 1/s

-  relDev., relative deviation
   :math:`(NetRate - LinFit)/LinFit \cdot 100`, in %

-  an u-test value, defined as
   :math:`(NetRate - LinFit)/\sqrt{u^{2}(NetRate) + u^{2}(LinFit)}`.
   Absolute values being larger than 2, e.g. may indicate deviations
   from the model

After the table two rows follow showing the parameter values
:math:`a_{i}` and their relative standard uncertainties :math:`{ra}_{i}`
obtained from the LSQ fitting. The first parameter, :math:`a_{1}`, gives
the value of the Y-90 net counting rate being decay corrected to the
time of the Y-90/Sr-90 separation. Chi2R is the value obtained for the
reduced Chi-square.

**After closing** the editor window, **the model itself can again be
edited** under the menu item “Edit->Decay curve->model of the decay
curve” or with corresponding icon |preferences-system| if the first result is
considered for improvement.

.. |preferences-system| image:: /icons/preferences-system.png
   :height: 2ex
   :align: middle
   :class: no-scaled-link