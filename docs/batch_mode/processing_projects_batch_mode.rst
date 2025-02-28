Processing projects in batch mode
---------------------------------

UncertRadio allows to evaluate several projects in a batch mode. This
mode is invoked by the menu item **Edit – Batch evaluation of projects**
which opens a dialog, like that one shown in section 5.6. Therein, a
simple text file is selected, which contains the project filenames, line
by line.

.. image:: /images/batch_processing_project.png
    :align: center

UncertRadio then starts the calculations, with or without MC simulation,
and writes the results into an **output file batch_out.csv** (CSV
format). For each evaluated project and each of its up to three output
quantities, the filename is written, followed by a table of values for
#EG, PE, uPE, BE, uBE, LQ, UQ, sLQ, sUQ, DT, DL. The column headers in
the output file indicate their relation to the parts 1 and 2 of ISO
11929:2019.

**Meaning of the symbols:**

No. of the output quantity, primary value and uncertainty of the output
quantity, best estimate and associated uncertainty, lower and upper
quantile, lower and upper quantile with the shortest distance, decision
threshold, detection limit.

In the case of MC simulation, a PNG graphics file is generated for each
project and output quantity.

If MC simulation is selected, it may be recommended to use different
batch list files, i.e., batch runs, for projects applying linear
unfolding or not, because the MC simulation is much more time consuming
for projects with linear unfolding.