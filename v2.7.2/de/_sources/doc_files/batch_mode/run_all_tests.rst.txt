Batch-mode testing the evaluation of example projects
------------------------------------------------------

UncertRadio allows to evaluate all example projects in a batch mode. The
results obtained are compared with reference results. Deviations found
are documented in a file.

This test is invoked by the main menu item **Options – QC batch test**.
The following dialog is shown:

.. image:: /images/batch_test_dialog.png
    :align: center

Two file names are pre-set which allow to directly start the test with
the `Apply` button.

The first file contains the project filenames and the reference values
of the example projects. The second file, the name of which may be
modified, is the output file. In the case that deviations are found for
a project, the reference values and the actually found values are given
therein and compared for the individual characteristic values by ratios.

Due to previous modifications or corrections applied to 6 specific
projects, their deviations from the reference values are documented for
them. The only important point is whether deviations are found for
others than these six projects. The deviations are given in detail in
vgltest.txt.

This test takes about 47 seconds for a CPU rate with about 4 GHz. While
running the test, only a static window is shown. The name of the project
currently being processed is shown only in the headline of the
UncertRadio window. After the test finished the following message is
shown:

Only if more than zero deviations are given therein, this
indicates a problem.

.. note::

   This test is always applied before publishing a new version of the
   UncertRadio software**.
