Example of an Excel application for a batch-like processing
-----------------------------------------------------------

The filenames of UR projects to be processed are collected in column A
of Table6 and the associated Sample_IDs in column B, respectively. In
this example all UR example project files belonging to the program are
listed there.

.. figure:: /images/image29.jpg
    :align: center
    :alt: Excel select projects table
    :scale: 75


Before starting the batch processing with the button
Start Autorun_UncertRadio the associated filenames have to be selected
as a two-column block as shown in the screen shot above.

After the batch processing loop in the VB code is finished, the CSV
output file AutoReport-Result.csv obtained by UR is copied to Table5 at
the end of that VB subroutine. A part of that sheet is shown in the
following screen shot.

.. figure:: /images/image30.jpg
    :align: center
    :alt: Excel result table
    :scale: 75
