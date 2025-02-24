Program testing
---------------

After having installed a new program version, there may be a need for
checking whether the analytical procedures applied by UncertRadio can be
expected to produce evaluation results agreeing with documented results
(reference values). Since version 2.3.03 this is possible as described
now. An already longer existing (internal) test routine had been used to
run the many projects, as given in section 3.3, in a batch processing
mode for comparing the results with those documented earlier.
This routine, previously with no access by the user, has been extended such
that it can be invoked now under the **Menu Options – QC batch test**.
It tests the analytical procedures for about 106 projects, which takes a
time of about 70 seconds.

This test requires the reference data file :file:`BatListRef-v06.txt`
which is part of UncertRadio. The dialog invoked
via **Menu Options – QC batch test** allows to select the reference data
file and an output file showing the comparison of actually calculated
and of reference values, project by project.

.. image:: /images/batch_test_dialog.png
    :align: center

After a run-time of about 70 seconds, an information is given about the
number of projects for which a disagreement was found. The details of
for these projects are given in the output file. This file normally is
very short, as only those projects are reported for which deviations
were found.