Serial evaluations of an existing project
-----------------------------------------

The necessity may arise to re-evaluate an existing project for the case
of modified values/uncertainties of some of its input quantities.
Modified values may result from further measurements; they may also
occur, e.g., in an exercise of how the values of the decision threshold
of the detection limit depend on some of the input values/uncertainties.
Such a procedure is now supported by the menu item „\ **Edit – Serial
evaluation**\ “.

Values/uncertainties of some of the input quantities to be modified can
be transferred by the user into a CSV file, an example of which might be
the following:


.. table:: Serial Evaluation Results

    +-------+--------+-------+--------+
    | eps1  | u(eps1)| eps4  | u(eps4)|
    +=======+========+=======+========+
    | 0.338 | 0.045  | 0.390 | 0.055  |
    +-------+--------+-------+--------+
    | 0.36  | 0.045  | 0.370 | 0.049  |
    +-------+--------+-------+--------+
    | 0.35  | 0.047  | 0.360 | 0.052  |
    +-------+--------+-------+--------+

The first line contains the symbols of the input quantities the
values/uncertainties of which are to be modified; it is followed by
lines (=sets) of modified values. Up to 60 symbols/values can be used;
the symbols must be ones being already defined in the project. The first
line is to be interpreted as follows:

Symbol the modified value associated with Symbol

u(Symbol) the modified standard uncertainty associated with Symbol

hw(Symbol) the modified half-width value associated with the
rectangularly distributed Symbol (from which the uncertainty is
calculated internally)

Allowed symbols in this context are those being declared as
“independent“ in the table “Values, Uncertainties“,

The example given above is now part of the UncertRadio installation as a
file called :file:`J_Aluft_serial_EN.csv`. It is meant for using it with the
existing project :file:`J-ALUFT-Sr89-Sr-90_EN.txp`.

With activating the menu item “\ **Edit – Serial evaluation**\ “, the
following dialog is invoked by which the evaluation can be started after
having defined the setup of this evaluation:

.. figure:: /images/serial_evaluation_dialog.png
    :align: center
    :alt: Serial evaluation dialog

UncertRadio then produces one or two output files (csv type) for the
results obtained – without and with MC simulation. Their names are
derived from the name of the input csv file, as in the case of the above
example:

:file:`J_Aluft_serial_EN_res.csv`

:file:`J_Aluft_serial_EN_mc.csv`

These files contain values for:

File; #EG; PE; uPE; BE; uBE; LQ; UQ; sLQ; sUQ; DT; DL;

(project filename, No. of the output quantity, primary value and
uncertainty of the output quantity, best estimate and associated
uncertainty, lower and upper quantile, decision threshold, detection
limit)
