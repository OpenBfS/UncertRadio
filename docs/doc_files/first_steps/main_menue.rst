The Main Menu
-------------

.. image:: /images/main_menue.png
    :align: center

Menu File
^^^^^^^^^

After having started the program, it is ready for dealing with a new
measurement evaluation which is called **project**. The TABs "Procedure"
and "Equations" are enabled.

A measurement problem which is already existing as a project file
(extension .txp) can be loaded into the program under the menu item
"\ **File – Load Project**\ " or with the icon |project-open_icon|
which is automatically followed by the complete sequence of calculations
which is finished when the TAB "Results" is enabled and made active.
This may take some seconds which is pointed out also by an additional
dialog, which vanishes when all calculations are done. Now, the user may
work on that project.

.. |project-open_icon| image:: /icons/document-open.png
    :height: 2ex
    :align: middle
    :class: no-scaled-link


If problems occur during the automatic sequence of calculations through
the TABs while the project is loading, this sequence can be omitted with
the menu item "\ **Options – Project Load – without calculations**\ ".

Under the menu item "\ **File - Save Project**\ " or with the icon
|project-save| a measurement evaluation being in progress can be
saved as project file (extension .txp) under the same file name or it
may be saved under a different project file name with the menu item
"\ **File - Save Project As**\ " or with |project-save-as|. With
"\ **File - Close Project**\ " or with |project-close| the project file can be
closed. A csv file format can also be selected for loading or saving a
project file.

.. |project-save| image:: /icons/document-save.png
    :height: 2ex
    :align: middle
    :class: no-scaled-link

.. |project-save-as| image:: /icons/document-save-as.png
    :height: 2ex
    :align: middle
    :class: no-scaled-link

.. |project-close| image:: /icons/application-exit.png
    :height: 2ex
    :align: middle
    :class: no-scaled-link

Menu Edit
^^^^^^^^^

A report file "Report.txt" describing the present status of the project
can be produced under the menu item "\ **Edit - Report**\ ". The
contents of this file may be displayed with the internal text editor via
the TAB "Text Editor" or saved with another filename. The results of all
output quantities are written to the report file, starting from the
uncertainty budgets for the second or third output quantity.

If more than one output quantities are involved, then, under the menu
item "**Edit - Select output quantity**", one of these can be selected
and to which then the calculations of its uncertainty, the uncertainty
budget, Detection threshold and Detection limit refer to.

Note: If **another output quantity is selected** by the user, this
implies that the **selections of the gross and net counting rate symbols
must be changed accordingly**, unless the evaluation method is linear
unfolding. The program then switches to the TAB "Equations" and gives an
appropriate hint in the rightmost field of the status bar. If it is,
however, a method of linear unfolding, where net and gross counting
rates need not to be selected manually, all steps of calculations up to
the TAB "Results" are performed automatically in one step.

The menu item "\ **Edit – Decay curve**\ " allows editing some
sub-dialogs and the primary fitting results if the procedure of linear
unfolding was invoked by a call to **LINFIT(..)** within the equations:

-  Sub-menu "\ **model of decay curve**\ ", or, equivalently the icon
   |preferences-system| in the toolbar:

allows editing parameters of the evaluation model;

-  Sub-menu "\ **data input**\ ", or, equivalently the icon |FittingData_24| in
   the toolbar:

invokes the sub-dialog for editing the input data of the decay curve;

-  Sub-menu "\ **Curve-fit table**\ ", or, equivalently the icon
   |FittingResults_24| in the toolbar:

opens an editor window for viewing the primary fitting results.

If parameters or data have been modified while working within these
sub-dialogs the evaluation is re-started and terminated at the TAB
"\ **Results**\ ".

The menu item "\ **Edit – Gamma spect**\ " allows editing of single
dialogs or result, if the linear unfolding was activated by a call to
**Gamspk1(..)** within the equations.

-  Sub-menu "\ **Edit gamma lines**\ ", or, equivalently, the icon
   |FittingData_24| within the toolbar:

This calls the dialog for editing the individual gamma line data;

-  Sub-menu "\ **Average line activities**\ ", or, equivalently, the
   icon |FittingResults_24| within the toolbar:

This opens the Editor window for inspection of the results obtained for
the weighted mean.

If changes have occurred within these sub-menus, the evaluation is
repeated through to the TAB "Results".

The menu item "\ **Edit – Calibration curve"** invokes a dialog, which
allows the data input of a calibration curve, to fit a polynomial to it
and to take for a specific calibration point value and uncertainty from
the latter, which in turn are used in UR then.

The new menu item "\ **Edit – Change symbol name**\ " allows to change
the name of a specific symbol throughout the dialogs and program
internal fields or arrays. The specification and unit associated with
this symbol are maintained. If a symbol name needs to be changed, this
should be done with this menu item instead of changing this name
directly in the equations.

The new menu item "\ **Edit – Serial evaluation**\ " has been introduced
allowing the manifold evaluation of a project with partially modified
input quantity values/uncertainties. The description of this new option
is given in the new section 5.6 in chapter 6.

Menu Options
^^^^^^^^^^^^

With the menu item **"Options – pre-settings"** the values of, e.g., the
two Quantiles of the normal distribution can be defined corresponding to
the probabilities α and β of the errors of first and second kind. The
**Language** can also be selected there, with which UR shall be used
(German or English).

Under the menu item "\ **Options – Project Load**\ " one may choose
whether the calculations during the automatic run through the TABs while
the project is loaded shall be switched off; at program start "with
calculations" is activated.

If using linear unfolding (invoked by a call to Linfit(), the menu item
„\ **Options – LSQ-Export to R**\ " introduced in autumn 2013, exports
some input data of the fitting routine into some extra text files, e.g.
UR-Export-to-R.txt, which can be used for import into the statistics
package **R** which allows a comparison of the results between UR and
**R**. By default this option is activated.

Since version 2.1.10 the menu item "\ **Options – Model type"** allows
to select from three different types of measurement models:

-  **positive linear, with detection limit**:

the output value **increases** linearly with increasing "gross" quantity
(this is the model type already used for evaluating activity and
dosimetric measurements);

-  **GUM only, without detection limit**:

only value and uncertainty are to be determined, e.g., determining a
mass by weighing. In this model type neither a gross count rate nor a
net count rate nor a detection limit are required.

-  **negative linear, with detection limit (new)**:

the output value **decreases** linearly with increasing "gross"
quantity, *new since version 2.1.10*.

The case of determining the detection limit for the emanation
coefficient of Rn-222 may serve as an example, where in the Ra-226
source the (non-emanated) Rn-222 (Bi-214/Pb-214) activity must become
smaller than that of Ra-226 in order to detect the emanation (both
activities are measured in the Ra-226 source by gamma-ray spectrometry).

It is also possible to establish such UR projects for which only value
and uncertainty of the output quantity are to be calculated; e.g. for
estimating a mass by weighing. In such a case neither a gross nor net
counting rate needs to be defined. For preventing the program from
asking for the latter, one can activate (check) the menu item
"\ **Options – only GUM, without DL**\ "\ **.**

For a project using linear unfolding with more than two output
quantities a confidence ellipse may displayed graphically for each pair
of quantities under the menu item "\ **Options – Calculate confidence
ellipse"**. The correlation matrix is also shown in the associated
dialog.

Important icons in the toolbar
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

From the **remaining icons in the toolbar** the more important ones are:

-  the **"update icon"**\ |view-refresh|, by which the calculations
   from the TAB "Values, Uncertainties" through the TAB "Results" can be
   performed in a single step, after changes in e.g. input data were
   observed;

-  the "\ **delete rows icon"** |delete-row| allows to remove such rows
   which have been selected in advance by the mouse within grids, such
   as "Table of Symbols" and others, also in other dialogs;

a block of rows may also be selected for this purpose:

   select the upper row by mouse click, hold the shift key pressed down
   and click into the lower row;

-  the UR Help can be invoked with the icon |help-icon|;

-  a page of the Help for advices in case of problems can be invoked
   with the icon |dialog-information|;

-  the "\ **fontname icon**\ " |preferences-desktop-font| allows choosing fontname and
   fontsize;


-  the "\ **mean-handling icon**\ " |format-justify-fill| allows input of values of
   a variable and the selection of such variable and of the type of
   mean.

-  The icon |Distrib_24| invokes a dialog showing the actual **parameters
   of a special distribution density** connected to an input quantity.
   This requires that the row of this input quantity within the table
   "values, uncertainties" is highlighted.

-  Short informations about special UR functions can be displayed by the
   icon |ur_functions|.

**User guidance is given in the status bar at the bottom of the
UncertRadio window, in the right-most field. If a project has been
changed in some details this is indicated in the status bar to the left
of the latter ("unsaved").**

For working with tables: see `edit
tables <#within-tables-delete-rows-working-with-column-blocks>`__\ **.**

A certain number of projects files have been added to the program
showing different examples of measurement evaluation. For an
introduction it is recommended to load such an example project and go
through it: `"How to view stepwise an already existing
project" <#URH_PRANSICHT_EN>`__.


.. |view-refresh| image:: /icons/view-refresh.png
   :height: 2ex
   :align: middle
   :class: no-scaled-link

.. |preferences-system| image:: /icons/preferences-system.png
   :height: 2ex
   :align: middle
   :class: no-scaled-link

.. |FittingData_24| image:: /icons/FittingData_24.png
   :height: 2ex
   :align: middle
   :class: no-scaled-link

.. |FittingResults_24| image:: /icons/FittingResults_24.png
   :height: 2ex
   :align: middle
   :class: no-scaled-link

.. |delete-row| image:: /icons/DeleteRow_24.png
   :height: 2ex
   :align: middle
   :class: no-scaled-link

.. |help-icon| image:: /icons/help-contents.png
   :height: 2ex
   :align: middle
   :class: no-scaled-link

.. |dialog-information| image:: /icons/dialog-information.png
   :height: 2ex
   :align: middle
   :class: no-scaled-link

.. |format-justify-fill| image:: /icons/format-justify-fill.png
   :height: 2ex
   :align: middle
   :class: no-scaled-link

.. |preferences-desktop-font| image:: /icons/preferences-desktop-font.png
   :height: 2ex
   :align: middle
   :class: no-scaled-link

.. |Distrib_24| image:: /icons/Distrib_24.png
   :height: 2ex
   :align: middle
   :class: no-scaled-link

.. |ur_functions| image:: /icons/ur_functions.png
   :height: 2ex
   :align: middle
   :class: no-scaled-link