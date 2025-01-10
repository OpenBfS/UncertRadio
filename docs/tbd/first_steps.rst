First steps
===========

Starting the program, the main menu
-----------------------------------

**Menu File**

After having started the program, it is ready for dealing with a new
measurement evaluation which is called **project**. The TABs "Procedure"
and "Equations" are enabled.

A measurement problem which is already existing as a project file
(extension .txp) can be loaded into the program under the menu item
"\ **File – Load Project**\ " or with the icon |document-open|
which is automatically followed by the complete sequence of calculations
which is finished when the TAB "Results" is enabled and made active.
This may take some seconds which is pointed out also by an additional
dialog, which vanishes when all calculations are done. Now, the user may
work on that project.

.. |document-open| image:: _static/images/en/document-open.png
    :align: middle


If problems occur during the automatic sequence of calculations through
the TABs while the project is loading, this sequence can be omitted with
the menu item "\ **Options – Project Load – without calculations**\ ".

Under the menu item "\ **File - Save Project**\ " or with the icon
|document-save.png|\ a measurement evaluation being in progress can be
saved as project file (extension .txp) under the same file name or it
may be saved under a different project file name with the menu item
"\ **File - Save Project As**\ " or with |document-save-as.png|.With
"\ **File - Close Project**\ " or with |image4| the project file can be
closed. A csv file format can also be selected for loading or saving a
project file.

**Menu Edit**

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
   |image5| in the toolbar:

allows editing parameters of the evaluation model;

-  Sub-menu "\ **data input**\ ", or, equivalently the icon |image6| in
   the toolbar:

invokes the sub-dialog for editing the input data of the decay curve;

-  Sub-menu "\ **Curve-fit table**\ ", or, equivalently the icon
   |image7| in the toolbar:

opens an editor window for viewing the primary fitting results.

If parameters or data have been modified while working within these
sub-dialogs the evaluation is re-started and terminated at the TAB
"\ **Results**\ ".

The menu item "\ **Edit – Gamma spect**\ " allows editing of single
dialogs or result, if the linear unfolding was activated by a call to
**Gamspk1(..)** within the equations.

-  Sub-menu "\ **Edit gamma lines**\ ", or, equivalently, the icon
   |image8| within the toolbar:

This calls the dialog for editing the individual gamma line data;

-  Sub-menu "\ **Average line activities**\ ", or, equivalently, the
   icon |image9| within the toolbar:

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

**Menu Options**

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

From the **remaining icons in the toolbar** the more important ones are:

-  the **"update icon"**\ |view-refresh.png|, by which the calculations
   from the TAB "Values, Uncertainties" through the TAB "Results" can be
   performed in a single step, after changes in e.g. input data were
   observed;

-  the "\ **delete rows icon"** |image10| allows to remove such rows
   which have been selected in advance by the mouse within grids, such
   as "Table of Symbols" and others, also in other dialogs;

a block of rows may also be selected for this purpose:

   select the upper row by mouse click, hold the shift key pressed down
   and click into the lower row;

-  the CHM Help can be invoked with the icon |help-contents.png|;

-  a page of the CHM Help for advices in case of problems can be invoked
   with the icon |dialog-information.png|;

-  the "\ **fontname icon**\ " |image11| allows choosing fontname and
   fontsize;

-  background-colors can be modified by the "\ **color icon**\ "
   |image12|.

*:mark:`--> At present, changed colors **cannot** be transferred into
the program window.`*

-  the "\ **mean-handling icon**\ " |image13| allows input of values of
   a variable and the selection of such variable and of the type of
   mean.

-  The icon |image14| invokes a dialog showing the actual **parameters
   of a special distribution density** connected to an input quantity.
   This requires that the row of this input quantity within the table
   "values, uncertainties" is highlighted.

-  Short informations about special UR functions can be displayed by the
   icon |image15|.

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

Viewing an existing project
---------------------------

To become acquainted with the functioning of the program it is
recommended to look into existing example projects. The sequence of
steps and the buttons and tabs to be clicked as shown below is nearly
the same when creating a new project.

**User guidance is presented in the last field of the status bar at the
bottom of the UncertRadio window.**

   • File - Load Project |document-open.png| : Load an existing project
   file (extensions txp or csv) of a measurement problem into the
   program, whereby all calculations are done and, finally, the TAB
   "Results" becomes visible.

   • select TAB "Procedure" for comments on that procedure

   • select TAB "Equations"

   • click button "Load symbols from equations"

   • click button "Load symbols from finalized symbol table"

-  click button "Accept all" -> this enables the TAB "Values,
      Uncertainties"

..

   • look at the equations and the table of symbols

   • select TAB "Values, uncertainties"

   • click Button "Calculation of uncertainties" -> this enables the TAB
   "Uncertainty budget"

   • look at details in the uncertainty table

   • select TAB "Uncertainty budget" showing the uncertainty budget of
   the output quantity

   • select TAB "Results" showing all results including Decision
   threshold and Detection limit

   • a Monte Carlo simulation of the calculations as an alternative
   method may be started by clicking the button TAB "Start"

   • Closing the project file |image16|, loading the next project (see
   above), or terminating the program: File – Quit program.

   **Note regarding safety:**

   If one has changed values while going through the different parts of
   the program, one should NOT save the project when the program asks
   for it, because otherwise the project file would be modified.

Scrolling within tables or in the equations field is possible with the
mouse wheel after clicking within that table of field.

Examples for trial
------------------

List of example projects
~~~~~~~~~~~~~~~~~~~~~~~~

Note: In most cases the associated literature for an example project is
cited within the projects; see TAB "Procedure" after loading a project.
See also Help topic "Used literature".

In the mid of the year 2021, all projects were checked with respect to
the correct use of physical units. Only very few errors were found and
corrected. See chapter
`2.27 <#calculation-of-physical-units-for-dependent-variables>`__ for
the necessary changes.

+-----------------------------------------------------------------------+
| All example project files indicated by background color can be        |
| considered as being evaluated, because they originate from associated |
| publications, or they were confirmed by colleagues through            |
| independent intercomparison calculations.                             |
+-----------------------------------------------------------------------+

+----------------------------+-----------------------------------------+
| **Examples with 1 output   |                                         |
| quantity:**                |                                         |
+----------------------------+-----------------------------------------+
| **without linear           |                                         |
| unfolding:**               |                                         |
+----------------------------+-----------------------------------------+
| ISO-Example-1a_EN.txp      | Alpha activity concentration in liquid  |
|                            | material: measurement with an Alpha     |
|                            | detector; corresponds to example 1a,    |
|                            | section D.2.1, ISO 11929:2010           |
+----------------------------+-----------------------------------------+
| ISO-Example-1b_EN.txp      | Alpha activity concentration in liquid  |
|                            | material: measurement with a rate       |
|                            | meter; corresponds to Example 1b,       |
|                            | Section D.2.2, ISO 11929:2010           |
+----------------------------+-----------------------------------------+
| ISO-Example-2a_EN.txp      | Sr-90 in soil, several determinations;  |
|                            | corresponds to Example 2, Section       |
|                            | D.3.1, ISO 11929:2010; DL 5 % too small |
+----------------------------+-----------------------------------------+
| ISO-Example-2a_V2_EN.txp   | simplified version of                   |
|                            | ISO-Example-2a_EN.txp                   |
+----------------------------+-----------------------------------------+
| ISO-Example-2b_EN.txp      | Sr-90 in soil, several determinations;  |
|                            | corresponds to Example 2, Section       |
|                            | D.3.2, ISO 11929:2010;                  |
+----------------------------+-----------------------------------------+
| ISO-Example-2b_V2_EN.txp   | simplified version of                   |
|                            | ISO-Example-2b_EN.txp                   |
+----------------------------+-----------------------------------------+
| ISO-Example-3a_EN.txp      | I-131 accumulation on air filter;       |
|                            | corresponds to Example 3(a) section     |
|                            | D.4, ISO 11929:2010                     |
+----------------------------+-----------------------------------------+
| ISO-Example-3b_EN.txp      | I-131 accumulation on air filter;       |
|                            | corresponds to Example 3(b) section     |
|                            | D.4, ISO 11929:2010                     |
+----------------------------+-----------------------------------------+
| ISO-Example-4_EN.txp       | Activity concentration with peak        |
|                            | evaluation (Ge-Detector);               |
|                            | corresponds to Example 4, Section       |
|                            | D.5.1, ISO 11929:2010                   |
+----------------------------+-----------------------------------------+
| ISO-Example-5_EN.txp       | Peak net counting rate                  |
|                            | (NaI(Tl)-Detector);                     |
|                            | corresponds to Example 5 Section D.5.2, |
|                            | ISO 11929:2010                          |
+----------------------------+-----------------------------------------+
|                            |                                         |
+----------------------------+-----------------------------------------+
| Michel-2000-b_EN.txp       | I-129 determination in soil by AMS (one |
|                            | value changed)                          |
+----------------------------+-----------------------------------------+
| Sterlinski-2008-NAA_EN.txp | Cs determination in tobacco by neutron  |
|                            | activation analysis (**en**) (new       |
|                            | example)                                |
+----------------------------+-----------------------------------------+
| ISO-Neutron-Dose_EN.txp    | Measuring neutron dose in a mixed       |
|                            | radiation field; replaces               |
|                            | AKS-Neutron-Dose_EN.txp                 |
+----------------------------+-----------------------------------------+
| ISO-Photon-Dose_EN.txp     | Measuring neutron dose in a mixed       |
|                            | radiation field; replaces               |
|                            | AKS-Photon-Dose_EN.txp                  |
+----------------------------+-----------------------------------------+
|                            |                                         |
+----------------------------+-----------------------------------------+
| More                       | Sr-90 determination by LSC in a         |
| no-Sr90_IAEA-135_V4_EN.txp | IAEA-135 sample                         |
+----------------------------+-----------------------------------------+
| Alpha-I                    | Alpha-spectrometry of Pu-238 in fish    |
| AEA-1401-Kanisch_V2_EN.txp |                                         |
+----------------------------+-----------------------------------------+
| vTI-Alpha-Americium_EN.txp | Alpha-spectrometry of Americium-241 in  |
|                            | fish; rather complex due to two         |
|                            | interferences: a) in-growing Am-241     |
|                            | from a Pu-241 impurity of the Pu‑242    |
|                            | tracer and b) Am-241 as impurity of the |
|                            | Am-243 tracer; **a corresponding Excel  |
|                            | solution confirmed the results**        |
+----------------------------+-----------------------------------------+
| R                          | Gamma spectrometric determination of    |
| a226_U235-at-186keV_EN.txp | Ra-226 (186 keV line) with subtraction  |
|                            | of a U-235 contribution (interference); |
|                            | **a corresponding Excel solution        |
|                            | confirmed the results**                 |
+----------------------------+-----------------------------------------+
|                            |                                         |
+----------------------------+-----------------------------------------+
| DWD_AB-                    | Monitoring artificial-alpha activity    |
| Gesamt-Aeros-Alpha1_EN.txp | concentration in air, version 1         |
+----------------------------+-----------------------------------------+
| DWD_AB-                    | Monitoring artificial-alpha activity    |
| Gesamt-Aeros-Alpha3_EN.txp | concentration in air, version 3         |
+----------------------------+-----------------------------------------+
| DWD_AB                     | Monitoring artificial-beta activity     |
| -Gesamt-Aeros-Beta1_EN.txp | concentration in air, version 1         |
+----------------------------+-----------------------------------------+
| DWD_AB                     | Monitoring artificial-beta activity     |
| -Gesamt-Aeros-Beta3_EN.txp | concentration in air, version 3         |
+----------------------------+-----------------------------------------+
| Gamma-Dist_EN.txp          | Application of the (N+1) rule for the   |
|                            | case of very low count numbers of gross |
|                            | and background counting rates           |
+----------------------------+-----------------------------------------+
| Lira-GammaDist_EN.txp      | Application of the (N+1) rule for the   |
|                            | case of very low count numbers of gross |
|                            | and background counting rates;          |
|                            | **example from Lira & Grientschnig,     |
|                            | 2010**                                  |
+----------------------------+-----------------------------------------+
| Fe-55-with-LSC-an          | LSC measurement of Fe-55 using the      |
| d-standard-addition_EN.TXP | method of standard addition             |
|                            |                                         |
|                            | The gross count rate symbol is          |
|                            | occurring twice in the calculation      |
|                            | equation, in the nominator and in the   |
|                            | denominator of that expression. See the |
|                            | description given in the project file.  |
|                            |                                         |
|                            | (**Independently confirmed by D.        |
|                            | Schrammel (KIT) and Prof. Michel** )    |
+----------------------------+-----------------------------------------+
| NLWK                       | Fe-55 beta measurement evaluation       |
| N_Fe-55_with_KALFIT_EN.txp | including a full calibration curve for  |
|                            | the counting efficiency                 |
+----------------------------+-----------------------------------------+
| Ex                         | Example 8 of the new (German)           |
| ample_8_with_KALFIT_EN.txp | Supplement 1 to DIN ISO 11929 (2014).   |
+----------------------------+-----------------------------------------+
| Mean-theta_EN.txp          | Sr-90-Measurement with several          |
|                            | individual measurements of a reference  |
|                            | sample and input of its data set to the |
|                            | project, which allows deriving a        |
|                            | parameter theta, by which mean und      |
|                            | uncertainty can be calculated.          |
+----------------------------+-----------------------------------------+
| Temper                     | Interpolation of a linear temperature   |
| aturCurve_KALFIT_V2_EN.txp | calibration curve, **example from JCGM  |
|                            | 100:2008**: b(t) = y1 +y2*(t - t0)      |
+----------------------------+-----------------------------------------+
| B                          | total Gamma measurement in seawater,    |
| SH_total-Gamma_var1_DE.txp | version 1                               |
+----------------------------+-----------------------------------------+
| B                          | total Gamma measurement in seawater,    |
| SH_total-Gamma_var2_DE.txp | version 2                               |
+----------------------------+-----------------------------------------+
| Ac228_binomial_V2_EN.txp   | Measuring a short-lived radionuclide    |
|                            | with long counting: binomial            |
|                            | distributed sample contribution to the  |
|                            | gross counts                            |
+----------------------------+-----------------------------------------+
| Ra226_                     | Same as the one show above, but with    |
| U235-at-186keV_EN_long.txp | longer symbol names                     |
+----------------------------+-----------------------------------------+
| sumEval_sum_V3_EN.txp      | Combining 4 measurements by summation   |
|                            | for determining one output quantity     |
+----------------------------+-----------------------------------------+
| sumEval_mean_V3_EN.txp     | Combining 4 measurements by averaging   |
|                            | for determining one output quantity     |
+----------------------------+-----------------------------------------+
| PresetCounts_EN.txp        | Simple single channel measurement for   |
|                            | demonstrating the case of measurement   |
|                            | with pre-set counts (number of counts n |
|                            | are fixed; counting duration t is       |
|                            | variable)                               |
+----------------------------+-----------------------------------------+
| A set of projects          | Projects prepared for the examples      |
| i                          | considered in the **standard ISO        |
| so11929-4_Example-6_EN.txp | 11929-4:2022**                          |
| through                    |                                         |
| is                         |                                         |
| o11929-4_Example-17_EN.txp |                                         |
+----------------------------+-----------------------------------------+
|                            |                                         |
+----------------------------+-----------------------------------------+
| **with linear unfolding:** |                                         |
+----------------------------+-----------------------------------------+
| v                          | Y-90 decay curve, including blank       |
| TI-Y90-16330_Blw_V2_EN.txp | contribution (fish sample)              |
+----------------------------+-----------------------------------------+
| v                          | Y-90 decay curve, including blank       |
| TI-Y90-16671_Blw_V2_EN.txp | contribution (fish sample)              |
+----------------------------+-----------------------------------------+
| v                          | Y-90 decay curve, including blank       |
| TI-Y90-16748_Blw_V2_EN.txp | contribution (fish sample)              |
+----------------------------+-----------------------------------------+
| Several-peaks-             | Activity, determined by weighted mean   |
| nuclide-activity-V3_EN.txp | from several γ-peaks of a radionuclide  |
+----------------------------+-----------------------------------------+
| La14                       | Activity, determined by weighted mean   |
| 0_REMSPEC-4Lines-V3_DE.txp | from 4 γ-lines of the radionuclide      |
|                            | La-140                                  |
+----------------------------+-----------------------------------------+
| Ra                         | Measurement of the decay curve of       |
| tel_Annex1_Beispiel_EN.txp | Fluor-18 (half-live of 1.829 h);        |
|                            | **example from Ratel et al.,            |
|                            | Metrologia, 2015**                      |
+----------------------------+-----------------------------------------+
|                            |                                         |
+----------------------------+-----------------------------------------+
|                            |                                         |
+----------------------------+-----------------------------------------+
| **Example with more than 1 |                                         |
| output quantity:**         |                                         |
+----------------------------+-----------------------------------------+
| **without linear           |                                         |
| unfolding:**               |                                         |
+----------------------------+-----------------------------------------+
| Jan                        | Sr-89/Sr-90 determination in            |
| szen-Sr-89-Sr-90_V4_EN.txp | soil/sediment **IAEA-1401**             |
+----------------------------+-----------------------------------------+
| J-A                        | Sr-89/Sr-90 determination in exhaust    |
| LUFT-Sr-89-Sr-90_V2_EN.txp | air                                     |
+----------------------------+-----------------------------------------+
| Galpha_b                   | Total alpha- and total-beta             |
| eta_Rusconi_2006_V2_EN.txp | determination in water by LSC           |
|                            | measurements in two windows, with       |
|                            | alpha/beta discrimination               |
+----------------------------+-----------------------------------------+
| dwd_sr89_sr                | Determination of Beta emitters Sr-89    |
| 90_TDCR_procedureV2_EN.txp | und Sr-90 by a TDCR-procedure, as       |
|                            | realised with a HIDEX LSC Counter       |
+----------------------------+-----------------------------------------+
| **with linear unfolding:** |                                         |
+----------------------------+-----------------------------------------+
| Sr89-Sr90_Schrammel_EN.txp | Sr-89/Sr-90 determination by LSC, with  |
|                            | 1 energy window; simple                 |
+----------------------------+-----------------------------------------+
| DWD-LSC-3Kanal-V2_EN.txp   | Sr-89/Sr-90 determination by LSC, with  |
|                            | 3 energy windows; complex               |
+----------------------------+-----------------------------------------+
| D                          | Sr-89/Sr-90 determination by LSC, with  |
| WD-LSC-3Kanal-V2-fd_EN.txp | 3 energy windows; complex; with using   |
|                            | the (decay) function fd()               |
+----------------------------+-----------------------------------------+
| J-ALU                      | Sr-89/Sr-90 determination in exhaust    |
| FT_Sr-89_Sr-90_Linf_EN.txp | air (compare with                       |
|                            | J-ALUFT-Sr-89-Sr-90_V2_EN.txp)          |
+----------------------------+-----------------------------------------+
| LUBW_Sr-89_Sr-90_          | Sr-89/Sr-90 determination with Sr-85    |
| with-Sr-85-fixed_V2_EN.txp | tracer added, where the tracer count    |
|                            | rate contribution is NOT subject to     |
|                            | fitting                                 |
+----------------------------+-----------------------------------------+
| Sr89-Sr90                  | Sr-89/Sr-90 determination by LSC;       |
| _IAEA_AQ-27_2013_V2_EN.txp | energy window and counting efficiencies |
|                            | vary between 1st and 2nd measurement    |
+----------------------------+-----------------------------------------+
| Sr89_Sr                    | Sr-89/Sr-90 determination by LSC, with  |
| 90_LSC-without-Sr85_EN.txp | 3 energy windows; without Sr-85-tracer; |
|                            | with covariances between window         |
|                            | efficiencies                            |
+----------------------------+-----------------------------------------+
| Tritiu                     | Measurement of HT and HTO in air with   |
| m_4Bubbler_used_1-3_DE.txp | applying a 4-fold-Bubbler (according to |
|                            | J.-M. Duda, JER 189 (2018) 235-249),    |
|                            | application of linear unfolding;        |
|                            | bubblers 1,2 and 3 were evaluated       |
+----------------------------+-----------------------------------------+
| Tritiu                     | Measurement of HT and HTO in air with   |
| m_4Bubbler_used_2-3_DE.txp | applying a 4-fold-Bubbler (according to |
|                            | J.-M. Duda, JER 189 (2018) 235-249),    |
|                            | application of linear unfolding;        |
|                            | bubblers 2 and 3 were evaluated         |
+----------------------------+-----------------------------------------+
|                            |                                         |
+----------------------------+-----------------------------------------+
|                            |                                         |
+----------------------------+-----------------------------------------+
| **Other examples from the  |                                         |
| literature, evaluated,     |                                         |
| without detection          |                                         |
| limits:**                  |                                         |
+----------------------------+-----------------------------------------+
| Neutr                      | Determination of neutron dose           |
| on-Dose-Cox-2006_V2_EN.txp | equivalent                              |
+----------------------------+-----------------------------------------+
| Calibration-of             | calibration of a weight                 |
| -weight-Cox-2001_V2_EN.txp |                                         |
+----------------------------+-----------------------------------------+
| Kessel-1-2006_EN.txp       | calibration of a mass of nominally 10   |
|                            | kg                                      |
+----------------------------+-----------------------------------------+
| Kessel-2a-2006_EN.txp      | Pb mol mass determination, with several |
|                            | correlations                            |
+----------------------------+-----------------------------------------+
| Kessel-2b-2006_EN.txp      | alternative Pb mol mass determination,  |
|                            | with several correlations               |
+----------------------------+-----------------------------------------+
| Wuebbeler-Ex1_EN.txp       | MC example for non-gaussian             |
|                            | distribution                            |
+----------------------------+-----------------------------------------+
| Wuebbeler-Ex2_EN.txp       | MC example for non-gaussian             |
|                            | distribution                            |
+----------------------------+-----------------------------------------+
| Pear                       | Application of weighted total LS (WTLS) |
| sonYork_with_KALFIT_EN.txp | to the data Pearson & York data set     |
+----------------------------+-----------------------------------------+
|                            |                                         |
+----------------------------+-----------------------------------------+
| **Example for a "negative" |                                         |
| linear Model:**            |                                         |
+----------------------------+-----------------------------------------+
| Rn-222-Emanation_EN.txp    | Detection limit calculation for a       |
|                            | Rn-222 emanation coefficient            |
+----------------------------+-----------------------------------------+
|                            |                                         |
+----------------------------+-----------------------------------------+
|                            |                                         |
+----------------------------+-----------------------------------------+

Revision of physical units in the examples
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

With version 2.4.13 of UncertRadio an option was introduced for deriving
the physical units of dependent quantities by calculations; see chapters
2.27, 2.26 und 7.21. In the context of applying this option, the units
used in the example projects of section 3.3.1 were tested for
correctness. Several modifications were found to be necessary. They
mean, that not all units introduced earlier by the author of this
program were correct or functional.

In some cases notes about the modifications were documented within the
project file, TAB "Procedure". In most of the cases, it was necessary to
change the existing unit "1" (or " ") for detection probability
variables (often called eps…) to "1/Bq/s". This helps the output
variable to receive the unit part "Bq" instead of "1/s". These latter
changes were only seldom documented within the project files. In the
example J-ALUFT-Sr-89-Sr-90_V2_EN.txp, two parameters, a and b, used for
calculating eps2, got the new units "1/Bq/s/mg" and "1/Bq/s",
respectively.

In another case, Ra226_U235-at-186keV_EN.txp, the equation RRa = RS –
RU5 resulted in a difference of the units "1/s" and "Bq"; in this case,
in the program the first one was then applied as the unit name for RRa.

In the case of Ac228_binomial_V2_EN.txp, the detection probability epsD
is used two times, as part of the parameter p, which should be
dimensionless as a parameter of the binomial distribution, and as a part
of the calibration factor. This dilemma was solved such that the epsD
which is used within the expression for p, receives the unit "1" (or "
"), while a second variable epsD_U was introduced, which as part of the
calibration factor got the unit "1/Bq/h", but the measurement value was
set equal to one (without uncertainty).

In such examples explicitly containing scaling factors of 60 for the
unit "min" or 1/1000 for the unit "g", two special switching variables
(or Trigger variables) were attributed to these factors; see section
`2.2.6 <#using-switching-variables-in-equations>`__.

For a group of projects their version number (\_Vx\_) in the file name
was increased:

Ac228_binomial_V2_EN.txp

DWD_sr89_sr90_TDCR_procedure_V2_EN.txp

Galpha_beta_Rusconi_2006_V2_EN.txp

J-ALUFT-Sr89-Sr-90_V2_EN.txp

Janszen-Sr-89-Sr-90_V3_EN.txp

Moreno-Sr90_IAEA-135_V2_EN.txp

sumEval_sum_V2_EN.txp

sumEval_mean_V2_EN.txp

vTI-Y90-16330_Blw_V2_EN.txp

vTI-Y90-16671_Blw_V2_EN.txp

vTI-Y90-16748_Blw_V2_EN.txp

A consequence of calculating units of dependent variables (withing the
menu item "test physical units") is the transformation to basic units.
In some cases, the associated scaling factors change the output quantity
value powers of 10:

Galpha_beta_Rusconi_2006_V2_EN.txp: factor 1000 (1/g 1/kg) (permanently
changed)

Sterlinski-2008-NAA_EN.txp: factor 1.0E-9 (due to a unit "ng/g")

sumEval_summe_V2_DE.txp: factor 1.0E+4 (1/cm2 1/m2)

sumEval_mitteln_V2_DE.txp: factor 1.0E+4 (1/cm2 1/m2)

The last three changes do not apply if the test of physical units is not
used, i.e., if the program is used in the normal mode!

A new version was prepared for the file with reference values of the
example projects:

BatListRef_v04.txt

Options dialog - Presetting
---------------------------

+-----------------------------------------------------------------------+
| |image17|                                                             |
+=======================================================================+
+-----------------------------------------------------------------------+

This dialog allows the definition of the following parameters used in
the calculations of Decision threshold and Detection limit:

-  **Quantiles of the normal distribution**, :math:`k_{1 - \alpha}`
   **and** :math:`k_{1 - \beta}`\ **, and their associated probabilities
   for the errors of the first and second kind,**
   :math:`\mathbf{\alpha}` **and** :math:`\mathbf{\beta}`\ **,**
   respectively. The program only works with the values of the quantiles
   :math:`k_{1 - \alpha}` and :math:`k_{1 - \beta}`; it is, however,
   possible to define first the error probabilities from which in turn
   the quantiles are determined then.

..

   **Presetting** in the case of preparing a new project:

   | k-alpha = 1.644854, alpha = 0.05
   | k-beta = 1.644854, beta = 0.05.

   If a quantile value is modified, the associated probability value
   must be changed; this is indicated by disabling its editing and the
   button "adjust" (above) becomes activatable. In the case of modifying
   a probability value first, the procedure is just vice versa. By
   clicking the "adjust" button, the other associated value is adjusted;
   without this adjustment the dialog cannot be closed.

-  the **method of defining Decision thresholds and Detection limits**:
   only the method of ISO 11929:2019 is available.

-  For the calculation of the limits of the (two-sided) confidence
   interval (better: coverage interval), the value of the associated
   probability :math:`(1 - \gamma)` is needed, which can be given in the
   corresponding field. The default value is
   :math:`(1 - \gamma) = 0.95`.

-  The value of the **coverage factor k** is required for calculating
   the expanded uncertainty, which can be defined in the field
   **coverage factor output**. Its pre-set value is 1 (internal variable
   coverf).

-  If in a project file the uncertainties of the listed independent
   input quantities are given as expanded uncertainties, its associated
   coverage factor needs to be removed while reading them in. This
   coverage factor can be given in the field **coverage factor input**.
   Its pre-set value is 1 (internal variable coverin).

..

   Usually, coverin=1 is applied when working with a project within
   UncertRadio by dialog. The value coverin=2 probably will be
   restricted to the case when UncertRadio used in an automated way from
   within another program. An Excel application prepares a project file
   filled with data within Excel and calls UncertRadio for evaluating
   it. If the input quantity uncertainties had been entered in Excel
   with coverin=2,and the parameter coverin=2 is given in the project
   file, UncertRadio converts these uncertainties to k=1 by dividing
   them by the coverin value. The internal calculations are all done
   with a coverage factor of 1. Just before the output to an external
   CSV file, which then will be imported by Excel, UncertRadio
   multiplies the uncertainty values with the value of the parameter
   coverf, which also has to be defined in the project file.

   When using UncertRadio in the automated way, care has to be taken
   about how to correctly insert the uncertainty values into the project
   file: refer to `chapter
   3.6.3 <#notes-about-the-input-of-input-quantity-uncertainties>`__.

-  The dialog contains an **input field for a variable GamDistAdd**.
   This parameter represents the x in the (N+x) rule for counts or
   counting rates, between 0 and 1. The values 0, ½ und 1 for GamDistAdd
   correspond to common priors within a Bayesian view, which are
   proportional to :math:`\nu^{- (1 - c)}`, with *c*\ =GamDistAdd. For
   variables for which the (N+x) rule has been selected, a corresponding
   Gamma distribution is assumed.

+--------------------------+---------------+---------------------------+
| *c*\ =GamDistAdd         | (1-c)         | mean                      |
+==========================+===============+===========================+
| 0                        | 1             | N+0                       |
+--------------------------+---------------+---------------------------+
| 1/2                      | 1/2           | N+1/2                     |
+--------------------------+---------------+---------------------------+
| 1                        | 0             | N+1                       |
+--------------------------+---------------+---------------------------+

-  The **language** output within the program and its dialogs and the
   can be switched between German and English after the program’s start;
   the list separator character can also be selected. See also: `country
   specific parameters <#URH_COUNTRYSPECS_EN>`__.

See also: `Note on a subsequent call of this options
dialog <#implication-of-changes-within-the-options-menu>`__

Advice in case of problems <#advice-in-case-of-problems>`__
------------------------------------------------------------

**Advice in case of problems**

-  If the project runs though without any messages, but the results are
   not plausible, e.g. the detection limit iteration does not converge:

Then check at first whether the symbols for the net and gross counting
rates are correctly selected.

This means also, that always a separate equation must exist which
defines the net counting rate.

-  The detection limit iteration also starts working improperly if the
   relative uncertainty approaches a value of 61 % or even fails in case
   of more than 61 %. The reason for this behavior originates from ISO
   11929 itself.

-  Also, values in the table of the TAB "Values, uncertainties", which
   are equal to zero, lead to strange behavior. This may happen after
   re-editing the equations, where shifts between the lists of symbol
   names and values may occur. Thus, the latter should be checked for
   after one would have done significant re-editing of the equations.

-  *Symbols* which *became dispensable* after having edited the
   equations: it is referred to `section
   4.2 <#URH_Dispensable_Symbols_EN>`__ for their removal.

-  Calls to the special UR specific functions LINFIT or KALFIT should
   always be defined in separate equations; the closing brackets of
   these calls must not be followed by additional operators or symbols
   in the same equation.

-  For projects using linear unfolding (e.g., for combined
   Sr-89/Sr-90-analyes):

Since version 1.08 some modifications were applied to the dialog for
defining the model of a decay curve fitting. In the case of problems, it
is recommended therefore, to invoke also this dialog directly for
checking if selections/options are still the right ones.

-  A simple way in UR for initiating a complete re-calculation of the
   output values consists in selecting the current out output quantity
   again in the menu Edit - Output quantity.

-  The toolbar icon |view-refresh.png|\ initiates a complete
   re-calculation of characteristic values after having made changes to
   the model or the input data. If the program halts at the TAB "Values,
   Uncertainties", press the |view-refresh.png| icon once more.

-  Editing cells in tables:

one click into the cell row marks the row, the second click opens the
cell for editing. An entry into a cell must be confirmed with the ENTER
key; the TAB key is not sufficient.

-  Paste the Windows Clipboard into a table cell:

..

   Note that paste with the short-cut CTRL-V only works for the first
   time, but no longer thereafter;

   Thereafter, this works only correctly, after having marked the cell,
   by using the paste option from the associated context menu;

-  To optimize the column widths is possible by double-clicking the
   small vertical line between two column heads; the mouse pointer
   changes its shape shortly before double-clicking.

-  For importing column blocks from Excel or from Notepad++ into columns
   of a UR2 table: see the end of the Help chapter 7.7.

-

Structure of the project file
-----------------------------

Knowing the structure of a project file is useful for a semi-automated
usage of UncertRadio. For a regularly repeated Sr-89/Sr-90 analysis,
e.g., with linear unfolding and a fixed scheme, one will probably be
able to establish a basic version of that UncertRadio project file where
some parts in it will (nearly) not change. Another part will change,
however, from measurement to measurement. The latter data may e.g. be
written directly into that file using a small Visual Basic program (MS
Excel).

In addition to the existing version of a project file as .txp text file
the structure of which is described below, a CSV file version was
introduced in autumn 2013; the latter will be discussed at the end of
this help topic.

Project file as text file in \*.TXP format
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This may done by first copying the basic version of the project file to
a working project file, also with extension .txp, e.g. to "Test2.txp".
Only when passing special keywords (those with preceding @, see below),
those data from the actual measurement are written into that working
file. After closing the working file one can call UR e.g. directly from
a Visual Basic program such that UR is initially loading just this
edited project file, by some command like "start uncertradio.exe
Test2.txp". Such as call can also be done like:

D:\\UR > Uncertradio.exe Test2.txp

The table following below shows the simple structure of a project file
(extension TXP).

Note: numbers always with . as decimal point (no comma!)!

+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| De  |   |   |   |   |   |         |   | E |   |   |                         |
| uts |   |   |   |   |   |         |   | n |   |   |                         |
| che |   |   |   |   |   |         |   | g |   |   |                         |
| B   |   |   |   |   |   |         |   | l |   |   |                         |
| esc |   |   |   |   |   |         |   | i |   |   |                         |
| hre |   |   |   |   |   |         |   | s |   |   |                         |
| ibu |   |   |   |   |   |         |   | h |   |   |                         |
| ng: |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | p |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | : |   |   |                         |
+=====+===+===+===+===+===+=========+===+===+===+===+=========================+
| **P |   |   |   |   |   |         |   |   |   |   |                         |
| roj |   |   |   |   |   |         |   |   |   |   |                         |
| ect |   |   |   |   |   |         |   |   |   |   |                         |
| :** |   |   |   |   |   |         |   |   |   |   |                         |
| N   |   |   |   |   |   |         |   |   |   |   |                         |
| ame |   |   |   |   |   |         |   |   |   |   |                         |
| der |   |   |   |   |   |         |   |   |   |   |                         |
| C   |   |   |   |   |   |         |   |   |   |   |                         |
| SV- |   |   |   |   |   |         |   |   |   |   |                         |
| Pro |   |   |   |   |   |         |   |   |   |   |                         |
| jek |   |   |   |   |   |         |   |   |   |   |                         |
| tda |   |   |   |   |   |         |   |   |   |   |                         |
| tei |   |   |   |   |   |         |   |   |   |   |                         |
| /   |   |   |   |   |   |         |   |   |   |   |                         |
| n   |   |   |   |   |   |         |   |   |   |   |                         |
| ame |   |   |   |   |   |         |   |   |   |   |                         |
| of  |   |   |   |   |   |         |   |   |   |   |                         |
| CSV |   |   |   |   |   |         |   |   |   |   |                         |
| p   |   |   |   |   |   |         |   |   |   |   |                         |
| roj |   |   |   |   |   |         |   |   |   |   |                         |
| ect |   |   |   |   |   |         |   |   |   |   |                         |
| f   |   |   |   |   |   |         |   |   |   |   |                         |
| ile |   |   |   |   |   |         |   |   |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| **@ |   |   |   |   |   |         |   |   |   |   |                         |
| Tit |   |   |   |   |   |         |   |   |   |   |                         |
| elt |   |   |   |   |   |         |   |   |   |   |                         |
| ext |   |   |   |   |   |         |   |   |   |   |                         |
| :** |   |   |   |   |   |         |   |   |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
|     |   |   |   |   |   |         |   | t |   |   |                         |
|   T |   |   |   |   |   |         |   | e |   |   |                         |
| ext |   |   |   |   |   |         |   | x |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
| zur |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
| Bes |   |   |   |   |   |         |   | s |   |   |                         |
| chr |   |   |   |   |   |         |   | c |   |   |                         |
| eib |   |   |   |   |   |         |   | r |   |   |                         |
| ung |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
| des |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|   V |   |   |   |   |   |         |   | g |   |   |                         |
| erf |   |   |   |   |   |         |   | t |   |   |                         |
| ahr |   |   |   |   |   |         |   | h |   |   |                         |
| ens |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | p |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| *   |   |   |   |   |   |         |   |   |   |   |                         |
| *@F |   |   |   |   |   |         |   |   |   |   |                         |
| orm |   |   |   |   |   |         |   |   |   |   |                         |
| elt |   |   |   |   |   |         |   |   |   |   |                         |
| ext |   |   |   |   |   |         |   |   |   |   |                         |
| :** |   |   |   |   |   |         |   |   |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
|     |   |   |   |   |   |         |   | i |   |   |                         |
|  es |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
| fol |   |   |   |   |   |         |   | o |   |   |                         |
| gen |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
| die |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | w |   |   |                         |
|  Gl |   |   |   |   |   |         |   | e |   |   |                         |
| eic |   |   |   |   |   |         |   | d |   |   |                         |
| hun |   |   |   |   |   |         |   | b |   |   |                         |
| gen |   |   |   |   |   |         |   | y |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
| zur |   |   |   |   |   |         |   | h |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|   B |   |   |   |   |   |         |   | e |   |   |                         |
| ere |   |   |   |   |   |         |   | q |   |   |                         |
| chn |   |   |   |   |   |         |   | u |   |   |                         |
| ung |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
| der |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
| Akt |   |   |   |   |   |         |   | n |   |   |                         |
| ivi |   |   |   |   |   |         |   | s |   |   |                         |
| tät |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | h |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | v |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | y |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| *   |   |   |   |   |   |         |   |   |   |   |                         |
| *@F |   |   |   |   |   |         |   |   |   |   |                         |
| orm |   |   |   |   |   |         |   |   |   |   |                         |
| elt |   |   |   |   |   |         |   |   |   |   |                         |
| ext |   |   |   |   |   |         |   |   |   |   |                         |
| Fit |   |   |   |   |   |         |   |   |   |   |                         |
| :** |   |   |   |   |   |         |   |   |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
|     |   |   |   |   |   |         |   | i |   |   |                         |
|  es |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
| fol |   |   |   |   |   |         |   | o |   |   |                         |
| gen |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
| die |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | w |   |   |                         |
|  Gl |   |   |   |   |   |         |   | e |   |   |                         |
| eic |   |   |   |   |   |         |   | d |   |   |                         |
| hun |   |   |   |   |   |         |   | b |   |   |                         |
| gen |   |   |   |   |   |         |   | y |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
| der |   |   |   |   |   |         |   | q |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|  en |   |   |   |   |   |         |   | a |   |   |                         |
| erg |   |   |   |   |   |         |   | t |   |   |                         |
| iea |   |   |   |   |   |         |   | i |   |   |                         |
| bhä |   |   |   |   |   |         |   | o |   |   |                         |
| ngi |   |   |   |   |   |         |   | n |   |   |                         |
| gen |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|   F |   |   |   |   |   |         |   | e |   |   |                         |
| unk |   |   |   |   |   |         |   | s |   |   |                         |
| tio |   |   |   |   |   |         |   | c |   |   |                         |
| nen |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|  im |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
| Ver |   |   |   |   |   |         |   | n |   |   |                         |
| fah |   |   |   |   |   |         |   | g |   |   |                         |
| ren |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | h |   |   |                         |
| *Ga |   |   |   |   |   |         |   | e |   |   |                         |
| msp |   |   |   |   |   |         |   | e |   |   |                         |
| k1* |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|   o |   |   |   |   |   |         |   | r |   |   |                         |
| der |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | y |   |   |                         |
| der |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|  Ab |   |   |   |   |   |         |   | p |   |   |                         |
| kli |   |   |   |   |   |         |   | e |   |   |                         |
| ngf |   |   |   |   |   |         |   | n |   |   |                         |
| unk |   |   |   |   |   |         |   | d |   |   |                         |
| tio |   |   |   |   |   |         |   | e |   |   |                         |
| nen |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|  im |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
| Ver |   |   |   |   |   |         |   | r |   |   |                         |
| fah |   |   |   |   |   |         |   | v |   |   |                         |
| ren |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|   * |   |   |   |   |   |         |   | i |   |   |                         |
| Lin |   |   |   |   |   |         |   | n |   |   |                         |
| fit |   |   |   |   |   |         |   | t |   |   |                         |
| 1*. |   |   |   |   |   |         |   | h |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | h |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | * |   |   |                         |
|     |   |   |   |   |   |         |   | G |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | p |   |   |                         |
|     |   |   |   |   |   |         |   | k |   |   |                         |
|     |   |   |   |   |   |         |   | 1 |   |   |                         |
|     |   |   |   |   |   |         |   | * |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | h |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | y |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | h |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | h |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | * |   |   |                         |
|     |   |   |   |   |   |         |   | L |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | 1 |   |   |                         |
|     |   |   |   |   |   |         |   | * |   |   |                         |
|     |   |   |   |   |   |         |   | . |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| **@ |   |   |   |   |   |         |   |   |   |   |                         |
| Sym |   |   |   |   |   |         |   |   |   |   |                         |
| bol |   |   |   |   |   |         |   |   |   |   |                         |
| e-G |   |   |   |   |   |         |   |   |   |   |                         |
| RID |   |   |   |   |   |         |   |   |   |   |                         |
| :** |   |   |   |   |   |         |   |   |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
|     |   |   |   |   |   |         |   | v |   |   |                         |
|  fo |   |   |   |   |   |         |   | a |   |   |                         |
| lge |   |   |   |   |   |         |   | l |   |   |                         |
| nde |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
| Var |   |   |   |   |   |         |   | s |   |   |                         |
| iab |   |   |   |   |   |         |   | a |   |   |                         |
| len |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
| wer |   |   |   |   |   |         |   | a |   |   |                         |
| den |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|  We |   |   |   |   |   |         |   | r |   |   |                         |
| rte |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
|  zu |   |   |   |   |   |         |   | u |   |   |                         |
| geo |   |   |   |   |   |         |   | t |   |   |                         |
| rdn |   |   |   |   |   |         |   | e |   |   |                         |
| et: |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | h |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | w |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | v |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | : |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| nc  | A |   |   |   |   |         |   |   | n |   |                         |
| hs= | n |   |   |   |   |         |   |   | u |   |                         |
| 1   | z |   |   |   |   |         |   |   | m |   |                         |
|     | a |   |   |   |   |         |   |   | b |   |                         |
| nE  | h |   |   |   |   |         |   |   | e |   |                         |
| Gr= | l |   |   |   |   |         |   |   | r |   |                         |
| 1   | d |   |   |   |   |         |   |   | o |   |                         |
|     | e |   |   |   |   |         |   |   | f |   |                         |
| ng  | r |   |   |   |   |         |   |   | c |   |                         |
| rs= | M |   |   |   |   |         |   |   | o |   |                         |
| 8   | e |   |   |   |   |         |   |   | u |   |                         |
|     | s |   |   |   |   |         |   |   | n |   |                         |
| n   | s |   |   |   |   |         |   |   | t |   |                         |
| ab= | k |   |   |   |   |         |   |   | i |   |                         |
| 3   | a |   |   |   |   |         |   |   | n |   |                         |
|     | n |   |   |   |   |         |   |   | g |   |                         |
| n   | ä |   |   |   |   |         |   |   | c |   |                         |
| mu= | l |   |   |   |   |         |   |   | h |   |                         |
| 5   | e |   |   |   |   |         |   |   | a |   |                         |
|     |   |   |   |   |   |         |   |   | n |   |                         |
|     | A |   |   |   |   |         |   |   | n |   |                         |
|     | n |   |   |   |   |         |   |   | e |   |                         |
|     | z |   |   |   |   |         |   |   | l |   |                         |
|     | a |   |   |   |   |         |   |   | s |   |                         |
|     | h |   |   |   |   |         |   |   |   |   |                         |
|     | l |   |   |   |   |         |   |   | n |   |                         |
|     | d |   |   |   |   |         |   |   | u |   |                         |
|     | e |   |   |   |   |         |   |   | m |   |                         |
|     | r |   |   |   |   |         |   |   | b |   |                         |
|     | E |   |   |   |   |         |   |   | e |   |                         |
|     | r |   |   |   |   |         |   |   | r |   |                         |
|     | g |   |   |   |   |         |   |   | o |   |                         |
|     | e |   |   |   |   |         |   |   | f |   |                         |
|     | b |   |   |   |   |         |   |   | o |   |                         |
|     | n |   |   |   |   |         |   |   | u |   |                         |
|     | i |   |   |   |   |         |   |   | t |   |                         |
|     | s |   |   |   |   |         |   |   | p |   |                         |
|     | g |   |   |   |   |         |   |   | u |   |                         |
|     | r |   |   |   |   |         |   |   | t |   |                         |
|     | ö |   |   |   |   |         |   |   | q |   |                         |
|     | ß |   |   |   |   |         |   |   | u |   |                         |
|     | e |   |   |   |   |         |   |   | a |   |                         |
|     | n |   |   |   |   |         |   |   | n |   |                         |
|     |   |   |   |   |   |         |   |   | t |   |                         |
|     | A |   |   |   |   |         |   |   | i |   |                         |
|     | n |   |   |   |   |         |   |   | t |   |                         |
|     | z |   |   |   |   |         |   |   | i |   |                         |
|     | a |   |   |   |   |         |   |   | e |   |                         |
|     | h |   |   |   |   |         |   |   | s |   |                         |
|     | l |   |   |   |   |         |   |   |   |   |                         |
|     | d |   |   |   |   |         |   |   | n |   |                         |
|     | e |   |   |   |   |         |   |   | u |   |                         |
|     | r |   |   |   |   |         |   |   | m |   |                         |
|     | V |   |   |   |   |         |   |   | b |   |                         |
|     | a |   |   |   |   |         |   |   | e |   |                         |
|     | r |   |   |   |   |         |   |   | r |   |                         |
|     | i |   |   |   |   |         |   |   | o |   |                         |
|     | a |   |   |   |   |         |   |   | f |   |                         |
|     | b |   |   |   |   |         |   |   | v |   |                         |
|     | l |   |   |   |   |         |   |   | a |   |                         |
|     | e |   |   |   |   |         |   |   | r |   |                         |
|     | n |   |   |   |   |         |   |   | i |   |                         |
|     | ( |   |   |   |   |         |   |   | a |   |                         |
|     | S |   |   |   |   |         |   |   | b |   |                         |
|     | y |   |   |   |   |         |   |   | l |   |                         |
|     | m |   |   |   |   |         |   |   | e |   |                         |
|     | b |   |   |   |   |         |   |   | s |   |                         |
|     | o |   |   |   |   |         |   |   | ( |   |                         |
|     | l |   |   |   |   |         |   |   | s |   |                         |
|     | e |   |   |   |   |         |   |   | y |   |                         |
|     | ) |   |   |   |   |         |   |   | m |   |                         |
|     |   |   |   |   |   |         |   |   | b |   |                         |
|     | A |   |   |   |   |         |   |   | o |   |                         |
|     | n |   |   |   |   |         |   |   | l |   |                         |
|     | z |   |   |   |   |         |   |   | s |   |                         |
|     | a |   |   |   |   |         |   |   | ) |   |                         |
|     | h |   |   |   |   |         |   |   |   |   |                         |
|     | l |   |   |   |   |         |   |   | n |   |                         |
|     | d |   |   |   |   |         |   |   | u |   |                         |
|     | e |   |   |   |   |         |   |   | m |   |                         |
|     | r |   |   |   |   |         |   |   | b |   |                         |
|     | a |   |   |   |   |         |   |   | e |   |                         |
|     | b |   |   |   |   |         |   |   | r |   |                         |
|     | h |   |   |   |   |         |   |   | o |   |                         |
|     | ä |   |   |   |   |         |   |   | f |   |                         |
|     | n |   |   |   |   |         |   |   | d |   |                         |
|     | g |   |   |   |   |         |   |   | e |   |                         |
|     | i |   |   |   |   |         |   |   | p |   |                         |
|     | g |   |   |   |   |         |   |   | e |   |                         |
|     | e |   |   |   |   |         |   |   | n |   |                         |
|     | n |   |   |   |   |         |   |   | d |   |                         |
|     | S |   |   |   |   |         |   |   | e |   |                         |
|     | y |   |   |   |   |         |   |   | n |   |                         |
|     | m |   |   |   |   |         |   |   | t |   |                         |
|     | b |   |   |   |   |         |   |   | s |   |                         |
|     | o |   |   |   |   |         |   |   | y |   |                         |
|     | l |   |   |   |   |         |   |   | m |   |                         |
|     | e |   |   |   |   |         |   |   | b |   |                         |
|     | ( |   |   |   |   |         |   |   | o |   |                         |
|     | a |   |   |   |   |         |   |   | l |   |                         |
|     | ) |   |   |   |   |         |   |   | s |   |                         |
|     |   |   |   |   |   |         |   |   | ( |   |                         |
|     | A |   |   |   |   |         |   |   | a |   |                         |
|     | n |   |   |   |   |         |   |   | ) |   |                         |
|     | z |   |   |   |   |         |   |   |   |   |                         |
|     | a |   |   |   |   |         |   |   | n |   |                         |
|     | h |   |   |   |   |         |   |   | u |   |                         |
|     | l |   |   |   |   |         |   |   | m |   |                         |
|     | d |   |   |   |   |         |   |   | b |   |                         |
|     | e |   |   |   |   |         |   |   | e |   |                         |
|     | r |   |   |   |   |         |   |   | r |   |                         |
|     | u |   |   |   |   |         |   |   | o |   |                         |
|     | n |   |   |   |   |         |   |   | f |   |                         |
|     | a |   |   |   |   |         |   |   | i |   |                         |
|     | b |   |   |   |   |         |   |   | n |   |                         |
|     | h |   |   |   |   |         |   |   | d |   |                         |
|     | ä |   |   |   |   |         |   |   | e |   |                         |
|     | n |   |   |   |   |         |   |   | p |   |                         |
|     | g |   |   |   |   |         |   |   | e |   |                         |
|     | i |   |   |   |   |         |   |   | n |   |                         |
|     | g |   |   |   |   |         |   |   | d |   |                         |
|     | e |   |   |   |   |         |   |   | e |   |                         |
|     | n |   |   |   |   |         |   |   | n |   |                         |
|     | S |   |   |   |   |         |   |   | t |   |                         |
|     | y |   |   |   |   |         |   |   | s |   |                         |
|     | m |   |   |   |   |         |   |   | y |   |                         |
|     | b |   |   |   |   |         |   |   | m |   |                         |
|     | o |   |   |   |   |         |   |   | b |   |                         |
|     | l |   |   |   |   |         |   |   | o |   |                         |
|     | e |   |   |   |   |         |   |   | l |   |                         |
|     | ( |   |   |   |   |         |   |   | s |   |                         |
|     | u |   |   |   |   |         |   |   | ( |   |                         |
|     | ) |   |   |   |   |         |   |   | u |   |                         |
|     |   |   |   |   |   |         |   |   | ) |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| Hi  |   |   |   |   |   |         |   | F |   |   |                         |
| ern |   |   |   |   |   |         |   | o |   |   |                         |
| ach |   |   |   |   |   |         |   | r |   |   |                         |
| fol |   |   |   |   |   |         |   | e |   |   |                         |
| gen |   |   |   |   |   |         |   | a |   |   |                         |
| ze  |   |   |   |   |   |         |   | c |   |   |                         |
| ile |   |   |   |   |   |         |   | h |   |   |                         |
| nwe |   |   |   |   |   |         |   | s |   |   |                         |
| ise |   |   |   |   |   |         |   | y |   |   |                         |
| die |   |   |   |   |   |         |   | m |   |   |                         |
| In  |   |   |   |   |   |         |   | b |   |   |                         |
| for |   |   |   |   |   |         |   | o |   |   |                         |
| mat |   |   |   |   |   |         |   | l |   |   |                         |
| ion |   |   |   |   |   |         |   | f |   |   |                         |
| en, |   |   |   |   |   |         |   | r |   |   |                         |
| die |   |   |   |   |   |         |   | o |   |   |                         |
| zum |   |   |   |   |   |         |   | m |   |   |                         |
| „S  |   |   |   |   |   |         |   | t |   |   |                         |
| ymb |   |   |   |   |   |         |   | h |   |   |                         |
| ole |   |   |   |   |   |         |   | e |   |   |                         |
| -Gr |   |   |   |   |   |         |   | „ |   |   |                         |
| id" |   |   |   |   |   |         |   | s |   |   |                         |
| im  |   |   |   |   |   |         |   | y |   |   |                         |
| TAB |   |   |   |   |   |         |   | m |   |   |                         |
| „   |   |   |   |   |   |         |   | b |   |   |                         |
| Gle |   |   |   |   |   |         |   | o |   |   |                         |
| ich |   |   |   |   |   |         |   | l |   |   |                         |
| ung |   |   |   |   |   |         |   | g |   |   |                         |
| en" |   |   |   |   |   |         |   | r |   |   |                         |
| ge  |   |   |   |   |   |         |   | i |   |   |                         |
| hör |   |   |   |   |   |         |   | d |   |   |                         |
| en, |   |   |   |   |   |         |   | " |   |   |                         |
| für |   |   |   |   |   |         |   | o |   |   |                         |
| je  |   |   |   |   |   |         |   | f |   |   |                         |
| des |   |   |   |   |   |         |   | t |   |   |                         |
| Sym |   |   |   |   |   |         |   | h |   |   |                         |
| bol |   |   |   |   |   |         |   | e |   |   |                         |
| (in |   |   |   |   |   |         |   | T |   |   |                         |
| den |   |   |   |   |   |         |   | A |   |   |                         |
| Zei |   |   |   |   |   |         |   | B |   |   |                         |
| len |   |   |   |   |   |         |   | " |   |   |                         |
| di  |   |   |   |   |   |         |   | E |   |   |                         |
| ent |   |   |   |   |   |         |   | q |   |   |                         |
| das |   |   |   |   |   |         |   | u |   |   |                         |
| Z   |   |   |   |   |   |         |   | a |   |   |                         |
| eic |   |   |   |   |   |         |   | t |   |   |                         |
| hen |   |   |   |   |   |         |   | i |   |   |                         |
| #   |   |   |   |   |   |         |   | o |   |   |                         |
| als |   |   |   |   |   |         |   | n |   |   |                         |
| Tr  |   |   |   |   |   |         |   | s |   |   |                         |
| enn |   |   |   |   |   |         |   | " |   |   |                         |
| zei |   |   |   |   |   |         |   | f |   |   |                         |
| che |   |   |   |   |   |         |   | o |   |   |                         |
| n): |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
| -   |   |   |   |   |   |         |   | o |   |   |                         |
| der |   |   |   |   |   |         |   | w |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|  Na |   |   |   |   |   |         |   | g |   |   |                         |
| me, |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
| -   |   |   |   |   |   |         |   | f |   |   |                         |
| der |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
| Typ |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|  (a |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|   o |   |   |   |   |   |         |   | s |   |   |                         |
| der |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | v |   |   |                         |
| u), |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
| -   |   |   |   |   |   |         |   | ( |   |   |                         |
| die |   |   |   |   |   |         |   | w |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|   E |   |   |   |   |   |         |   | h |   |   |                         |
| inh |   |   |   |   |   |         |   | # |   |   |                         |
| eit |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
| und |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | p |   |   |                         |
| -   |   |   |   |   |   |         |   | a |   |   |                         |
| die |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
| „Be |   |   |   |   |   |         |   | i |   |   |                         |
| deu |   |   |   |   |   |         |   | n |   |   |                         |
| tun |   |   |   |   |   |         |   | g |   |   |                         |
| g"; |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | h |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | ) |   |   |                         |
|     |   |   |   |   |   |         |   | : |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | , |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | y |   |   |                         |
|     |   |   |   |   |   |         |   | p |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | ( |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | ) |   |   |                         |
|     |   |   |   |   |   |         |   | , |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | h |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | y |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | ‘ |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | „ |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | " |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| **  |   |   |   |   |   |         |   |   |   |   |                         |
| @Me |   |   |   |   |   |         |   |   |   |   |                         |
| nu1 |   |   |   |   |   |         |   |   |   |   |                         |
| und |   |   |   |   |   |         |   |   |   |   |                         |
| Me  |   |   |   |   |   |         |   |   |   |   |                         |
| nu2 |   |   |   |   |   |         |   |   |   |   |                         |
| :** |   |   |   |   |   |         |   |   |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
|     |   |   |   |   |   |         |   | n |   |   |                         |
|   N |   |   |   |   |   |         |   | u |   |   |                         |
| umm |   |   |   |   |   |         |   | m |   |   |                         |
| ern |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
| der |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|   S |   |   |   |   |   |         |   | o |   |   |                         |
| ymb |   |   |   |   |   |         |   | f |   |   |                         |
| ole |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | h |   |   |                         |
|   d |   |   |   |   |   |         |   | e |   |   |                         |
| er: |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | y |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | : |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| k   |   | N |   |   |   |         |   | n |   |   |                         |
| net |   | e |   |   |   |         |   | e |   |   |                         |
| to= |   | t |   |   |   |         |   | t |   |   |                         |
| 2   |   | t |   |   |   |         |   | c |   |   |                         |
|     |   | o |   |   |   |         |   | o |   |   |                         |
|     |   | z |   |   |   |         |   | u |   |   |                         |
|     |   | ä |   |   |   |         |   | n |   |   |                         |
|     |   | h |   |   |   |         |   | t |   |   |                         |
|     |   | l |   |   |   |         |   | i |   |   |                         |
|     |   | r |   |   |   |         |   | n |   |   |                         |
|     |   | a |   |   |   |         |   | g |   |   |                         |
|     |   | t |   |   |   |         |   | r |   |   |                         |
|     |   | e |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| kb  |   | B |   |   |   |         |   | g |   |   |                         |
| rut |   | r |   |   |   |         |   | r |   |   |                         |
| to= |   | u |   |   |   |         |   | o |   |   |                         |
| 0   |   | t |   |   |   |         |   | s |   |   |                         |
|     |   | t |   |   |   |         |   | s |   |   |                         |
|     |   | o |   |   |   |         |   | c |   |   |                         |
|     |   | z |   |   |   |         |   | o |   |   |                         |
|     |   | ä |   |   |   |         |   | u |   |   |                         |
|     |   | h |   |   |   |         |   | n |   |   |                         |
|     |   | l |   |   |   |         |   | t |   |   |                         |
|     |   | r |   |   |   |         |   | i |   |   |                         |
|     |   | a |   |   |   |         |   | n |   |   |                         |
|     |   | t |   |   |   |         |   | g |   |   |                         |
|     |   | e |   |   |   |         |   | r |   |   |                         |
|     |   | ( |   |   |   |         |   | a |   |   |                         |
|     |   | 0 |   |   |   |         |   | t |   |   |                         |
|     |   | : |   |   |   |         |   | e |   |   |                         |
|     |   | n |   |   |   |         |   | ( |   |   |                         |
|     |   | i |   |   |   |         |   | 0 |   |   |                         |
|     |   | c |   |   |   |         |   | : |   |   |                         |
|     |   | h |   |   |   |         |   | n |   |   |                         |
|     |   | t |   |   |   |         |   | o |   |   |                         |
|     |   | v |   |   |   |         |   | t |   |   |                         |
|     |   | e |   |   |   |         |   | u |   |   |                         |
|     |   | r |   |   |   |         |   | s |   |   |                         |
|     |   | w |   |   |   |         |   | e |   |   |                         |
|     |   | e |   |   |   |         |   | d |   |   |                         |
|     |   | n |   |   |   |         |   | w |   |   |                         |
|     |   | d |   |   |   |         |   | i |   |   |                         |
|     |   | e |   |   |   |         |   | t |   |   |                         |
|     |   | t |   |   |   |         |   | h |   |   |                         |
|     |   | b |   |   |   |         |   | p |   |   |                         |
|     |   | e |   |   |   |         |   | r |   |   |                         |
|     |   | i |   |   |   |         |   | o |   |   |                         |
|     |   | V |   |   |   |         |   | c |   |   |                         |
|     |   | e |   |   |   |         |   | e |   |   |                         |
|     |   | r |   |   |   |         |   | d |   |   |                         |
|     |   | f |   |   |   |         |   | u |   |   |                         |
|     |   | a |   |   |   |         |   | r |   |   |                         |
|     |   | h |   |   |   |         |   | e |   |   |                         |
|     |   | r |   |   |   |         |   | s |   |   |                         |
|     |   | e |   |   |   |         |   | u |   |   |                         |
|     |   | n |   |   |   |         |   | s |   |   |                         |
|     |   | m |   |   |   |         |   | i |   |   |                         |
|     |   | i |   |   |   |         |   | n |   |   |                         |
|     |   | t |   |   |   |         |   | g |   |   |                         |
|     |   | E |   |   |   |         |   | l |   |   |                         |
|     |   | n |   |   |   |         |   | i |   |   |                         |
|     |   | t |   |   |   |         |   | n |   |   |                         |
|     |   | f |   |   |   |         |   | e |   |   |                         |
|     |   | a |   |   |   |         |   | a |   |   |                         |
|     |   | l |   |   |   |         |   | r |   |   |                         |
|     |   | t |   |   |   |         |   | u |   |   |                         |
|     |   | u |   |   |   |         |   | n |   |   |                         |
|     |   | n |   |   |   |         |   | f |   |   |                         |
|     |   | g |   |   |   |         |   | o |   |   |                         |
|     |   | ) |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | ) |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| **  |   |   |   |   |   |         |   |   |   |   |                         |
| @Un |   |   |   |   |   |         |   |   |   |   |                         |
| c-G |   |   |   |   |   |         |   |   |   |   |                         |
| rid |   |   |   |   |   |         |   |   |   |   |                         |
| :** |   |   |   |   |   |         |   |   |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| Es  |   |   |   |   |   |         |   | f |   |   |                         |
| fo  |   |   |   |   |   |         |   | o |   |   |                         |
| lgt |   |   |   |   |   |         |   | r |   |   |                         |
| für |   |   |   |   |   |         |   | e |   |   |                         |
| j   |   |   |   |   |   |         |   | a |   |   |                         |
| ede |   |   |   |   |   |         |   | c |   |   |                         |
| Va  |   |   |   |   |   |         |   | h |   |   |                         |
| ria |   |   |   |   |   |         |   | v |   |   |                         |
| ble |   |   |   |   |   |         |   | a |   |   |                         |
| e   |   |   |   |   |   |         |   | r |   |   |                         |
| ine |   |   |   |   |   |         |   | i |   |   |                         |
| Zei |   |   |   |   |   |         |   | a |   |   |                         |
| le, |   |   |   |   |   |         |   | b |   |   |                         |
| in  |   |   |   |   |   |         |   | l |   |   |                         |
| der |   |   |   |   |   |         |   | e |   |   |                         |
| mit |   |   |   |   |   |         |   | t |   |   |                         |
| ei  |   |   |   |   |   |         |   | h |   |   |                         |
| ner |   |   |   |   |   |         |   | e |   |   |                         |
| csv |   |   |   |   |   |         |   | f |   |   |                         |
| -St |   |   |   |   |   |         |   | o |   |   |                         |
| ruk |   |   |   |   |   |         |   | l |   |   |                         |
| tur |   |   |   |   |   |         |   | l |   |   |                         |
| (#  |   |   |   |   |   |         |   | o |   |   |                         |
| als |   |   |   |   |   |         |   | w |   |   |                         |
| T   |   |   |   |   |   |         |   | i |   |   |                         |
| ren |   |   |   |   |   |         |   | n |   |   |                         |
| nze |   |   |   |   |   |         |   | g |   |   |                         |
| ich |   |   |   |   |   |         |   | e |   |   |                         |
| en) |   |   |   |   |   |         |   | l |   |   |                         |
| fo  |   |   |   |   |   |         |   | e |   |   |                         |
| lge |   |   |   |   |   |         |   | m |   |   |                         |
| nde |   |   |   |   |   |         |   | e |   |   |                         |
| El  |   |   |   |   |   |         |   | n |   |   |                         |
| eme |   |   |   |   |   |         |   | t |   |   |                         |
| nte |   |   |   |   |   |         |   | s |   |   |                         |
| (   |   |   |   |   |   |         |   | a |   |   |                         |
| Zah |   |   |   |   |   |         |   | r |   |   |                         |
| len |   |   |   |   |   |         |   | e |   |   |                         |
| wer |   |   |   |   |   |         |   | g |   |   |                         |
| te: |   |   |   |   |   |         |   | i |   |   |                         |
| dou |   |   |   |   |   |         |   | v |   |   |                         |
| ble |   |   |   |   |   |         |   | e |   |   |                         |
| p   |   |   |   |   |   |         |   | n |   |   |                         |
| rec |   |   |   |   |   |         |   | i |   |   |                         |
| isi |   |   |   |   |   |         |   | n |   |   |                         |
| on; |   |   |   |   |   |         |   | o |   |   |                         |
| -   |   |   |   |   |   |         |   | n |   |   |                         |
| 999 |   |   |   |   |   |         |   | e |   |   |                         |
| .d0 |   |   |   |   |   |         |   | r |   |   |                         |
| be  |   |   |   |   |   |         |   | o |   |   |                         |
| deu |   |   |   |   |   |         |   | w |   |   |                         |
| tet |   |   |   |   |   |         |   | ( |   |   |                         |
| „ni |   |   |   |   |   |         |   | w |   |   |                         |
| cht |   |   |   |   |   |         |   | i |   |   |                         |
| de  |   |   |   |   |   |         |   | t |   |   |                         |
| fin |   |   |   |   |   |         |   | h |   |   |                         |
| ier |   |   |   |   |   |         |   | a |   |   |                         |
| t") |   |   |   |   |   |         |   | c |   |   |                         |
| s   |   |   |   |   |   |         |   | s |   |   |                         |
| teh |   |   |   |   |   |         |   | v |   |   |                         |
| en: |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   |   |   |   |         |   | # |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | p |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | h |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | : |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | p |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | , |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   | 9 |   |   |                         |
|     |   |   |   |   |   |         |   | 9 |   |   |                         |
|     |   |   |   |   |   |         |   | 9 |   |   |                         |
|     |   |   |   |   |   |         |   | . |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | 0 |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | " |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | " |   |   |                         |
|     |   |   |   |   |   |         |   | : |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| -   |   |   |   |   |   |         |   | - |   |   |                         |
|   S |   |   |   |   |   |         |   |   |   |   |                         |
| ymb |   |   |   |   |   |         |   |   |   |   |                         |
| ol; |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | y |   |   |                         |
| -   |   |   |   |   |   |         |   | m |   |   |                         |
|   W |   |   |   |   |   |         |   | b |   |   |                         |
| ert |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
| der |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| Grö |   |   |   |   |   |         |   |   |   |   |                         |
| ße; |   |   |   |   |   |         |   | v |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
| -   |   |   |   |   |   |         |   | l |   |   |                         |
|  In |   |   |   |   |   |         |   | u |   |   |                         |
| dex |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| des |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|   V |   |   |   |   |   |         |   |   |   |   |                         |
| ert |   |   |   |   |   |         |   | o |   |   |                         |
| eil |   |   |   |   |   |         |   | f |   |   |                         |
| ung |   |   |   |   |   |         |   |   |   |   |                         |
| sty |   |   |   |   |   |         |   |   |   |   |                         |
| ps: |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| ..  |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|   1 |   |   |   |   |   |         |   | h |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|   N |   |   |   |   |   |         |   |   |   |   |                         |
| orm |   |   |   |   |   |         |   |   |   |   |                         |
| alv |   |   |   |   |   |         |   |   |   |   |                         |
| ert |   |   |   |   |   |         |   |   |   |   |                         |
| eil |   |   |   |   |   |         |   |   |   |   |                         |
| ung |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | q |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|   2 |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
| Rec |   |   |   |   |   |         |   | t |   |   |                         |
| hte |   |   |   |   |   |         |   | i |   |   |                         |
| ckv |   |   |   |   |   |         |   | t |   |   |                         |
| ert |   |   |   |   |   |         |   | y |   |   |                         |
| eil |   |   |   |   |   |         |   | ; |   |   |                         |
| ung |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|   3 |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | I |   |   |                         |
| Dre |   |   |   |   |   |         |   | n |   |   |                         |
| iec |   |   |   |   |   |         |   | d |   |   |                         |
| ksv |   |   |   |   |   |         |   | e |   |   |                         |
| ert |   |   |   |   |   |         |   | x |   |   |                         |
| eil |   |   |   |   |   |         |   |   |   |   |                         |
| ung |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|   4 |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|  (N |   |   |   |   |   |         |   | o |   |   |                         |
| +x) |   |   |   |   |   |         |   | f |   |   |                         |
| -Re |   |   |   |   |   |         |   |   |   |   |                         |
| gel |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| für |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|  Im |   |   |   |   |   |         |   |   |   |   |                         |
| pul |   |   |   |   |   |         |   | t |   |   |                         |
| san |   |   |   |   |   |         |   | h |   |   |                         |
| zah |   |   |   |   |   |         |   | e |   |   |                         |
| len |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|   5 |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|   L |   |   |   |   |   |         |   |   |   |   |                         |
| ogn |   |   |   |   |   |         |   | d |   |   |                         |
| orm |   |   |   |   |   |         |   | i |   |   |                         |
| alv |   |   |   |   |   |         |   | s |   |   |                         |
| ert |   |   |   |   |   |         |   | t |   |   |                         |
| eil |   |   |   |   |   |         |   | r |   |   |                         |
| ung |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|   6 |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
| Gam |   |   |   |   |   |         |   | o |   |   |                         |
| mav |   |   |   |   |   |         |   | n |   |   |                         |
| ert |   |   |   |   |   |         |   |   |   |   |                         |
| eil |   |   |   |   |   |         |   |   |   |   |                         |
| ung |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|   7 |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|  Bi |   |   |   |   |   |         |   | y |   |   |                         |
| nom |   |   |   |   |   |         |   | p |   |   |                         |
| ia+ |   |   |   |   |   |         |   | e |   |   |                         |
| Poi |   |   |   |   |   |         |   |   |   |   |                         |
| sso |   |   |   |   |   |         |   | . |   |   |                         |
| n-V |   |   |   |   |   |         |   | . |   |   |                         |
| ert |   |   |   |   |   |         |   |   |   |   |                         |
| eil |   |   |   |   |   |         |   |   |   |   |                         |
| ung |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | 1 |   |   |                         |
|   8 |   |   |   |   |   |         |   |   |   |   |                         |
| ..2 |   |   |   |   |   |         |   |   |   |   |                         |
| -Pa |   |   |   |   |   |         |   |   |   |   |                         |
| ram |   |   |   |   |   |         |   | n |   |   |                         |
| ete |   |   |   |   |   |         |   | o |   |   |                         |
| rBe |   |   |   |   |   |         |   | r |   |   |                         |
| tav |   |   |   |   |   |         |   | m |   |   |                         |
| ert |   |   |   |   |   |         |   | a |   |   |                         |
| eil |   |   |   |   |   |         |   | l |   |   |                         |
| ung |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| 9.. |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
| t-V |   |   |   |   |   |         |   | s |   |   |                         |
| ert |   |   |   |   |   |         |   | t |   |   |                         |
| eil |   |   |   |   |   |         |   | r |   |   |                         |
| ung |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
| -   |   |   |   |   |   |         |   | u |   |   |                         |
|  Te |   |   |   |   |   |         |   | t |   |   |                         |
| xt- |   |   |   |   |   |         |   | i |   |   |                         |
| For |   |   |   |   |   |         |   | o |   |   |                         |
| mel |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| für |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | 2 |   |   |                         |
| die |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| Sta |   |   |   |   |   |         |   | r |   |   |                         |
| nda |   |   |   |   |   |         |   | e |   |   |                         |
| rdu |   |   |   |   |   |         |   | c |   |   |                         |
| nsi |   |   |   |   |   |         |   | t |   |   |                         |
| che |   |   |   |   |   |         |   | a |   |   |                         |
| rhe |   |   |   |   |   |         |   | n |   |   |                         |
| it; |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
| -   |   |   |   |   |   |         |   | l |   |   |                         |
|   Z |   |   |   |   |   |         |   | a |   |   |                         |
| ahl |   |   |   |   |   |         |   | r |   |   |                         |
| enw |   |   |   |   |   |         |   |   |   |   |                         |
| ert |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
| der |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
| Sta |   |   |   |   |   |         |   | r |   |   |                         |
| nda |   |   |   |   |   |         |   | i |   |   |                         |
| rdu |   |   |   |   |   |         |   | b |   |   |                         |
| nsi |   |   |   |   |   |         |   | u |   |   |                         |
| che |   |   |   |   |   |         |   | t |   |   |                         |
| rhe |   |   |   |   |   |         |   | i |   |   |                         |
| it; |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
| -   |   |   |   |   |   |         |   |   |   |   |                         |
| Hal |   |   |   |   |   |         |   |   |   |   |                         |
| bre |   |   |   |   |   |         |   |   |   |   |                         |
| ite |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | 3 |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| der |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|   V |   |   |   |   |   |         |   | r |   |   |                         |
| ert |   |   |   |   |   |         |   | i |   |   |                         |
| eil |   |   |   |   |   |         |   | a |   |   |                         |
| ung |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|  (D |   |   |   |   |   |         |   | l |   |   |                         |
| rei |   |   |   |   |   |         |   | a |   |   |                         |
| eck |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|   o |   |   |   |   |   |         |   |   |   |   |                         |
| der |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|   R |   |   |   |   |   |         |   | t |   |   |                         |
| ech |   |   |   |   |   |         |   | r |   |   |                         |
| tec |   |   |   |   |   |         |   | i |   |   |                         |
| k); |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
| -   |   |   |   |   |   |         |   | t |   |   |                         |
|  1: |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|  ab |   |   |   |   |   |         |   |   |   |   |                         |
| sol |   |   |   |   |   |         |   |   |   |   |                         |
| ute |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | 4 |   |   |                         |
|   b |   |   |   |   |   |         |   |   |   |   |                         |
| zw. |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | ( |   |   |                         |
|  2: |   |   |   |   |   |         |   | N |   |   |                         |
|     |   |   |   |   |   |         |   | + |   |   |                         |
|     |   |   |   |   |   |         |   | x |   |   |                         |
|  re |   |   |   |   |   |         |   | ) |   |   |                         |
| lat |   |   |   |   |   |         |   |   |   |   |                         |
| ive |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|   U |   |   |   |   |   |         |   | u |   |   |                         |
| nsi |   |   |   |   |   |         |   | l |   |   |                         |
| che |   |   |   |   |   |         |   | e |   |   |                         |
| rhe |   |   |   |   |   |         |   |   |   |   |                         |
| its |   |   |   |   |   |         |   |   |   |   |                         |
| ang |   |   |   |   |   |         |   |   |   |   |                         |
| abe |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
| bei |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| der |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|  Ei |   |   |   |   |   |         |   | n |   |   |                         |
| nga |   |   |   |   |   |         |   | t |   |   |                         |
| be; |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
| -   |   |   |   |   |   |         |   | g |   |   |                         |
|   b |   |   |   |   |   |         |   |   |   |   |                         |
| ere |   |   |   |   |   |         |   |   |   |   |                         |
| chn |   |   |   |   |   |         |   |   |   |   |                         |
| ete |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|  ab |   |   |   |   |   |         |   | b |   |   |                         |
| sol |   |   |   |   |   |         |   | e |   |   |                         |
| ute |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| Sta |   |   |   |   |   |         |   |   |   |   |                         |
| nda |   |   |   |   |   |         |   |   |   |   |                         |
| rdu |   |   |   |   |   |         |   |   |   |   |                         |
| nsi |   |   |   |   |   |         |   | 5 |   |   |                         |
| che |   |   |   |   |   |         |   |   |   |   |                         |
| rhe |   |   |   |   |   |         |   |   |   |   |                         |
| it. |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | 6 |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | 7 |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | + |   |   |                         |
|     |   |   |   |   |   |         |   | p |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | 8 |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | 2 |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   | p |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | 9 |   |   |                         |
|     |   |   |   |   |   |         |   | . |   |   |                         |
|     |   |   |   |   |   |         |   | . |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | x |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | y |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | v |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | h |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | y |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | H |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   | w |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | h |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | ( |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | ) |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | p |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | h |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | y |   |   |                         |
|     |   |   |   |   |   |         |   | : |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | . |   |   |                         |
|     |   |   |   |   |   |         |   | . |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | 1 |   |   |                         |
|     |   |   |   |   |   |         |   | : |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | , |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | 2 |   |   |                         |
|     |   |   |   |   |   |         |   | : |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | v |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | y |   |   |                         |
|     |   |   |   |   |   |         |   | . |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| *   |   |   |   |   |   |         |   |   |   |   |                         |
| *@C |   |   |   |   |   |         |   |   |   |   |                         |
| ova |   |   |   |   |   |         |   |   |   |   |                         |
| r-G |   |   |   |   |   |         |   |   |   |   |                         |
| rid |   |   |   |   |   |         |   |   |   |   |                         |
| :** |   |   |   |   |   |         |   |   |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| Fa  |   |   |   |   |   |         |   | I |   |   |                         |
| lls |   |   |   |   |   |         |   | f |   |   |                         |
| Ko  |   |   |   |   |   |         |   | c |   |   |                         |
| var |   |   |   |   |   |         |   | o |   |   |                         |
| ian |   |   |   |   |   |         |   | v |   |   |                         |
| zen |   |   |   |   |   |         |   | a |   |   |                         |
| def |   |   |   |   |   |         |   | r |   |   |                         |
| ini |   |   |   |   |   |         |   | i |   |   |                         |
| ert |   |   |   |   |   |         |   | a |   |   |                         |
| si  |   |   |   |   |   |         |   | n |   |   |                         |
| nd, |   |   |   |   |   |         |   | c |   |   |                         |
| fol |   |   |   |   |   |         |   | e |   |   |                         |
| gen |   |   |   |   |   |         |   | s |   |   |                         |
| di  |   |   |   |   |   |         |   | a |   |   |                         |
| ese |   |   |   |   |   |         |   | r |   |   |                         |
| hi  |   |   |   |   |   |         |   | e |   |   |                         |
| ern |   |   |   |   |   |         |   | d |   |   |                         |
| ach |   |   |   |   |   |         |   | e |   |   |                         |
| fo  |   |   |   |   |   |         |   | f |   |   |                         |
| lgt |   |   |   |   |   |         |   | i |   |   |                         |
| für |   |   |   |   |   |         |   | n |   |   |                         |
| je  |   |   |   |   |   |         |   | e |   |   |                         |
| des |   |   |   |   |   |         |   | d |   |   |                         |
| Va  |   |   |   |   |   |         |   | , |   |   |                         |
| ria |   |   |   |   |   |         |   | t |   |   |                         |
| ble |   |   |   |   |   |         |   | h |   |   |                         |
| npa |   |   |   |   |   |         |   | e |   |   |                         |
| ar: |   |   |   |   |   |         |   | y |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
| -   |   |   |   |   |   |         |   | r |   |   |                         |
| die |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | h |   |   |                         |
|   z |   |   |   |   |   |         |   | a |   |   |                         |
| wei |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|  Sy |   |   |   |   |   |         |   | t |   |   |                         |
| mbo |   |   |   |   |   |         |   | e |   |   |                         |
| l-N |   |   |   |   |   |         |   | r |   |   |                         |
| umm |   |   |   |   |   |         |   | i |   |   |                         |
| ern |   |   |   |   |   |         |   | z |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
| des |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
| Var |   |   |   |   |   |         |   | e |   |   |                         |
| iab |   |   |   |   |   |         |   | a |   |   |                         |
| len |   |   |   |   |   |         |   | c |   |   |                         |
| paa |   |   |   |   |   |         |   | h |   |   |                         |
| rs; |   |   |   |   |   |         |   | p |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
| -   |   |   |   |   |   |         |   | i |   |   |                         |
|   1 |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
| für |   |   |   |   |   |         |   | v |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
| Kov |   |   |   |   |   |         |   | i |   |   |                         |
| ari |   |   |   |   |   |         |   | a |   |   |                         |
| anz |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|   o |   |   |   |   |   |         |   | s |   |   |                         |
| der |   |   |   |   |   |         |   | : |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|   2 |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
| für |   |   |   |   |   |         |   | h |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| Kor |   |   |   |   |   |         |   |   |   |   |                         |
| rel |   |   |   |   |   |         |   |   |   |   |                         |
| ati |   |   |   |   |   |         |   |   |   |   |                         |
| on; |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| -   |   |   |   |   |   |         |   | t |   |   |                         |
|   g |   |   |   |   |   |         |   | w |   |   |                         |
| gf. |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|   T |   |   |   |   |   |         |   |   |   |   |                         |
| ext |   |   |   |   |   |         |   |   |   |   |                         |
| for |   |   |   |   |   |         |   |   |   |   |                         |
| mel |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | y |   |   |                         |
| für |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
| die |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|   K |   |   |   |   |   |         |   |   |   |   |                         |
| ova |   |   |   |   |   |         |   |   |   |   |                         |
| ria |   |   |   |   |   |         |   |   |   |   |                         |
| nz; |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
| -   |   |   |   |   |   |         |   | u |   |   |                         |
|   W |   |   |   |   |   |         |   | m |   |   |                         |
| ert |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
| der |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| Kov |   |   |   |   |   |         |   |   |   |   |                         |
| ari |   |   |   |   |   |         |   |   |   |   |                         |
| anz |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
| (1) |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|   b |   |   |   |   |   |         |   |   |   |   |                         |
| zw. |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|  Ko |   |   |   |   |   |         |   | t |   |   |                         |
| rre |   |   |   |   |   |         |   | h |   |   |                         |
| lat |   |   |   |   |   |         |   | e |   |   |                         |
| ion |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| (2) |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | p |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | 1 |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | v |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | , |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | 2 |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | y |   |   |                         |
|     |   |   |   |   |   |         |   | , |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | v |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | x |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | v |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | v |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | ( |   |   |                         |
|     |   |   |   |   |   |         |   | 1 |   |   |                         |
|     |   |   |   |   |   |         |   | ) |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | ( |   |   |                         |
|     |   |   |   |   |   |         |   | 2 |   |   |                         |
|     |   |   |   |   |   |         |   | ) |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| **@ |   |   |   |   |   |         |   |   |   |   |                         |
| Abk |   |   |   |   |   |         |   |   |   |   |                         |
| lin |   |   |   |   |   |         |   |   |   |   |                         |
| g-G |   |   |   |   |   |         |   |   |   |   |                         |
| rid |   |   |   |   |   |         |   |   |   |   |                         |
| :** |   |   |   |   |   |         |   |   |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| M   |   |   | 7 |   |   |         |   | 7 |   |   |                         |
| odP |   |   | c |   |   |         |   | c |   |   |                         |
| ar= |   |   | h |   |   |         |   | h |   |   |                         |
| 1 1 |   |   | e |   |   |         |   | e |   |   |                         |
| 1 1 |   |   | c |   |   |         |   | c |   |   |                         |
| 1 0 |   |   | k |   |   |         |   | k |   |   |                         |
| 0   |   |   | b |   |   |         |   | b |   |   |                         |
|     |   |   | o |   |   |         |   | o |   |   |                         |
| 8   |   |   | x |   |   |         |   | x |   |   |                         |
| We  |   |   | / |   |   |         |   | / |   |   |                         |
| rte |   |   | r |   |   |         |   | r |   |   |                         |
| (8  |   |   | a |   |   |         |   | a |   |   |                         |
| va  |   |   | d |   |   |         |   | d |   |   |                         |
| lue |   |   | i |   |   |         |   | i |   |   |                         |
| s): |   |   | o |   |   |         |   | o |   |   |                         |
|     |   |   | b |   |   |         |   | b |   |   |                         |
| #1  |   |   | u |   |   |         |   | u |   |   |                         |
| -3: |   |   | t |   |   |         |   | t |   |   |                         |
| i   |   |   | t |   |   |         |   | t |   |   |                         |
| fit |   |   | o |   |   |         |   | o |   |   |                         |
| (.) |   |   | n |   |   |         |   | n |   |   |                         |
|     |   |   | - |   |   |         |   | v |   |   |                         |
| #4: |   |   | W |   |   |         |   | a |   |   |                         |
|     |   |   | e |   |   |         |   | l |   |   |                         |
| #5: |   |   | r |   |   |         |   | u |   |   |                         |
|     |   |   | t |   |   |         |   | e |   |   |                         |
| #6: |   |   | e |   |   |         |   | s |   |   |                         |
|     |   |   | i |   |   |         |   | w |   |   |                         |
| #7: |   |   | m |   |   |         |   | i |   |   |                         |
|     |   |   | G |   |   |         |   | t |   |   |                         |
| #8: |   |   | r |   |   |         |   | h |   |   |                         |
|     |   |   | i |   |   |         |   | i |   |   |                         |
| 0   |   |   | d |   |   |         |   | n |   |   |                         |
| 1.0 |   |   | ; |   |   |         |   | t |   |   |                         |
| 1.2 |   |   |   |   |   |         |   | h |   |   |                         |
| 005 |   |   | w |   |   |         |   | e |   |   |                         |
| 12  |   |   | e |   |   |         |   | g |   |   |                         |
| :12 |   |   | l |   |   |         |   | r |   |   |                         |
| :07 |   |   | c |   |   |         |   | i |   |   |                         |
|     |   |   | h |   |   |         |   | d |   |   |                         |
| 1   |   |   | e |   |   |         |   | d |   |   |                         |
|     |   |   | P |   |   |         |   | e |   |   |                         |
|     |   |   | a |   |   |         |   | f |   |   |                         |
|     |   |   | r |   |   |         |   | i |   |   |                         |
|     |   |   | a |   |   |         |   | n |   |   |                         |
|     |   |   | m |   |   |         |   | i |   |   |                         |
|     |   |   | e |   |   |         |   | n |   |   |                         |
|     |   |   | t |   |   |         |   | g |   |   |                         |
|     |   |   | e |   |   |         |   | t |   |   |                         |
|     |   |   | r |   |   |         |   | h |   |   |                         |
|     |   |   | f |   |   |         |   | e |   |   |                         |
|     |   |   | i |   |   |         |   | m |   |   |                         |
|     |   |   | t |   |   |         |   | o |   |   |                         |
|     |   |   | t |   |   |         |   | d |   |   |                         |
|     |   |   | e |   |   |         |   | e |   |   |                         |
|     |   |   | n |   |   |         |   | l |   |   |                         |
|     |   |   | ? |   |   |         |   | o |   |   |                         |
|     |   |   | ( |   |   |         |   | f |   |   |                         |
|     |   |   | 1 |   |   |         |   | t |   |   |                         |
|     |   |   | : |   |   |         |   | h |   |   |                         |
|     |   |   | f |   |   |         |   | e |   |   |                         |
|     |   |   | i |   |   |         |   | d |   |   |                         |
|     |   |   | t |   |   |         |   | e |   |   |                         |
|     |   |   | t |   |   |         |   | c |   |   |                         |
|     |   |   | e |   |   |         |   | a |   |   |                         |
|     |   |   | n |   |   |         |   | y |   |   |                         |
|     |   |   | ; |   |   |         |   | c |   |   |                         |
|     |   |   | 2 |   |   |         |   | u |   |   |                         |
|     |   |   | : |   |   |         |   | r |   |   |                         |
|     |   |   | f |   |   |         |   | v |   |   |                         |
|     |   |   | e |   |   |         |   | e |   |   |                         |
|     |   |   | s |   |   |         |   | ; |   |   |                         |
|     |   |   | t |   |   |         |   |   |   |   |                         |
|     |   |   | h |   |   |         |   | w |   |   |                         |
|     |   |   | a |   |   |         |   | h |   |   |                         |
|     |   |   | l |   |   |         |   | i |   |   |                         |
|     |   |   | t |   |   |         |   | c |   |   |                         |
|     |   |   | e |   |   |         |   | h |   |   |                         |
|     |   |   | n |   |   |         |   | p |   |   |                         |
|     |   |   | ; |   |   |         |   | a |   |   |                         |
|     |   |   | 3 |   |   |         |   | r |   |   |                         |
|     |   |   | : |   |   |         |   | a |   |   |                         |
|     |   |   | w |   |   |         |   | m |   |   |                         |
|     |   |   | e |   |   |         |   | e |   |   |                         |
|     |   |   | g |   |   |         |   | t |   |   |                         |
|     |   |   | l |   |   |         |   | e |   |   |                         |
|     |   |   | a |   |   |         |   | r |   |   |                         |
|     |   |   | s |   |   |         |   | s |   |   |                         |
|     |   |   | s |   |   |         |   | t |   |   |                         |
|     |   |   | e |   |   |         |   | o |   |   |                         |
|     |   |   | n |   |   |         |   | f |   |   |                         |
|     |   |   | ) |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   | g |   |   |         |   | ? |   |   |                         |
|     |   |   | e |   |   |         |   | ( |   |   |                         |
|     |   |   | w |   |   |         |   | 1 |   |   |                         |
|     |   |   | i |   |   |         |   | : |   |   |                         |
|     |   |   | c |   |   |         |   | f |   |   |                         |
|     |   |   | h |   |   |         |   | i |   |   |                         |
|     |   |   | t |   |   |         |   | t |   |   |                         |
|     |   |   | e |   |   |         |   | ; |   |   |                         |
|     |   |   | t |   |   |         |   | 2 |   |   |                         |
|     |   |   | e |   |   |         |   | : |   |   |                         |
|     |   |   | r |   |   |         |   | f |   |   |                         |
|     |   |   | F |   |   |         |   | i |   |   |                         |
|     |   |   | i |   |   |         |   | x |   |   |                         |
|     |   |   | t |   |   |         |   | i |   |   |                         |
|     |   |   | ? |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   | B |   |   |         |   | 3 |   |   |                         |
|     |   |   | e |   |   |         |   | : |   |   |                         |
|     |   |   | n |   |   |         |   | o |   |   |                         |
|     |   |   | u |   |   |         |   | m |   |   |                         |
|     |   |   | t |   |   |         |   | i |   |   |                         |
|     |   |   | z |   |   |         |   | t |   |   |                         |
|     |   |   | u |   |   |         |   | i |   |   |                         |
|     |   |   | n |   |   |         |   | t |   |   |                         |
|     |   |   | g |   |   |         |   | ) |   |   |                         |
|     |   |   | K |   |   |         |   |   |   |   |                         |
|     |   |   | o |   |   |         |   | u |   |   |                         |
|     |   |   | v |   |   |         |   | s |   |   |                         |
|     |   |   | a |   |   |         |   | e |   |   |                         |
|     |   |   | r |   |   |         |   | w |   |   |                         |
|     |   |   | i |   |   |         |   | e |   |   |                         |
|     |   |   | a |   |   |         |   | i |   |   |                         |
|     |   |   | n |   |   |         |   | g |   |   |                         |
|     |   |   | z |   |   |         |   | h |   |   |                         |
|     |   |   | e |   |   |         |   | t |   |   |                         |
|     |   |   | n |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   | W |   |   |         |   | f |   |   |                         |
|     |   |   | a |   |   |         |   | i |   |   |                         |
|     |   |   | h |   |   |         |   | t |   |   |                         |
|     |   |   | l |   |   |         |   | ? |   |   |                         |
|     |   |   | d |   |   |         |   |   |   |   |                         |
|     |   |   | e |   |   |         |   | u |   |   |                         |
|     |   |   | r |   |   |         |   | s |   |   |                         |
|     |   |   | F |   |   |         |   | e |   |   |                         |
|     |   |   | i |   |   |         |   | c |   |   |                         |
|     |   |   | t |   |   |         |   | o |   |   |                         |
|     |   |   | m |   |   |         |   | v |   |   |                         |
|     |   |   | e |   |   |         |   | a |   |   |                         |
|     |   |   | t |   |   |         |   | r |   |   |                         |
|     |   |   | h |   |   |         |   | i |   |   |                         |
|     |   |   | o |   |   |         |   | a |   |   |                         |
|     |   |   | d |   |   |         |   | n |   |   |                         |
|     |   |   | e |   |   |         |   | c |   |   |                         |
|     |   |   | : |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   | 0 |   |   |         |   | ? |   |   |                         |
|     |   |   | : |   |   |         |   |   |   |   |                         |
|     |   |   | W |   |   |         |   | u |   |   |                         |
|     |   |   | L |   |   |         |   | s |   |   |                         |
|     |   |   | S |   |   |         |   | e |   |   |                         |
|     |   |   | , |   |   |         |   | o |   |   |                         |
|     |   |   | 1 |   |   |         |   | f |   |   |                         |
|     |   |   | : |   |   |         |   | f |   |   |                         |
|     |   |   | P |   |   |         |   | i |   |   |                         |
|     |   |   | L |   |   |         |   | t |   |   |                         |
|     |   |   | S |   |   |         |   | t |   |   |                         |
|     |   |   | Q |   |   |         |   | i |   |   |                         |
|     |   |   | , |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   | 2 |   |   |         |   | m |   |   |                         |
|     |   |   | : |   |   |         |   | e |   |   |                         |
|     |   |   | P |   |   |         |   | t |   |   |                         |
|     |   |   | M |   |   |         |   | h |   |   |                         |
|     |   |   | L |   |   |         |   | o |   |   |                         |
|     |   |   | E |   |   |         |   | d |   |   |                         |
|     |   |   | , |   |   |         |   | : |   |   |                         |
|     |   |   | 3 |   |   |         |   |   |   |   |                         |
|     |   |   | : |   |   |         |   | 0 |   |   |                         |
|     |   |   | W |   |   |         |   | : |   |   |                         |
|     |   |   | T |   |   |         |   | W |   |   |                         |
|     |   |   | L |   |   |         |   | L |   |   |                         |
|     |   |   | S |   |   |         |   | S |   |   |                         |
|     |   |   |   |   |   |         |   | , |   |   |                         |
|     |   |   | W |   |   |         |   | 1 |   |   |                         |
|     |   |   | T |   |   |         |   | : |   |   |                         |
|     |   |   | L |   |   |         |   | P |   |   |                         |
|     |   |   | S |   |   |         |   | L |   |   |                         |
|     |   |   | : |   |   |         |   | S |   |   |                         |
|     |   |   | n |   |   |         |   | Q |   |   |                         |
|     |   |   | e |   |   |         |   | , |   |   |                         |
|     |   |   | i |   |   |         |   |   |   |   |                         |
|     |   |   | n |   |   |         |   | 2 |   |   |                         |
|     |   |   | / |   |   |         |   | : |   |   |                         |
|     |   |   | j |   |   |         |   | P |   |   |                         |
|     |   |   | a |   |   |         |   | M |   |   |                         |
|     |   |   | ( |   |   |         |   | L |   |   |                         |
|     |   |   | j |   |   |         |   | E |   |   |                         |
|     |   |   | e |   |   |         |   | , |   |   |                         |
|     |   |   | t |   |   |         |   | 3 |   |   |                         |
|     |   |   | z |   |   |         |   | : |   |   |                         |
|     |   |   | t |   |   |         |   | W |   |   |                         |
|     |   |   | ü |   |   |         |   | T |   |   |                         |
|     |   |   | b |   |   |         |   | L |   |   |                         |
|     |   |   | e |   |   |         |   | S |   |   |                         |
|     |   |   | r |   |   |         |   |   |   |   |                         |
|     |   |   | f |   |   |         |   | W |   |   |                         |
|     |   |   | l |   |   |         |   | T |   |   |                         |
|     |   |   | ü |   |   |         |   | L |   |   |                         |
|     |   |   | s |   |   |         |   | S |   |   |                         |
|     |   |   | s |   |   |         |   | : |   |   |                         |
|     |   |   | i |   |   |         |   | n |   |   |                         |
|     |   |   | g |   |   |         |   | o |   |   |                         |
|     |   |   | ) |   |   |         |   | / |   |   |                         |
|     |   |   |   |   |   |         |   | y |   |   |                         |
|     |   |   | N |   |   |         |   | e |   |   |                         |
|     |   |   | a |   |   |         |   | s |   |   |                         |
|     |   |   | c |   |   |         |   | ( |   |   |                         |
|     |   |   | h |   |   |         |   | n |   |   |                         |
|     |   |   | w |   |   |         |   | o |   |   |                         |
|     |   |   | e |   |   |         |   | w |   |   |                         |
|     |   |   | i |   |   |         |   | r |   |   |                         |
|     |   |   | s |   |   |         |   | e |   |   |                         |
|     |   |   | w |   |   |         |   | d |   |   |                         |
|     |   |   | a |   |   |         |   | u |   |   |                         |
|     |   |   | h |   |   |         |   | n |   |   |                         |
|     |   |   | r |   |   |         |   | d |   |   |                         |
|     |   |   | s |   |   |         |   | a |   |   |                         |
|     |   |   | c |   |   |         |   | n |   |   |                         |
|     |   |   | h |   |   |         |   | t |   |   |                         |
|     |   |   | e |   |   |         |   | ) |   |   |                         |
|     |   |   | i |   |   |         |   |   |   |   |                         |
|     |   |   | n |   |   |         |   | c |   |   |                         |
|     |   |   | l |   |   |         |   | o |   |   |                         |
|     |   |   | i |   |   |         |   | u |   |   |                         |
|     |   |   | c |   |   |         |   | n |   |   |                         |
|     |   |   | h |   |   |         |   | t |   |   |                         |
|     |   |   | k |   |   |         |   | i |   |   |                         |
|     |   |   | e |   |   |         |   | n |   |   |                         |
|     |   |   | i |   |   |         |   | g |   |   |                         |
|     |   |   | t |   |   |         |   | e |   |   |                         |
|     |   |   | e |   |   |         |   | f |   |   |                         |
|     |   |   | n |   |   |         |   | f |   |   |                         |
|     |   |   | v |   |   |         |   | i |   |   |                         |
|     |   |   | a |   |   |         |   | c |   |   |                         |
|     |   |   | r |   |   |         |   | i |   |   |                         |
|     |   |   | i |   |   |         |   | e |   |   |                         |
|     |   |   | a |   |   |         |   | n |   |   |                         |
|     |   |   | b |   |   |         |   | c |   |   |                         |
|     |   |   | e |   |   |         |   | i |   |   |                         |
|     |   |   | l |   |   |         |   | e |   |   |                         |
|     |   |   | z |   |   |         |   | s |   |   |                         |
|     |   |   | w |   |   |         |   | v |   |   |                         |
|     |   |   | i |   |   |         |   | a |   |   |                         |
|     |   |   | s |   |   |         |   | r |   |   |                         |
|     |   |   | c |   |   |         |   | y |   |   |                         |
|     |   |   | h |   |   |         |   | w |   |   |                         |
|     |   |   | e |   |   |         |   | i |   |   |                         |
|     |   |   | n |   |   |         |   | t |   |   |                         |
|     |   |   | M |   |   |         |   | h |   |   |                         |
|     |   |   | e |   |   |         |   | m |   |   |                         |
|     |   |   | s |   |   |         |   | e |   |   |                         |
|     |   |   | s |   |   |         |   | a |   |   |                         |
|     |   |   | u |   |   |         |   | s |   |   |                         |
|     |   |   | n |   |   |         |   | u |   |   |                         |
|     |   |   | g |   |   |         |   | r |   |   |                         |
|     |   |   | e |   |   |         |   | e |   |   |                         |
|     |   |   | n |   |   |         |   | m |   |   |                         |
|     |   |   | : |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   | 0 |   |   |         |   | t |   |   |                         |
|     |   |   | : |   |   |         |   | s |   |   |                         |
|     |   |   | n |   |   |         |   | : |   |   |                         |
|     |   |   | e |   |   |         |   |   |   |   |                         |
|     |   |   | i |   |   |         |   | 0 |   |   |                         |
|     |   |   | n |   |   |         |   | : |   |   |                         |
|     |   |   | ; |   |   |         |   | n |   |   |                         |
|     |   |   | 1 |   |   |         |   | o |   |   |                         |
|     |   |   | : |   |   |         |   | ; |   |   |                         |
|     |   |   | j |   |   |         |   | 1 |   |   |                         |
|     |   |   | a |   |   |         |   | : |   |   |                         |
|     |   |   |   |   |   |         |   | y |   |   |                         |
|     |   |   | D |   |   |         |   | e |   |   |                         |
|     |   |   | a |   |   |         |   | s |   |   |                         |
|     |   |   | t |   |   |         |   |   |   |   |                         |
|     |   |   | u |   |   |         |   | D |   |   |                         |
|     |   |   | m |   |   |         |   | a |   |   |                         |
|     |   |   | / |   |   |         |   | t |   |   |                         |
|     |   |   | U |   |   |         |   | e |   |   |                         |
|     |   |   | h |   |   |         |   | t |   |   |                         |
|     |   |   | r |   |   |         |   | i |   |   |                         |
|     |   |   | z |   |   |         |   | m |   |   |                         |
|     |   |   | e |   |   |         |   | e |   |   |                         |
|     |   |   | i |   |   |         |   | o |   |   |                         |
|     |   |   | t |   |   |         |   | f |   |   |                         |
|     |   |   | S |   |   |         |   | e |   |   |                         |
|     |   |   | r |   |   |         |   | . |   |   |                         |
|     |   |   | / |   |   |         |   | g |   |   |                         |
|     |   |   | Y |   |   |         |   | . |   |   |                         |
|     |   |   | - |   |   |         |   | S |   |   |                         |
|     |   |   | S |   |   |         |   | r |   |   |                         |
|     |   |   | e |   |   |         |   | / |   |   |                         |
|     |   |   | p |   |   |         |   | Y |   |   |                         |
|     |   |   | a |   |   |         |   | s |   |   |                         |
|     |   |   | r |   |   |         |   | e |   |   |                         |
|     |   |   | a |   |   |         |   | p |   |   |                         |
|     |   |   | t |   |   |         |   | a |   |   |                         |
|     |   |   | . |   |   |         |   | r |   |   |                         |
|     |   |   | ; |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   | Z |   |   |         |   | i |   |   |                         |
|     |   |   | e |   |   |         |   | o |   |   |                         |
|     |   |   | i |   |   |         |   | n |   |   |                         |
|     |   |   | t |   |   |         |   | : |   |   |                         |
|     |   |   | e |   |   |         |   |   |   |   |                         |
|     |   |   | i |   |   |         |   | t |   |   |                         |
|     |   |   | n |   |   |         |   | i |   |   |                         |
|     |   |   | h |   |   |         |   | m |   |   |                         |
|     |   |   | e |   |   |         |   | e |   |   |                         |
|     |   |   | i |   |   |         |   | b |   |   |                         |
|     |   |   | t |   |   |         |   | a |   |   |                         |
|     |   |   | ( |   |   |         |   | s |   |   |                         |
|     |   |   | 1 |   |   |         |   | e |   |   |                         |
|     |   |   | : |   |   |         |   | ( |   |   |                         |
|     |   |   | s |   |   |         |   | 1 |   |   |                         |
|     |   |   | ; |   |   |         |   | : |   |   |                         |
|     |   |   | 2 |   |   |         |   | s |   |   |                         |
|     |   |   | : |   |   |         |   | ; |   |   |                         |
|     |   |   | m |   |   |         |   | 2 |   |   |                         |
|     |   |   | i |   |   |         |   | : |   |   |                         |
|     |   |   | n |   |   |         |   | m |   |   |                         |
|     |   |   | ) |   |   |         |   | i |   |   |                         |
|     |   |   | . |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | ) |   |   |                         |
|     |   |   |   |   |   |         |   | . |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| Hi  |   |   |   |   |   |         |   | H |   |   |                         |
| ern |   |   |   |   |   |         |   | e |   |   |                         |
| ach |   |   |   |   |   |         |   | r |   |   |                         |
| f   |   |   |   |   |   |         |   | e |   |   |                         |
| olg |   |   |   |   |   |         |   | a |   |   |                         |
| en, |   |   |   |   |   |         |   | f |   |   |                         |
| für |   |   |   |   |   |         |   | t |   |   |                         |
| j   |   |   |   |   |   |         |   | e |   |   |                         |
| ede |   |   |   |   |   |         |   | r |   |   |                         |
| M   |   |   |   |   |   |         |   | , |   |   |                         |
| ess |   |   |   |   |   |         |   | f |   |   |                         |
| ung |   |   |   |   |   |         |   | o |   |   |                         |
| der |   |   |   |   |   |         |   | r |   |   |                         |
| Abk |   |   |   |   |   |         |   | e |   |   |                         |
| lin |   |   |   |   |   |         |   | a |   |   |                         |
| gku |   |   |   |   |   |         |   | c |   |   |                         |
| rve |   |   |   |   |   |         |   | h |   |   |                         |
| e   |   |   |   |   |   |         |   | m |   |   |                         |
| ine |   |   |   |   |   |         |   | e |   |   |                         |
| Zei |   |   |   |   |   |         |   | a |   |   |                         |
| le, |   |   |   |   |   |         |   | s |   |   |                         |
| die |   |   |   |   |   |         |   | u |   |   |                         |
| fo  |   |   |   |   |   |         |   | r |   |   |                         |
| lge |   |   |   |   |   |         |   | e |   |   |                         |
| nde |   |   |   |   |   |         |   | m |   |   |                         |
| Da  |   |   |   |   |   |         |   | e |   |   |                         |
| ten |   |   |   |   |   |         |   | n |   |   |                         |
| en  |   |   |   |   |   |         |   | t |   |   |                         |
| thä |   |   |   |   |   |         |   | o |   |   |                         |
| lt: |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
| -   |   |   |   |   |   |         |   | h |   |   |                         |
|   S |   |   |   |   |   |         |   | e |   |   |                         |
| tar |   |   |   |   |   |         |   | d |   |   |                         |
| tda |   |   |   |   |   |         |   | e |   |   |                         |
| tum |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | y |   |   |                         |
| der |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|   M |   |   |   |   |   |         |   | v |   |   |                         |
| ess |   |   |   |   |   |         |   | e |   |   |                         |
| ung |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|   – |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | w |   |   |                         |
|   o |   |   |   |   |   |         |   | i |   |   |                         |
| der |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|   Z |   |   |   |   |   |         |   | v |   |   |                         |
| eit |   |   |   |   |   |         |   | e |   |   |                         |
| dif |   |   |   |   |   |         |   | n |   |   |                         |
| fer |   |   |   |   |   |         |   | , |   |   |                         |
| enz |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
| zum |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|  Ab |   |   |   |   |   |         |   | n |   |   |                         |
| tre |   |   |   |   |   |         |   | i |   |   |                         |
| nn- |   |   |   |   |   |         |   | n |   |   |                         |
| Dat |   |   |   |   |   |         |   | g |   |   |                         |
| um; |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
| -   |   |   |   |   |   |         |   | l |   |   |                         |
| Mes |   |   |   |   |   |         |   | l |   |   |                         |
| sda |   |   |   |   |   |         |   | o |   |   |                         |
| uer |   |   |   |   |   |         |   | w |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
| der |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|  Br |   |   |   |   |   |         |   | t |   |   |                         |
| utt |   |   |   |   |   |         |   | a |   |   |                         |
| ome |   |   |   |   |   |         |   | : |   |   |                         |
| ssu |   |   |   |   |   |         |   |   |   |   |                         |
| ng; |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| -   |   |   |   |   |   |         |   |   |   |   |                         |
|  Br |   |   |   |   |   |         |   | s |   |   |                         |
| utt |   |   |   |   |   |         |   | t |   |   |                         |
| oim |   |   |   |   |   |         |   | a |   |   |                         |
| pul |   |   |   |   |   |         |   | r |   |   |                         |
| se; |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| -   |   |   |   |   |   |         |   |   |   |   |                         |
| Bru |   |   |   |   |   |         |   |   |   |   |                         |
| tto |   |   |   |   |   |         |   |   |   |   |                         |
| zäh |   |   |   |   |   |         |   |   |   |   |                         |
| lra |   |   |   |   |   |         |   |   |   |   |                         |
| te; |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
| -   |   |   |   |   |   |         |   | t |   |   |                         |
| Uns |   |   |   |   |   |         |   | e |   |   |                         |
| ich |   |   |   |   |   |         |   |   |   |   |                         |
| erh |   |   |   |   |   |         |   |   |   |   |                         |
| eit |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| der |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
| Bru |   |   |   |   |   |         |   |   |   |   |                         |
| tto |   |   |   |   |   |         |   |   |   |   |                         |
| zäh |   |   |   |   |   |         |   |   |   |   |                         |
| lra |   |   |   |   |   |         |   |   |   |   |                         |
| te; |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| -   |   |   |   |   |   |         |   | t |   |   |                         |
| Mes |   |   |   |   |   |         |   | h |   |   |                         |
| sda |   |   |   |   |   |         |   | e |   |   |                         |
| uer |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| der |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| Nul |   |   |   |   |   |         |   | m |   |   |                         |
| lef |   |   |   |   |   |         |   | e |   |   |                         |
| fek |   |   |   |   |   |         |   | a |   |   |                         |
| tme |   |   |   |   |   |         |   | s |   |   |                         |
| ssu |   |   |   |   |   |         |   | u |   |   |                         |
| ng; |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
| -   |   |   |   |   |   |         |   | m |   |   |                         |
|   I |   |   |   |   |   |         |   | e |   |   |                         |
| mpu |   |   |   |   |   |         |   | n |   |   |                         |
| lse |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| der |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| Nul |   |   |   |   |   |         |   |   |   |   |                         |
| lef |   |   |   |   |   |         |   | – |   |   |                         |
| fek |   |   |   |   |   |         |   |   |   |   |                         |
| tme |   |   |   |   |   |         |   |   |   |   |                         |
| ssu |   |   |   |   |   |         |   |   |   |   |                         |
| ng; |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| -   |   |   |   |   |   |         |   |   |   |   |                         |
|   N |   |   |   |   |   |         |   | o |   |   |                         |
| ull |   |   |   |   |   |         |   | r |   |   |                         |
| eff |   |   |   |   |   |         |   |   |   |   |                         |
| ekt |   |   |   |   |   |         |   |   |   |   |                         |
| zäh |   |   |   |   |   |         |   |   |   |   |                         |
| lra |   |   |   |   |   |         |   |   |   |   |                         |
| te; |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| -   |   |   |   |   |   |         |   | t |   |   |                         |
| Uns |   |   |   |   |   |         |   | i |   |   |                         |
| ich |   |   |   |   |   |         |   | m |   |   |                         |
| erh |   |   |   |   |   |         |   | e |   |   |                         |
| eit |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| der |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|   N |   |   |   |   |   |         |   | d |   |   |                         |
| ull |   |   |   |   |   |         |   | i |   |   |                         |
| eff |   |   |   |   |   |         |   | f |   |   |                         |
| ekt |   |   |   |   |   |         |   | f |   |   |                         |
| zäh |   |   |   |   |   |         |   | e |   |   |                         |
| lra |   |   |   |   |   |         |   | r |   |   |                         |
| te; |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
| -   |   |   |   |   |   |         |   | c |   |   |                         |
|  Ne |   |   |   |   |   |         |   | e |   |   |                         |
| tto |   |   |   |   |   |         |   |   |   |   |                         |
| zäh |   |   |   |   |   |         |   |   |   |   |                         |
| lra |   |   |   |   |   |         |   |   |   |   |                         |
| te; |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| -   |   |   |   |   |   |         |   |   |   |   |                         |
| Uns |   |   |   |   |   |         |   | t |   |   |                         |
| ich |   |   |   |   |   |         |   | o |   |   |                         |
| erh |   |   |   |   |   |         |   |   |   |   |                         |
| eit |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| der |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|  Ne |   |   |   |   |   |         |   | a |   |   |                         |
| tto |   |   |   |   |   |         |   | t |   |   |                         |
| zäh |   |   |   |   |   |         |   | e |   |   |                         |
| lra |   |   |   |   |   |         |   | t |   |   |                         |
| te; |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | h |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | h |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | p |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | y |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | k |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | . |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | k |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | k |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | y |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | k |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | y |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| **@ |   |   |   |   |   |         |   |   |   |   |                         |
| Gam |   |   |   |   |   |         |   |   |   |   |                         |
| spk |   |   |   |   |   |         |   |   |   |   |                         |
| 1-G |   |   |   |   |   |         |   |   |   |   |                         |
| rid |   |   |   |   |   |         |   |   |   |   |                         |
| :** |   |   |   |   |   |         |   |   |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| Cur |   |   |   | f |   |         |   | f |   |   |                         |
| veU |   |   |   | ü |   |         |   | o |   |   |                         |
| se= |   |   |   | r |   |         |   | r |   |   |                         |
| 0 0 |   |   |   | C |   |         |   | c |   |   |                         |
| 0 0 |   |   |   | h |   |         |   | h |   |   |                         |
|     |   |   |   | e |   |         |   | e |   |   |                         |
|     |   |   |   | c |   |         |   | c |   |   |                         |
|     |   |   |   | k |   |         |   | k |   |   |                         |
|     |   |   |   | b |   |         |   | b |   |   |                         |
|     |   |   |   | o |   |         |   | o |   |   |                         |
|     |   |   |   | x |   |         |   | x |   |   |                         |
|     |   |   |   | - |   |         |   | v |   |   |                         |
|     |   |   |   | W |   |         |   | a |   |   |                         |
|     |   |   |   | e |   |         |   | l |   |   |                         |
|     |   |   |   | r |   |         |   | u |   |   |                         |
|     |   |   |   | t |   |         |   | e |   |   |                         |
|     |   |   |   | e |   |         |   | s |   |   |                         |
|     |   |   |   | i |   |         |   | w |   |   |                         |
|     |   |   |   | m |   |         |   | i |   |   |                         |
|     |   |   |   | D |   |         |   | t |   |   |                         |
|     |   |   |   | i |   |         |   | h |   |   |                         |
|     |   |   |   | a |   |         |   | i |   |   |                         |
|     |   |   |   | l |   |         |   | n |   |   |                         |
|     |   |   |   | o |   |         |   | t |   |   |                         |
|     |   |   |   | g |   |         |   | h |   |   |                         |
|     |   |   |   | „ |   |         |   | e |   |   |                         |
|     |   |   |   | D |   |         |   | d |   |   |                         |
|     |   |   |   | e |   |         |   | i |   |   |                         |
|     |   |   |   | f |   |         |   | a |   |   |                         |
|     |   |   |   | i |   |         |   | l |   |   |                         |
|     |   |   |   | n |   |         |   | o |   |   |                         |
|     |   |   |   | i |   |         |   | g |   |   |                         |
|     |   |   |   | t |   |         |   | „ |   |   |                         |
|     |   |   |   | i |   |         |   | D |   |   |                         |
|     |   |   |   | o |   |         |   | e |   |   |                         |
|     |   |   |   | n |   |         |   | f |   |   |                         |
|     |   |   |   | e |   |         |   | i |   |   |                         |
|     |   |   |   | n |   |         |   | n |   |   |                         |
|     |   |   |   | e |   |         |   | i |   |   |                         |
|     |   |   |   | r |   |         |   | t |   |   |                         |
|     |   |   |   | g |   |         |   | i |   |   |                         |
|     |   |   |   | i |   |         |   | o |   |   |                         |
|     |   |   |   | e |   |         |   | n |   |   |                         |
|     |   |   |   | a |   |         |   | o |   |   |                         |
|     |   |   |   | b |   |         |   | f |   |   |                         |
|     |   |   |   | h |   |         |   | e |   |   |                         |
|     |   |   |   | ä |   |         |   | n |   |   |                         |
|     |   |   |   | n |   |         |   | e |   |   |                         |
|     |   |   |   | g |   |         |   | r |   |   |                         |
|     |   |   |   | i |   |         |   | g |   |   |                         |
|     |   |   |   | g |   |         |   | y |   |   |                         |
|     |   |   |   | e |   |         |   | d |   |   |                         |
|     |   |   |   | r |   |         |   | e |   |   |                         |
|     |   |   |   | K |   |         |   | p |   |   |                         |
|     |   |   |   | u |   |         |   | e |   |   |                         |
|     |   |   |   | r |   |         |   | n |   |   |                         |
|     |   |   |   | v |   |         |   | d |   |   |                         |
|     |   |   |   | e |   |         |   | e |   |   |                         |
|     |   |   |   | n |   |         |   | n |   |   |                         |
|     |   |   |   | " |   |         |   | t |   |   |                         |
|     |   |   |   | ( |   |         |   | c |   |   |                         |
|     |   |   |   | n |   |         |   | u |   |   |                         |
|     |   |   |   | u |   |         |   | r |   |   |                         |
|     |   |   |   | r |   |         |   | v |   |   |                         |
|     |   |   |   | U |   |         |   | e |   |   |                         |
|     |   |   |   | R |   |         |   | s |   |   |                         |
|     |   |   |   | 1 |   |         |   | " |   |   |                         |
|     |   |   |   | - |   |         |   | ( |   |   |                         |
|     |   |   |   | P |   |         |   | o |   |   |                         |
|     |   |   |   | r |   |         |   | n |   |   |                         |
|     |   |   |   | o |   |         |   | l |   |   |                         |
|     |   |   |   | j |   |         |   | y |   |   |                         |
|     |   |   |   | e |   |         |   | U |   |   |                         |
|     |   |   |   | k |   |         |   | R |   |   |                         |
|     |   |   |   | t |   |         |   | 1 |   |   |                         |
|     |   |   |   | e |   |         |   | p |   |   |                         |
|     |   |   |   | ) |   |         |   | r |   |   |                         |
|     |   |   |   | ; |   |         |   | o |   |   |                         |
|     |   |   |   | w |   |         |   | j |   |   |                         |
|     |   |   |   | i |   |         |   | e |   |   |                         |
|     |   |   |   | r |   |         |   | c |   |   |                         |
|     |   |   |   | d |   |         |   | t |   |   |                         |
|     |   |   |   | n |   |         |   | s |   |   |                         |
|     |   |   |   | i |   |         |   | ) |   |   |                         |
|     |   |   |   | c |   |         |   | ; |   |   |                         |
|     |   |   |   | h |   |         |   | n |   |   |                         |
|     |   |   |   | t |   |         |   | o |   |   |                         |
|     |   |   |   | m |   |         |   | l |   |   |                         |
|     |   |   |   | e |   |         |   | o |   |   |                         |
|     |   |   |   | h |   |         |   | n |   |   |                         |
|     |   |   |   | r |   |         |   | g |   |   |                         |
|     |   |   |   | v |   |         |   | e |   |   |                         |
|     |   |   |   | e |   |         |   | r |   |   |                         |
|     |   |   |   | r |   |         |   | u |   |   |                         |
|     |   |   |   | w |   |         |   | s |   |   |                         |
|     |   |   |   | e |   |         |   | e |   |   |                         |
|     |   |   |   | n |   |         |   | d |   |   |                         |
|     |   |   |   | d |   |         |   | ; |   |   |                         |
|     |   |   |   | e |   |         |   |   |   |   |                         |
|     |   |   |   | t |   |         |   |   |   |   |                         |
|     |   |   |   | ; |   |         |   |   |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| U   |   |   |   | W |   |         |   | R |   |   |                         |
| nit |   |   |   | e |   |         |   | a |   |   |                         |
| Rad |   |   |   | r |   |         |   | d |   |   |                         |
| io= |   |   |   | t |   |         |   | i |   |   |                         |
| **  |   |   |   | e |   |         |   | o |   |   |                         |
| 1** |   |   |   | d |   |         |   | b |   |   |                         |
| :ma |   |   |   | e |   |         |   | u |   |   |                         |
| rk: |   |   |   | r |   |         |   | t |   |   |                         |
| `1` |   |   |   | R |   |         |   | t |   |   |                         |
| **  |   |   |   | a |   |         |   | o |   |   |                         |
| 1** |   |   |   | d |   |         |   | n |   |   |                         |
| :ma |   |   |   | i |   |         |   | v |   |   |                         |
| rk: |   |   |   | o |   |         |   | a |   |   |                         |
| `2` |   |   |   | b |   |         |   | l |   |   |                         |
| **  |   |   |   | u |   |         |   | u |   |   |                         |
| 1** |   |   |   | t |   |         |   | e |   |   |                         |
| **  |   |   |   | t |   |         |   | s |   |   |                         |
| 1** |   |   |   | o |   |         |   | ( |   |   |                         |
| **  |   |   |   | n |   |         |   | 1 |   |   |                         |
| 1** |   |   |   | s |   |         |   | o |   |   |                         |
|     |   |   |   | ( |   |         |   | r |   |   |                         |
| *R  |   |   |   | 1 |   |         |   | 2 |   |   |                         |
| edu |   |   |   | o |   |         |   | ) |   |   |                         |
| ced |   |   |   | d |   |         |   | w |   |   |                         |
| t   |   |   |   | e |   |         |   | i |   |   |                         |
| o*, |   |   |   | r |   |         |   | t |   |   |                         |
| si  |   |   |   | 2 |   |         |   | h |   |   |                         |
| nce |   |   |   | ) |   |         |   | i |   |   |                         |
| V.  |   |   |   | i |   |         |   | n |   |   |                         |
| 2   |   |   |   | m |   |         |   | t |   |   |                         |
| .4. |   |   |   | D |   |         |   | h |   |   |                         |
| 19: |   |   |   | i |   |         |   | e |   |   |                         |
|     |   |   |   | a |   |         |   | d |   |   |                         |
| U   |   |   |   | l |   |         |   | i |   |   |                         |
| nit |   |   |   | o |   |         |   | a |   |   |                         |
| Rad |   |   |   | g |   |         |   | l |   |   |                         |
| io= |   |   |   | z |   |         |   | o |   |   |                         |
| **  |   |   |   | u |   |         |   | g |   |   |                         |
| 1** |   |   |   | r |   |         |   | f |   |   |                         |
| **  |   |   |   | E |   |         |   | o |   |   |                         |
| 1** |   |   |   | i |   |         |   | r |   |   |                         |
| **  |   |   |   | n |   |         |   | i |   |   |                         |
| 1** |   |   |   | g |   |         |   | n |   |   |                         |
| **  |   |   |   | a |   |         |   | p |   |   |                         |
| 1** |   |   |   | b |   |         |   | u |   |   |                         |
| **  |   |   |   | e |   |         |   | t |   |   |                         |
| 1** |   |   |   | d |   |         |   | o |   |   |                         |
|     |   |   |   | e |   |         |   | f |   |   |                         |
| *(  |   |   |   | r |   |         |   | c |   |   |                         |
| see |   |   |   | G |   |         |   | o |   |   |                         |
| s   |   |   |   | a |   |         |   | u |   |   |                         |
| ect |   |   |   | m |   |         |   | n |   |   |                         |
| ion |   |   |   | m |   |         |   | t |   |   |                         |
| 7.1 |   |   |   | a |   |         |   | i |   |   |                         |
| 6)* |   |   |   | z |   |         |   | n |   |   |                         |
|     |   |   |   | ä |   |         |   | g |   |   |                         |
|     |   |   |   | h |   |         |   | r |   |   |                         |
|     |   |   |   | l |   |         |   | a |   |   |                         |
|     |   |   |   | r |   |         |   | t |   |   |                         |
|     |   |   |   | a |   |         |   | e |   |   |                         |
|     |   |   |   | t |   |         |   | s |   |   |                         |
|     |   |   |   | e |   |         |   | o |   |   |                         |
|     |   |   |   | n |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| U   |   |   |   | W |   |         |   | r |   |   |                         |
| nit |   |   |   | e |   |         |   | a |   |   |                         |
| Rad |   |   |   | r |   |         |   | d |   |   |                         |
| io= |   |   |   | t |   |         |   | i |   |   |                         |
| 1 1 |   |   |   | e |   |         |   | o |   |   |                         |
| 1 2 |   |   |   | d |   |         |   | b |   |   |                         |
| 1 1 |   |   |   | e |   |         |   | u |   |   |                         |
| 1   |   |   |   | r |   |         |   | t |   |   |                         |
|     |   |   |   | R |   |         |   | t |   |   |                         |
|     |   |   |   | a |   |         |   | o |   |   |                         |
|     |   |   |   | d |   |         |   | n |   |   |                         |
|     |   |   |   | i |   |         |   | v |   |   |                         |
|     |   |   |   | o |   |         |   | a |   |   |                         |
|     |   |   |   | b |   |         |   | l |   |   |                         |
|     |   |   |   | u |   |         |   | u |   |   |                         |
|     |   |   |   | t |   |         |   | e |   |   |                         |
|     |   |   |   | t |   |         |   | s |   |   |                         |
|     |   |   |   | o |   |         |   | w |   |   |                         |
|     |   |   |   | n |   |         |   | i |   |   |                         |
|     |   |   |   | s |   |         |   | t |   |   |                         |
|     |   |   |   | i |   |         |   | h |   |   |                         |
|     |   |   |   | m |   |         |   | i |   |   |                         |
|     |   |   |   | D |   |         |   | n |   |   |                         |
|     |   |   |   | i |   |         |   | t |   |   |                         |
|     |   |   |   | a |   |         |   | h |   |   |                         |
|     |   |   |   | l |   |         |   | e |   |   |                         |
|     |   |   |   | o |   |         |   | d |   |   |                         |
|     |   |   |   | g |   |         |   | i |   |   |                         |
|     |   |   |   | z |   |         |   | a |   |   |                         |
|     |   |   |   | u |   |         |   | l |   |   |                         |
|     |   |   |   | r |   |         |   | o |   |   |                         |
|     |   |   |   | E |   |         |   | g |   |   |                         |
|     |   |   |   | i |   |         |   | f |   |   |                         |
|     |   |   |   | n |   |         |   | o |   |   |                         |
|     |   |   |   | g |   |         |   | r |   |   |                         |
|     |   |   |   | a |   |         |   | i |   |   |                         |
|     |   |   |   | b |   |         |   | n |   |   |                         |
|     |   |   |   | e |   |         |   | p |   |   |                         |
|     |   |   |   | d |   |         |   | u |   |   |                         |
|     |   |   |   | e |   |         |   | t |   |   |                         |
|     |   |   |   | r |   |         |   | o |   |   |                         |
|     |   |   |   | G |   |         |   | f |   |   |                         |
|     |   |   |   | a |   |         |   | c |   |   |                         |
|     |   |   |   | m |   |         |   | o |   |   |                         |
|     |   |   |   | m |   |         |   | u |   |   |                         |
|     |   |   |   | a |   |         |   | n |   |   |                         |
|     |   |   |   | z |   |         |   | t |   |   |                         |
|     |   |   |   | ä |   |         |   | i |   |   |                         |
|     |   |   |   | h |   |         |   | n |   |   |                         |
|     |   |   |   | l |   |         |   | g |   |   |                         |
|     |   |   |   | r |   |         |   | r |   |   |                         |
|     |   |   |   | a |   |         |   | a |   |   |                         |
|     |   |   |   | t |   |         |   | t |   |   |                         |
|     |   |   |   | e |   |         |   | e |   |   |                         |
|     |   |   |   | n |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| Me  |   |   |   | M |   |         |   | t |   |   |                         |
| anT |   |   |   | i |   |         |   | y |   |   |                         |
| yp= |   |   |   | t |   |         |   | p |   |   |                         |
| 1   |   |   |   | t |   |         |   | e |   |   |                         |
|     |   |   |   | e |   |         |   | o |   |   |                         |
|     |   |   |   | l |   |         |   | f |   |   |                         |
|     |   |   |   | w |   |         |   | m |   |   |                         |
|     |   |   |   | e |   |         |   | e |   |   |                         |
|     |   |   |   | r |   |         |   | a |   |   |                         |
|     |   |   |   | t |   |         |   | n |   |   |                         |
|     |   |   |   | t |   |         |   | u |   |   |                         |
|     |   |   |   | y |   |         |   | s |   |   |                         |
|     |   |   |   | p |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | v |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | v |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| F   |   |   |   | ( |   |         |   | f |   |   |                         |
| BT= |   |   |   | 1 |   |         |   | a |   |   |                         |
| 1.1 |   |   |   | + |   |         |   | c |   |   |                         |
| 200 |   |   |   | b |   |         |   | t |   |   |                         |
|     |   |   |   | / |   |         |   | o |   |   |                         |
|     |   |   |   | ( |   |         |   | r |   |   |                         |
|     |   |   |   | 2 |   |         |   | e |   |   |                         |
|     |   |   |   | L |   |         |   | q |   |   |                         |
|     |   |   |   | ) |   |         |   | u |   |   |                         |
|     |   |   |   | ) |   |         |   | i |   |   |                         |
|     |   |   |   | - |   |         |   | v |   |   |                         |
|     |   |   |   | ä |   |         |   | a |   |   |                         |
|     |   |   |   | q |   |         |   | l |   |   |                         |
|     |   |   |   | u |   |         |   | e |   |   |                         |
|     |   |   |   | i |   |         |   | n |   |   |                         |
|     |   |   |   | v |   |         |   | t |   |   |                         |
|     |   |   |   | a |   |         |   | t |   |   |                         |
|     |   |   |   | l |   |         |   | o |   |   |                         |
|     |   |   |   | e |   |         |   | ( |   |   |                         |
|     |   |   |   | n |   |         |   | 1 |   |   |                         |
|     |   |   |   | t |   |         |   | + |   |   |                         |
|     |   |   |   | e |   |         |   | b |   |   |                         |
|     |   |   |   | r |   |         |   | / |   |   |                         |
|     |   |   |   | F |   |         |   | ( |   |   |                         |
|     |   |   |   | a |   |         |   | 2 |   |   |                         |
|     |   |   |   | k |   |         |   | L |   |   |                         |
|     |   |   |   | t |   |         |   | ) |   |   |                         |
|     |   |   |   | o |   |         |   | ) |   |   |                         |
|     |   |   |   | r |   |         |   |   |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| E   |   |   |   | s |   |         |   | s |   |   |                         |
| cor |   |   |   | o |   |         |   | h |   |   |                         |
| rUs |   |   |   | l |   |         |   | a |   |   |                         |
| e=1 |   |   |   | l |   |         |   | l |   |   |                         |
|     |   |   |   | e |   |         |   | l |   |   |                         |
|     |   |   |   | n |   |         |   | p |   |   |                         |
|     |   |   |   | E |   |         |   | e |   |   |                         |
|     |   |   |   | f |   |         |   | a |   |   |                         |
|     |   |   |   | f |   |         |   | k |   |   |                         |
|     |   |   |   | i |   |         |   | e |   |   |                         |
|     |   |   |   | z |   |         |   | f |   |   |                         |
|     |   |   |   | i |   |         |   | f |   |   |                         |
|     |   |   |   | e |   |         |   | i |   |   |                         |
|     |   |   |   | n |   |         |   | c |   |   |                         |
|     |   |   |   | z |   |         |   | i |   |   |                         |
|     |   |   |   | - |   |         |   | e |   |   |                         |
|     |   |   |   | K |   |         |   | n |   |   |                         |
|     |   |   |   | o |   |         |   | c |   |   |                         |
|     |   |   |   | r |   |         |   | y |   |   |                         |
|     |   |   |   | r |   |         |   | c |   |   |                         |
|     |   |   |   | e |   |         |   | o |   |   |                         |
|     |   |   |   | l |   |         |   | r |   |   |                         |
|     |   |   |   | a |   |         |   | r |   |   |                         |
|     |   |   |   | t |   |         |   | e |   |   |                         |
|     |   |   |   | i |   |         |   | l |   |   |                         |
|     |   |   |   | o |   |         |   | a |   |   |                         |
|     |   |   |   | n |   |         |   | t |   |   |                         |
|     |   |   |   | e |   |         |   | i |   |   |                         |
|     |   |   |   | n |   |         |   | o |   |   |                         |
|     |   |   |   | v |   |         |   | n |   |   |                         |
|     |   |   |   | e |   |         |   | s |   |   |                         |
|     |   |   |   | r |   |         |   | b |   |   |                         |
|     |   |   |   | w |   |         |   | e |   |   |                         |
|     |   |   |   | e |   |         |   | u |   |   |                         |
|     |   |   |   | n |   |         |   | s |   |   |                         |
|     |   |   |   | d |   |         |   | e |   |   |                         |
|     |   |   |   | e |   |         |   | d |   |   |                         |
|     |   |   |   | t |   |         |   | ? |   |   |                         |
|     |   |   |   | w |   |         |   |   |   |   |                         |
|     |   |   |   | e |   |         |   |   |   |   |                         |
|     |   |   |   | r |   |         |   |   |   |   |                         |
|     |   |   |   | d |   |         |   |   |   |   |                         |
|     |   |   |   | e |   |         |   |   |   |   |                         |
|     |   |   |   | n |   |         |   |   |   |   |                         |
|     |   |   |   | ? |   |         |   |   |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| Hi  |   |   |   |   |   |         |   | H |   |   |                         |
| ern |   |   |   |   |   |         |   | e |   |   |                         |
| ach |   |   |   |   |   |         |   | r |   |   |                         |
| f   |   |   |   |   |   |         |   | e |   |   |                         |
| olg |   |   |   |   |   |         |   | a |   |   |                         |
| en, |   |   |   |   |   |         |   | f |   |   |                         |
| für |   |   |   |   |   |         |   | t |   |   |                         |
| j   |   |   |   |   |   |         |   | e |   |   |                         |
| ede |   |   |   |   |   |         |   | r |   |   |                         |
| G   |   |   |   |   |   |         |   | , |   |   |                         |
| amm |   |   |   |   |   |         |   | f |   |   |                         |
| ali |   |   |   |   |   |         |   | o |   |   |                         |
| nie |   |   |   |   |   |         |   | r |   |   |                         |
| e   |   |   |   |   |   |         |   | e |   |   |                         |
| ine |   |   |   |   |   |         |   | a |   |   |                         |
| Zei |   |   |   |   |   |         |   | c |   |   |                         |
| le: |   |   |   |   |   |         |   | h |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
| -   |   |   |   |   |   |         |   | a |   |   |                         |
|   1 |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
| für |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|   V |   |   |   |   |   |         |   | e |   |   |                         |
| erw |   |   |   |   |   |         |   | o |   |   |                         |
| end |   |   |   |   |   |         |   | n |   |   |                         |
| ung |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
| der |   |   |   |   |   |         |   | w |   |   |                         |
|     |   |   |   |   |   |         |   | : |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| Lin |   |   |   |   |   |         |   | - |   |   |                         |
| ie, |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | 1 |   |   |                         |
|  so |   |   |   |   |   |         |   |   |   |   |                         |
| nst |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|  0; |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| -   |   |   |   |   |   |         |   | f |   |   |                         |
|   E |   |   |   |   |   |         |   | o |   |   |                         |
| ner |   |   |   |   |   |         |   | r |   |   |                         |
| gie |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| der |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|  Li |   |   |   |   |   |         |   | u |   |   |                         |
| nie |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
| (ke |   |   |   |   |   |         |   | g |   |   |                         |
| V); |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| -   |   |   |   |   |   |         |   |   |   |   |                         |
|   W |   |   |   |   |   |         |   |   |   |   |                         |
| ert |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
| der |   |   |   |   |   |         |   | h |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|  Ne |   |   |   |   |   |         |   |   |   |   |                         |
| tto |   |   |   |   |   |         |   |   |   |   |                         |
| zäh |   |   |   |   |   |         |   |   |   |   |                         |
| lra |   |   |   |   |   |         |   |   |   |   |                         |
| te, |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|  Rn |   |   |   |   |   |         |   | n |   |   |                         |
| et; |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | , |   |   |                         |
| -   |   |   |   |   |   |         |   |   |   |   |                         |
|   W |   |   |   |   |   |         |   |   |   |   |                         |
| ert |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| der |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | 0 |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| Unt |   |   |   |   |   |         |   |   |   |   |                         |
| erg |   |   |   |   |   |         |   |   |   |   |                         |
| run |   |   |   |   |   |         |   |   |   |   |                         |
| d-K |   |   |   |   |   |         |   |   |   |   |                         |
| ont |   |   |   |   |   |         |   |   |   |   |                         |
| inu |   |   |   |   |   |         |   | o |   |   |                         |
| um- |   |   |   |   |   |         |   | t |   |   |                         |
| Zäh |   |   |   |   |   |         |   | h |   |   |                         |
| lra |   |   |   |   |   |         |   | e |   |   |                         |
| te, |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | w |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
| RT; |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
| -   |   |   |   |   |   |         |   | ; |   |   |                         |
|   W |   |   |   |   |   |         |   |   |   |   |                         |
| ert |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| der |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|   N |   |   |   |   |   |         |   | r |   |   |                         |
| ett |   |   |   |   |   |         |   | g |   |   |                         |
| o-N |   |   |   |   |   |         |   | y |   |   |                         |
| ull |   |   |   |   |   |         |   |   |   |   |                         |
| eff |   |   |   |   |   |         |   |   |   |   |                         |
| ekt |   |   |   |   |   |         |   |   |   |   |                         |
| zäh |   |   |   |   |   |         |   |   |   |   |                         |
| lra |   |   |   |   |   |         |   |   |   |   |                         |
| te, |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
| Rbg |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|  im |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| sep |   |   |   |   |   |         |   | t |   |   |                         |
| ara |   |   |   |   |   |         |   | h |   |   |                         |
| ten |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| Spe |   |   |   |   |   |         |   |   |   |   |                         |
| ktr |   |   |   |   |   |         |   |   |   |   |                         |
| um; |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| -   |   |   |   |   |   |         |   | l |   |   |                         |
|   W |   |   |   |   |   |         |   | i |   |   |                         |
| ert |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| der |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|   E |   |   |   |   |   |         |   |   |   |   |                         |
| ffi |   |   |   |   |   |         |   |   |   |   |                         |
| zie |   |   |   |   |   |         |   | ( |   |   |                         |
| nz, |   |   |   |   |   |         |   | k |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | V |   |   |                         |
|  ef |   |   |   |   |   |         |   | ) |   |   |                         |
| fi; |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| -   |   |   |   |   |   |         |   | - |   |   |                         |
| Uns |   |   |   |   |   |         |   |   |   |   |                         |
| ich |   |   |   |   |   |         |   |   |   |   |                         |
| erh |   |   |   |   |   |         |   | v |   |   |                         |
| eit |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
| der |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|   E |   |   |   |   |   |         |   |   |   |   |                         |
| ffi |   |   |   |   |   |         |   |   |   |   |                         |
| zie |   |   |   |   |   |         |   |   |   |   |                         |
| nz; |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
| -   |   |   |   |   |   |         |   | f |   |   |                         |
|  Ga |   |   |   |   |   |         |   |   |   |   |                         |
| mma |   |   |   |   |   |         |   |   |   |   |                         |
| -Em |   |   |   |   |   |         |   |   |   |   |                         |
| iss |   |   |   |   |   |         |   |   |   |   |                         |
| ion |   |   |   |   |   |         |   |   |   |   |                         |
| sra |   |   |   |   |   |         |   |   |   |   |                         |
| te, |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
| pga |   |   |   |   |   |         |   |   |   |   |                         |
| mm; |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| -   |   |   |   |   |   |         |   |   |   |   |                         |
| Uns |   |   |   |   |   |         |   |   |   |   |                         |
| ich |   |   |   |   |   |         |   |   |   |   |                         |
| erh |   |   |   |   |   |         |   | c |   |   |                         |
| eit |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
| von |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
| pga |   |   |   |   |   |         |   | g |   |   |                         |
| mm; |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| -   |   |   |   |   |   |         |   |   |   |   |                         |
|   W |   |   |   |   |   |         |   |   |   |   |                         |
| ert |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
| der |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|   S |   |   |   |   |   |         |   | ; |   |   |                         |
| elb |   |   |   |   |   |         |   |   |   |   |                         |
| sts |   |   |   |   |   |         |   | - |   |   |                         |
| chw |   |   |   |   |   |         |   |   |   |   |                         |
| äch |   |   |   |   |   |         |   |   |   |   |                         |
| ung |   |   |   |   |   |         |   | v |   |   |                         |
| sko |   |   |   |   |   |         |   | a |   |   |                         |
| rre |   |   |   |   |   |         |   | l |   |   |                         |
| kti |   |   |   |   |   |         |   | u |   |   |                         |
| on, |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| f_a |   |   |   |   |   |         |   |   |   |   |                         |
| tt; |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| -   |   |   |   |   |   |         |   |   |   |   |                         |
| Uns |   |   |   |   |   |         |   | o |   |   |                         |
| ich |   |   |   |   |   |         |   | f |   |   |                         |
| erh |   |   |   |   |   |         |   |   |   |   |                         |
| eit |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| von |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
| f_a |   |   |   |   |   |         |   | h |   |   |                         |
| tt; |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| -   |   |   |   |   |   |         |   |   |   |   |                         |
| | W |   |   |   |   |   |         |   |   |   |   |                         |
| ert |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
| der |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | k |   |   |                         |
|   K |   |   |   |   |   |         |   | g |   |   |                         |
| oin |   |   |   |   |   |         |   | r |   |   |                         |
| zid |   |   |   |   |   |         |   | o |   |   |                         |
| enz |   |   |   |   |   |         |   | u |   |   |                         |
| sum |   |   |   |   |   |         |   | n |   |   |                         |
| mat |   |   |   |   |   |         |   | d |   |   |                         |
| ion |   |   |   |   |   |         |   |   |   |   |                         |
| sko |   |   |   |   |   |         |   |   |   |   |                         |
| rre |   |   |   |   |   |         |   |   |   |   |                         |
| kti |   |   |   |   |   |         |   |   |   |   |                         |
| on, |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
| | f |   |   |   |   |   |         |   | o |   |   |                         |
| _co |   |   |   |   |   |         |   | n |   |   |                         |
| in; |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
| -   |   |   |   |   |   |         |   | n |   |   |                         |
| Uns |   |   |   |   |   |         |   | u |   |   |                         |
| ich |   |   |   |   |   |         |   | u |   |   |                         |
| erh |   |   |   |   |   |         |   | m |   |   |                         |
| eit |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| von |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
| f_c |   |   |   |   |   |         |   | c |   |   |                         |
| oin |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | , |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | R |   |   |                         |
|     |   |   |   |   |   |         |   | T |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | v |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | p |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | k |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | p |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | , |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | R |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | v |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | h |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | p |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | k |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | y |   |   |                         |
|     |   |   |   |   |   |         |   | , |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | y |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | G |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | , |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | p |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | y |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | p |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | v |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | , |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | _ |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | y |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | _ |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | | |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | v |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | g |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | , |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | | |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | _ |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | - |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | y |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | _ |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| **  |   |   |   |   |   |         |   |   |   |   |                         |
| @Ka |   |   |   |   |   |         |   |   |   |   |                         |
| lfi |   |   |   |   |   |         |   |   |   |   |                         |
| t-G |   |   |   |   |   |         |   |   |   |   |                         |
| rid |   |   |   |   |   |         |   |   |   |   |                         |
| :** |   |   |   |   |   |         |   |   |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| Ka  |   |   |   |   |   | 2       |   |   |   |   | 2 integer field values  |
| lPa |   |   |   |   |   | Inte    |   |   |   |   | in the dialog:          |
| rs= |   |   |   |   |   | ger-Fel |   |   |   |   |                         |
| 5 2 |   |   |   |   |   | d-Werte |   |   |   |   | number of calibration   |
|     |   |   |   |   |   | im      |   |   |   |   | points                  |
| #1: |   |   |   |   |   | Dialog: |   |   |   |   |                         |
| 5   |   |   |   |   |   |         |   |   |   |   | Degree of fit           |
|     |   |   |   |   |   | Anzahl  |   |   |   |   | polynomial (0-3)        |
| #2: |   |   |   |   |   | der     |   |   |   |   |                         |
| 2   |   |   |   |   |   | K       |   |   |   |   | (0: Mean of the         |
|     |   |   |   |   |   | alibrie |   |   |   |   | y-values)               |
| Cti |   |   |   |   |   | rpunkte |   |   |   |   |                         |
| tel |   |   |   |   |   |         |   |   |   |   | Title of the            |
|     |   |   |   |   |   | Grad    |   |   |   |   | calibration curve       |
|     |   |   |   |   |   | des     |   |   |   |   |                         |
|     |   |   |   |   |   | Fit-P   |   |   |   |   |                         |
|     |   |   |   |   |   | olynoms |   |   |   |   |                         |
|     |   |   |   |   |   | (0-3)   |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   | (0:     |   |   |   |   |                         |
|     |   |   |   |   |   | Mit     |   |   |   |   |                         |
|     |   |   |   |   |   | telwert |   |   |   |   |                         |
|     |   |   |   |   |   | der     |   |   |   |   |                         |
|     |   |   |   |   |   | Y       |   |   |   |   |                         |
|     |   |   |   |   |   | -Werte) |   |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   | Titel   |   |   |   |   |                         |
|     |   |   |   |   |   | der     |   |   |   |   |                         |
|     |   |   |   |   |   | Kalibri |   |   |   |   |                         |
|     |   |   |   |   |   | erkurve |   |   |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| Hi  |   |   |   |   |   |         | H |   |   |   |                         |
| ern |   |   |   |   |   |         | e |   |   |   |                         |
| ach |   |   |   |   |   |         | r |   |   |   |                         |
| fol |   |   |   |   |   |         | e |   |   |   |                         |
| gen |   |   |   |   |   |         | a |   |   |   |                         |
| ze  |   |   |   |   |   |         | f |   |   |   |                         |
| ile |   |   |   |   |   |         | t |   |   |   |                         |
| nwe |   |   |   |   |   |         | e |   |   |   |                         |
| ise |   |   |   |   |   |         | r |   |   |   |                         |
| 4   |   |   |   |   |   |         | , |   |   |   |                         |
| We  |   |   |   |   |   |         | 4 |   |   |   |                         |
| rte |   |   |   |   |   |         | v |   |   |   |                         |
| der |   |   |   |   |   |         | a |   |   |   |                         |
| Kal |   |   |   |   |   |         | l |   |   |   |                         |
| ibr |   |   |   |   |   |         | u |   |   |   |                         |
| ier |   |   |   |   |   |         | e |   |   |   |                         |
| kur |   |   |   |   |   |         | s |   |   |   |                         |
| ve: |   |   |   |   |   |         | o |   |   |   |                         |
|     |   |   |   |   |   |         | f |   |   |   |                         |
| X   |   |   |   |   |   |         | t |   |   |   |                         |
| -We |   |   |   |   |   |         | h |   |   |   |                         |
| rt, |   |   |   |   |   |         | e |   |   |   |                         |
|     |   |   |   |   |   |         | c |   |   |   |                         |
| Std |   |   |   |   |   |         | a |   |   |   |                         |
| Uns |   |   |   |   |   |         | l |   |   |   |                         |
| ich |   |   |   |   |   |         | i |   |   |   |                         |
| erh |   |   |   |   |   |         | b |   |   |   |                         |
| eit |   |   |   |   |   |         | r |   |   |   |                         |
| des |   |   |   |   |   |         | a |   |   |   |                         |
| X   |   |   |   |   |   |         | t |   |   |   |                         |
| -We |   |   |   |   |   |         | i |   |   |   |                         |
| rts |   |   |   |   |   |         | o |   |   |   |                         |
| (o  |   |   |   |   |   |         | n |   |   |   |                         |
| der |   |   |   |   |   |         | c |   |   |   |                         |
| l   |   |   |   |   |   |         | u |   |   |   |                         |
| eer |   |   |   |   |   |         | r |   |   |   |                         |
| o   |   |   |   |   |   |         | v |   |   |   |                         |
| der |   |   |   |   |   |         | e |   |   |   |                         |
| 1), |   |   |   |   |   |         | f |   |   |   |                         |
|     |   |   |   |   |   |         | o |   |   |   |                         |
| Y   |   |   |   |   |   |         | l |   |   |   |                         |
| -We |   |   |   |   |   |         | l |   |   |   |                         |
| rt, |   |   |   |   |   |         | o |   |   |   |                         |
|     |   |   |   |   |   |         | w |   |   |   |                         |
| Std |   |   |   |   |   |         | , |   |   |   |                         |
| Uns |   |   |   |   |   |         | l |   |   |   |                         |
| ich |   |   |   |   |   |         | i |   |   |   |                         |
| erh |   |   |   |   |   |         | n |   |   |   |                         |
| eit |   |   |   |   |   |         | e |   |   |   |                         |
| des |   |   |   |   |   |         | b |   |   |   |                         |
| Y   |   |   |   |   |   |         | y |   |   |   |                         |
| -We |   |   |   |   |   |         | l |   |   |   |                         |
| rts |   |   |   |   |   |         | i |   |   |   |                         |
| (o  |   |   |   |   |   |         | n |   |   |   |                         |
| der |   |   |   |   |   |         | e |   |   |   |                         |
| l   |   |   |   |   |   |         | : |   |   |   |                         |
| eer |   |   |   |   |   |         |   |   |   |   |                         |
| o   |   |   |   |   |   |         | X |   |   |   |                         |
| der |   |   |   |   |   |         | v |   |   |   |                         |
| 1)  |   |   |   |   |   |         | a |   |   |   |                         |
|     |   |   |   |   |   |         | l |   |   |   |                         |
| (1  |   |   |   |   |   |         | u |   |   |   |                         |
| be  |   |   |   |   |   |         | e |   |   |   |                         |
| deu |   |   |   |   |   |         | , |   |   |   |                         |
| tet |   |   |   |   |   |         |   |   |   |   |                         |
| i   |   |   |   |   |   |         | S |   |   |   |                         |
| den |   |   |   |   |   |         | t |   |   |   |                         |
| tis |   |   |   |   |   |         | d |   |   |   |                         |
| che |   |   |   |   |   |         | u |   |   |   |                         |
| Wic |   |   |   |   |   |         | n |   |   |   |                         |
| htu |   |   |   |   |   |         | c |   |   |   |                         |
| ng) |   |   |   |   |   |         | e |   |   |   |                         |
|     |   |   |   |   |   |         | r |   |   |   |                         |
|     |   |   |   |   |   |         | t |   |   |   |                         |
|     |   |   |   |   |   |         | a |   |   |   |                         |
|     |   |   |   |   |   |         | i |   |   |   |                         |
|     |   |   |   |   |   |         | n |   |   |   |                         |
|     |   |   |   |   |   |         | t |   |   |   |                         |
|     |   |   |   |   |   |         | y |   |   |   |                         |
|     |   |   |   |   |   |         | o |   |   |   |                         |
|     |   |   |   |   |   |         | f |   |   |   |                         |
|     |   |   |   |   |   |         | X |   |   |   |                         |
|     |   |   |   |   |   |         | v |   |   |   |                         |
|     |   |   |   |   |   |         | a |   |   |   |                         |
|     |   |   |   |   |   |         | l |   |   |   |                         |
|     |   |   |   |   |   |         | u |   |   |   |                         |
|     |   |   |   |   |   |         | e |   |   |   |                         |
|     |   |   |   |   |   |         | ( |   |   |   |                         |
|     |   |   |   |   |   |         | o |   |   |   |                         |
|     |   |   |   |   |   |         | r |   |   |   |                         |
|     |   |   |   |   |   |         | e |   |   |   |                         |
|     |   |   |   |   |   |         | m |   |   |   |                         |
|     |   |   |   |   |   |         | p |   |   |   |                         |
|     |   |   |   |   |   |         | t |   |   |   |                         |
|     |   |   |   |   |   |         | y |   |   |   |                         |
|     |   |   |   |   |   |         | o |   |   |   |                         |
|     |   |   |   |   |   |         | r |   |   |   |                         |
|     |   |   |   |   |   |         | 1 |   |   |   |                         |
|     |   |   |   |   |   |         | ) |   |   |   |                         |
|     |   |   |   |   |   |         | , |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         | Y |   |   |   |                         |
|     |   |   |   |   |   |         | v |   |   |   |                         |
|     |   |   |   |   |   |         | a |   |   |   |                         |
|     |   |   |   |   |   |         | l |   |   |   |                         |
|     |   |   |   |   |   |         | u |   |   |   |                         |
|     |   |   |   |   |   |         | e |   |   |   |                         |
|     |   |   |   |   |   |         | , |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         | S |   |   |   |                         |
|     |   |   |   |   |   |         | t |   |   |   |                         |
|     |   |   |   |   |   |         | d |   |   |   |                         |
|     |   |   |   |   |   |         | u |   |   |   |                         |
|     |   |   |   |   |   |         | n |   |   |   |                         |
|     |   |   |   |   |   |         | c |   |   |   |                         |
|     |   |   |   |   |   |         | e |   |   |   |                         |
|     |   |   |   |   |   |         | r |   |   |   |                         |
|     |   |   |   |   |   |         | t |   |   |   |                         |
|     |   |   |   |   |   |         | a |   |   |   |                         |
|     |   |   |   |   |   |         | i |   |   |   |                         |
|     |   |   |   |   |   |         | n |   |   |   |                         |
|     |   |   |   |   |   |         | t |   |   |   |                         |
|     |   |   |   |   |   |         | y |   |   |   |                         |
|     |   |   |   |   |   |         | o |   |   |   |                         |
|     |   |   |   |   |   |         | f |   |   |   |                         |
|     |   |   |   |   |   |         | Y |   |   |   |                         |
|     |   |   |   |   |   |         | v |   |   |   |                         |
|     |   |   |   |   |   |         | a |   |   |   |                         |
|     |   |   |   |   |   |         | l |   |   |   |                         |
|     |   |   |   |   |   |         | u |   |   |   |                         |
|     |   |   |   |   |   |         | e |   |   |   |                         |
|     |   |   |   |   |   |         | ( |   |   |   |                         |
|     |   |   |   |   |   |         | o |   |   |   |                         |
|     |   |   |   |   |   |         | r |   |   |   |                         |
|     |   |   |   |   |   |         | e |   |   |   |                         |
|     |   |   |   |   |   |         | m |   |   |   |                         |
|     |   |   |   |   |   |         | p |   |   |   |                         |
|     |   |   |   |   |   |         | t |   |   |   |                         |
|     |   |   |   |   |   |         | y |   |   |   |                         |
|     |   |   |   |   |   |         | o |   |   |   |                         |
|     |   |   |   |   |   |         | r |   |   |   |                         |
|     |   |   |   |   |   |         | 1 |   |   |   |                         |
|     |   |   |   |   |   |         | ) |   |   |   |                         |
|     |   |   |   |   |   |         |   |   |   |   |                         |
|     |   |   |   |   |   |         | ( |   |   |   |                         |
|     |   |   |   |   |   |         | 1 |   |   |   |                         |
|     |   |   |   |   |   |         | m |   |   |   |                         |
|     |   |   |   |   |   |         | e |   |   |   |                         |
|     |   |   |   |   |   |         | a |   |   |   |                         |
|     |   |   |   |   |   |         | n |   |   |   |                         |
|     |   |   |   |   |   |         | s |   |   |   |                         |
|     |   |   |   |   |   |         | i |   |   |   |                         |
|     |   |   |   |   |   |         | d |   |   |   |                         |
|     |   |   |   |   |   |         | e |   |   |   |                         |
|     |   |   |   |   |   |         | n |   |   |   |                         |
|     |   |   |   |   |   |         | t |   |   |   |                         |
|     |   |   |   |   |   |         | i |   |   |   |                         |
|     |   |   |   |   |   |         | c |   |   |   |                         |
|     |   |   |   |   |   |         | a |   |   |   |                         |
|     |   |   |   |   |   |         | l |   |   |   |                         |
|     |   |   |   |   |   |         | w |   |   |   |                         |
|     |   |   |   |   |   |         | e |   |   |   |                         |
|     |   |   |   |   |   |         | i |   |   |   |                         |
|     |   |   |   |   |   |         | g |   |   |   |                         |
|     |   |   |   |   |   |         | h |   |   |   |                         |
|     |   |   |   |   |   |         | t |   |   |   |                         |
|     |   |   |   |   |   |         | i |   |   |   |                         |
|     |   |   |   |   |   |         | n |   |   |   |                         |
|     |   |   |   |   |   |         | g |   |   |   |                         |
|     |   |   |   |   |   |         | ) |   |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| **  |   |   |   |   |   |         |   |   |   |   |                         |
| @So |   |   |   |   |   |         |   |   |   |   |                         |
| nst |   |   |   |   |   |         |   |   |   |   |                         |
| ige |   |   |   |   |   |         |   |   |   |   |                         |
| :** |   |   |   |   |   |         |   |   |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
|     |   |   | Q |   |   |         |   | Q |   |   |                         |
| kal |   |   | u |   |   |         |   | u |   |   |                         |
| pha |   |   | a |   |   |         |   | a |   |   |                         |
| =3. |   |   | n |   |   |         |   | n |   |   |                         |
| 000 |   |   | t |   |   |         |   | t |   |   |                         |
| 000 |   |   | i |   |   |         |   | i |   |   |                         |
|     |   |   | l |   |   |         |   | l |   |   |                         |
|     |   |   | z |   |   |         |   | e |   |   |                         |
|  kb |   |   | u |   |   |         |   | a |   |   |                         |
| eta |   |   | m |   |   |         |   | s |   |   |                         |
| =1. |   |   | F |   |   |         |   | s |   |   |                         |
| 644 |   |   | e |   |   |         |   | o |   |   |                         |
| 854 |   |   | h |   |   |         |   | c |   |   |                         |
|     |   |   | l |   |   |         |   | i |   |   |                         |
|     |   |   | e |   |   |         |   | a |   |   |                         |
| cov |   |   | r |   |   |         |   | t |   |   |                         |
| erf |   |   | 1 |   |   |         |   | e |   |   |                         |
| =1. |   |   | . |   |   |         |   | d |   |   |                         |
| 000 |   |   | A |   |   |         |   | w |   |   |                         |
|     |   |   | r |   |   |         |   | i |   |   |                         |
|     |   |   | t |   |   |         |   | t |   |   |                         |
|  co |   |   | α |   |   |         |   | h |   |   |                         |
| ver |   |   | ; |   |   |         |   | t |   |   |                         |
| in= |   |   |   |   |   |         |   | y |   |   |                         |
| 1.0 |   |   | Q |   |   |         |   | p |   |   |                         |
|     |   |   | u |   |   |         |   | e |   |   |                         |
|     |   |   | a |   |   |         |   | I |   |   |                         |
|  1- |   |   | n |   |   |         |   | e |   |   |                         |
| gam |   |   | t |   |   |         |   | r |   |   |                         |
| ma= |   |   | i |   |   |         |   | r |   |   |                         |
| 0.9 |   |   | l |   |   |         |   | o |   |   |                         |
| 500 |   |   | z |   |   |         |   | r |   |   |                         |
|     |   |   | u |   |   |         |   | , |   |   |                         |
|     |   |   | m |   |   |         |   | α |   |   |                         |
|  Ga |   |   | F |   |   |         |   | ; |   |   |                         |
| mDi |   |   | e |   |   |         |   |   |   |   |                         |
| stA |   |   | h |   |   |         |   | Q |   |   |                         |
| dd= |   |   | l |   |   |         |   | u |   |   |                         |
| 1.0 |   |   | e |   |   |         |   | a |   |   |                         |
|     |   |   | r |   |   |         |   | n |   |   |                         |
|     |   |   | 2 |   |   |         |   | t |   |   |                         |
|   M |   |   | . |   |   |         |   | i |   |   |                         |
| ode |   |   | A |   |   |         |   | l |   |   |                         |
| lTy |   |   | r |   |   |         |   | e |   |   |                         |
| pe= |   |   | t |   |   |         |   | a |   |   |                         |
| Pos |   |   | β |   |   |         |   | s |   |   |                         |
| Lin |   |   | ; |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   | E |   |   |         |   | c |   |   |                         |
|  Bi |   |   | r |   |   |         |   | i |   |   |                         |
| nPo |   |   | w |   |   |         |   | a |   |   |                         |
| i=8 |   |   | e |   |   |         |   | t |   |   |                         |
|     |   |   | i |   |   |         |   | e |   |   |                         |
|  10 |   |   | t |   |   |         |   | d |   |   |                         |
|     |   |   | e |   |   |         |   | w |   |   |                         |
|  12 |   |   | r |   |   |         |   | i |   |   |                         |
|     |   |   | u |   |   |         |   | t |   |   |                         |
|   9 |   |   | n |   |   |         |   | h |   |   |                         |
|     |   |   | g |   |   |         |   | t |   |   |                         |
| **N |   |   | s |   |   |         |   | y |   |   |                         |
| ote |   |   | f |   |   |         |   | p |   |   |                         |
| :** |   |   | a |   |   |         |   | e |   |   |                         |
| NWG |   |   | k |   |   |         |   | I |   |   |                         |
| Typ |   |   | t |   |   |         |   | I |   |   |                         |
| is  |   |   | o |   |   |         |   | e |   |   |                         |
| no  |   |   | r |   |   |         |   | r |   |   |                         |
|     |   |   | f |   |   |         |   | r |   |   |                         |
|     |   |   | ü |   |   |         |   | o |   |   |                         |
| lon |   |   | r |   |   |         |   | r |   |   |                         |
| ger |   |   | d |   |   |         |   | , |   |   |                         |
|     |   |   | i |   |   |         |   | β |   |   |                         |
|   u |   |   | e |   |   |         |   | ; |   |   |                         |
| sed |   |   | E |   |   |         |   |   |   |   |                         |
|     |   |   | r |   |   |         |   | c |   |   |                         |
|     |   |   | g |   |   |         |   | o |   |   |                         |
|     |   |   | e |   |   |         |   | v |   |   |                         |
|     |   |   | b |   |   |         |   | e |   |   |                         |
|     |   |   | n |   |   |         |   | r |   |   |                         |
|     |   |   | i |   |   |         |   | a |   |   |                         |
|     |   |   | s |   |   |         |   | g |   |   |                         |
|     |   |   | u |   |   |         |   | e |   |   |                         |
|     |   |   | n |   |   |         |   | f |   |   |                         |
|     |   |   | s |   |   |         |   | a |   |   |                         |
|     |   |   | i |   |   |         |   | c |   |   |                         |
|     |   |   | c |   |   |         |   | t |   |   |                         |
|     |   |   | h |   |   |         |   | o |   |   |                         |
|     |   |   | e |   |   |         |   | r |   |   |                         |
|     |   |   | r |   |   |         |   | f |   |   |                         |
|     |   |   | h |   |   |         |   | o |   |   |                         |
|     |   |   | e |   |   |         |   | r |   |   |                         |
|     |   |   | i |   |   |         |   | u |   |   |                         |
|     |   |   | t |   |   |         |   | n |   |   |                         |
|     |   |   | ; |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   | E |   |   |         |   | r |   |   |                         |
|     |   |   | r |   |   |         |   | t |   |   |                         |
|     |   |   | w |   |   |         |   | a |   |   |                         |
|     |   |   | e |   |   |         |   | i |   |   |                         |
|     |   |   | i |   |   |         |   | n |   |   |                         |
|     |   |   | t |   |   |         |   | t |   |   |                         |
|     |   |   | e |   |   |         |   | y |   |   |                         |
|     |   |   | r |   |   |         |   | o |   |   |                         |
|     |   |   | u |   |   |         |   | f |   |   |                         |
|     |   |   | n |   |   |         |   | t |   |   |                         |
|     |   |   | g |   |   |         |   | h |   |   |                         |
|     |   |   | s |   |   |         |   | e |   |   |                         |
|     |   |   | f |   |   |         |   | o |   |   |                         |
|     |   |   | a |   |   |         |   | u |   |   |                         |
|     |   |   | k |   |   |         |   | t |   |   |                         |
|     |   |   | t |   |   |         |   | p |   |   |                         |
|     |   |   | o |   |   |         |   | u |   |   |                         |
|     |   |   | r |   |   |         |   | t |   |   |                         |
|     |   |   | f |   |   |         |   | q |   |   |                         |
|     |   |   | ü |   |   |         |   | u |   |   |                         |
|     |   |   | r |   |   |         |   | a |   |   |                         |
|     |   |   | d |   |   |         |   | n |   |   |                         |
|     |   |   | i |   |   |         |   | t |   |   |                         |
|     |   |   | e |   |   |         |   | i |   |   |                         |
|     |   |   | E |   |   |         |   | t |   |   |                         |
|     |   |   | i |   |   |         |   | y |   |   |                         |
|     |   |   | n |   |   |         |   | ; |   |   |                         |
|     |   |   | g |   |   |         |   |   |   |   |                         |
|     |   |   | a |   |   |         |   | c |   |   |                         |
|     |   |   | b |   |   |         |   | o |   |   |                         |
|     |   |   | e |   |   |         |   | v |   |   |                         |
|     |   |   | u |   |   |         |   | e |   |   |                         |
|     |   |   | n |   |   |         |   | r |   |   |                         |
|     |   |   | s |   |   |         |   | a |   |   |                         |
|     |   |   | i |   |   |         |   | g |   |   |                         |
|     |   |   | c |   |   |         |   | e |   |   |                         |
|     |   |   | h |   |   |         |   | f |   |   |                         |
|     |   |   | e |   |   |         |   | a |   |   |                         |
|     |   |   | r |   |   |         |   | c |   |   |                         |
|     |   |   | h |   |   |         |   | t |   |   |                         |
|     |   |   | e |   |   |         |   | o |   |   |                         |
|     |   |   | i |   |   |         |   | r |   |   |                         |
|     |   |   | t |   |   |         |   | f |   |   |                         |
|     |   |   | u |   |   |         |   | o |   |   |                         |
|     |   |   | n |   |   |         |   | r |   |   |                         |
|     |   |   | a |   |   |         |   | u |   |   |                         |
|     |   |   | b |   |   |         |   | n |   |   |                         |
|     |   |   | h |   |   |         |   | c |   |   |                         |
|     |   |   | . |   |   |         |   | e |   |   |                         |
|     |   |   | G |   |   |         |   | r |   |   |                         |
|     |   |   | r |   |   |         |   | t |   |   |                         |
|     |   |   | ö |   |   |         |   | a |   |   |                         |
|     |   |   | ß |   |   |         |   | i |   |   |                         |
|     |   |   | e |   |   |         |   | n |   |   |                         |
|     |   |   | n |   |   |         |   | t |   |   |                         |
|     |   |   | ; |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   | W |   |   |         |   | s |   |   |                         |
|     |   |   | a |   |   |         |   | o |   |   |                         |
|     |   |   | h |   |   |         |   | f |   |   |                         |
|     |   |   | r |   |   |         |   | t |   |   |                         |
|     |   |   | s |   |   |         |   | h |   |   |                         |
|     |   |   | c |   |   |         |   | e |   |   |                         |
|     |   |   | h |   |   |         |   | i |   |   |                         |
|     |   |   | e |   |   |         |   | n |   |   |                         |
|     |   |   | i |   |   |         |   | d |   |   |                         |
|     |   |   | n |   |   |         |   | e |   |   |                         |
|     |   |   | l |   |   |         |   | p |   |   |                         |
|     |   |   | i |   |   |         |   | e |   |   |                         |
|     |   |   | c |   |   |         |   | n |   |   |                         |
|     |   |   | h |   |   |         |   | d |   |   |                         |
|     |   |   | k |   |   |         |   | e |   |   |                         |
|     |   |   | e |   |   |         |   | n |   |   |                         |
|     |   |   | i |   |   |         |   | t |   |   |                         |
|     |   |   | t |   |   |         |   | i |   |   |                         |
|     |   |   | z |   |   |         |   | n |   |   |                         |
|     |   |   | u |   |   |         |   | p |   |   |                         |
|     |   |   | m |   |   |         |   | u |   |   |                         |
|     |   |   | V |   |   |         |   | t |   |   |                         |
|     |   |   | e |   |   |         |   | q |   |   |                         |
|     |   |   | r |   |   |         |   | u |   |   |                         |
|     |   |   | t |   |   |         |   | a |   |   |                         |
|     |   |   | r |   |   |         |   | n |   |   |                         |
|     |   |   | a |   |   |         |   | t |   |   |                         |
|     |   |   | u |   |   |         |   | i |   |   |                         |
|     |   |   | e |   |   |         |   | t |   |   |                         |
|     |   |   | n |   |   |         |   | i |   |   |                         |
|     |   |   | s |   |   |         |   | e |   |   |                         |
|     |   |   | i |   |   |         |   | s |   |   |                         |
|     |   |   | n |   |   |         |   | ; |   |   |                         |
|     |   |   | t |   |   |         |   |   |   |   |                         |
|     |   |   | e |   |   |         |   | p |   |   |                         |
|     |   |   | r |   |   |         |   | r |   |   |                         |
|     |   |   | v |   |   |         |   | o |   |   |                         |
|     |   |   | a |   |   |         |   | b |   |   |                         |
|     |   |   | l |   |   |         |   | a |   |   |                         |
|     |   |   | l |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   | P |   |   |         |   | l |   |   |                         |
|     |   |   | a |   |   |         |   | i |   |   |                         |
|     |   |   | r |   |   |         |   | t |   |   |                         |
|     |   |   | a |   |   |         |   | y |   |   |                         |
|     |   |   | m |   |   |         |   | a |   |   |                         |
|     |   |   | e |   |   |         |   | s |   |   |                         |
|     |   |   | t |   |   |         |   | s |   |   |                         |
|     |   |   | e |   |   |         |   | o |   |   |                         |
|     |   |   | r |   |   |         |   | c |   |   |                         |
|     |   |   | f |   |   |         |   | i |   |   |                         |
|     |   |   | ü |   |   |         |   | a |   |   |                         |
|     |   |   | r |   |   |         |   | t |   |   |                         |
|     |   |   | G |   |   |         |   | e |   |   |                         |
|     |   |   | a |   |   |         |   | d |   |   |                         |
|     |   |   | m |   |   |         |   | w |   |   |                         |
|     |   |   | m |   |   |         |   | i |   |   |                         |
|     |   |   | a |   |   |         |   | t |   |   |                         |
|     |   |   | v |   |   |         |   | h |   |   |                         |
|     |   |   | e |   |   |         |   | t |   |   |                         |
|     |   |   | r |   |   |         |   | h |   |   |                         |
|     |   |   | t |   |   |         |   | e |   |   |                         |
|     |   |   | e |   |   |         |   | c |   |   |                         |
|     |   |   | i |   |   |         |   | o |   |   |                         |
|     |   |   | l |   |   |         |   | n |   |   |                         |
|     |   |   | u |   |   |         |   | f |   |   |                         |
|     |   |   | n |   |   |         |   | i |   |   |                         |
|     |   |   | g |   |   |         |   | d |   |   |                         |
|     |   |   | ; |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   | M |   |   |         |   | c |   |   |                         |
|     |   |   | o |   |   |         |   | e |   |   |                         |
|     |   |   | d |   |   |         |   | i |   |   |                         |
|     |   |   | e |   |   |         |   | n |   |   |                         |
|     |   |   | l |   |   |         |   | t |   |   |                         |
|     |   |   | l |   |   |         |   | e |   |   |                         |
|     |   |   | T |   |   |         |   | r |   |   |                         |
|     |   |   | y |   |   |         |   | v |   |   |                         |
|     |   |   | p |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   | ( |   |   |         |   |   |   |   |                         |
|     |   |   | o |   |   |         |   | P |   |   |                         |
|     |   |   | n |   |   |         |   | a |   |   |                         |
|     |   |   | e |   |   |         |   | r |   |   |                         |
|     |   |   | o |   |   |         |   | a |   |   |                         |
|     |   |   | f |   |   |         |   | m |   |   |                         |
|     |   |   | : |   |   |         |   | e |   |   |                         |
|     |   |   | P |   |   |         |   | t |   |   |                         |
|     |   |   | o |   |   |         |   | e |   |   |                         |
|     |   |   | s |   |   |         |   | r |   |   |                         |
|     |   |   | L |   |   |         |   | f |   |   |                         |
|     |   |   | i |   |   |         |   | o |   |   |                         |
|     |   |   | n |   |   |         |   | r |   |   |                         |
|     |   |   | / |   |   |         |   | G |   |   |                         |
|     |   |   | G |   |   |         |   | a |   |   |                         |
|     |   |   | U |   |   |         |   | m |   |   |                         |
|     |   |   | M |   |   |         |   | m |   |   |                         |
|     |   |   | o |   |   |         |   | a |   |   |                         |
|     |   |   | n |   |   |         |   | d |   |   |                         |
|     |   |   | l |   |   |         |   | i |   |   |                         |
|     |   |   | y |   |   |         |   | s |   |   |                         |
|     |   |   | / |   |   |         |   | t |   |   |                         |
|     |   |   | N |   |   |         |   | r |   |   |                         |
|     |   |   | e |   |   |         |   | i |   |   |                         |
|     |   |   | g |   |   |         |   | b |   |   |                         |
|     |   |   | L |   |   |         |   | u |   |   |                         |
|     |   |   | i |   |   |         |   | t |   |   |                         |
|     |   |   | n |   |   |         |   | i |   |   |                         |
|     |   |   | ) |   |   |         |   | o |   |   |                         |
|     |   |   | ; |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | ; |   |   |                         |
|     |   |   | B |   |   |         |   |   |   |   |                         |
|     |   |   | i |   |   |         |   | T |   |   |                         |
|     |   |   | n |   |   |         |   | y |   |   |                         |
|     |   |   | o |   |   |         |   | p |   |   |                         |
|     |   |   | m |   |   |         |   | e |   |   |                         |
|     |   |   | / |   |   |         |   | o |   |   |                         |
|     |   |   | P |   |   |         |   | f |   |   |                         |
|     |   |   | o |   |   |         |   | m |   |   |                         |
|     |   |   | i |   |   |         |   | o |   |   |                         |
|     |   |   | s |   |   |         |   | d |   |   |                         |
|     |   |   | s |   |   |         |   | e |   |   |                         |
|     |   |   | o |   |   |         |   | l |   |   |                         |
|     |   |   | n |   |   |         |   | ( |   |   |                         |
|     |   |   | - |   |   |         |   | o |   |   |                         |
|     |   |   | F |   |   |         |   | n |   |   |                         |
|     |   |   | a |   |   |         |   | e |   |   |                         |
|     |   |   | l |   |   |         |   | o |   |   |                         |
|     |   |   | l |   |   |         |   | f |   |   |                         |
|     |   |   | : |   |   |         |   | P |   |   |                         |
|     |   |   | S |   |   |         |   | o |   |   |                         |
|     |   |   | y |   |   |         |   | s |   |   |                         |
|     |   |   | m |   |   |         |   | L |   |   |                         |
|     |   |   | b |   |   |         |   | i |   |   |                         |
|     |   |   | o |   |   |         |   | n |   |   |                         |
|     |   |   | l |   |   |         |   | / |   |   |                         |
|     |   |   | - |   |   |         |   | G |   |   |                         |
|     |   |   | N |   |   |         |   | U |   |   |                         |
|     |   |   | u |   |   |         |   | M |   |   |                         |
|     |   |   | m |   |   |         |   | o |   |   |                         |
|     |   |   | m |   |   |         |   | n |   |   |                         |
|     |   |   | e |   |   |         |   | l |   |   |                         |
|     |   |   | r |   |   |         |   | y |   |   |                         |
|     |   |   | n |   |   |         |   | / |   |   |                         |
|     |   |   | v |   |   |         |   | N |   |   |                         |
|     |   |   | o |   |   |         |   | e |   |   |                         |
|     |   |   | n |   |   |         |   | g |   |   |                         |
|     |   |   | p |   |   |         |   | L |   |   |                         |
|     |   |   | , |   |   |         |   | i |   |   |                         |
|     |   |   | R |   |   |         |   | n |   |   |                         |
|     |   |   | 0 |   |   |         |   | ) |   |   |                         |
|     |   |   | , |   |   |         |   | ; |   |   |                         |
|     |   |   | t |   |   |         |   |   |   |   |                         |
|     |   |   | m |   |   |         |   | B |   |   |                         |
|     |   |   | , |   |   |         |   | i |   |   |                         |
|     |   |   | l |   |   |         |   | n |   |   |                         |
|     |   |   | a |   |   |         |   | o |   |   |                         |
|     |   |   | m |   |   |         |   | m |   |   |                         |
|     |   |   | b |   |   |         |   | / |   |   |                         |
|     |   |   | d |   |   |         |   | P |   |   |                         |
|     |   |   | a |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | i |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | c |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | : |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | y |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | n |   |   |                         |
|     |   |   |   |   |   |         |   | u |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | e |   |   |                         |
|     |   |   |   |   |   |         |   | r |   |   |                         |
|     |   |   |   |   |   |         |   | s |   |   |                         |
|     |   |   |   |   |   |         |   | o |   |   |                         |
|     |   |   |   |   |   |         |   | f |   |   |                         |
|     |   |   |   |   |   |         |   | p |   |   |                         |
|     |   |   |   |   |   |         |   | , |   |   |                         |
|     |   |   |   |   |   |         |   | R |   |   |                         |
|     |   |   |   |   |   |         |   | 0 |   |   |                         |
|     |   |   |   |   |   |         |   | , |   |   |                         |
|     |   |   |   |   |   |         |   | t |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | , |   |   |                         |
|     |   |   |   |   |   |         |   | l |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
|     |   |   |   |   |   |         |   | m |   |   |                         |
|     |   |   |   |   |   |         |   | b |   |   |                         |
|     |   |   |   |   |   |         |   | d |   |   |                         |
|     |   |   |   |   |   |         |   | a |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| **  |   |   |   |   |   |         |   |   |   |   |                         |
| @Me |   |   |   |   |   |         |   |   |   |   |                         |
| ans |   |   |   |   |   |         |   |   |   |   |                         |
| :** |   |   |   |   |   |         |   |   |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+
| me  |   |   |   |   | e |         |   |   |   | a |                         |
| ant |   |   |   |   | i |         |   |   |   | s |                         |
| yp= |   |   |   |   | n |         |   |   |   | e |                         |
| 2 2 |   |   |   |   | e |         |   |   |   | r |                         |
|     |   |   |   |   | F |         |   |   |   | i |                         |
| re  |   |   |   |   | o |         |   |   |   | e |                         |
| fme |   |   |   |   | l |         |   |   |   | s |                         |
| an= |   |   |   |   | g |         |   |   |   | o |                         |
| 2   |   |   |   |   | e |         |   |   |   | f |                         |
| (>  |   |   |   |   | v |         |   |   |   | n |                         |
| 0)  |   |   |   |   | o |         |   |   |   | t |                         |
|     |   |   |   |   | n |         |   |   |   | y |                         |
| g   |   |   |   |   | n |         |   |   |   | p |                         |
| efo |   |   |   |   | M |         |   |   |   | e |                         |
| lgt |   |   |   |   | i |         |   |   |   | s |                         |
| von |   |   |   |   | t |         |   |   |   | o |                         |
| /   |   |   |   |   | t |         |   |   |   | f |                         |
| fo  |   |   |   |   | e |         |   |   |   | m |                         |
| llo |   |   |   |   | l |         |   |   |   | e |                         |
| wed |   |   |   |   | w |         |   |   |   | a |                         |
| by: |   |   |   |   | e |         |   |   |   | n |                         |
|     |   |   |   |   | r |         |   |   |   | s |                         |
| n   |   |   |   |   | t |         |   |   |   | ( |                         |
| Zei |   |   |   |   | - |         |   |   |   | i |                         |
| len |   |   |   |   | T |         |   |   |   | n |                         |
| der |   |   |   |   | y |         |   |   |   | t |                         |
| A   |   |   |   |   | p |         |   |   |   | e |                         |
| rt: |   |   |   |   | e |         |   |   |   | g |                         |
| / n |   |   |   |   | n |         |   |   |   | e |                         |
| li  |   |   |   |   | ( |         |   |   |   | r |                         |
| nes |   |   |   |   | i |         |   |   |   | , |                         |
| li  |   |   |   |   | n |         |   |   |   | v |                         |
| ke: |   |   |   |   | t |         |   |   |   | a |                         |
|     |   |   |   |   | e |         |   |   |   | l |                         |
| „   |   |   |   |   | g |         |   |   |   | u |                         |
| nam |   |   |   |   | e |         |   |   |   | e |                         |
| e_d |   |   |   |   | r |         |   |   |   | s |                         |
| ata |   |   |   |   | , |         |   |   |   | 1 |                         |
| 1.5 |   |   |   |   | W |         |   |   |   | , |                         |
| 2.0 |   |   |   |   | e |         |   |   |   | 2 |                         |
| 2.  |   |   |   |   | r |         |   |   |   | o |                         |
| 2…" |   |   |   |   | t |         |   |   |   | r |                         |
|     |   |   |   |   | e |         |   |   |   | 3 |                         |
|     |   |   |   |   | 1 |         |   |   |   | ) |                         |
|     |   |   |   |   | , |         |   |   |   | o |                         |
|     |   |   |   |   | 2 |         |   |   |   | f |                         |
|     |   |   |   |   | o |         |   |   |   | t |                         |
|     |   |   |   |   | d |         |   |   |   | h |                         |
|     |   |   |   |   | e |         |   |   |   | e |                         |
|     |   |   |   |   | r |         |   |   |   | n |                         |
|     |   |   |   |   | 3 |         |   |   |   | m |                         |
|     |   |   |   |   | ) |         |   |   |   | e |                         |
|     |   |   |   |   | d |         |   |   |   | a |                         |
|     |   |   |   |   | e |         |   |   |   | n |                         |
|     |   |   |   |   | r |         |   |   |   | v |                         |
|     |   |   |   |   | n |         |   |   |   | a |                         |
|     |   |   |   |   | M |         |   |   |   | r |                         |
|     |   |   |   |   | i |         |   |   |   | i |                         |
|     |   |   |   |   | t |         |   |   |   | a |                         |
|     |   |   |   |   | t |         |   |   |   | b |                         |
|     |   |   |   |   | e |         |   |   |   | l |                         |
|     |   |   |   |   | l |         |   |   |   | e |                         |
|     |   |   |   |   | w |         |   |   |   | s |                         |
|     |   |   |   |   | e |         |   |   |   |   |                         |
|     |   |   |   |   | r |         |   |   |   | M |                         |
|     |   |   |   |   | t |         |   |   |   | e |                         |
|     |   |   |   |   | - |         |   |   |   | a |                         |
|     |   |   |   |   | V |         |   |   |   | n |                         |
|     |   |   |   |   | a |         |   |   |   | i |                         |
|     |   |   |   |   | r |         |   |   |   | n |                         |
|     |   |   |   |   | i |         |   |   |   | g |                         |
|     |   |   |   |   | a |         |   |   |   | o |                         |
|     |   |   |   |   | b |         |   |   |   | f |                         |
|     |   |   |   |   | l |         |   |   |   | m |                         |
|     |   |   |   |   | e |         |   |   |   | e |                         |
|     |   |   |   |   | n |         |   |   |   | a |                         |
|     |   |   |   |   |   |         |   |   |   | n |                         |
|     |   |   |   |   | B |         |   |   |   | t |                         |
|     |   |   |   |   | e |         |   |   |   | y |                         |
|     |   |   |   |   | d |         |   |   |   | p |                         |
|     |   |   |   |   | e |         |   |   |   | n |                         |
|     |   |   |   |   | u |         |   |   |   | u |                         |
|     |   |   |   |   | t |         |   |   |   | m |                         |
|     |   |   |   |   | u |         |   |   |   | b |                         |
|     |   |   |   |   | n |         |   |   |   | e |                         |
|     |   |   |   |   | g |         |   |   |   | r |                         |
|     |   |   |   |   | d |         |   |   |   | : |                         |
|     |   |   |   |   | e |         |   |   |   |   |                         |
|     |   |   |   |   | r |         |   |   |   | ( |                         |
|     |   |   |   |   | m |         |   |   |   | " |                         |
|     |   |   |   |   | e |         |   |   |   | u |                         |
|     |   |   |   |   | a |         |   |   |   | n |                         |
|     |   |   |   |   | n |         |   |   |   | k |                         |
|     |   |   |   |   | t |         |   |   |   | n |                         |
|     |   |   |   |   | y |         |   |   |   | o |                         |
|     |   |   |   |   | p |         |   |   |   | w |                         |
|     |   |   |   |   | - |         |   |   |   | n |                         |
|     |   |   |   |   | N |         |   |   |   | r |                         |
|     |   |   |   |   | u |         |   |   |   | a |                         |
|     |   |   |   |   | m |         |   |   |   | n |                         |
|     |   |   |   |   | m |         |   |   |   | d |                         |
|     |   |   |   |   | e |         |   |   |   | o |                         |
|     |   |   |   |   | r |         |   |   |   | m |                         |
|     |   |   |   |   | : |         |   |   |   | i |                         |
|     |   |   |   |   |   |         |   |   |   | n |                         |
|     |   |   |   |   | ( |         |   |   |   | f |                         |
|     |   |   |   |   | „ |         |   |   |   | l |                         |
|     |   |   |   |   | u |         |   |   |   | u |                         |
|     |   |   |   |   | n |         |   |   |   | e |                         |
|     |   |   |   |   | b |         |   |   |   | n |                         |
|     |   |   |   |   | e |         |   |   |   | c |                         |
|     |   |   |   |   | k |         |   |   |   | e |                         |
|     |   |   |   |   | a |         |   |   |   | s |                         |
|     |   |   |   |   | n |         |   |   |   | " |                         |
|     |   |   |   |   | n |         |   |   |   | ) |                         |
|     |   |   |   |   | t |         |   |   |   |   |                         |
|     |   |   |   |   | e |         |   |   |   | 1 |                         |
|     |   |   |   |   | z |         |   |   |   | : |                         |
|     |   |   |   |   | u |         |   |   |   | s |                         |
|     |   |   |   |   | f |         |   |   |   | e |                         |
|     |   |   |   |   | ä |         |   |   |   | e |                         |
|     |   |   |   |   | l |         |   |   |   | E |                         |
|     |   |   |   |   | l |         |   |   |   | q |                         |
|     |   |   |   |   | i |         |   |   |   | . |                         |
|     |   |   |   |   | g |         |   |   |   | ( |                         |
|     |   |   |   |   | e |         |   |   |   | 1 |                         |
|     |   |   |   |   | E |         |   |   |   | ) |                         |
|     |   |   |   |   | i |         |   |   |   | i |                         |
|     |   |   |   |   | n |         |   |   |   | n |                         |
|     |   |   |   |   | f |         |   |   |   | 6 |                         |
|     |   |   |   |   | l |         |   |   |   | . |                         |
|     |   |   |   |   | ü |         |   |   |   | 9 |                         |
|     |   |   |   |   | s |         |   |   |   | . |                         |
|     |   |   |   |   | s |         |   |   |   | 1 |                         |
|     |   |   |   |   | e |         |   |   |   |   |                         |
|     |   |   |   |   | " |         |   |   |   | 2 |                         |
|     |   |   |   |   | ) |         |   |   |   | : |                         |
|     |   |   |   |   |   |         |   |   |   | s |                         |
|     |   |   |   |   | 1 |         |   |   |   | e |                         |
|     |   |   |   |   | : |         |   |   |   | e |                         |
|     |   |   |   |   | s |         |   |   |   | E |                         |
|     |   |   |   |   | i |         |   |   |   | q |                         |
|     |   |   |   |   | e |         |   |   |   | . |                         |
|     |   |   |   |   | h |         |   |   |   | ( |                         |
|     |   |   |   |   | e |         |   |   |   | 3 |                         |
|     |   |   |   |   | G |         |   |   |   | ) |                         |
|     |   |   |   |   | l |         |   |   |   | i |                         |
|     |   |   |   |   | . |         |   |   |   | n |                         |
|     |   |   |   |   | ( |         |   |   |   | 6 |                         |
|     |   |   |   |   | 1 |         |   |   |   | . |                         |
|     |   |   |   |   | ) |         |   |   |   | 9 |                         |
|     |   |   |   |   | i |         |   |   |   | . |                         |
|     |   |   |   |   | n |         |   |   |   | 1 |                         |
|     |   |   |   |   | 6 |         |   |   |   |   |                         |
|     |   |   |   |   | . |         |   |   |   | 3 |                         |
|     |   |   |   |   | 9 |         |   |   |   | : |                         |
|     |   |   |   |   | . |         |   |   |   | s |                         |
|     |   |   |   |   | 1 |         |   |   |   | e |                         |
|     |   |   |   |   |   |         |   |   |   | e |                         |
|     |   |   |   |   | 2 |         |   |   |   | E |                         |
|     |   |   |   |   | : |         |   |   |   | q |                         |
|     |   |   |   |   | s |         |   |   |   | . |                         |
|     |   |   |   |   | i |         |   |   |   | ( |                         |
|     |   |   |   |   | e |         |   |   |   | 5 |                         |
|     |   |   |   |   | h |         |   |   |   | ) |                         |
|     |   |   |   |   | e |         |   |   |   | i |                         |
|     |   |   |   |   | G |         |   |   |   | n |                         |
|     |   |   |   |   | l |         |   |   |   | 6 |                         |
|     |   |   |   |   | . |         |   |   |   | . |                         |
|     |   |   |   |   | ( |         |   |   |   | 9 |                         |
|     |   |   |   |   | 3 |         |   |   |   | . |                         |
|     |   |   |   |   | ) |         |   |   |   | 1 |                         |
|     |   |   |   |   | i |         |   |   |   |   |                         |
|     |   |   |   |   | n |         |   |   |   | d |                         |
|     |   |   |   |   | 6 |         |   |   |   | a |                         |
|     |   |   |   |   | . |         |   |   |   | t |                         |
|     |   |   |   |   | 9 |         |   |   |   | a |                         |
|     |   |   |   |   | . |         |   |   |   | r |                         |
|     |   |   |   |   | 1 |         |   |   |   | e |                         |
|     |   |   |   |   |   |         |   |   |   | c |                         |
|     |   |   |   |   | 3 |         |   |   |   | o |                         |
|     |   |   |   |   | : |         |   |   |   | r |                         |
|     |   |   |   |   | s |         |   |   |   | d |                         |
|     |   |   |   |   | i |         |   |   |   | s |                         |
|     |   |   |   |   | e |         |   |   |   | e |                         |
|     |   |   |   |   | h |         |   |   |   | l |                         |
|     |   |   |   |   | e |         |   |   |   | e |                         |
|     |   |   |   |   | G |         |   |   |   | c |                         |
|     |   |   |   |   | l |         |   |   |   | t |                         |
|     |   |   |   |   | . |         |   |   |   | e |                         |
|     |   |   |   |   | ( |         |   |   |   | d |                         |
|     |   |   |   |   | 5 |         |   |   |   | f |                         |
|     |   |   |   |   | ) |         |   |   |   | o |                         |
|     |   |   |   |   | i |         |   |   |   | r |                         |
|     |   |   |   |   | n |         |   |   |   | r |                         |
|     |   |   |   |   | 6 |         |   |   |   | e |                         |
|     |   |   |   |   | . |         |   |   |   | f |                         |
|     |   |   |   |   | 9 |         |   |   |   | e |                         |
|     |   |   |   |   | . |         |   |   |   | r |                         |
|     |   |   |   |   | 1 |         |   |   |   | e |                         |
|     |   |   |   |   |   |         |   |   |   | n |                         |
|     |   |   |   |   | f |         |   |   |   | c |                         |
|     |   |   |   |   | ü |         |   |   |   | e |                         |
|     |   |   |   |   | r |         |   |   |   | p |                         |
|     |   |   |   |   | R |         |   |   |   | u |                         |
|     |   |   |   |   | e |         |   |   |   | r |                         |
|     |   |   |   |   | f |         |   |   |   | p |                         |
|     |   |   |   |   | e |         |   |   |   | o |                         |
|     |   |   |   |   | r |         |   |   |   | s |                         |
|     |   |   |   |   | e |         |   |   |   | e |                         |
|     |   |   |   |   | n |         |   |   |   | o |                         |
|     |   |   |   |   | z |         |   |   |   | f |                         |
|     |   |   |   |   | - |         |   |   |   |   |                         |
|     |   |   |   |   | Z |         |   |   |   | " |                         |
|     |   |   |   |   | w |         |   |   |   | k |                         |
|     |   |   |   |   | e |         |   |   |   | n |                         |
|     |   |   |   |   | c |         |   |   |   | o |                         |
|     |   |   |   |   | k |         |   |   |   | w |                         |
|     |   |   |   |   | s |         |   |   |   | n |                         |
|     |   |   |   |   | e |         |   |   |   | i |                         |
|     |   |   |   |   | l |         |   |   |   | n |                         |
|     |   |   |   |   | e |         |   |   |   | f |                         |
|     |   |   |   |   | k |         |   |   |   | l |                         |
|     |   |   |   |   | t |         |   |   |   | u |                         |
|     |   |   |   |   | i |         |   |   |   | e |                         |
|     |   |   |   |   | e |         |   |   |   | n |                         |
|     |   |   |   |   | r |         |   |   |   | c |                         |
|     |   |   |   |   | t |         |   |   |   | e |                         |
|     |   |   |   |   | e |         |   |   |   | s |                         |
|     |   |   |   |   | r |         |   |   |   | " |                         |
|     |   |   |   |   | D |         |   |   |   | : |                         |
|     |   |   |   |   | a |         |   |   |   | s |                         |
|     |   |   |   |   | t |         |   |   |   | e |                         |
|     |   |   |   |   | e |         |   |   |   | e |                         |
|     |   |   |   |   | n |         |   |   |   | E |                         |
|     |   |   |   |   | s |         |   |   |   | q |                         |
|     |   |   |   |   | a |         |   |   |   | s |                         |
|     |   |   |   |   | t |         |   |   |   | . |                         |
|     |   |   |   |   | z |         |   |   |   | ( |                         |
|     |   |   |   |   |   |         |   |   |   | 6 |                         |
|     |   |   |   |   | „ |         |   |   |   | ) |                         |
|     |   |   |   |   | b |         |   |   |   | a |                         |
|     |   |   |   |   | e |         |   |   |   | n |                         |
|     |   |   |   |   | k |         |   |   |   | d |                         |
|     |   |   |   |   | a |         |   |   |   | ( |                         |
|     |   |   |   |   | n |         |   |   |   | 7 |                         |
|     |   |   |   |   | n |         |   |   |   | ) |                         |
|     |   |   |   |   | t |         |   |   |   | i |                         |
|     |   |   |   |   | e |         |   |   |   | n |                         |
|     |   |   |   |   | E |         |   |   |   | 6 |                         |
|     |   |   |   |   | i |         |   |   |   | . |                         |
|     |   |   |   |   | n |         |   |   |   | 9 |                         |
|     |   |   |   |   | f |         |   |   |   | . |                         |
|     |   |   |   |   | l |         |   |   |   | 1 |                         |
|     |   |   |   |   | ü |         |   |   |   |   |                         |
|     |   |   |   |   | s |         |   |   |   | a |                         |
|     |   |   |   |   | s |         |   |   |   | v |                         |
|     |   |   |   |   | e |         |   |   |   | a |                         |
|     |   |   |   |   | " |         |   |   |   | r |                         |
|     |   |   |   |   | : |         |   |   |   | i |                         |
|     |   |   |   |   | s |         |   |   |   | a |                         |
|     |   |   |   |   | i |         |   |   |   | b |                         |
|     |   |   |   |   | e |         |   |   |   | l |                         |
|     |   |   |   |   | h |         |   |   |   | e |                         |
|     |   |   |   |   | e |         |   |   |   | - |                         |
|     |   |   |   |   | G |         |   |   |   | i |                         |
|     |   |   |   |   | l |         |   |   |   | d |                         |
|     |   |   |   |   | . |         |   |   |   | e |                         |
|     |   |   |   |   | ( |         |   |   |   | n |                         |
|     |   |   |   |   | 6 |         |   |   |   | t |                         |
|     |   |   |   |   | ) |         |   |   |   | i |                         |
|     |   |   |   |   | u |         |   |   |   | f |                         |
|     |   |   |   |   | . |         |   |   |   | i |                         |
|     |   |   |   |   | ( |         |   |   |   | e |                         |
|     |   |   |   |   | 7 |         |   |   |   | r |                         |
|     |   |   |   |   | ) |         |   |   |   | , |                         |
|     |   |   |   |   |   |         |   |   |   | f |                         |
|     |   |   |   |   | i |         |   |   |   | o |                         |
|     |   |   |   |   | n |         |   |   |   | l |                         |
|     |   |   |   |   | 6 |         |   |   |   | l |                         |
|     |   |   |   |   | . |         |   |   |   | o |                         |
|     |   |   |   |   | 9 |         |   |   |   | w |                         |
|     |   |   |   |   | . |         |   |   |   | e |                         |
|     |   |   |   |   | 1 |         |   |   |   | d |                         |
|     |   |   |   |   |   |         |   |   |   | b |                         |
|     |   |   |   |   | E |         |   |   |   | y |                         |
|     |   |   |   |   | i |         |   |   |   | t |                         |
|     |   |   |   |   | n |         |   |   |   | h |                         |
|     |   |   |   |   | V |         |   |   |   | e |                         |
|     |   |   |   |   | a |         |   |   |   | s |                         |
|     |   |   |   |   | r |         |   |   |   | i |                         |
|     |   |   |   |   | i |         |   |   |   | n |                         |
|     |   |   |   |   | a |         |   |   |   | g |                         |
|     |   |   |   |   | b |         |   |   |   | l |                         |
|     |   |   |   |   | l |         |   |   |   | e |                         |
|     |   |   |   |   | e |         |   |   |   | v |                         |
|     |   |   |   |   | n |         |   |   |   | a |                         |
|     |   |   |   |   | - |         |   |   |   | l |                         |
|     |   |   |   |   | B |         |   |   |   | u |                         |
|     |   |   |   |   | e |         |   |   |   | e |                         |
|     |   |   |   |   | z |         |   |   |   | s |                         |
|     |   |   |   |   | e |         |   |   |   | o |                         |
|     |   |   |   |   | i |         |   |   |   | f |                         |
|     |   |   |   |   | c |         |   |   |   | t |                         |
|     |   |   |   |   | h |         |   |   |   | h |                         |
|     |   |   |   |   | n |         |   |   |   | e |                         |
|     |   |   |   |   | e |         |   |   |   | v |                         |
|     |   |   |   |   | r |         |   |   |   | a |                         |
|     |   |   |   |   | , |         |   |   |   | r |                         |
|     |   |   |   |   | g |         |   |   |   | i |                         |
|     |   |   |   |   | e |         |   |   |   | a |                         |
|     |   |   |   |   | f |         |   |   |   | b |                         |
|     |   |   |   |   | o |         |   |   |   | l |                         |
|     |   |   |   |   | l |         |   |   |   | e |                         |
|     |   |   |   |   | g |         |   |   |   | w |                         |
|     |   |   |   |   | t |         |   |   |   | i |                         |
|     |   |   |   |   | v |         |   |   |   | t |                         |
|     |   |   |   |   | o |         |   |   |   | h |                         |
|     |   |   |   |   | n |         |   |   |   | t |                         |
|     |   |   |   |   | d |         |   |   |   | h |                         |
|     |   |   |   |   | e |         |   |   |   | e |                         |
|     |   |   |   |   | n |         |   |   |   | n |                         |
|     |   |   |   |   | E |         |   |   |   | a |                         |
|     |   |   |   |   | i |         |   |   |   | m |                         |
|     |   |   |   |   | n |         |   |   |   | e |                         |
|     |   |   |   |   | z |         |   |   |   | " |                         |
|     |   |   |   |   | e |         |   |   |   | n |                         |
|     |   |   |   |   | l |         |   |   |   | a |                         |
|     |   |   |   |   | w |         |   |   |   | m |                         |
|     |   |   |   |   | e |         |   |   |   | e |                         |
|     |   |   |   |   | r |         |   |   |   | " |                         |
|     |   |   |   |   | t |         |   |   |   | o |                         |
|     |   |   |   |   | e |         |   |   |   | f |                         |
|     |   |   |   |   | n |         |   |   |   | t |                         |
|     |   |   |   |   | d |         |   |   |   | h |                         |
|     |   |   |   |   | e |         |   |   |   | e |                         |
|     |   |   |   |   | r |         |   |   |   | s |                         |
|     |   |   |   |   | V |         |   |   |   | y |                         |
|     |   |   |   |   | a |         |   |   |   | m |                         |
|     |   |   |   |   | r |         |   |   |   | b |                         |
|     |   |   |   |   | i |         |   |   |   | o |                         |
|     |   |   |   |   | a |         |   |   |   | l |                         |
|     |   |   |   |   | b |         |   |   |   | l |                         |
|     |   |   |   |   | l |         |   |   |   | i |                         |
|     |   |   |   |   | e |         |   |   |   | s |                         |
|     |   |   |   |   | n |         |   |   |   | t |                         |
|     |   |   |   |   | m |         |   |   |   |   |                         |
|     |   |   |   |   | i |         |   |   |   |   |                         |
|     |   |   |   |   | t |         |   |   |   |   |                         |
|     |   |   |   |   | d |         |   |   |   |   |                         |
|     |   |   |   |   | e |         |   |   |   |   |                         |
|     |   |   |   |   | m |         |   |   |   |   |                         |
|     |   |   |   |   | N |         |   |   |   |   |                         |
|     |   |   |   |   | a |         |   |   |   |   |                         |
|     |   |   |   |   | m |         |   |   |   |   |                         |
|     |   |   |   |   | e |         |   |   |   |   |                         |
|     |   |   |   |   | n |         |   |   |   |   |                         |
|     |   |   |   |   | „ |         |   |   |   |   |                         |
|     |   |   |   |   | n |         |   |   |   |   |                         |
|     |   |   |   |   | a |         |   |   |   |   |                         |
|     |   |   |   |   | m |         |   |   |   |   |                         |
|     |   |   |   |   | e |         |   |   |   |   |                         |
|     |   |   |   |   | " |         |   |   |   |   |                         |
|     |   |   |   |   | a |         |   |   |   |   |                         |
|     |   |   |   |   | u |         |   |   |   |   |                         |
|     |   |   |   |   | s |         |   |   |   |   |                         |
|     |   |   |   |   | d |         |   |   |   |   |                         |
|     |   |   |   |   | e |         |   |   |   |   |                         |
|     |   |   |   |   | r |         |   |   |   |   |                         |
|     |   |   |   |   | S |         |   |   |   |   |                         |
|     |   |   |   |   | y |         |   |   |   |   |                         |
|     |   |   |   |   | m |         |   |   |   |   |                         |
|     |   |   |   |   | b |         |   |   |   |   |                         |
|     |   |   |   |   | o |         |   |   |   |   |                         |
|     |   |   |   |   | l |         |   |   |   |   |                         |
|     |   |   |   |   | l |         |   |   |   |   |                         |
|     |   |   |   |   | i |         |   |   |   |   |                         |
|     |   |   |   |   | s |         |   |   |   |   |                         |
|     |   |   |   |   | t |         |   |   |   |   |                         |
|     |   |   |   |   | e |         |   |   |   |   |                         |
+-----+---+---+---+---+---+---------+---+---+---+---+-------------------------+

Project file in the Excel-compatible \*.CSV format
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The structure of the CSV format is similarly to that of the \*.TXP
format. It is also included in the preceding table describing the
structure of the txp format. The following figure shows an example.
**@Sonstige** from the txp format is found under **Optionen**.

:mark:`**NOTE**: If one loads a TXP project file into UR and saves it in
CSV format, one should not edit this CSV file with Excel and saves it
again from within Excel. The reason is that Excel saves this CSV file by
modifying the real numbers in it by reducing their numbers of decimals,
which may lead then to slightly different results when loading this file
again into UR. Such a manual editing of the CSV file should better be
done by using a simple text editor such as Notepad.`

As the automated usage of UR can be done via an Excel application, it
could be confirmed that this problem does not occur when using the
export to CSV from within Excel via an VBA code.

|image18|

|image19|

Notes about the input of input quantity uncertainties
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is to be noted that uncertainties of input quantities can be entered
in different fields. These are given in the following table, which
corresponds to the TAB "Values, uncertainties".

Using the \*.TXP format of the project file, these fields are found
under the section @Unc-Grid: described under the 6\ :sup:`th` to the
9\ :sup:`th` bullet; in the CSV format these are the columns 5 – 8 (E –
H) under the section "Values, uncertainties".

An uncertainty values must not be entered in column 10; dependent on the
distribution types, it has to be entered in one of those columns given
as the columns 6 to 9 in the following table.

+---+----------+-------+-----------------------------------------------+
| 6 | **StdDev | text  | formula of the standard deviation of the      |
| : | f        | field | quantity; no formula if (N+x) rule has been   |
|   | ormula** |       | selected;                                     |
|   |          |       | (the internal coverage factor is always 1);   |
|   |          |       | always "." has to used for the decimal point  |
+===+==========+=======+===============================================+
| 7 | **StdDev | n     | value of the uncertainty for normal           |
| : | value**  | umber | distribution;                                 |
|   |          | field |                                               |
|   |          |       | | if the (N+x) rule has been selected,        |
|   |          |       |   **nothing** shall be entered in these       |
|   |          |       |   cells!                                      |
|   |          |       | | (the internal coverage factor is always 1)  |
+---+----------+-------+-----------------------------------------------+
| 8 | **Half   | n     | Half width of rectangular/triangular          |
| : | width**  | umber | distribution                                  |
|   |          | field | (the internal coverage factor is always 1)    |
+---+----------+-------+-----------------------------------------------+
| 9 | **abs    | se    | select whether the uncertainty from col. Sp.  |
| : | ./rel.** | lect. | 6, 7 or 8                                     |
|   |          | field |                                               |
|   |          |       | are to be taken *absolute* or as *relative*   |
|   |          |       | value.                                        |
+---+----------+-------+-----------------------------------------------+
| 1 | **abs.   | n     | (combined) absolute standard uncertainty      |
| 0 | st       | umber | calculated by the program form the values of  |
| : | d.Unc.** | field | the columns 6, 7, 8 and 9;                    |
|   |          |       |                                               |
|   |          |       | Note: a value entered by the user will always |
|   |          |       | be over-written by the program!               |
+---+----------+-------+-----------------------------------------------+

If values are entered in columns 6 to 8 with a coverage factor unequal
to 1, the parameter coverin within the same project file must be set
equal to the coverage factor just applied. UncertRadio then converts all
input uncertainties to the internal coverage factor 1; after the
calculations, UncertRadio multiplies the output uncertainties with the
value of the parameter coverf defined in the project file.

Font and colors
---------------

Usually, so-called CSS file („cascaded style sheets", often rather
complex) are applied for adjusting character fonts and colors. For UR
this is reduced to a shortened file **Settings.ini** (belonging to
GTK+3). The latter is configured for Windows and contains only two
entries.

File **Settings.ini**:

   [Settings]

   gtk-theme-name = win64

   gtk-font-name = Sans Normal 10

Note that the strings to the left of the = characters must not be
modified. On starting the UR program the Settings\*.ini file is loaded.
This file shall be part of the UncertRadio.exe path.

For modifying values for *gtk-font-name* the **Fontname Icon** of the
toolbar can be used.

With the **fontname icon** |image20| the font type and/or the font size
can be modified easily. It has to be considered that by enlarging the
font size the program’s window also increases.

With the dialog button "\ **Apply**\ " a selected fontname is applied to
the UR window. If the font is considered acceptable, it can be saved in
the file Settings.ini with the dialog button "\ **Save**\ "; this
however requires that this fontname had been applied once to the program
window. The new fontname will not be saved if this dialog is instead
closed by the button "\ **Quit**\ ".

*Note: at present, the color button cannot yet be applied.* The **Color
icon** |image21| allows choosing the colors of the *permanent dialog
background* or the *selected-background* (the latter indicating that a
dialog object got the focus). The color selection itself is initiated by
clicking the larger rectangular dialog area containing the color button.
In a further dialog a color may be selected from a color palette. By
clicking the + sign another dialog opens supporting the mouse-driven
color selection. At the top of this dialog a text field is shown, e.g.
with a string like "#E7E3BA", which displays the hexadecimal
representation of the selected RGB color; in a field to the left of this
entry, the currently selected color is displayed. Such a color code can
also be entered directly into the text field, which enables a fine-tune
"search" for the color. The hexadecimal code is the one, which is saved
with "\ **Save**\ " in one of the Settings.ini files. The application of
the dialog buttons "\ **Save**\ ", "\ **Apply**\ " und "\ **Quit**\ " is
the same as for the fontname dialog.

Graphics window
---------------

Graphical representations are now displayed in a window containing three
Tabs for applications:

-  Monte Carlo-Simulation (TAB MC),

-  an application not yet „given free" (TAB MCMC) and

-  a graphic related to linear unfolding displaying measured values,
   standard uncertainties and the fitting curve (TAB LFIT).

The associated graphical representations are saved as PNG files:

-  MCplotfile.png

-  -

-  CurvePlot.png

The button "Copy" allows to save the graphic as a file in a format which
has been selected with the combobox, where the user is asked for the
filename.

This window can be closed with the button "Close" at the end of the
window.

The graphic associated with the TAB LFit can be invoked by the icon
|image22| in the toolbar.

|image23|

Program testing
---------------

After having installed a new program version, there may be a need for
checking whether the analytical procedures applied by UncertRadio can be
expected to produce evaluation results agreeing with documented results
(reference values). Since version 2.3.03 this is possible as described
now. An already longer existing (internal) test routine had been used to
run the many projects, as given in section 3.3, in a batch processing
mode for comparing the results with those documented earlier. This
routine, previously with no access by the user, has been extended such
that it can be invoked now under the **Menu Options – QC batch test**.
It tests the analytical procedures for about 106 projects, which takes a
time of about 70 seconds.

This test requires the reference data file

BatListRef-v02.txt

which is now part of the UncertRadio setup procedure. The dialog invoked
via **Menu Options – QC batch test** allows to select the reference data
file and an output file showing the comparison of actually calculated
and of reference values, project by project.

|image24|

After a run-time of about 70 seconds an information is given about the
number of projects for which a disagreement was found. The details of
for these projects are given in the output file. This file normally is
very short, as only those projects are given there for which deviations
were found.