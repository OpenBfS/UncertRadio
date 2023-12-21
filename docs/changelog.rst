Changelog
=========

**UncertRadio - Version 2.4.32 2023/09**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 23.09.2023

**Revision of the Windows-Help:**

-  A new chapter 5.8 was added, which describes the background of the
   test behind the menu item Options – QC-batch-best.

**Addition of example projects:**

-  In the project file iso11929-4_Example-11_EN.txp few input data
   values were corrected; the corrected version is:
   iso11929-4_Example-11_V2_EN.txp.

**Revision of the program:**

-  The graphical layout of the fitted decay curves was improved, also
   with respect to be able to better differentiate between the
   individual curves.

-  With closing the text editor Window related to viewing the fit
   results, the associated decay curves graphics window is now also
   hided.

-  Recently, the dialog for the input of decay curve data was modified
   with respect to its layout. It has again been considered for
   improving the usability of individual dialog fields. The mode of this
   dialog was changed to non-modal which allows again to be able to
   delete table rows being marked with the mouse.

-  .

**Revision of the Excel application:**

-  none

**UncertRadio - Version 2.4.31 2023/09**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 14.09.2023

**Revision of the Windows-Help:**

-  none

**Addition of example projects:**

-  none

**Revision of the program:**

-  A recently introduced error in a callback function of entry fields of
   the GUI lead to the problem that for instance values of the quantiles
   could not be edited within the options dialog. This error was
   removed.

-  Up to now, covariance values given by the user as explicit values or
   as formulas, were not checked for yielding a correlation with an
   absolute value larger than one. This check is now included.

-  Sometimes, although not always understandable, a warning could appear
   that the relative uncertainty of the calibration factor w would
   exceed a limit of 1/kbeta which would prevent to calculate the
   detection limit. The previous estimation of urel(w), while a separate
   equation for w or the internally used factor Flinear did not exist,
   was replaced by a simple uncertainty propagation.

-  .

**Revision of the Excel application:**

-  none

**UncertRadio - Version 2.4.30 2023/08**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 21.08.2023

**Revision of the Windows-Help:**

-  A change within the file UR2_cfg.dat, related to winRelSize, is
   described in section 1.3.

**Addition of example projects:**

-  none

**Revision of the program:**

-  In the case of using linear unfolding and the conditions of having
   only one counting channel and having defined only one output
   quantity, but 2 to three fitting parameters, the plot of the decay
   curve is modified. In addition to the fitted sum of decay functions,
   the individual component curves are also plotted.

-  When using a dataset for calculating and using a mean value of type 3
   („classical“, see section 6.12.1 of the CHM Help), the effect was
   observed with MC simulation that the obtained distributions appeared
   as being too narrow. This was solved by removing a wrong factor of
   :math:`1/\sqrt{m}` .

-  The factor windowRelSize for adjusting the program window size was
   replaced by two such entries, winRelSizeWidth and winRelSizeHeight,
   for width and height, respectively. See the details in UR2_cfg.dat
   given in section 1.3 (Help). In the case that the screen contains
   more than one monitor, an error was removed, which could lead to
   Window being largher than the monitor size.

-  Within the dialog for the input of data for calculating a mean of an
   input quantity, the combobox for selecting a refence data set was
   modified. An “empty” dataset can now be selected, if erroneously a
   reference data set had been selected, which was not one.

-  If a symbol name contained a substring „ln“ or „LN“, the program
   tried to interpret this as an ln() function call. This error was
   removed. Another error was removed, which in MC simulation for a
   project with linear unfolding, with exactly one output quantity, but
   two or three fitting parameters, led to a constant, non-varying value
   of the net count rate.

-  For one single case of limiting the standard deviation formulae to
   not more than100 characters, this limit was removed.

-  With this version(2.4.30) it is allowed for a better reading
   structure, to include also empty equation lines within the textview
   for equations. These empty lines are visible only within the
   textview, while internally for computations, they are omitted. If the
   function Parser indicates a syntax error for an equation n, then,
   compatible with the textview, the empty lines are included in n,
   i.e., therein, empty lines are included in counting the equations.

-  .

**Revision of the Excel application:**

-  none

**UncertRadio - Version 2.4.29 2023/08**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 07.08.2023

**Revision of the Windows-Help:**

-  none

**Addition of example projects:**

-  The project PearsonYork_with_KALFIT_EN.txp (WTLS for a polynomial
   calibration curve) was added.

**Revision of the program:**

-  For a better visibility, a frame was added to a set of further 6
   Entry fields of some dialogs.

-  One specific callback function used by some elements within dialogs
   was modified for preventing an unwanted disappearing of such a
   dialog.

-  Another entry, for the UR function fd() (decay factor formula), was
   added to the file InfoFX1.txt.

-  The package for WTLS was modified for adding a further new option. It
   allows to fit by WTLS a polynomial of a single independent variable X
   to a calibration curve when the values of X are also associated with
   uncertainties. It extends the functionality of the KalFit function.

**Revision of the Excel application:**

-  none

**UncertRadio - Version 2.4.28 2023/07**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 26.07.2023

**Revision of the Windows-Help:**

-  none

**Addition of example projects:**

-  none

**Revision of the program:**

-  Since version 2.4.24, an error has occurred in the model dialog of
   linear unfolding, which has prevented a proper editing of the
   textview for the Xi formulae, because the program left this dialog
   each time after inserting or deleting a single character. This error
   has been removed.

-  The upper part of the same dialog was changed. A new checkbox allows
   to explicitly define, whether time differences are to be derived as
   differences of complete date/time values, or difference values, in
   seconds, are input directly in the table below.

-  In the case of linear unfolding with more than one output quantity,
   the meaning of the cell background colours was also applied to the
   fitting parameters Fitp1, Fitp2 and Fitp3. The white background
   colour is used only for a fitting parameter which is declared as to
   be fixed.

**Revision of the Excel application:**

-  none

**UncertRadio - Version 2.4.27 2023/07**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 17.07.2023

**Revision of the Windows-Help:**

-  Small addition to chapter 7.4.3 (Chi-square options)

**Addition of example projects:**

-  none

**Revision of the program:**

-  For applying linear unfolding with Linfit, the list of argument
   parameters was shortened since version 2.4.24. The program part of
   internally identifying the full list of input quantities used by
   Linfit has been improved, also because an error was observed, if the
   shortened list of arguments had been given.

-  The remaining work on the program can be summarized under “tuning“.
   This refers to the part of linear unfolding and resulted mainly in a
   reduction of processing time. Unnecessary repetitions of
   computational steps were suppressed. A further effort was spent on
   the most time-consuming procedure, the WTLS procedure. Repetitions of
   parts of the output into the file fort66.txt also have been reduced.

-  The processing time required for running the test “QC batch test“ was
   reduced by about 25 %.

**Revision of the Excel application:**

-  none

**UncertRadio - Version 2.4.26 2023/06**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 30.06.2023

**Revision of the Windows-Help:**

-  The chapter 7.4.3 (Chi-square options) was enhanced by results of a
   statistical test procedure; minor additions were added to chapters
   2.6 and 5.2.

**Addition of example projects:**

-  none

**Revision of the program:**

-  In the GUI handling of the dialog for the linear model of evaluation
   an error was observed. After any change of any field in this dialog,
   even if only 1 character were changed, the error was, that the dialog
   was closed immediately, i.e., before clicking the OK or Cancel
   button. The remaining calculations through to the “Results” were
   repeated, as if the update icon had been pressed. This behaviour was
   corrected, the modifications are considered only after leaving the
   dialog via OK or Cancel.

-  For the evaluation of a decay curve, the piece-wise linear fit curve
   displayed in the graphic window has been replaced by a fit curve with
   higher resolution.

-  A small change was applied to the file chooser dialog. It refers to
   the option „save as“. The given filename is transferred into the top
   entry field, with excluding the path name. As before, the desired
   pathname is selected in the lower parts of the dialog. With „save
   as“, both name parts are combined then for saving the file.

-

**Revision of the Excel application:**

-  In the VBA module Modul_auto_single_UR, within the routine
   Init_pathnames, another Excel function is applied for setting the
   PATH variable which is necessary for a correct start o UncertRadio.
   This function is SetEnvironmentA; see also the CHM Help chapter 5.2.

The actual version of the Excel application is

UR2_SingleAutoRun_V11.xlsm

**UncertRadio - Version 2.4.25 2023/06**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 06.06.2023

**Revision of the Windows-Help:**

-  a small addition to chapter 2.3

**Addition of example projects:**

-  J-ALUFT_Sr-89_Sr-90_Linf_EN.txp, which is the linear unfolding
   version of J-ALUFT-Sr89-Sr-90_V2_EN.txp

**Revision of the program:**

-  Within the menu „Edit – Select output quantity“ a GUI error was
   removed, which for displaying the symbol name could remove an
   underscore character within the name of the symbol.

-  Another error, introduced with the last program version, was removed,
   which could incorrectly modify internal symbol-related arrays. This
   could lead to a mistake within the menu item for checking physical
   units.

-  The new example project (see above) was included in the file
   BatListRef_v05.txt.

-  Two new environment variables, XDG_DATA_HOME und XDG_DATA_DIRS, are
   created temporarily by the program, which point to the sub-folder
   GTKuser64\\share.

**Revision of the Excel application:**

none

**UncertRadio - Version 2.4.24 2023/04**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 18.04.2023

**Revision of the Windows-Help:**

-  chapter 3.4, 6.3

**Addition of example projects:**

-  none

**Revision of the program:**

-  The dialog „Options“ was modified and thereby simplified with respect
   to editing quantiles and associated probabilities.

-  For grouping the icons in the toolbar, vertical separation elements
   were inserted.

-  An error was removed, which could occur in establishing a new
   project, if the project was saved too early, before the symbols had
   been extracted from the equations. The only menu item which allowed
   saving too early, was deactivated during the phase of defining the
   equations.

-  The call of the special UncertRadio function Linfit for the treatment
   of a decay curve has been simplified such that the number of
   arguments was reduced to only three parameters/quantity symbols:

Rd = Linfit(1, Rbl, tmess, tstart)

Further argument parameters are no longer required.

**Revision of the Excel application:**

none

**UncertRadio - Version 2.4.23 2023/04**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 02.04.2023

**Revision of the Windows-Help:**

-  none

**Addition of example projects:**

-  none

**Revision of the program:**

-  An error occurring during printout of a matrix by the routine
   MatWrite was removed. For printing only a part of the matrix, the
   true matrix dimensions were changed which resulted in a wrong
   sequence of matrix elements.

-  In the uncertainty propagation with Upropa, a seldom occurring error
   was corrected. The error led to a value of zero for the decision
   threshold. The collection of example projects was not affected by
   this error.

-  A new parameter, windowRelSize, has been introduced in the file
   UR2_cfg.dat, which allows to define the relative size of the UR2
   window in such a way that the widows size remains the same during
   working. This parameter has to be set already in the cfg file,
   because it is implemented into the GUI file before the GtkBuilder
   build the GUI. Setting this option later, into a running GUI, would
   not work.

-  A new icon **f\ (x)** was introduced in the toolbar, which allows to
   get short information, given in the text file InfoFX1.txt, about how
   to apply the special UR2 functions.

**Revision of the Excel application:**

none

**UncertRadio - Version 2.4.22 2023/02**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 01.03.2023

**Revision of the Windows-Help:**

-  The chapters 5.1 and 5.2 have been updated.

**Addition of example projects:**

-  From the existing example projects, the following two have been
   modified with respect to applying units and added with their
   increased versions:

..

   Calibration-of-weight-Cox-2001_V2_EN.txp,
   Neutron-Dose-Cox-2006_V2_EN.txp

**Revision of the program:**

-  For the MC simulation of projects of the type „GUM only, without
   detection limits“ a failure error was removed that two variables
   applied for calculating detection limits were not set to zero.

-  Already for the previous UncertRadio version, a TAB had been removed
   from the graphics dialog. This led to a shift within remaining TABS,
   with the consequence that copying a graphic to the Windows clipboard
   failed. This error was removed.

-  Few further modifications were applied to the option for testing
   physical units. To prevent a crash while loading the associated two
   text files, the working path was added internally to the filename.
   The units for m² and m³ have been removed from UnitsTable.txt because
   they are only powers of m (meter). Applied unit names like m², m2,
   cm², cm2 have been transferred to the file Units_other.txt.

-  Two existing projects were modified with respect to the units used
   therein: their versions were added to the reference data file,
   without changing its filename:

BatListRef_v05.txt

**Revision of the Excel application:**

-  The Excel application has been modified to avoid problems with
   antivirus software. The necessary adaption of specific folders within
   the Excel application has been changed. Now, those folder names are
   taken, which are already given in the configuration file UR2_cfg.dat.
   The batch file UR2_start_xls.bat, being critical for antivirus
   software, is removed completely. Instead, Excel VBA directly modifies
   the Windows PATH variable and starts UncerRadio.exe also directly.
   The necessary changes are addressed in the chapters 5.1 and 5.2 of
   the CHM Help.

The actual version of the Excel application is:

UR2_SingleAutoRun_V10.xlsm

**UncertRadio - Version 2.4.21 2023/02**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 20.02.2023

**Revision of the Windows-Help:**

-  The chapter 6.12.3, dealing with variances under random influences,
   has been extended, in order to be more explicit in describing the
   equations and tests for the procedures A („not counts“) and B
   (“counts, with influence”). These two cases are related to examples
   13 and 14 given in ISO 11929-4.

-  The chapter 7.21 presenting the option for testing the calculation of
   physical units was improved and significantly enlarged.

**Addition of example projects:**

-  A set of 13 example projects have been added, which were prepared for
      the examples considered in ISO 11929-4:2022:

..

   Iso11929-4_Example_6_EN.txp through Iso11929-4_Example_17_EN.txp

-  From the existing example projects, the following ones have been
   modified with respect to applying units and added with their
   increased versions:

..

   Janszen-Sr-89-Sr-90_V4_EN.txp, Moreno-Sr90_IAEA-135_V3_EN.txp,

   sumEval_mean_V3_EN.txp, sumEval_sum_V3_EN.txp

-

**Revision of the program:**

-  For preparing a report, an output error related to the expanded
   uncertainty of the primary value was removed.

-  The graphics output of the MC simulation was improved. The error was
   removed that numbers of the axis labels where slightly cut. The
   min-/max range of the X-axis was slightly enlarged.

-  For better readability, other fonts are now used in the textviews of
   the TAB “Procedure“ and of the TextEditor TAB.

-  Within the dialog for the input of data of a decay curve, a tooltip
   was added to the selection of the time unit for counting times and
   count rates. It clarifies that the time unit underlying the column
   “start date“ must always be the “second“.

-  By occasion, an erroneous input of values led to a program error of
   calculating partial derivatives. This error observed only once has
   been removed.

-  In the case of a MC simulation of t-distributed values for
   calculating a mean under random influence an error occurred if the
   values to be averaged were of the type “not counts”. Instead of
   dividing by a factor sqrt((m-1)/(m-3)), it was divided by the square
   of this factor. This error was removed.

-  The option for testing the calculation of units was improved and its
   application has become safer. This option for the first time could be
   integrated (only as programmers test) in the “QC-Batch-Test“, i.e.,
   into the batch mode processing of the example projects. This helped
   identifying some projects, the deviations or special features of
   which are discussed in the CHM Help file, chapter 7.21.6.

-  A new version was prepared for the file with reference values of the
   example projects:

BatListRef_v05.txt

-

**UncertRadio - Version 2.4.20 2022/08**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 14.08.2022

**Revision of the Windows-Help:**

-  none

**Addition of example projects:**

-  none

**Revision of the program:**

-  With the previous program version, it could happen that graphical
   presentations within the corresponding separate window appeared only
   as black background, or they could appear as being cut at their
   right-hand side. The origin of this behaviour was found in two
   sections of code within GTK-Fortran drawing routines. After adapting
   them appropriately the graphics again are correctly displayed.

-  Another error found by occasion referred to applying Poisson MLE to
   the linear unfolding of a decay curve with low count numbers. For an
   internal output option, a linear fit was included erroneously, which
   disturbed the PMLE fit following it. This program segment was
   deactivated.

-

**UncertRadio - Version 2.4.19 2022/08**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 01.08.2022

**Revision of the Windows-Help:**

-  The following chapters were modified: 3.6.1 (Changes regarding the
   radio buttons in the gamma spectrometry dialog), 7.10, 7.11.1 und
   7.16 (pictures of the dialogs were included). 7.16 describes the
   changes related to these radio buttons.

**Addition of example projects:**

-  none

**Revision of the program:**

-  The usage of the dialog „Values of the spectrum evaluation“ (gamma
   spectrometry) was modified for the input of values. With this program
   version, all values of the measurement quantities are to be given as
   absolute values. Using the radio buttons, only the associated
   uncertainties can be entered as relative (in %) or absolute values.
   For the consequence of the reduction from 7 to 5 radio buttons, refer
   to the chapter 7.16 of the help file.

-

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 26.06.2022

**Revision of the Windows-Help:**

-  None

**Addition of example projects:**

-  none

**Revision of the program:**

-  A programming error was removed which up to now lead to losing the
   very last letter of the last line within the textview field for the
   equations if it were not followed by further blank characters. This
   led to a warning message caused by the last symbol with its missing
   last letter.

-  Within the TAB “Equations“ a new label called “functions“ was
   inserted above the equations’ textview field. By touching it, it
   shows by a tooltip text for information about the names of special
   functions like that for a square root. The program now also allows
   for using the function ln(x) instead of log(x).

**UncertRadio - Version 2.4.17 2022/03**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 31.03.2022

**Revision of the Windows-Help:**

-  None

**Addition of example projects:**

-  none

**Revision of the program:**

-  Since this program version the sub-path „share\\locale\\“ is moved
   from the sub-path GTKUser64 to the UR2 install path. This improves
   the language dependence of buttons.

-  Within a text view for equations of formulae it could happen, that by
   editing the text view “isolated“ LF characters occurred, not being
   accompanied by a preceding carriage return character (CR); this
   corresponds to the Unix format used within GTK, while Windows uses
   CR+LF as end of line. This could cause errors in interpreting the
   corresponding line of text. This problem was solved by replacing each
   “isolated“ LF character by the two characters CR+LF. This treatment
   was applied to all the text views used in UncertRadio including that
   one under the TAB “Procedure”.

“Isoloated“ LF characters were also observed in overall 19 txp project
files (obviously restricted to the DE projects). These LF characters
were replaced in these 19 files as described above. The original
date/time information of the latest modification was then re-attributed
to the modified files by using a windows powershell command.

If there are other project files with this LF problem: Open such a file
with the editor Notepad++, enable it for showing all control characters,
change the end-of-line style to Unix and after that back to the Windows
style (CR+LF), and finally save the project file.

-  During the calculations for linear unfolding a decay curve it could
   happen, that after clicking „calculation of uncertainties” under the
   TAB „Values, uncertainties” for the first time, an uncertainty value
   of the output quantity is shown being by far too large. This could
   also cause a related error message, saying that the detection limit
   could not be calculated. After repeating the calculation (button
   calculation of uncertainties) this error disappeared. The origin of
   the problem was the time correction factor of 60 (minutes seconds),
   which was applied wrongly if the time base of minutes was selected in
   the dialog for decay curve data input. The error was removed.

-  When using WTLS for evaluating a decay curve an error occurs when
   setting the first and third fit parameter to „fit“ and the middle one
   to “omit” or “fix it”. This problem was solved in that way, that the
   systems of equations used in the WTLS routines were reduced to the
   dimension of two (two parameters to be fitted), i.e., completely
   omitting the “middle” fitting parameter (not to be fitted). Note,
   that it is not allowed in that routine to set a fitting parameter to
   “fix it”; this case would now be warned about by a Windows message.

-

**UncertRadio - Version 2.4.16 2022/02**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 22.02.2022

**Revision of the Windows-Help:**

-  None

**Addition of example projects:**

-  none

**Revision of the program:**

-  An error was removed regarding those projects, in which the activity
   is calculated from several gamma peaks of a radionuclide. The error
   was caused by using a wrong index variable in the MC routine.

-  The menu item “LSQ export to R?” meanwhile could no longer be
   activated. The error was removed.

-

**UncertRadio - Version 2.4.15 2022/02**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 08.02.2022

**Revision of the Windows-Help:**

-  None

**Addition of example projects:**

-  none

**Revision of the program:**

-  An error was removed which could end up in a program crash during the
   preparation of a Report output. It was caused by extra-long formula
   text lines, which were divided into more than one line, by which the
   number of text lines became larger than the memory-allocated number
   of lines.

-  The small problem was removed by which parts of the program window
   under the TAB „Results“ appeared to be vertically dislocated.

-  The most recent updates for the Fortran-Compiler and for GTK3 are
   applied now.

-

**UncertRadio - Version 2.4.14 2022/01**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 18.01.2022

**Revision of the Windows-Help:**

-  None

**Addition of example projects:**

-  The text given under the TAB “Procedure” within the example project
      La140_REMSPEC-4Lines-V2_EN.txp was completely outdated. The text
      has been corrected and the project has been renamed to
      **La140_REMSPEC-4Lines-V3_EN.txp**. The reference file
      **BatListRef_v04.txt** was adapted accordingly, its name, however,
      was not changed.

**Revision of the program:**

-  The error existed that after having closed a project, without leaving
   the program, the two textview fields in the TAB “Procedure“ and in
   the TAB “Equations“ were not reset (erased) to their initial state.
   This error has been removed; furthermore, additional variables have
   also been set to their initial states.

-  The program code has been revisited and code of some test routines
   removed which is no longer required. This reduced the size of the
   executable by about 165 kB.

-  The report file obtained by the menu item „Report“ was observed to
   contain only zero lines of the text under the TAB „Procedure“. This
   error was corrected.

-  The exchange of text data with the GTK-textviews (TAB “Procedure”,
   text views for equations) was modified to remove empty lines at the
   end of the texts.

**UncertRadio - Version 2.4.13 2021/09**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 07.09.2021

**Revision of the Windows-Help:**

-  The following new chapters were added to the Help file:

2.2.5 Preventing "hidden" covariances

2.2.6 Using switching variables in equations

2.2.7 Calculation of physical units for dependent variables

3.3.2 Revision of physical units in the examples

7.21 Treatment of physical units

7.21.1 Collection of basic units and derived units

7.21.2 Explaining the calculation of units of dependent quantities

1. Invoking the test of unit calculations

**Addition of example projects:**

-  With the introduction of a test of physical units of dependent
      quantities this was applied to all example projects. See chapter
      `3.3.2 <#revision-of-physical-units-in-the-examples>`__ for the
      changes which were found necessary after this test.

**Revision of the program:**

-  A new menu item was introduced, which allows by **an algorithm to**
   **“calculate” physical units of dependent variables**. Following a
   normal evaluation of a project, this can be used as a test for the
   correctness of units of dependent variables to get hints about
   erroneous units. The chapter
   `2.2.7 <#calculation-of-physical-units-for-dependent-variables>`__
   gives a short introduction, the details are described in chapter
   `7.21 <#treatment-of-physical-units>`__ containing three sections.

The test is based on two new tables about basic and derived physical
units, which are available by two CSV files. The use of these files is
described in chapter
`7.21.1 <#collection-of-basic-units-and-derived-units>`__.

The actual state of this cannot yet be considered as working perfect.
Further work will be required.

-  A new version was prepared for the file with the reference values of
   the example projects:

BatListRef_v04.txt.

**UncertRadio - Version 2.4.12 2021/06**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 18.06.2021

**Revision of the Windows-Help:**

-  In the CHM Help, some aspects have been added to the sections 3.4,
   3.5, 3.63 (new), 4.2 and 4.3, regarding details of how to input
   uncertainties in the case of coverin=2 and how to remove those have
   variables (symbols), which have become dispensable after having
   re-edited the equations.

**Addition of example projects:**

-  None

**Revision of the program:**

-  The problem has been removed that the states of buttons, whether
   being sensitive or not sensitive, since recently had become nearly
   indistinguishably.

**UncertRadio - Version 2.4.11 2021/06**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 10.06.2021

**Revision of the Windows-Help:**

-  none

**Addition of example projects:**

-  None

**Revision of the program:**

-  The program error, by which the parameter coverin (coverage factor
   for input uncertainties) was erroneously written as coverf when
   saving TXP project as CSV file, was corrected.

-  When having introduced the contrast mode it was overlooked that the
   uncertainty budget and the entries used for output under the TAB
   “Results” became editable again. This has been deactivated.

-  Within an MC simulation the treatment of a special case for a count
   number N was lost, by which N=1 should be set internally, if N=0 was
   given by the user. This replacement has been reactivated.

-  For the output of a report (menu Edit), a bug was removed, which
   implied that just the set of equations was suppressed in the report
   file.

**UncertRadio - Version 2.4.10 2021/04**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 16.04.2021

**Revision of the Windows-Help:**

-  none

**Addition of example projects:**

-  None

**Revision of the program:**

-  For the contrast mode option introduced with version 2.4.09 few
   changes with respect to colours were added. The colour layout was
   trimmed such that it now corresponds the one used before 2.4.09, if
   there is no contrast mode entry, or only ContrastMode=F, in the
   configuration file UR2_cfg.dat . A program error regarding a specific
   colour code could induce that more than one cell for the uncertainty
   formula got the colour green. This has been removed.

**UncertRadio - Version 2.4.09 2021/04**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 06.04.2021

**Revision of the Windows-Help:**

-  In the chapter 3.6.1, the description for the item designated by
   @Means: as part of the structure of the txp project file was
   extended. The chapter 6.9.1 being related to this item was also
   extended, so that it becomes clearer how to apply the different type
   of mean values.

-  In the chapter 6.9.2 a description is given for the modified handling
   of input by which the individual values for a mean value have to be
   entered in the corresponding grid.

**Addition of example projects:**

-  None

**Revision of the program:**

-  For the estimation of a mean of a dataset, the handling within the
   dialog “Mean values of input values“ has been extended and is
   therefore safer to use. The input of individual values has been
   improved: After having entered a value the next cell below it is
   selected and already opened for input. For details, see chapter 6.92
   of the CHM Help file.

The maximum number of values was increased from 40 to 200.

-  The possible input of an ESC character has been removed for all
   dialogs of the program. This removes the following fatal error. An
   ESC key, given by the user, maybe by occasion, lead to the problem
   that this dialog was deleted internally by GTK, but without
   terminating the internal GTK loop associated with running this
   dialog. The effect from the continued run of this loop was that the
   program‘s GUI behaviour was severely disturbed and could no longer be
   terminated correctly.

-  The program has been extended in the dialog „Options“ by a new option
   „contrast mode”. By activating the contrast mode, the whole colour
   layout is changed; by using white text on dark background the
   contrast is increased. Combined with an already implemented zooming
   option for the graphics window and the toolbar option for increasing
   the text font size, this is considered as a first contribution to
   barrier-free working. The contrast mode option can be appended to the
   configuration file UR2_cfg.dat by the following entry-line
   ContrastMode=T under [Local] (T: true, F: false).

**UncertRadio - Version 2.4.08 2021/03**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 25.03.2021

**Revision of the Windows-Help:**

-  None

**Addition of example projects:**

-  None

**Revision of the program:**

-  The GUI behind the page in the TAB “Results“ has been reorganized
   internally; its content is the same as before.

-  An option for zooming (in three steps) has been added to the
   graphical window which is invoked when starting a MC-Simulation.

-  Few further program errors were found and removed. One of it, found
   in the context of interpreting the equations, occurred for the first
   time.

**UncertRadio - Version 2.4.07 2021/03**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 11.03.2021

**Revision of the Windows-Help:**

-  None

**Addition of example projects:**

-  None

**Revision of the program:**

-  The details of applying the uncertainty-related coverage factor
   coverf for the output of the expanded uncertainty of the output
   quantity were re-considered. Usually, coverf=1 is applied. If
   coverf=2 (i.e. k=2) is set (Dialog Options), the output quantity
   uncertainties in the TAB “Results“ are given multiplied by coverf.
   This is not new; however, other cases of uncertainty output have been
   adapted, as for instance, in the output files generated by the
   batch-processing options. This factor is also considered also when
   invoking a report output file. Contrarily, with in the TABS “Values,
   uncertainties“ and “Uncertainty budget“, and in the output files
   produced by the menu item “QC batch test”, the uncertainties are not
   multiplied by coverf.

-  Since this version, the new parameter coverin is available declaring
   the coverage factor contained in input values of uncertainty values.
   It is used if uncertainties used as input to UncertRadio contain a
   coverage factor unequal to one. This factor is removed before these
   uncertainties enter the internal calculations within the program.
   Uncertainties entered directly under the TAB “Values, uncertainties“
   must continue to be standard uncertaibties.

-  The values of the parameters coverf and coverin can be edited within
   the dialog “Options“ under “Coverage factor output“ or “Coverage
   factor input“. Default values are 1.0 for both parameters.

-  It is assumed, that coverage factors are not applied to such
   uncertainty input data belonging to, for instance, a decay curve to
   be evaluated by least squares. This refers to count rate
   uncertainties, given in the dialog for the data defining a decay
   curve, to a calibration curve or to input data of the gamma
   spectrometry dialog for evaluating an activity by a weighted mean
   from more than one gamma peaks.

-  If in the TAB “Values, uncertainties“ a first line in the table of
   covariances was entered, with a formula for the covariance but no
   input value, an error occurred. This problem was removed.

**UncertRadio - Version 2.4.06 2021/01**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 31.01.2021

**Revision of the Windows-Help:**

-  None

**Addition of example projects:**

-  None

**Revision of the program:**

-  Few further allocation-related errors were removed, which could lead
   to abortion of the program. An error was corrected, which, after
   deleting rows in the symbol table in the process of re-editing of
   equations, had the effect that behind these rows the association of
   values in the distribution type column and the symbols was incorrect.

-  For the function SumEval warning dialogs have been added, if the
   arguments defined for this function are inconsistent with respect to
   the numbers of symbols (number of defined symbol names inconsistent
   with the second argument).

**UncertRadio - Version 2.4.05 2020/12**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 12.12.2020

**Revision of the Windows-Help:**

-  A paragraph was inserted in chapter 7.13 describing a special
   constraint when working with weighted mean.

**Addition of example projects:**

-  None

**Revision of the program:**

-  The implementation of using allocatable arrays since summer of 2020
   still resulted in problems of insufficient dimensioning within the
   Monte Carlo simulation. The associated routines, especially that for
   preparing arrays and covariance matrices required for generating
   random values for correlated variables, was improved and adjusted. As
   a result, this has been successfully tested by running a batch of,
   for instance, 50 example projects within one UncertRadio run; the
   menu item „Batch evaluation of projects” is well suited for it.
   Within the testing, also some other problems, also related to
   specific examples, could be solved/removed.

-  The method of Brent for searching roots, used for solving iterations
   for calculating the decision threshold and the detection limit, often
   did not converge, especially within MC simualtions. Its application
   requires a search for two starting values which safely encompass the
   solution value. This part could be improved now so that Brents method
   now converges much more reliably.

-  When using a weights mean (UR functions Gamspk1(), SumEval()), the
   uncertainty can be calculated by two methods: a) within these two UR
   functions and b) by that overall uncertainty propagation applied by
   UncertRadio for the output quantities. In a), the inverse of the
   variances of the single activity values are applied as weights. These
   weights must be considered as constants, which means that they are
   calculated once as a function of input parameters and thereafter
   should be kept constant. The latter requirement, however, is not
   fulfilled within UR’s overall uncertainty propagation. Therefore, the
   program was adjusted now to prevent such variations while calculating
   partial derivatives.

This modification refers to the examples (maximum deviations
after/before in %);

La140_REMSPEC-4Lines-V2_EN.txp 0.25 %

Several-peaks-nuclide-activity-V3_EN.txp 0.46 %

sumEval_mean_EN.txp 0.49 %

**Revision of the Excel application:**

-  The actual version of the Excel application is

UR2_SingleAutoRun_V9.xlsm

**UncertRadio - Version 2.4.04 2020/11**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 23.11.2020

**Revision of the Windows-Help:**

-  New chapters 6.13, 7.20 have been added.

-  The new chapter 7.20 describes, how the gamma distributions of count
   rate variables, being necessary for ISO 11929:2019 part 2, are
   generated within UncertRadio, for the case of pre-setting the
   counting duration (working since long) and also for the case of
   pre-setting count numbers (new).

**Addition of example projects:**

-  5 new example projects have been added:

..

   sumEval_summe_EN.txp, sumEval_mitteln_EN.txp,
   Tritium_4Bubbler_used_1-3_EN.txp,

   Tritium_4Bubbler_used_2-3_EN.txp, ImpulsVorwahl_EN.txp

**Revision of the program:**

-  There are cases in which the measurement of an activity requires to
   combine several measurements, maybe of different types of measurement
   or referring to different compartments. The **new function SumEval**
   was implemented in UncertRadio. As an option, the individual
   measurements can be added or averaged. The new Help File chapter 6.13
   describes how to use this function. Two associated example projects
   were added to the program.

-  The treatment of a measurement with pre-setting a count number and
   thereby applying a Gamma distribution to the associated count rate is
   new in UncertRadio. Refer to the new Help File chapter 7.20 for more
   information.

-  The four new example project files were added to the reference data
   file used for the QC batch run, of which now the new version
   BatListRef_v03.txt is applied.

-  The layout of the dialog for the input of individual data used for
   the calculation of mean and variance was a bit enhanced.

-  The output of a Report was improved, because it could happen, that in
   cased of projects with more than one output quantity, the result
   values of these quantities appeared in a wrong sequence. Formatting
   errors for printing interval length values were corrected.

-  For the weighted total least squares analysis, the better fitting
   mnemonic WTLS is used in the program (instead of TLSQ).

-  The newer version 5.15.0 of PLPLOT has been implemented.

-  The parts related to the communication between UncertRadio and the
   Excel application was improved; see also chapter 5.2 of the Help
   file.

-  For the new Tritium bubbler example projects (equations with two
   unknowns), the Linfit function has been used, although this example
   does not represent a decay curve. It had to be modified for including
   the Xi functions by setting the option to define Xi(t) separately for
   each measurement.

**Revision of the Excel application:**

-  The **batch file** UR2_Start_xls.bat existing for starting
   UncertRadio from within Excel is now explicitly applied. Refer to
   section 5.2 and 5.3 for the changes implemented in the VBA code of
   the Excel application. A module RUN_UR_AUTOSEP was introduced, which,
   compared to the VBA module SINGLERUN_UR, runs an UR evaluation
   without intermediately saving the results in the
   AutoReport-Result.csv file.

-  The actual version of the Excel application is

UR2_SingleAutoRun_V9.xlsm

**UncertRadio - Version 2.4.03 2020/09**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 30.09.2020

**Revision of the Windows-Help:**

-  The chapters 5.2 and 5.3 were extended

**Addition of example projects:**

-  none

**Revision of the program:**

-  An error was removed which could occur when querying the monitor
   attributes and which could lead to a program crash.

-  With this version the primary value of the output quantity and its
   associated uncertainty are also calculated for the MC simulation.

-  Newer versions of the Fortran compiler and GTK3 were implemented.
   This initially led to further programming errors, most of them caused
   by incorrect memory allocation of array. This allowed to eliminate
   them, which thereby improved the UR safety at run-time.

-  In the context of the afore-mentioned error removal, the handling of
   editing a project for the special gamma spectrometry tool for
   evaluating the activity from several gamma lines has been updated
   substantially and made safer. Regarding the data exchange between the
   GTK3 GUI and the Fortran code, this tool internally becomes more
   complicated if covariances between peak efficiencies are involved,
   more complicated than in the case of evaluating decay curves.

-  If a project contained a quantity with the special symbol type ‘p‘ (a
   parameter value, without uncertainty), that was overwritten with ‘a‘
   or ‘u‘ when opening the project. This problem was removed.

-  It was observed that the GUI menu “Options – Model type” did no
   longer work correctly: always the first (the standard case) of the
   three model types was indicated as selected. Internally, however, the
   correct model type was used. The GUI related error was removed.

**Revision of the Excel application:**

-  The **batch file** UR2_Start_xls.bat existing for starting
   UncertRadio from within Excel is now explicitly applied. Refer to
   section 5.2 and 5.3 for the changes implemented in the VBA code of
   the Excel application. A module RUN_UR_AUTOSEP was introduced, which,
   compared to the VBA module SINGLERUN_UR, runs an UR evaluation
   without intermediately saving the results in the
   AutoReport-Result.csv file.

-  The actual version of the Excel application is

UR2_SingleAutoRun_V8.xlsm

**UncertRadio - Version 2.4.02 2020/07**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 16.07.2020

**Revision of the Windows-Help:**

-  none

**Addition of example projects:**

-  none

**Revision of the program:**

-  Two errors were removed which could occur within modifying equations
   within the TAB “Equations” leading then to a program crash.

-  An error was removed, which after having edited equations eventually
   could result in the elimination of in input value and/or an
   uncertainty value. Another error could be corrected which, after
   having edited input values/uncertainties in the TAB „Values,
   Uncertainties“, led to the wrong behaviour that the correct values of
   dependent quantities only showed up after pressing the „Calculate… “
   two times.

**UncertRadio - Version 2.4.01 2020/07**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 08.07.2020

**Revision of the Windows-Help:**

-  none

**Addition of example projects:**

-  none

**Revision of the program:**

-  Two errors were removed within a subroutine for running the MC
   simulation:

-  Due to a limited length of an array only up to three simulation runs
   were allowed; a fourth run led to a program crash;

-  In the early January of 2020, a special treatment of a gross count
   rate, to be treated as gamma-distributed, was de-selected. Only by
   now, this turned out to be an error and this treatment re-activated.
   Unfortunately, this error did not show up during the QC batch run and
   was detected only by now. This error occurred since the program
   version 2.3.05.

**UncertRadio - Version 2.4.00 2020/06**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 05.06.2020

**Revision of the Windows-Help:**

-  Section 2.2 was slightly extended and re-structured;

-  A new section 2.3 „Equations as tree topology“ has been added; the
   numbers of the few sections following it are increased;

-  A list of the implemented probability distributions has been added in
   section 6.11;

-  In section 5.1 the reference to the new BATSER alternative to the
   „serial evaluation of a project was given.

**Addition of example projects:**

-  An example project with applying longer symbol names,

..

   Ra226_U235-at-186keV_EN_long.txp, was added, which numerically agrees
   with the existing project Ra226_U235-at-186keV_EN.txp.

-  Few of the equations in the project Sr-89/Sr-90-Projekt of the PTB
      were slightly changed (described in the project file) without
      affecting the results. The new project name is:

..

   Janszen-Sr-89-Sr-90_V2_DE.txp

**Revision of the program:**

-  The Fortran related part of the program has been revised, especially
   by applying character arrays with varying string lengths („of
   deferred shape”); see also section 2.4. This allowed to simplify some
   things, e.g., the transfer from and to the textviews for procedure
   description and the equations. This caused an extensive revision of
   the program, by which probably also some weaknesses of the old code
   could have been removed.

-  It was found that with running the QC batch, some projects existed
   more than once in the reference file BatListRef-v01.txt, while for
   others, with more than one output quantity, only the first ones were
   evaluated. This was corrected, and the old reference file was
   transferred to the new one named BatListRef-v02.txt.

-  An alternative to the batch to be started by the UR2 menu item
   “Serial evaluation of a project“ was added. Thus, that batch is not
   started from within the program, but by a command line instruction.
   For this purpose, the code word AUTO was amended by the code word
   BATSER. The command for the example case given in section 5.6 is:

>uncertradio.exe BATSER J-ALUFT-Sr89-Sr-90_DE.txp J_Aluft_serial_DE.csv

See also section 5.1.

-  After installation of a newer GTK3 version it appeared the some of
   the toolbar icons could no longer be found as usual. The previously
   used icons were replaced by new ones (black colored), the old ones
   are stored under “legacy“. This required a program adaption, by which
   the corresponding legacy icons were moved to the just recently
   inferred UR2 subfolder “icons“ and by modifying the GUI such that
   these icons are now identified by their graphics file name.

-  It was observed meanwhile that the folder “recently opened“ of the
   dialog for opening a file did no longer work (the recent manager). It
   was necessary to adapt the program to some change of the filename
   format used in the Windows file “recently-used.xbel“.

-  A new CHM Help chapter 2.3 “Equations as tree topology“ has been
   written. It describes, how a new Fortran routine tries to identify
   those count rate symbol names, which contribute to the expression of
   the net count rate, from the user-supplied equations and their
   hierarchical structure. When executing the QC batch run, the results
   of it are written in short form to a text file named fort.64. Surely,
   this tool is not yet completely developed.

-  The format of the file UR-Saved-Results.csv, used for saving results,
   as well as that of the AutoReport files used in the Excel application
   (see below) has been slightly changed (column labels were changed;
   two columns for the shortest probability interval were added).
   *Therefore, already existing versions of these files should be
   renamed, before UncertRadio then will create the versions with the
   new format*.

-  The program is now prepared for the evaluation of measurements with
   preset counts instead of preset time by attributing an Erlang
   distribution (that is a gamma distribution for integer numbers of
   counts) to the counting duration variable: Select “Npreset“ as
   distribution type. An example is still outstanding.

-

**Revision of the Excel application:**

-  A **batch file** UR2_Start_xls.bat was introduced for starting
   UncertRadio from within Excel. In its Table6, the corresponding
   sub-folders have been added to project filenames. The format of the
   AutoReport file(s) have been slightly changed. Refer to section 5.1
   and 5.2 for the changes implemented in the VBA code of the Excel
   application.

-  The actual version of the Excel application is

UR2_SingleAutoRun_V7.xlsm

-  Debug.print is now used for control printing within VBA.

**UncertRadio - Version 2.3.08 2020/04**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 06.04.2020

**Revision of the Windows-Help:**

-  a new paragraph “Using several monitors” appended to chapter 1.3

**Addition of example projects:**

-  none

**Revision of the program:**

-  If more than monitor is used at a workstation, it may happen that the
   UncertRadio window is only partly visible or covers more than one
   monitor width. The extensions and adaptions applied for the current
   version 2.3.08 refer to the newly introduced differentiation between
   a “screen“ and several “monitors“, all lying within the screen
   region.

-  The configuration file UR2_cfg.dat has therefore been extended by an
   entry “Monitor#=“.

-  The reader is referred to the new paragraph “Using several monitors“
   at the end of chapter 1.3.

**UncertRadio - Version 2.3.07 2020/02**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 28.02.2020

**Revision of the Windows-Help:**

-  language change extended in chapter 1.3

**Addition of example projects:**

-  none

**Revision of the program:**

-  Previously, a complete translation of dialog items into a different
   language while the program is running was not easy because the “GTK
   stock buttons“ could only be translated at the program start, before
   building the window. As GTK is on the way to give up the option of
   automatic translation of these stock buttons, they have been replaced
   by standard GTK buttons having a visible layout quite similar as
   before. This now allows a nearly complete change of the language via
   the menu Options – Pre-settings from within the program. Se also
   chapter 1.3.

-  With the “stock buttons“ replacement some new icon files are
   installed within the program folder.

-  Up to now, a small file “fort.66“ was created after the program
   start, which by double clicking a project file was written into the
   projects folder. This is eliminated since Version 2.3.07, the
   corresponding file output is now written into an internal array,
   which is later written at the begin of the file “fort66.txt“.

-  The label of the new button in the Tab “Equations“ has been renamed
   to “Change symbol”.

-  Working with two screens increases the number of pixels, e.g., to the
   double of the width. For avoiding conflicts, the maximum values for
   width and height are limited to 1920 and 1080, respectively.

-  A problem was solved by which the program was terminated in the case
   of very long command line argument strings.

-  With an example project applying linear unfolding it was found that
   for the „total least-squares“ method the uncertainty of the output
   quantity was too small compared to the Monte Carlo based result. The
   reason for that was identified as a program error, which became more
   obvious only in the case of a large relative uncertainty of a
   specific detection efficiency, more than 15 % in this example. The
   error has been corrected,

**UncertRadio - Version 2.3.06 2020/02**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 10.02.2020

**Revision of the Windows-Help:**

-  none

**Addition of example projects:**

-  none

**Revision of the program:**

-  Since version 2.3.06 2020/02, in calling the function

Gamspk1(E, tlive)

it is now allowed to use symbol names for the live time other than
tlive. This symbol must necessarily be the second one in the Gamspk1
call.

-  Under the Tab “Equations“, above the equations text field, a button
   “Symbol-Name“ has been added, which has the same function as the menu
   item “Edit – Change symbol name”, but which is more “visible”. Within
   re-editing equations, this shall support to change a symbol name only
   by this button and NOT by changing it directly within the equations.

-  Up to now, it was possible to start UncertRadio more than once. This
   generally may result in errors, especially also when saving the
   projects. This is prohibited now by UncertRadio, which after the
   start checks the Windows task list for a second UncertRadio process
   (instance). If a second instance is found, the program which was
   started later, terminates with a warning message.

-

**UncertRadio - Version 2.3.05 2020/01**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 9.01.2020

**Revision of the Windows-Help:**

-  The new chapters 6.11 “Special distributions and their properties”
   and 6.12 “Gross quantity: Variance interpolation for a mean“ have
   been added, where 6.12 also replaces a part of chapter 4.3.

**Addition of example projects:**

-  With reference to the improvements according to the new chapters 6.11
      and 6.12 new versions of two older project files have been added:

..

   ISO-example-2a_V2_EN.txp and ISO-example-2b_V2_EN.txp

   The old project version ISO-Beispiel-2a_DE.txp is no longer
   considered in the file BatListRef_v01.txt, because its interpolation
   formula did not take the contribution by urel(w) into account (-> 5 %
   deviation).

**Revision of the program:**

-  When using the “Batch evaluation of projects“, the plot files are no
   longer written into the file with projects.

-  In the report output text file the length value of a confidence
   interval was corrected.

-  A formula within the „green cell“ in the table „Values,
   uncertainties“ which appeared to be rather long, enlarged the width
   of the program window significantly, even after another TAB would
   have been selected. This has been changed: the window width has been
   reduced for other TABs.

-  The CHM Help file compilation was changed such that mathematical
   symbols and formulae are no longer too small compared to the text
   size.

-  A Help Button was added to the dialog, by which the parameters of the
   combined Binomial-/Poisson distribution for a long duration of
   counting a short-lived radionuclide (chapter 6.10)-can be edited.

-  In advance of possible applications, also the Beta- and the
   t-distribution have been implemented besides the already existing
   gamma distribution; see the new chapter 6.11.

-  For a gross quantity, the value of which is estimated by a mean value
   of a measurement series, it was necessary to enter a special formula
   into the „green cell“ (uncertainty of the gross quantity) for the
   purpose of interpolating between two known variance values. Since the
   present program version, this is no longer necessary, because the
   corresponding calculations are now completely performed within the
   program. The new CHM Help chapter 6.12 explains how it works.

-  In this context the format of a project file has been extended by the
   entry „refmean=x“ if a **reference** data set is applied.

-

**UncertRadio - Version 2.3.04 2019/11**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 2.12.2019

**Revision of the Windows-Help:**

-  The new chapter 5.7 “Processing project in batch mode“ has been
   added.

**Addition of example projects:**

-  To the following existing projects, a text line “GamDistAdd=1” was
      added (not affecting the known results):

..

   Gamma_dist_de.txp and Gamma_dist_en.txp

**Revision of the program:**

-  A new menu item “\ **Edit – Batch evaluation of projects**\ “ was
   implemented, by which several projects can be processed. It allows to
   include also the MC simulation. A CSV file is obtained which for each
   project and each of the output quantities contains a small table
   block of result values. This is described in the new chapter 5.7.

-  For the menu item „\ **Edit – Serial evaluation**\ “ the missing
   language dependence of the list separator character for CSV files was
   corrected.

-  The handling of the dialog applied for treating values of one or more
   measurement series Messreihe (Toolbar icon |image2|; chapter 6.9.2)
   has been improved. The option of the „classic“ mean value type was
   extended to be applicable also for more than 3 single values.

-  The behavior of positioning the graphics window used for the MC
   simulation was treated in a way, that, if possible, it should appear
   to the right of the main program window. The position of the main
   window was shifted to the left.

-  The modification of the internal function parser to allow applying
   the UncertRadio-internal function uval(x) (see chapter 7.8) within
   user-supplied equations has been extended and improved.

-  For the application of the MC simulation in the case of deriving the
   decision threshold value, the number of iterations, by which the
   deviation of the mean of the associated distribution from zero is
   minimized, is restricted now to 7 iterations. This reduces the
   computing time without noticeable loss of precision.

-  In specific cases, an uncertainty function of the gross quantity is
   required within the “green“ cell in the TAB Values, uncertainties in
   form of an interpolation formula. It may happen then, that this
   formula contains quantities above the green cell, which are expected
   to be calculated after the calculation in the green cell. The program
   has been slightly extended and assures that the values of such
   quantities are available when calculating the green cell formula.

-  According to ISO 11929-1:2019, the internal parameter **GamDistAdd**
   (see dialog of the menu Options – Pre-settings) from now on will be
   initialized with the value 0.0. The corresponding value obtained by
   opening an already existing project file is however maintained.

-  Two older projects exist (Gamma_dist_de.txp, Gamma_dist_en.txp),
   which apply gamma-distributed counts but GamDistAdd was not
   explicitly given in the project file. When reading this file,
   GamDistAdd was set to 1 by the program. Therefore, the results
   documented in the text file BatListRef_v01.txt had been calculated
   with GamDistAdd=1. Since the present program version does no longer
   set a missing value GamDistAdd equal to 1, the line GamDistAdd=1 has
   been added now to the two project files. This preserves the
   consistency of the current program version with the reference file
   BatListRef_v01.txt.

**UncertRadio - Version 2.3.03 2019/10**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 15.10.2019

**Revision of the Windows-Help:**

-  New chapters were added: 3.9 “Program testing” and 6.10 “Measuring a
   short-lived radionuclide with comparably long counting duration”.

**Addition of example projects:**

-  new project referring to chapter 6.10: Ac228_binomial_EN.txp

-  to the following existing projects, a text line
      “ModelType=GUM_restricted” was added (not affecting the known
      results):

..

   Calibration-of-weight-Cox-2001_EN.txp; Neutron-Dose-Cox-2006_EN.txp;

   Kessel-1-2006_EN.txp; Kessel-2a-2006_EN.txp; Kessel-2b-2006_EN.txp;

   Wuebbeler-Ex1_EN.txp; Wuebbeler-Ex2_EN.txp.

**Revision of the program:**

-  For improving quality control, a test is offered now under the Menu
   Options – **QC_Batch-Test**, by which all the example projects being
   installed with UncertRadio are processed (excluding MC simulations).
   The results obtained are compared with reference values taken from a
   text file (part of installation). Deviations occurring are written to
   an output file. See the new chapter 3.9 in the CHM Help.

-  The program was extended to cover the special case that the
   distribution of the gross counts follows a superposition of a
   binomial distribution (sample contribution) and a Poisson
   distribution (background contribution). This case can occur with
   measuring an activity of a short-lived radionuclide, if the product
   :math:`\lambda t_{m}` significantly exceeds 0.1, or even 1.0. See the
   new chapter 6.10 of the CHM Help for more details.

-  The usage of entry fields has been improved.

-

**Revision of the setup program:**

-  **With version 2.3.01 (January 2019) a modified version of the batch
   file UR2_Start.bat is installed, in which the folder GTKuser64\\bin
   now is the first entry of the temporarily modified path variable;
   until recently, this entry had been appended to the path variable.**
   See also Section 1.2.

**This shall prevent the runtime system from loading the necessary DLL
files from different installations/programs.**

-  Since version 2.2.07, with executing the de-installation,
   unins000.exe, the name “UncertRadio.exe“ in the windows registry
   entries for the .txp und .csv links is removed. After a next new
   installation, this shall prevent from calling an old UncertRadio.exe
   file by double-clicking a project file. See also chapter 1.2,
   “Country specific parameters” of the CHM Help.

**UncertRadio - Version 2.3.02 2019/05**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 05.05.2019

**Revision of the Windows-Help:**

-  none

**Addition of example projects:**

-  none, but corrected the file J_Aluft_serial.csv as example for the
      serial evaluation, introduced with this program version, of the
      project file J-ALUFT-Sr89-Sr-90_EN.txp.

**Revision of the program:**

-  With version 2.3.01 an error occurred, that the types of symbols, „a“
   for dependent and „u“ for independent, got wrong values. This bug was
   removed.

-  The layout of tables in the UncertRadio GUI was improved with respect
   to column widths to guarantee the complete visibility of numbers, but
   without extending the columns widths horizontally too much.
   Especially the dialog for input of data of a decay curve got handier
   now. To optimize the column widths is possible by double-clicking the
   small vertical line between two column heads; the mouse pointer
   changes its shape shortly before double-clicking.

-  GTK3+, GTK-Fortran and the Fortran compiler were updated to recent
   versions.

**Revision of the setup program:**

-  **With version 2.3.01 (January 2019) a modified version of the batch
   file UR2_Start.bat is installed, in which the folder GTKuser64\\bin
   now is the first entry of the temporarily modified path variable;
   until recently, this entry had been appended to the path variable.**
   See also Section 1.2.

**This shall prevent the runtime system from loading the necessary DLL
files from different installations/programs.**

-  Since version 2.2.07, with executing the de-installation,
   unins000.exe, the name “UncertRadio.exe“ in the windows registry
   entries for the .txp und .csv links is removed. After a next new
   installation, this shall prevent from calling an old UncertRadio.exe
   file by double-clicking a project file. See also chapter 1.2,
   “Country specific parameters” of the CHM Help.

**UncertRadio - Version 2.3.01 2019/03**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 13.03.2019

**Revision of the Windows-Help:**

-  Section 5.6 has been added.

**Addition of example projects:**

-  none, but the file J_Aluft_serial.csv as example for the serial
      evaluation, introduced with this program version, of the project
      file J-ALUFT-Sr89-Sr-90_EN.txp.

**Revision of the program:**

-  Until recently, the termination of UncertRadio showed the bothersome
   effect, that within a short time a cascade of window-frames flashed
   over the screen. Up to now, this could not be prevented by GTK means.
   However, this effect can be suppressed, if with Code::Blocks the
   UncertRadio.exe is not build as a “GUI application” but as “Console
   application“. That has the only disadvantage that many GTK warning
   messages are written into the console window. Fortran error messages
   would also be written. The best compromise has been found in adding
   the /min option to the startbat file. In short, this means:

>start uncertradio much text may appear in the console window;

>start /min uncertradio no further text appears in the console window.

Further details: see section 1.3

-  The program was adapted to a 64-bit Gfortran compiler; by using
   64-bit versions of the DLL libraries of other software products (see
   section 2.3), it is now running as a 64-bit executable.

NOTE: If at starting the program a Windows message appears, which
indicates the error code (0xc000007b), this means, that Windows found a
32-bit version of a DLL file not related to UncertRadio instead of the
64-bit version. Then, the environment variable PATH should be checked.

-  Up to now, the running program could become large in the main memory.
   This is due to large arrays used by the author within Bayes
   statistics-related test routines, which, however, are not accessible
   by the ISO 11929 user. The memory allocation to theses arrays is now
   disabled. In addition, some array length values have been slightly
   reduced, e.g., those used for symbols, measured values and
   uncertainties.

-  It has been observed that data entry tables in the dialogs for decay
   curves, for gamma-ray spectrometry and for calibration curves showed
   too much horizontal expansions on the screen. This has been corrected
   by applying now an “expected” maximum string length for the per grid
   cell of each table column.

-  In section 6.9 the program change is outlined, that with using data
   sets for estimating mean values, the third option of the type of
   means has been set to „classical“ (i.e. non-Bayesian interpretation),
   what can be useful for comparison purposes.

-  An error was removed which could result in disappearing of single
   cell contents of the columns *unit* or *meaning* when re-editing the
   equations.

-  The menu item “\ **Edit – Serial evaluation**\ “ has been introduced
   allowing the manifold evaluation of a project with partially modified
   input quantity values/uncertainties. The description of this new
   option is given in the new section 5.6 in chapter 5.

-

**Revision of the setup program:**

-  **With version 2.3.01 (January 2019) a modified version of the batch
   file UR2_Start.bat is installed, in which the folder GTKuser64\\bin
   now is the first entry of the temporarily modified path variable;
   until recently, this entry had been appended to the path variable.**
   See also Section 1.2.

**This shall prevent the runtime system from loading the necessary DLL
files from different installations/programs.**

-  Since version 2.2.07, with executing the de-installation,
   unins000.exe, the name “UncertRadio.exe“ in the windows registry
   entries for the .txp und .csv links is removed. After a next new
   installation, this shall prevent from calling an old UncertRadio.exe
   file by double-clicking a project file. See also chapter 1.2,
   “Country specific parameters” of the CHM Help.

**UncertRadio - Version 2.2.11 2018/11**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 18.11.2018

**Revision of the Windows-Help:**

-  3 projects were included in section 3.3.

**Addition of example projects:**

-  Two versions of an UR2 project were added, by which the method of
      total gamma measurements in seawater is treated as it es applied
      by the BSH (Hamburg):

..

   BSH_total-Gamma_var1_DE.txp

   BSH_total-Gamma_var2_DE.txp

**Revision of the program:**

-  The flickering behaviour of Tooltips occurring with the last program
   version could be removed; it could not be removed from the file-open
   dialog, even after having updated GTK3 to version 3.24.1.

-  An error in calling the MATMUL function within MC simulation was
   removed; it occurred if a covariance was to be taken into account.

-  The numerical root searching procedure according to Ridders used for
   iterations was replaced by the even more effective procedure
   according to Brent.

-  The plotting window, initially placed in the lower middle of the
   screen, was shifted to the upper right. For the MC plots of the
   output quantity (# 1 of 3) the vertical scaling was slightly changed;
   its effect becomes more visible only the case of having many MC
   values below zero.

-  The relevant intermediate results of the MC simulations have been
   removed from the file fort66.txt into a separate file named
   MC_Tables.txt.

**Revision of the setup program:**

-  **With version 2.2.11 (November 2018) a modified version of the batch
   file UR2_Start.bat is installed, in which the folder GTKuser3\\bin
   now is the first entry of the temporarily modified path variable;
   until recently, this entry had been appended to the path variable.**
   See also Section 1.2.

**This shall prevent the runtime system from loading the necessary DLL
files from different installations/programs.**

-  Since version 2.2.07, with executing the de-installation,
   unins000.exe, the name “UncertRadio.exe“ in the windows registry
   entries for the .txp und .csv links is removed. After a next new
   installation, this shall prevent from calling an old UncertRadio.exe
   file by double-clicking a project file. See also chapter 1.2,
   “Country specific parameters” of the CHM Help.

**UncertRadio - Version 2.2.10 2018/09**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 18.09.2018

**Revision of the Windows-Help:**

-  none

**Addition of example projects:**

-  none

**Revision of the program:**

-  The problem having occurred in the GTK dialog for selecting a file
   could now be solved by updating to the recent GTK version 3.24.0.
   This prevented from “artificially freezing“ the GTK version at an
   older one.

-  Within the menu “Edit – change of a symbol name“, another programming
   error was removed. It could be encountered when changing a variable
   name Rb to e.g. Rg; if a symbol name Rb0 existed also, that was
   erroneously also renamed, into Rg0.

**Revision of the setup program:**

-  Since version 2.2.07, with executing the de-installation,
   unins000.exe, the name “UncertRadio.exe“ in the windows registry
   entries for the .txp und .csv links is removed. After a next new
   installation, this shall prevent from calling an old UncertRadio.exe
   file by double-clicking a project file. See also chapter 1.2,
   “Country specific parameters” of the CHM Help.

**UncertRadio - Version 2.2.09 2018/09**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 12.09.2018

**Revision of the Windows-Help:**

-  Chapters *2.2 (end), 2.3, 2.5, 3.3, 3.5 (end)* were extended

**Addition of example projects:**

-  new project: dwd_sr89_sr90_TDCR_procedure_EN.txp

**Revision of the program:**

-  The GTK version used within UR2 was changed into the last one,
   because in the latest dialog for opening a file the option was
   missing which allowed selections within the whole file system.

-  The plot window used for the MC simulation was slightly increased.

-  An error was removed, which in the case of a rather large uncertainty
   could result in the effect that decision threshold and detection
   limit were not calculated.

-  During using the menu “Edit – change of a symbol name“ program
   crashes could occur. This error was removed; it was found in a
   routine in which an implicit variable declaration was still allowed,
   so that erroneous Value-Variable associations could occur.

-  In a program call of the type “>uncertradio.exe d:\\ur2\\projekt.txp“
   a crash occurred when the extension “.exe” was omitted. This error
   was removed.

**Revision of the setup program:**

-  Since version 2.2.07, with executing the de-installation,
   unins000.exe, the name “UncertRadio.exe“ in the windows registry
   entries for the .txp und .csv links is removed. After a next new
   installation, this shall prevent from calling an old UncertRadio.exe
   file by double-clicking a project file. See also chapter 1.2,
   “Country specific parameters” of the CHM Help.

**UncertRadio - Version 2.2.08 2018/06**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 25.06.2018

**Revision of the Windows-Help:**

-  Chapter 6.7 was extended

**Addition of example projects:**

-  none

**Revision of the program:**

-  More recent versions of PLPLOT (5.13.0), GTK-Fortran (17.10),
   GFortran (7.3.0) and NuHelp (v2018.04.23) have been implemented.

-  A problem referring to internal string translations (Locale – UTF8)
   was identified. After removing it, this problem obviously was the
   reason for “spontaneous“ program crashes. This significantly
   increased the stability of working with UncertRadio with respect to
   such crashes.

-  The small dialog (LoadProject) which was shown when loading a project
   is no longer used. This reduces the variability of the view of the
   raising GUI.

-  If, under the TAB “Equations“, direct number representations were
   used within equations, the “E“ in e.g. 5.0E-5 was not correctly as
   part of a number interpreted; the error was removed.

-  If, under the “Values, uncertainties“, the “green“ cell for the
   formula of the gross count rate uncertainty belonged to an
   independent input quantity, it was observed that the calculated
   uncertainty values was not copied to the rightmost column. This error
   was removed.

-  The “N+1“ rule, applicable for rather small count numbers, has been
   modified with respect to the new version of the ISO 11929-2019, now
   designated as “N+x” rule. It means that the “x“ in “N+x“ is equal to
   zero, with exactly one exception for the case N=0, where x=1 is added
   to N. The value of the variable GamDistAdd representing the x (see
   dialog Options – Presettings) must be set to zero. Only if N=0
   occurs, the program internally replaces N by the value N=1. See also
   section 6.7.

**Revision of the setup program:**

-  Since version 2.2.07, with executing the de-installation,
   unins000.exe, the name “UncertRadio.exe“ in the windows registry
   entries for the .txp und .csv links is removed. After a next new
   installation, this shall prevent from calling an old UncertRadio.exe
   file by double-clicking a project file. See also chapter 1.2,
   “Country specific parameters” of the CHM Help.

**UncertRadio - Version 2.2.07 2018/03**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 21.03.2018

**Revision of the Windows-Help:**

-  Chapter 1.2 was extended in its section “Country specific parameters”

-  Chapter 7.11.3 was extended

**Addition of example projects:**

-  none

**Revision of the program:**

-  Up to now, within the FitDecay model dialog for linear unfolding,
   dummy entries of the form “X3 = 1.0“ were required if, for example,
   less than three of such formulae were really used. Since version
   2.2.07 such dummy entries can be omitted. The change of the program,
   however, was made in a way that assures that old projects containing
   such dummy entries can continue to be used.

**Revision of the setup program:**

-  Since version 2.2.07 the setup program installs that language
   shortcut into the file UR2_cfg.dat, which corresponds to the language
   being selected at the begin of executing the setup program.

-  Since version 2.2.07, with executing the de-installation,
   unins000.exe, the name “UncertRadio.exe“ in the windows registry
   entries for the .txp und .csv links is removed. After a next new
   installation, this shall prevent from calling an old UncertRadio.exe
   file by double-clicking a project file. See also chapter 1.2,
   “Country specific parameters” of the CHM Help.

**UncertRadio - Version 2.2.06 2018/02**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 27.02.2018

**Revision of the Windows-Help:**

-  Chapter 7.8: Introduction of the function uval(x);

Chapter 4.3: Extension of the formula for interpolation of count rate
variances

**Addition of example projects:**

-  none

**Revision of the program:**

-  There are cases in which an uncertainty value would be helpful within
   the equations by which the measurement model calculations are
   performed. An example is the relative uncertainty u(w)/w of the
   procedure dependent calibration factor w in models, e.g. in such
   models which require linear variance interpolation. Therefore, the
   function uval(x) has been introduced which gives the uncertainty u(x)
   of the quantity value x. In the example mentioned, the relative
   uncertainty of w can be obtained by the expression uval(w)/w within
   an equation.

-  If, for instance, the values of the gross count rate Rg and the
   background count rate are treated as mean values derived from several
   individual values, the uncertainty of a modified gross count rate
   value has to be calculated by interpolation between two count rate
   variances to derive the uncertainty of the modified output quantity
   value y. As the ISO 11929 Standard prescribes a linear interpolation
   of two output quantity variances, the interpolation between two count
   rate variances need not necessarily to be linear. It is described in
   Chapter 4.3, that the linear count rate variances interpolation
   formula as used up to now in UncertRadio, extended now by an
   additional term, is fully equivalent to the prescribed interpolation.

-

**UncertRadio - Version 2.2.05 2018/01**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 31.01.2018

**Revision of the Windows-Help:**

-  Chapters edited: 2.3

**Note**: The font size within the CHM Help can now be modified by
repeatedly clicking the font icon.

**Addition of example projects:**

-  none

**Revision of the program:**

-  Within the graphical representation of probability distributions
   obtained by MC simulation, the scaling of the two curves per plot
   (“blue“ and “red“) was re-adjusted such that their integrals yield
   the value 1. This adjustment, however, does not affect the
   characteristic values derived by the MC simulation.

-  The programming tools used for UncertRadio (GTK3+, Glade, Gfortran)
   have been updated to actual versions; see section 2.3. Now, the
   additional/unwanted surroundings of dialogs obtained from screenshots
   when copying them to a word document has vanished.

**UncertRadio - Version 2.2.04 2018/01**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 15.01.2018

**Revision of the Windows-Help:**

-  Chapters edited: 1.2: Manual editing of environment variables.

-  To the chapter 5.2 “Batch-mode processing with an Excel application”
   information about language dependent variables was added;

-  File name UR2_AutoRun_V5.xlsm replaced by UR2_AutoRun_V6.xlsm.

**Addition of example projects:**

-  modified project: none

**Revision of the program:**

-  Since the version 2.2.01 the installation now includes a batch file

UR2_start.bat,

enabling the start of UncertRadio in the case of insufficient user
rights under Windows. (see section `Installation <#installation>`__).

The PATH-command in this batch file has been modified with version
2.2.03; see chapter 1.2.

-  In automatic mode, controlled by Excel, UR2 interprets since this
   version the language-related parameters so that it uses the same
   values for the decimal point and the list separator.

**Modification in the Excel file UR2_AutoRun_V5.xlsm:**

-  In the VBA module “Modul_Auto_single_UR“ the subroutine
   “Init_pathnames” was extended by another variable for the
   sub-path-name for the UR projects and by language dependent
   variables.

-  For using UncertRadio from within Excel, the command string passed to
   UR2 was extended by an LC argument, e.g., LC=.,

This transfers the language dependent variable values to UncertRadio for
its project evaluation. It shall prevent from conflicts between both
programs regarding the decimal point and list separator characters: UR2
the uses the same as Excel.

See chapter 5.2 of this Help. The changes in the Excel module are marked
by the dates of 12.1.2018 and 13.1.2018.

**UncertRadio - Version 2.2.03 2018/01**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 03.01.2018

**Revision of the Windows-Help:**

-  Chapters edited: 1.2.

-  File name UR2_AutoRun_V4.xlsm replaced by UR2_AutoRun_V5.xlsm.

**Addition of example projects:**

-  modified project: none

**Revision of the program:**

-  Since the version 2.2.01 the installation now includes a batch file

UR2_start.bat,

enabling the start of UncertRadio in the case of insufficient user
rights under Windows. (see section `Installation <#installation>`__).

The PATH-command in this batch file has been modified; see chapter 1.2.

-  The program has been extended by a third language, French. The texts
   of GUI elements and Message dialogs were translated to French by
   Google-Translate. From now on, the desired language shortcut (DE, EN
   or FR) must explicitly be given in the file UR2_cfg.dat. Without
   using language related batch files, this is sufficient that the
   labels of all GUI elements, the Message dialogs and graphics appear
   with the same language. It is no longer necessary to additionally
   select the desired language within Windows. Decimal point and list
   separator characters are set internally dependent on the language
   shortcut. This also refers to the language dependent decimal point
   within numerical axis labels of the PLPLOT graphics.

-  A French version of the CHM Help file is not available for the
   program guide; the English help file version is used instead.

-  The simple way of controlling the language has the potential to lead
   to problems. If, for instance, the data in the file
   UR-Saved-Results.csv were stored in German mode, and new data shall
   be stored in the English mode, it may happen that the decimal point
   and list separator characters are in conflict. UncertRadio from now
   on checks for this potential problem in advance to a “Save to
   CSV“-action. In case of a conflict, the action is aborted, one
   obtains a warning message and the program exits with an exit code of
   3 when terminating the program. See also the note below referring to
   UR2_AutoRun_V5.xlsm.

-  The bug was removed, that two of the dialog-related help topics of
   the CHM help were not directly available.

-  Another intensive search for locations of potential numerical errors
   such as division by zero, invalid numbers or underflows, was
   performed which occasionally could lead to aborting the program. Only
   few such potential error locations were found and the corresponding
   code fragments re-formulated. An example is the MC simulation of
   Poisson-distributed count numbers: a tangens function, used within an
   exponential function in the random generator function, could result,
   within the exponent, in very large tan-values for argument values
   close to π/2. Another problem was a specific way of storing strings
   into cells of GTK-Treeviews (“Tables“) which was found to be
   “error-prone“; the corresponding program statements were reformulated
   and this type of possible errors removed.

**Modification in the Excel file UR2_AutoRun_V5.xlsm:**

-  The VBA module “Modul_Auto_single_UR“ in this file now checks whether
   UncertRadio is terminated with an exit code > 0. If in the
   UncertRadio run an error condition was found, then its exit code
   value of 3 is used within the VBA module for aborting this module
   run. This case occurs, if UncertRadio has found that in the CSV file
   “UR-Saved-Results.csv“, the decimal point and/or list separator
   characters differ from those used by UncertRadio for the actual “Save
   to CSV“ to be processed.

The changes (in few lines, <5) in the modules in UR2_AutoRun_V5.xlsm, as
compared to UR2_AutoRun_V4.xlsm, are marked with date string 1.1.2018,
which can be used to search for the changes.

-

**UncertRadio - Version 2.2.02 2017/12**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 18.12.2017

**Revision of the Windows-Help:**

-  Chapters edited: none.

**Addition of example projects:**

-  modified project: : TemperaturCurve_KALFIT_V2_EN.txp (only an
      information was added under the TAB “Procedure”)

**Revision of the program:**

-  Since the version 2.2.01 the installation now includes a batch file

UR2_start.bat,

enabling the start of UncertRadio in the case of insufficient user
rights under Windows. (see section `Installation <#installation>`__).

-  In the table under the TAB “Values, uncertainties” the number of
   significant digits was reduced by two digits to enhance the
   readability.

-  An error was removed which during reading a project file incorrectly
   interpreted the variable ModelType. This referred to only one project
   using a negative-linear model (Rn-222-Emanation_DE.txp).

-  The file selection dialog was improved:

during saving a file the corresponding button now has the correct label
“Save” instead of “Open”;

the file selection filters were corrected in the case of opening a text
file (.txt); filter and file extensions are now compatible.

-  The content of the report file was extended: both, the limits of the
   probabilistically-symmetric and the shortest coverage interval, are
   now given.

-  For internal reasons, the program was modified by two significant
   changes:

a) The structure of the rather large routine for executing the MC
simulation has been changed by splitting it up into smaller units. This
helps to improve its later interpretation.

   b) The method of the detection limit iteration was changed by
   applying now the method due to Ridders (see Numerical Recipes) for
   both, the analytical calculation and the MC simulation. The iteration
   method applied for the decision threshold in the MC simulation case
   (it brings the distribution mean closer to zero) has been replaced by
   a secant method. This allowed to significantly reduce the size of
   code of the MC simulation routines.

-  For testing the program after these modifications, an already
   existing internal routine for a series of project evaluations in a
   pseudo-batch mode was extended. The number of projects to be
   evaluated was increased, and the tests were extended to include also
   MC simulation. The analytical evaluation was tested with 109
   projects. The MC simulation was tested with about 50 projects
   (runtime about 90 min; the program is started only once for doing
   this).

During these tests some errors occurred. For most cases these errors did
however not occur when re-starting the program for each single project.
This behaviour pointed to some variables the values of which were not
correctly initialised (leading to a “memory” effect), so that errors
could occur during batch-mode processing. An example was the incorrect
initialising of covariances to be applied within MC simulation.

This time-consuming testing and the correction of encountered errors has
served to significantly reduce the risk of incorrect project
evaluations.

-  One of such errors occurred with the project ISO-Example-2a_EN.txp in
   which a linear variance interpolation is used; the values of decision
   threshold and detection limit of the MC simulation were found to be
   significantly larger than those obtained by analytical evaluation.
   This required a modification of the program: in this case, the gross
   count rate to be iterated must not be derived from the number Ng of
   gross counts, because u(Ng), given as sd/sqrt(mg), is not equal to
   sqrt(Ng).

-

**UncertRadio - Version 2.2.01 2017/12**

**Information about changes**

G. Kanisch, formerly TI-FI, Hamburg, 05.12.2017

**Revision of the Windows-Help:**

-  Chapters edited: follow the links under “Revision of the program“.

**Addition of example projects:**

-  new project: : Mean-theta_EN.txp

**Revision of the program:**

-  With this version the installation now includes a batch file

UR2_start.bat,

enabling the start of UncertRadio in the case of insufficient user
rights under Windows. (see section `Installation <#installation>`__).

-  With this version a tool for using data sets of measured values of an
   input quantity has been introduced. Such a data set becomes part of
   the project; values of mean and uncertainty are derived from it which
   are transferred to the table under the TAB “Values, uncertainties”.
   This option is invoked by setting the type value of symbol in the
   symbol table (TAB Equations), which usually is “a“ (dependent) or “u“
   (independent), to “m“ (mean). A new dialog was introduced, invoked by
   the new icon in the toolbar, which allows the treatment of a dataset
   (input, variance option). See `section
   6.9.2 <#applying-means-in-uncertradio>`__.

In addition to that, the symbol type value “p“ was introduced for such
an input quantity, which is to be applied as a parameter without
uncertainty.

The .txp and .csv formats of the project file were extended by that
information connected to data set-based quantities. (see `section
3.6.1 <#project-file-as-text-file-in-.txp-format>`__).

-  The procedure used until recently for calculating the shortest
   coverage interval was replaced by a simpler one, which now also may
   include the value zero as lower interval limit (see `section
   6.2 <#best-estimates-according-to-bayes-and-confidence-limits>`__).
   The checkbox „min. Coverage-Interval“ under the TAB „Results“ allows
   to toggle the displayed interval between the two types of intervals,
   „probabilistic symmetric“ and „shortest coverage interval“.

-  To improve the readability of graphics plots, the plot window has
   been increased somewhat, while the line thickness was decreased a
   bit.

-  Within the series of operations/calculations initiated by the button
   |view-refresh.png| a single step (TAB changing) was added, which was
   missing up to now and which also initiates calculations. The makes
   the use of this button safe.

-  It could happen through working with a project that sometimes a
   repetition of the calculations could end up in different results
   shown on the results TAB. But, after saving and loading the project
   again, this error did not show up. A specific internal variable has
   been identified as the source of this error which apparently was not
   correctly initialized.

-  Another bug removed referred to the observation that sometimes the
   two comboboxes for net and gross counting rates appeared as
   "unselected". This bug was removed.
