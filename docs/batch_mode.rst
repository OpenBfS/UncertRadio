Running UR in batch mode
========================

Using command line arguments
----------------------------

It is possible to run the UncertRadio program with adding command line
arguments to the calling string. The command line arguments, abbreviated
by Arg_n, are strings (without blank characters in them!) which are
passed over to UR as a sort of keywords at the starting time and may
serve e.g. for passing over the project filename to be processed to UR.
Such a call under Windows looks like:

>Start D:\\Uncertradio\\Uncertradio.exe Arg_1 Arg_2 Arg_3

For a first step towards an automated type of calculation of several UR
projects, one after another, the following three command line arguments
have been implemented, which are interpreted by UR:

>Start D:\\Uncertradio\\Uncertradio.exe AUTO Inputfile.txp Sample_ID

The first argument AUTO (capital letters) initiates the batch mode; the
second argument is the name of the UR project file to be processed; the
third argument is a sort of sample identification number (or a number
within an experimental batch) and can e.g. be used to differentiate
between such calculation results, which have between obtained for
different samples but for different copies of the same master project
file in which only a part of input parameters changed from experiment to
experiment.

Since the version 2.04.00 another batch mode is implemented by which a
given project can be evaluated serially for specified varied input
values. The evaluation corresponds to that one described in chapter 5.6.
The above argument AUTO is replaced by BATSER, followed by the project
file and the CSV file with records of specified input values in it. The
full command line then is (see chapter 4.5):

>Start D:\\UR2\\Uncertradio.exe BATSER J-ALUFT-Sr89-Sr-90_DE.txp
J_Aluft_serial_DE.csv

**Note**: **To avoid conflicts with antivirus software, the file
UR2_start_xls.bat, which previously was used for starting UncertRadio
from Excel, is no longer used. Instead, Excel-VBA invokes UncertRadio
directly, the temporary modification of the Windows-PATH variable is
also applied directly.**

Batch mode processing with an Excel application
-----------------------------------------------

The automated usage of UR is of practical value if the UR calls are
embedded in another program, which also takes control over the sequence
of calls to UR with different project files. As an example, this may be
a user-written program for processing the data evaluation for a certain
measurement procedure, where a master project file is used for
evaluating different batch samples such that only a part of the input
parameters varies from measurement to measurement. The latter, however,
requires that the user delivers a program part which modifies the
“variable” input parameters in a copy of the master project file based
on the `input format of a TXP project file <#_Structure_of_the>`__. This
work needs to be done by the user.

Based on the CSV format of an UR project, a first proposal for
demonstrating the automated UR calculations has been established by
Visual Basic within Excel. The filename of this Excel application is:

UR2_SingleAutoRun_V11.xlsm.

In this Excel file four spreadsheet tables are reserved for UR, which by
default are set as Table4, Table5, Table6 and Table7:

   Table4 : destination for import of an UR project as CSV file

   Table5 : destination of the result records obtained from having
   executed UR

   Table6 : list of UR project filenames for batch-like processing by UR

   Table7 : contains three buttons for executing different VBA macros

The Excel file contains a module called Modul_Auto_Single_UR. At the
beginning of this module an if statement guarantees that 32 bit as well
as 64 bit versions Excel can be used. Thereafter, the first of the four
spreadsheets mentioned above is defined:

Public Const FirstTableNum As Integer = 4

which may be changed by the user.

All other tables (spreadsheets) including newly inserted ones, except of
these four, may be freely used.

The module Modul_Auto_Single_UR containes several routines (macros)
which are shortly detailed below.

+-------------------+--------------------------------------------------+
| Init_pathnames    | In this small macro one has to fix the           |
|                   | path-names of UR and this Excel file. **Since    |
|                   | version 2.4.22 the necessary path-related        |
|                   | information are taken directly from the file     |
|                   | UR2_cfg.dat\ :**                                 |
|                   |                                                  |
|                   | Sub Init_pathnames()                             |
|                   |                                                  |
|                   | ‘Read the filename for UR_path, Excel_path and   |
|                   | UR_output_path from UR2’s configuraton file      |
|                   | UR2_cfg.dat:                                     |
|                   |                                                  |
|                   | **Fnum = FreeFile()**                            |
|                   |                                                  |
|                   | **Open Trim(UR_path) + "UR2_cfg.dat" For Input   |
|                   | As Fnum**                                        |
|                   |                                                  |
|                   | **For k = 1 To 100**                             |
|                   |                                                  |
|                   | **If (EOF(Fnum)) Then Exit For**                 |
|                   |                                                  |
|                   | **Input #Fnum, text**                            |
|                   |                                                  |
|                   | **i1 = InStr(1, text,                            |
|                   | UCase("UR_AUTO_output_path="),**                 |
|                   |                                                  |
|                   | **vbTextCompare)**                               |
|                   |                                                  |
|                   | **i2 = InStr(1, text, "=", vbTextCompare)**      |
|                   |                                                  |
|                   | **If (i2 > 0 And i1 > 0) Then**                  |
|                   |                                                  |
|                   | **UR_AUTO_output_path = Mid(text, i2 + 1, 300)** |
|                   |                                                  |
|                   | **Exit For**                                     |
|                   |                                                  |
|                   | **End If**                                       |
|                   |                                                  |
|                   | **.**                                            |
|                   |                                                  |
|                   | **. repeat the same for the second and third     |
|                   | information**                                    |
|                   |                                                  |
|                   | **In the case of “UR_path=”, add the following   |
|                   | variable:**                                      |
|                   |                                                  |
|                   | **URGTK_path = UR_path & "GTKUser64\\bin;" ‘     |
|                   | 26.6.2023**                                      |
|                   |                                                  |
|                   | **Next k**                                       |
|                   |                                                  |
|                   | **Close #Fnum**                                  |
|                   |                                                  |
|                   | **‘** Build the string for setting the           |
|                   | environment variable                             |
|                   |                                                  |
|                   | path:                                            |
|                   |                                                  |
|                   | **pathX = EnvironGetItem("path", "User")**       |
|                   |                                                  |
|                   | **Call SetEnvironmentVariableA("path", UR_path & |
|                   | \_**                                             |
|                   |                                                  |
|                   | **";" & URGTK_path & “;” & pathX)**              |
|                   |                                                  |
|                   | **Debug.Print "path=", EnvironGetItem("path",    |
|                   | "User")**                                        |
|                   |                                                  |
|                   | **‘ Language dependencies:**                     |
|                   |                                                  |
|                   | ‘Set Decimalpoint and ListSeparator characters : |
|                   |                                                  |
|                   | sDecimalPoint = GetDecimalSeparator()            |
|                   |                                                  |
|                   | sListSeparator = \_                              |
|                   |                                                  |
|                   | Application.International(xlListSeparator)       |
|                   |                                                  |
|                   | ‘Set language:                                   |
|                   |                                                  |
|                   | **Win_langg = "EN"**                             |
|                   |                                                  |
|                   | **Select Case Application.International(**       |
|                   |                                                  |
|                   | **XlApplicationInternational.xlCountryCode)**    |
|                   |                                                  |
|                   | **Case 1: Win_langg = "EN"**                     |
|                   |                                                  |
|                   | **Case 33: Win_langg = "FR"**                    |
|                   |                                                  |
|                   | **Case 49: Win_langg = "DE"**                    |
|                   |                                                  |
|                   | **End Select**                                   |
|                   |                                                  |
|                   | ...                                              |
|                   |                                                  |
|                   | End Sub                                          |
|                   |                                                  |
|                   | It is assumed that the UR project files are      |
|                   | located in the subfolder „pros\\en\\“. If        |
|                   | necessary, this has to be modified.              |
+===================+==================================================+
| Au                | A simple macro that allows a batchlike           |
| torun_UncertRadio | processing of those UR projects, after they have |
|                   | been selected within **Table6**. It is invoked   |
|                   | by a button from **Table7** (see below).         |
+-------------------+--------------------------------------------------+
| I                 | This macro allows importing an external UR       |
| mport_UR_CSV_file | project file given in CSV format into **Table4** |
|                   | of the Excel file. It is invoked by a button     |
|                   | within Table7 (see below).                       |
|                   |                                                  |
|                   | Since UR2-Version 2.4.03 this routine contains   |
|                   | at ist beginning an If-Then construct, which by  |
|                   | its activation allows with „Run_SheetName“ to    |
|                   | select a name of the worksheet.                  |
+-------------------+--------------------------------------------------+
| SingleRun_UR      | After editing of a project already existing in   |
|                   | Table4, this macro exports it into a CSV file    |
|                   | external to Excel, lets UR execute this project  |
|                   | and finally imports corresponding result records |
|                   | into Table5. It is invoked by a button within    |
|                   | Table7 (see below).                              |
|                   |                                                  |
|                   | In detail:                                       |
|                   |                                                  |
|                   | export of the edited **Table4**: Makro           |
|                   | DoTheExport,                                     |
|                   |                                                  |
|                   | execute this external CSV file with UR: Makro    |
|                   | DoSingleRun_UncertRadio,                         |
|                   |                                                  |
|                   | Import the results obtained by UR to **Table5:** |
|                   | Makro doFileQuery.                               |
+-------------------+--------------------------------------------------+
| Run_UR_AUTOSEP    | This macro also calls SingleRun_UR (with a new   |
|                   | public variable UR_AUTOSEP=True), but uses two   |
|                   | new tables (sheets), UR2_data und UR2_results,   |
|                   | for the project and the result values,           |
|                   | respectively; UR2 in this case does not save     |
|                   | data to the Auto_Report files; at the end, two   |
|                   | new CSV written by Excel and UR2 (with           |
|                   | extensions \*_xls.csv und \*_xls_res.csv) are    |
|                   | deleted.                                         |
+-------------------+--------------------------------------------------+

Just between calling the two macros Import_UR_CSV_file and SingleRun_UR
is the time in which the input data contained in Table4 can be edited by
the user, e.g. by entering new input data belonging to the next
measurement evaluated by the same project.

After running of these two main macros the results (Table5) can be used
for transferring them into own Excel sheets.

Within the VB code (makro Autorun_UncertRadio) the total command string
required for starting the evaluation of an external project, stored in
the variable UR_string, reads as follows:

since version 2.1.1:

UR_string = Trim(UR_path) & "UncertRadio.exe AUTO " & Chr(34) & \_

Trim(UR_path) & "pros\\" & Trim(fname) & Chr(34) & " " & Trim(sid)

Since version 2.2.4 following statement added:

' add the new language code LC=:

UR_string = Trim(UR_string) & " LC=" & Trim(Win_langg) &
Trim(sDecimalPoint) &\_

   Trim(sListSeparator)

Since version 2.4.03 the UR2_start_xls.bat is applied:

UR_string = Trim(UR_path) & "UR2_start_xls.bat AUTO " & Chr(34) &
Trim(fname) \_

& Chr(34) & " " & Trim(sid) ' 04.06.2020

UR_string = Trim(UR_string) & " " & Chr(34) & "LC=" & Trim(Win_langg) &
\_

   Trim(sDecimalPoint) & Trim(sListSeparator) & Chr(34)

The **file UR2_start_xls.bat introduced with version 2.4.03 is no longer
used since version 2.4.22 to avoid conflicts with antivirus software.**
Instead, UncertRadio is invoked by Excel directly, but only after having
modified the Windows-Path variable, also directly by Excel (see above):

UR_string = Trim(UR_path) & "uncertradio.exe AUTO " & Chr(34) &
Trim(UR_path) & \_

Trim(fname) & Chr(34) & " " & Trim(sid)

' add the language code LC=: (since 13.1.2018)

UR_string = Trim(UR_string) & " " & Chr(34) & "LC=" & Trim(Win_langg) &
\_

Trim(sDecimalPoint) & Trim(sListSeparator) & Chr(34)

Since version 2.4.26 (~26.6.2023), the environment variable path is set
as indicated above:

**pathX = EnvironGetItem("path", "User")**

**Call SetEnvironmentVariableA("path", UR_path & ";" & URGTK_path & “;”
& pathX)**

Example:

since version 2.1.1:

D:\\UR2\\UncertRadio.exe AUTO "D:\\GF_Pros\\UR2\\pros\\zzURpr.csv" 556

since version 2.2.4:

D:\\UR2\\UncertRadio.exe AUTO "D:\\GF_Pros\\UR2\\zzURpr.csv" 556 LC=,;

since version 2.4.03:

d:\\UR2\\UR2_start_xls.bat AUTO "d:\\UR2\\zzURpr.csv" 556 "LC=DE,;"

since version 2.4.22:

d:\\UR2\\uncertradio.exe AUTO "d:\\UR2\\zzURpr.csv" 556 "LC=DE,;"

The variables fname and sid contain the UR project filename and the
Sample_ID string. The pathname UR_Path has to be fixed by the user at
the beginning of the routine Autorun_UncertRadio.

Within the VBA code of SingleRun_UR the CSV project is transferred into
that path which has been declared in the variable Excel-Path:

   ' write out the UR project CSV file:

since version 2.1.1:

   file_csv = Trim(UR_path_unix) & "pros\\" & "zzURpr.csv"

since version 2.2.4:

   file_csv = Trim(UR_path) & "zzURpr.csv"

   Call DoTheExport(file_csv, ifehl)

   If (ifehl = 1) Then Exit Sub

   ' execute UR once with this input file:

   Call DoSingleRun_UncertRadio(file_csv, ifehl)

   If (ifehl = 1) Then Exit Sub

Processing the project file UR_fname by UncertRadio is executed within
Auturun_UncertRadio with a function bShellAndWait. It causes Excel to
wait until UR has finished its calculations and stopped. Then, within a
loop, the next data evaluation is processed.

Since version 2.4.00, the direct call to uncertradio.exe as applied in
the above command strings could be replaced by the batch file
UR2_start_xls.bat as introduced in section 5.1. However,
UR2_start_xls.bat is no longer used since version 2.4.26.

In the macro **DoSingleRun_UncertRadio** the string holding the filename
for the csv project output file has been changed (at two locations):

previous: file_csv = Trim(UR_path) & "zzURpr.csv"

since V. 2.4.04.: file_csv = Trim(UR_AUTO_output_path) & "zzURpr.csv"

previous: file_csv = Trim(UR_path) & filename_org

since V. 2.4.04.: file_csv = Trim(UR_AUTO_output_path) & filename_org

The four command line arguments are:

AUTO (%1)

trim(fname) (%2)

sid (%3)

LC=.. (%4)

The evaluation results obtained by UncertRadio for a project file are
stored in an ASCII text file and in a CSV file in a table-like
structure. The names of the output files are fixed within UR:

ASCII file: AutoReport-Results.txt

CSV file: AutoReport-Results.csv

The output of data into these files is done in a cumulative form
(appending rows at the end of the files). The numbers are written with
using that decimal-point character which is defined within Windows.

These two files may be deleted if they have grown; UR the produces then
new ones.

Meaning of the columns in the UR output files:

+------------+--------------------------+------------------------------+
| S          | Bedeutung                | Meaning                      |
| paltenbez. |                          |                              |
+============+==========================+==============================+
| #          | Nummer der Ergebnisgröße | number of the output         |
|            |                          | quantity                     |
+------------+--------------------------+------------------------------+
| File       | UR-Projekt-Dateiname     | filename of UR project       |
+------------+--------------------------+------------------------------+
| Sample_id  | Probe                    | identification of            |
|            | n/Analyse-Identifikation | sample/analysis              |
+------------+--------------------------+------------------------------+
| Date       | Datum + Uhrzeit          | date and time of evaluation  |
+------------+--------------------------+------------------------------+
| quantity   | Symbolname der           | name of the output           |
|            | Ergebnisgröße            | quantity’s symbol            |
+------------+--------------------------+------------------------------+
| PE         | Wert der Ergebnisgröße   | value of the output quantity |
+------------+--------------------------+------------------------------+
| uPE        | erweiterte Unsicherheit, | value of expanded            |
|            | enthält den Faktor k, s. | uncertainty using the        |
|            | weiter unten             | coverage factor k; see below |
+------------+--------------------------+------------------------------+
| BE         | bester Schätzwert        | best estimate                |
+------------+--------------------------+------------------------------+
| uBE        | dem besten Schätzwert    | uncertainty associated with  |
|            | beigeordnete erweiterte  | best estimate                |
|            | Unsicherheit             |                              |
+------------+--------------------------+------------------------------+
| LQ         | untere Grenze des        | lower limit of the           |
|            | Vertrauensbereichs       | confidence interval          |
+------------+--------------------------+------------------------------+
| UQ         | obere Grenze des         | upper limit of the           |
|            | Vertrauensbereichs       | confidence interval          |
+------------+--------------------------+------------------------------+
| sLQ        | untere Grenze des        | lower limit of the shortest  |
|            | kürzesten                | confidence interval          |
|            | Vertrauensbereichs       |                              |
+------------+--------------------------+------------------------------+
| sUQ        | obere Grenze des         | upper limit of the shortest  |
|            | kürzesten                | confidence interval          |
|            | Vertrauensbereichs       |                              |
+------------+--------------------------+------------------------------+
| DT\*       | Erkennungsgrenze         | decision threshold           |
+------------+--------------------------+------------------------------+
| DL#        | Nachweisgrenze           | detection limit              |
+------------+--------------------------+------------------------------+
| NT         | (Nachweisgrenzentyp;     | type of detection limit      |
|            | sollte nur noch 1 sein,  | calculation (can only be 1,  |
|            | d.h. ISO 11929)          | according to ISO 11929)      |
+------------+--------------------------+------------------------------+
| k          | Erweiterungsfaktor für   | coverage factor k for the    |
|            | die Unsicherheit         | uncertainty                  |
+------------+--------------------------+------------------------------+
| kalpha     | Wert von *k*\ :sub:`1-α` | value of *k*\ :sub:`1-α`     |
+------------+--------------------------+------------------------------+
| kbeta      | Wert von *k*\ :sub:`1-β` | value of *k*\ :sub:`1-β`     |
+------------+--------------------------+------------------------------+
| 1-gamma    | Wahrscheinlichkeit 1-γ   | confidence interval related  |
|            | für das                  | probability                  |
|            | Vertrauensintervall      |                              |
+------------+--------------------------+------------------------------+
| Chisqr     | reduziertes Chi-Quadrat, | reduced Chi-square value, in |
|            | im Falle linearer        | the case of linear unfolding |
|            | Entfaltung               |                              |
+------------+--------------------------+------------------------------+

Starting the macros
-------------------

Start buttons have been implemented in **Table7** for invoking the three
important macros:

|image26|

Run UR with a project edited within Excel
-----------------------------------------

After having imported an external UR-Project, given in the `CSV
format <#project-file-in-the-excel-compatible-.csv-format>`__ ,

   |image27|

The data can be edited in Table4.

Then, the three steps of exporting Table4 into an external csv file,
executing it with UR and re-importing the results record(s) can be
invoked with a single Button in **Table7**:

   |image28|

Example of an Excel application for a batch-like processing
-----------------------------------------------------------

The filenames of UR projects to be processed are collected in column A
of Table6 and the associated Sample_IDs in column B, respectively. In
this example all UR example project files belonging to the program are
listed there.

|image29|

Before starting the batch processing with the button
Start Autorun_UncertRadio the associated filenames have to be selected
as a two-column block as shown in the screen shot above.

After the batch processing loop in the VB code is finished, the CSV
output file AutoReport-Result.csv obtained by UR is copied to Table5 at
the end of that VB subroutine. A part of that sheet is shown in the
following screen shot.

|image30|

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

eps1; u(eps1); eps4; u(eps4)

0.338; 0.045; 0.390; 0.055

0.36; 0.045; 0.370; 0.049

0.35 0.047; 0.360; 0.052

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
file called J_Aluft_serial_EN.csv. It is meant for using it with the
existing project J-ALUFT-Sr89-Sr-90_EN.txp.

With activating the menu item “\ **Edit – Serial evaluation**\ “, the
following dialog is invoked by which the evaluation can be started after
having defined the setup of this evaluation:

|image31|

UncertRadio then produces one or two output files (csv type) for the
results obtained – without and with MC simulation. Their names are
derived from the name of the input csv file, as in the case of the above
example:

J_Aluft_serial_EN_res.csv

J_Aluft_serial_EN_mc.csv

These files contain values for:

File; #EG; PE; uPE; BE; uBE; LQ; UQ; sLQ; sUQ; DT; DL;

(project filename, No. of the output quantity, primary value and
uncertainty of the output quantity, best estimate and associated
uncertainty, lower and upper quantile, decision threshold, detection
limit)

Processing projects in batch mode
---------------------------------

UncertRadio allows to evaluate several projects in a batch mode. This
mode is invoked by the menu item **Edit – Batch evaluation of projects**
which opens a dialog, like that one shown in section 5.6. Therein, a
simple text file is selected, which contains the project filenames, line
by line.

|image32|

UncertRadio then starts the calculations, with or without MC simulation,
and writes the results into an **output file batch_out.csv** (CSV
format). For each evaluated project and each of its up to three output
quantities, the filename is written, followed by a table of values for
#EG, PE, uPE, BE, uBE, LQ, UQ, sLQ, sUQ, DT, DL. The column headers in
the output file indicate their relation to the parts 1 and 2 of ISO
11929:2019.

Meaning of the symbols:

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

5.

   1.
   2.
   3.
   4.
   5.
   6.
   7.

Batch-mode testing the evaluation of example projects
------------------------------------------------------

UncertRadio allows to evaluate all example projects in a batch mode. The
results obtained are compared with reference results. Deviations found
are documented in a file.

This test is invoked by the main menu item **Options – QC batch test**.
The following dialog is shown:

|Ein Bild, das Text, Screenshot, Schrift, Software enthält. Automatisch
generierte Beschreibung|

Two file names are pre-set which allow to directly start the test with
the Apply button.

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

|Ein Bild, das Text, Screenshot, Schrift, Reihe enthält. Automatisch
generierte Beschreibung|

Of the three text lines in this message, only the middle one is the
relevant one. Only if more than zero deviations are given therein, this
indicates a problem.

**This test is always applied before publishing a new version of the
UncertRadio software**.
