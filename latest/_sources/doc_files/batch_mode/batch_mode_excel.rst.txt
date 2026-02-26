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
on the :ref:`input format of a TXP project file <structure of the project file>`
This work needs to be done by the user.


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

+--------------------+-----------------------------------------------------+
|| Init_pathnames    || In this small macro one has to fix the             |
||                   || path-names of UR and this Excel file. **Since**    |
||                   || **version 2.4.22 the necessary path-related**      |
||                   || **information are taken directly from the file**   |
||                   || **UR2_cfg.dat\ :**                                 |
||                   ||                                                    |
||                   || Sub Init_pathnames()                               |
||                   ||                                                    |
||                   || ‘Read the filename for UR_path, Excel_path and     |
||                   || UR_output_path from UR2’s configuraton file        |
||                   || UR2_cfg.dat:                                       |
||                   ||                                                    |
||                   || **Fnum = FreeFile()**                              |
||                   || **Open Trim(UR_path) + "UR2_cfg.dat" For Input**   |
||                   || As **Fnum**                                        |
||                   || **For k = 1 To 100**                               |
||                   || **If (EOF(Fnum)) Then Exit For**                   |
||                   || **Input #Fnum, text**                              |
||                   || **i1 = InStr(1, text,**                            |
||                   || **UCase("UR_AUTO_output_path="),**                 |
||                   || **vbTextCompare)**                                 |
||                   || **i2 = InStr(1, text, "=", vbTextCompare)**        |
||                   || **If (i2 > 0 And i1 > 0) Then**                    |
||                   || **UR_AUTO_output_path = Mid(text, i2 + 1, 300)**   |
||                   || **Exit For**                                       |
||                   || **End If**                                         |
||                   || **.**                                              |
||                   ||                                                    |
||                   || **. repeat the same for the second and third**     |
||                   || **information**                                    |
||                   ||                                                    |
||                   || **In the case of “UR_path=”, add the following**   |
||                   || **variable:**                                      |
||                   || **URGTK_path = UR_path & "GTKUser64\\bin;" ‘**     |
||                   || **26.6.2023**                                      |
||                   || **Next k**                                         |
||                   || **Close #Fnum**                                    |
||                   || **‘** Build the string for setting the             |
||                   || environment variable                               |
||                   || path:                                              |
||                   || **pathX = EnvironGetItem("path", "User")**         |
||                   || **Call SetEnvironmentVariableA("path", UR_path &** |
||                   || **\_**                                             |
||                   || **";" & URGTK_path & “;” & pathX)**                |
||                   ||                                                    |
||                   || **Debug.Print "path=", EnvironGetItem("path",**    |
||                   || **"User")**                                        |
||                   ||                                                    |
||                   || **‘ Language dependencies:**                       |
||                   || ‘Set Decimalpoint and ListSeparator characters :   |
||                   || sDecimalPoint = GetDecimalSeparator()              |
||                   || sListSeparator = \_                                |
||                   || Application.International(xlListSeparator)         |
||                   ||                                                    |
||                   || ‘Set language:                                     |
||                   || **Win_langg = "EN"**                               |
||                   || **Select Case Application.International(**         |
||                   || **XlApplicationInternational.xlCountryCode)**      |
||                   ||                                                    |
||                   || **Case 1: Win_langg = "EN"**                       |
||                   || **Case 33: Win_langg = "FR"**                      |
||                   || **Case 49: Win_langg = "DE"**                      |
||                   || **End Select**                                     |
||                   || ...                                                |
||                   ||                                                    |
||                   || End Sub                                            |
||                   ||                                                    |
||                   || It is assumed that the UR project files are        |
||                   || located in the subfolder „pros\\en\\“. If          |
||                   || necessary, this has to be modified.                |
+====================+=====================================================+
|| Au                || A simple macro that allows a batchlike             |
|| torun_UncertRadio || processing of those UR projects, after they have   |
||                   || been selected within **Table6**. It is invoked     |
||                   || by a button from **Table7** (see below).           |
+--------------------+-----------------------------------------------------+
|| I                 || This macro allows importing an external UR         |
|| mport_UR_CSV_file || project file given in CSV format into **Table4**   |
||                   || of the Excel file. It is invoked by a button       |
||                   || within Table7 (see below).                         |
||                   ||                                                    |
||                   || Since UR2-Version 2.4.03 this routine contains     |
||                   || at ist beginning an If-Then construct, which by    |
||                   || its activation allows with „Run_SheetName“ to      |
||                   || select a name of the worksheet.                    |
+--------------------+-----------------------------------------------------+
|| SingleRun_UR      || After editing of a project already existing in     |
||                   || Table4, this macro exports it into a CSV file      |
||                   || external to Excel, lets UR execute this project    |
||                   || and finally imports corresponding result records   |
||                   || into Table5. It is invoked by a button within      |
||                   || Table7 (see below).                                |
||                   ||                                                    |
||                   || In detail:                                         |
||                   ||                                                    |
||                   || export of the edited **Table4**: Makro             |
||                   || DoTheExport,                                       |
||                   ||                                                    |
||                   || execute this external CSV file with UR: Makro      |
||                   || DoSingleRun_UncertRadio,                           |
||                   ||                                                    |
||                   || Import the results obtained by UR to **Table5:**   |
||                   || Makro doFileQuery.                                 |
+--------------------+-----------------------------------------------------+
|| Run_UR_AUTOSEP    || This macro also calls SingleRun_UR (with a new     |
||                   || public variable UR_AUTOSEP=True), but uses two     |
||                   || new tables (sheets), UR2_data und UR2_results,     |
||                   || for the project and the result values,             |
||                   || respectively; UR2 in this case does not save       |
||                   || data to the Auto_Report files; at the end, two     |
||                   || new CSV written by Excel and UR2 (with             |
||                   || extensions \*_xls.csv und \*_xls_res.csv) are      |
||                   || deleted.                                           |
+--------------------+-----------------------------------------------------+

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

