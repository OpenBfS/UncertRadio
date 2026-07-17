Batch mode processing with an Excel application
===============================================

.. note::
   **Platform compatibility**: The Excel batch mode described in this section
   is **Windows-only**. It requires **Microsoft Excel** (32-bit or 64-bit) and
   uses Windows-specific VBA APIs. **LibreOffice Calc and other spreadsheet
   applications are not supported**. The Excel file is provided as an example
   to demonstrate the possibility of batch processing, not as a required
   component for using UncertRadio.

Overview
--------

The automated usage of UncertRadio is of practical value when the UR calls are
embedded in another program, which also takes control over the sequence
of calls to UR with different project files. 

**Example use case**: A user-written program for processing data evaluation for a 
certain measurement procedure, where a master project file is used for evaluating 
different batch samples. In this scenario, only a part of the input parameters 
varies from measurement to measurement.

.. important::
   You must provide a program part which modifies the "variable" input 
   parameters in a copy of the master project file based on the 
   :ref:`input format of a TXP project file <structure of the project file>`.

Excel Application Setup
-----------------------

Based on the CSV format of an UR project, a practical batch processing solution 
has been established using Visual Basic within Excel. The Excel application file is:

**File**: ``UR2_SingleAutoRun.xlsm`` (located in `/share/UncertRadio`)

Spreadsheet Tables
~~~~~~~~~~~~~~~~~~

Four spreadsheet tables are reserved for UR, which by default are set as 
``Table4``, ``Table5``, ``Table6``, and ``Table7``:

- **Table4**: Destination for import of an UR project as CSV file
- **Table5**: Destination of result records obtained from executing UR
- **Table6**: List of UR project filenames for batch-like processing by UR
- **Table7**: Contains buttons for executing different VBA macros

Module Configuration
~~~~~~~~~~~~~~~~~~~~

The Excel file contains a module called ``Modul_Auto_Single_UR``.
It contains several routines (macros) which are detailed below.

**Autorun_UncertRadio**

A simple macro that performs batch-like processing of UR projects after they 
have been selected in ``Table6``. This macro is invoked by a button in ``Table7``.

`Function`: Iterates through a list of project filenames and executes UR 
for each one sequentially, enabling automated batch processing.

**Import_UR_CSV_file**

This macro allows importing an external UR project file in CSV format into 
``Table4`` of the Excel file. It is invoked by a button in ``Table7``.

`New feature (version 2.4.03+)`: Contains an ``if-then`` construct that allows 
selecting a worksheet name using the ``Run_SheetName`` variable.

**SingleRun_UR**

After editing a project already existing in ``Table4``, this macro performs the 
following steps:

1. Exports the project to a CSV file
2. Executes UR with this input file
3. Imports the corresponding result records into ``Table5``

This macro is invoked by a button in ``Table7``.

`Sub-routines`:
- ``Init_pathnames``: Sets the path names of UR and the Excel file. 
- ``DoTheExport``: Exports edited ``Table4`` to CSV
- ``DoSingleRun_UncertRadio``: Executes UR with the external CSV file
- ``doFileQuery``: Imports results into ``Table5``

**Run_UR_AUTOSEP**

This macro calls ``SingleRun_UR`` with a new public variable ``UR_AUTOSEP=True``. 
It uses two separate tables (sheets): ``UR2_data`` and ``UR2_results`` instead 
of ``Table4`` and ``Table5``.

**Key differences from SingleRun_UR**:

- UR2 does not save data to ``Auto_Report`` files in this mode
- Two CSV files are created with extensions ``*_xls.csv`` and ``*_xls_res.csv``
- These temporary files are deleted at the end of execution

Workflow
--------

The typical workflow between calling the two macros ``Import_UR_CSV_file`` and 
``SingleRun_UR`` is as follows:

1. Import a UR project from a CSV file using ``Import_UR_CSV_file``
2. **Edit the data** in ``Table4`` as needed (e.g., enter new input data for the next measurement)
3. Execute ``SingleRun_UR`` to export, process, and import results into ``Table5``
4. Use the results in ``Table5`` for further processing in Excel

After running these two main macros, the results (``Table5``) can be transferred into 
your own Excel sheets for further analysis or reporting.

Command Line Execution
----------------------

The command string for starting UR evaluation of an external project is stored in the 
variable ``UR_string``. This has evolved across versions:

Version 2.1.1
~~~~~~~~~~~~~

.. code-block:: vba

   UR_string = Trim(UR_path) & "UncertRadio.exe AUTO " & Chr(34) & _
       Trim(UR_path) & "pros\\" & Trim(fname) & Chr(34) & " " & Trim(sid)

**Example**:

.. code-block:: bash

   D:\UR2\UncertRadio.exe AUTO "D:\GF_Pros\UR2\pros\zzURpr.csv" 556

Version 2.2.4
~~~~~~~~~~~~~

Following statement added to include language code:

.. code-block:: vba

   UR_string = Trim(UR_string) & " LC=" & Trim(Win_langg) & _
       Trim(sDecimalPoint) & Trim(sListSeparator)

**Example**:

.. code-block:: bash

   D:\UR2\UncertRadio.exe AUTO "D:\GF_Pros\UR2\zzURpr.csv" 556 LC=,;

Version 2.4.03
~~~~~~~~~~~~~~

The ``UR2_start_xls.bat`` file was introduced (now deprecated):

.. code-block:: vba

   UR_string = Trim(UR_path) & "UR2_start_xls.bat AUTO " & Chr(34) & _
       Trim(fname) & Chr(34) & " " & Trim(sid)
   
   UR_string = Trim(UR_string) & " " & Chr(34) & "LC=" & Trim(Win_langg) & _
       Trim(sDecimalPoint) & Trim(sListSeparator) & Chr(34)

**Example**:

.. code-block:: bash

   d:\UR2\UR2_start_xls.bat AUTO "d:\UR2\zzURpr.csv" 556 "LC=DE,;"

Version 2.4.22 and Later
~~~~~~~~~~~~~~~~~~~~~~~~

The batch file approach was discontinued to avoid conflicts with antivirus software. 
UncertRadio is now invoked directly by Excel after modifying the Windows-Path variable:

.. code-block:: vba

   UR_string = Trim(UR_path) & "uncertradio.exe AUTO " & Chr(34) & _
       Trim(UR_path) & Trim(fname) & Chr(34) & " " & Trim(sid)
   
   UR_string = Trim(UR_string) & " " & Chr(34) & "LC=" & Trim(Win_langg) & _
       Trim(sDecimalPoint) & Trim(sListSeparator) & Chr(34)

**Example**:

.. code-block:: bash

   d:\UR2\uncertradio.exe AUTO "d:\UR2\zzURpr.csv" 556 "LC=DE,;"

Version 2.4.26 and Later
~~~~~~~~~~~~~~~~~~~~~~~~

The environment variable PATH is now set directly:

.. code-block:: vba

   pathX = EnvironGetItem("path", "User")
   Call SetEnvironmentVariableA("path", UR_path & ";" & URGTK_path & ";" & pathX)

Command Line Arguments
~~~~~~~~~~~~~~~~~~~~~~

The four command line arguments passed to UncertRadio are:

- **%1** - ``AUTO``: Specifies batch mode operation
- **%2** - ``fname``: UR project filename
- **%3** - ``sid``: Sample ID string
- **%4** - ``LC=...``: Language and locale settings

Variables
~~~~~~~~~

The variables used in the command strings are:

- ``fname``: UR project filename
- ``sid``: Sample ID string
- ``UR_path``: Installation path of UncertRadio

Must be set by the user at the beginning of the ``Autorun_UncertRadio`` routine.

CSV Project File Handling
-------------------------

Within the VBA code of ``SingleRun_UR``, the CSV project is transferred to the path 
declared in the variable ``Excel_Path``:

Version 2.1.1
~~~~~~~~~~~~~

.. code-block:: vba

   file_csv = Trim(UR_path_unix) & "pros\\" & "zzURpr.csv"

Version 2.2.4 and Later
~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: vba

   file_csv = Trim(UR_path) & "zzURpr.csv"
   Call DoTheExport(file_csv, ifehl)
   If (ifehl = 1) Then Exit Sub
   
   ' Execute UR with this input file
   Call DoSingleRun_UncertRadio(file_csv, ifehl)
   If (ifehl = 1) Then Exit Sub

Processing Flow
~~~~~~~~~~~~~~~

Processing is executed within ``Autorun_UncertRadio`` using the ``bShellAndWait`` function, 
which causes Excel to wait until UR has finished its calculations and stopped. Then, 
within a loop, the next data evaluation is processed.

Output File Changes
~~~~~~~~~~~~~~~~~~~

Since version 2.4.04, the string holding the filename for the CSV project output file 
has been changed (at two locations):

**Previous behavior**:

.. code-block:: vba

   file_csv = Trim(UR_path) & "zzURpr.csv"
   file_csv = Trim(UR_path) & filename_org

**Current behavior (v2.4.04+)**:

.. code-block:: vba

   file_csv = Trim(UR_AUTO_output_path) & "zzURpr.csv"
   file_csv = Trim(UR_AUTO_output_path) & filename_org

Output Files
------------

The evaluation results obtained by UncertRadio for a project file are stored in:

- **ASCII text file**: ``AutoReport-Results.txt``
- **CSV file**: ``AutoReport-Results.csv``

**Characteristics**:

- Data is appended to existing files (cumulative form)
- Numbers are written using the decimal-point character defined in Windows
- These files may be deleted when they grow too large; UR will create new ones

Output File Columns
~~~~~~~~~~~~~~~~~~~

The meaning of the columns in the UR output files is shown below. Columns are listed 
with German (Bedeutung) and English (Meaning) descriptions:

.. list-table:: UR Output File Columns
   :header-rows: 1
   :widths: 15, 30, 40

   * - Column
     - Description (DE)
     - Description (EN)
   * - #
     - Nummer der Ergebnisgröße
     - Number of the output quantity
   * - File
     - UR-Projekt-Dateiname
     - Filename of UR project
   * - Sample_id
     - Probe/Analyse-Identifikation
     - Identification of sample/analysis
   * - Date
     - Datum + Uhrzeit
     - Date and time of evaluation
   * - quantity
     - Symbolname der Ergebnisgröße
     - Name of the output quantity's symbol
   * - PE
     - Wert der Ergebnisgröße
     - Value of the output quantity
   * - uPE
     - Erweiterte Unsicherheit (mit Faktor k)
     - Value of expanded uncertainty (using coverage factor k)
   * - BE
     - Bester Schätzwert
     - Best estimate
   * - uBE
     - Unsicherheit des besten Schätzwerts
     - Uncertainty associated with best estimate
   * - LQ
     - Untere Grenze des Vertrauensbereichs
     - Lower limit of the confidence interval
   * - UQ
     - Obere Grenze des Vertrauensbereichs
     - Upper limit of the confidence interval
   * - sLQ
     - Untere Grenze des kürzesten Vertrauensbereichs
     - Lower limit of the shortest confidence interval
   * - sUQ
     - Obere Grenze des kürzesten Vertrauensbereichs
     - Upper limit of the shortest confidence interval
   * - DT*
     - Erkennungsgrenze
     - Decision threshold
   * - DL#
     - Nachweisgrenze
     - Detection limit
   * - NT
     - Nachweisgrenzentyp (sollte nur noch 1 sein, d.h. ISO 11929)
     - Type of detection limit calculation (can only be 1, according to ISO 11929)
   * - k
     - Erweiterungsfaktor für die Unsicherheit
     - Coverage factor k for the uncertainty
   * - kalpha
     - Wert von k₁₋α
     - Value of k₁₋α
   * - kbeta
     - Wert von k₁₋β
     - Value of k₁₋β
   * - 1-gamma
     - Wahrscheinlichkeit 1-γ für das Vertrauensintervall
     - Confidence interval related probability
   * - Chisqr
     - Reduziertes Chi-Quadrat (im Falle linearer Entfaltung)
     - Reduced Chi-square value (in case of linear unfolding)
