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

.. note::

   To avoid conflicts with antivirus software, the file
   UR2_start_xls.bat, which previously was used for starting UncertRadio
   from Excel, is no longer used. Instead, Excel-VBA invokes UncertRadio
   directly, the temporary modification of the Windows-PATH variable is
   also applied directly.
