% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_ia.r
\name{read.ia}
\alias{read.ia}
\title{read instar analytics export}
\usage{
read.ia(file)
}
\value{
A data.table. column header should match the variable names used in 
the TV package. Variable values should also match thode used in the 
TV package, e.g in id$lab[variable].
}
\description{
Define the Report and its Export in Instar Analytics according to these rules
to allow a predictable file format ready for automatic processing
}
\details{
Always display the report with only one header row. This is the 'long' format,
any further variable is added as rows not as columns, e.g.:

Timebands  |     Dates    | Sample | Universe | AvRch(000) | ...
'Whole Day' | '01.01.2017' |  1995  | 3351.017 | 2784.1940  | ...
'Whole Day' | '02.01.2017' |  1995  | 3351.017 | 2784.1940  | ...
'Whole Day' | '03.01.2017' |  1995  | 3351.017 | 2784.1940  | ...

This has to be defined 
in the 'period' sheet in Instar. If the job ist saved, this display formating
will be retained the next time the job is executed. The export table will be 
written the same way as the period table is formatted.

Make sure to not display any Totals in the report. You can disable Totals in
the 'Template' sheet.

If there are unwanted attributes in the period table select from the menue
report > attributes > channel to deselect columns. Only export columns that
you need

Which seperator to use should not matter when using data.table::fread(),
however ; or , is recommended

'Print labels' is needed otherwise header as well as grouping variable  will 
be missing completely. Use 'repeate every row/column'. Otherwise you have to 
fill empty cells in R using local observation carried forward na.locf().

numbers should be exported with the highest possible precision which is 9 
decimals. This can be set in options.

Exporting to .txt is probably most predictable. Only Export the report, do 
not export Technical Card, which would append in the same file.

For reproducebility save the job and batch, then Technical Card is not needed
}
