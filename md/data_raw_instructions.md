The **Raw data** panel displays the input data and associated
validation summaries (once the data have been loaded - that is, once
Stage 2 has been complete). The table above initially has a row for
each of the input files.

The title of each input file name is displayed in the first column
(**File**). The size and file creation time in the next two columns
(fields). The **Sheet** field lists the parsed sheets within the excel
file and the **Status** column indicates whether all the sheets are
valid (<span class="far fa-circle-check"></span>) or not (<span
class="far fa-circle-xmark"></span>).

To the left of the file name there is a black triangle. This is an
content expansion marker. When the triangle points to the right,
clicking anywhere in the cell containing the triangle will expand the
table to reveal additional rows (one for each of the sheets in that
excel file). The rows can be collapsed again by clicking on the cell
containing the downward pointing triangle.

When the additional rows are visible, the **Status**
field icons indicate whether the sheet was valid (<span class="fas
fa-circle-check"></span>) or not (<span class="fas
fa-circle-xmark"></span>).

Clicking on the cell containing the name for a Sheet will make this
the _focal_ Sheet of the **Data** and **Validation** tabs:

- the table in the **Data** tab displays the _focal_ content of the
  input data Sheet. Only the first 10 rows are displayed in the table,
  the others being accessable via the controls under the table.
  
  Note, all numerical values are displayed only to three decimal
  places, yet the actual underlying data is full resolution.
  
- the table in the **Validation** tab displays more details about
  which fields or rows of the _focal_ Sheet failed validation tests.
  
  If there were no validation issues, this table will be empty.
  Otherwise, the description field will indicate the nature of the
  violation and in the case of issues with an individual record, the
  offending row will be presented across the remaining cells in the
  row. For more information about the validation tests, please refer
  to the **Data requirements** box (to the right of this box in the
  app).

Underneath both the Data and Validation tables, there is a **Download
as csv** button. Via this button, you can download a comma separated
text file version of the data in the table for further review in a
spreadsheet of your choice. Once you click this button, you will be
prompted to navigate to a suitable location to store the file.

