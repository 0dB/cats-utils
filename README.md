# cats-utils
Transforming data pasted from an Excel pivot-table (tab-separated) into a format that can be copied and pasted into CATS. Using `purescript-flare` for the UI.

Features:

- round durations to 2 digits, using German number format (commas instead of periods)
- split input into weeks
- filter out rows in a week that have zero total duration
- insert blank columns where pivot table output from Excel is missing columns because of holidays or weekends
- shift calendar for month by specifying which day of the week the first of the month was

Instructions:

- Copy data from Excel pivot table and paste into the raw input field.
  - First row must contain dates (integers) and may have missing days for holidays and weekends.
  - First column must have account numbers.
  - The table data must be the effort for one account on a specific day (floating point number, using the German format).
  - Efforts will be rounded to 2 digits.
- Align the calendar with the current month using the radio buttons.

Note:

- In the output, for each week, accounts with a total of zero efforts in that week will not be shown.
- Lines in one week are grouped in blocks of six lines each to help organize the copy and paste (CATS will let you add at most six lines at once).
