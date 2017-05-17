# cats-utils
Using `purescript-flare` to transform data pasted from an Excel pivot-table (tab-separated) into a format that can be copied and pasted into CATS.

Features:

* round durations to 2 digits, using German number format (commas instead of periods)
* split input into weeks
* filter out rows in a week that have zero total duration
* insert where pivot table output from Excel is missing columns because of holidays or weekends
* shift calendar for month by specifying which day of the week the first of the month was
