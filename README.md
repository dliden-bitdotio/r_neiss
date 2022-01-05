# Scripts for Downloading and Processing NEISS data

The Consumer Product Safety Commission National Electronic Injury
Surveillance System provides a searchable database of product-related
injuries. Unfortunately, data can only be downloaded in separate files
for each year, and the codebook defining e.g. locations, injuries, and
products can be difficult to use.

These scripts (`process_holidays` and `process_neiss`) download the specified years of data and returns analysis-ready files.

Even more useful: data for 2016-2020 are available for use on
[bit.io](https://bit.io/bitdotio/neiss/).


To download and clean data from 2016-2020, just source the R
script. To customize the years, modify the years in the `main()`
function.
