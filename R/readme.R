# The only functions that are meant to be exported from this package are:
# * RevenueMetric, which is the hero function.
# * Assorted print and plot functions.
# 
# RevenueMetric has parameters that:
# * Contain data. In particular, data that represents lines on an invoice.
# * Indicate how data is to be processed, where the most central of these is FUN, 
# which determines the function that's used to analyze the data.
# 
# The data and some of the other parameters are then organized in a RevenueData object 
# (via the function RevenueData).
# 
# RevenueData is then sequentially filtedered by all the combinations of the 'profiling' data.
# 
# Each filtered instance is then passed to the select 'FUN'.
# 
# With a few exceptions (e.g., GrowthAccounting), the FUNs are wrappers of a function called 'calculate',
# which has subfuntions distributed throughout the files prefixed with calculate. These functions are then
# turned into an object with a consistent structure via the function 'createOutput', which
# creates an object with appropriate structure.
# 
# A few comments on calculations:
# * An important aspect of how the various functions in the 'calculate' files  are written is a principle of trying 
# to always only have a single function for a single type of operation. This does in cases cause the 
# code to be more verbose and less R-like than is common with traditional R coding. However, it's designed
# to minimize the chance of inconsistent results being generated by different FUNs, which has been found in
# earlier iterations to be common and extremely difficult to track down.
# * Most of the calculations can be thought of as being numerators, denomiators, or ratios.
# 
# The key output of FUN is then shown to the user, based on argument provided to 'output'.