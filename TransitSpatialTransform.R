# Must convert block to a higher geography.
# But how? Perhaps just the average is most reliable.
# The original blocks all essentially have a time buffer.
# When you aggregate to tract, block buffers will overlap.

# This is going to be interesting.

# Pseudocode:
# 1.  Upload .csv directory.
# 2.  Filter out .csvs that you don't plan to study.
# 3.  Keep only the 11-digit tract FIPS.
# 4.  Aggregate by tract FIPS, function = mean.
# 5.  Filter out tracts you don't plan to study:
#     these files might have larger extent than MSAs.
# 6.  Split file into separate data frames by MSA.
# 7.  Upload nationwide tract shapefile.
#     IN A LOOP:
# 8.  Merge MSA data frames to shapefile.
# 9.  Use sp.na.omit to eliminate empty tracts.
# 10. Export resulting shapefile by name.
