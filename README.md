# HASS512-R-Final
The R scripts, data file and results files here were used in a HASS512 Major Research Project 
submitted in 2023 to the University of New England, Armidale, Australia.

The R scripts are used for the following processes:

Colour classifications:
1. Takes a csv file of Munsell colours and creates a table of related ISCC colour block names, RGB colour identifiers, and CIE Lab co-ords.
2. cluster the colours into 'bins'

Similarity and Networks:
1. calculate the Jaccard similarity matrix
2. plot the similarity scores on a map of Iran - linking sites
3. create a network plot at varying thresholds from the similarity matrix
4. calculate summary statistics on the input data and the resulting matrices

Input file must be a .csv format containing binary presence/absence data with the sites in rows, and attributes in columns.
