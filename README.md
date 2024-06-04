# oxcelloron

*oxcelloron* is a collection of R scripts designed for data analysis of neuronal cell counts and brain structure areas derived from QuPath (https://github.com/qupath/qupath) output files (analysis based on images of brain sections, identifying neuronal cells stained in immunohistochemical staining).

*oxcelloron* aggregates this data, performs preprocessing, patching, statistical analysis, and data visualization. The tool is user-friendly and highly modifiable.

## Features

-   **Data Aggregation** Combines data from multiple QuPath output files into a single table.

-   **Preprocessing** Cleans and prepares data for analysis.

-   **Patching** Fills missing data points by copying values from the nearest available images.

-   **Statistical Analysis** Performs various statistical analyses on the data.

-   **Data Visualization** Generates visual representations of the data.

-   **Modular Analysis** Supports different analysis types through dedicated directories:

    -   **all6n** Analyzes all six images from each sample.

    -   **bregma2n** Analyzes the two most central images per sample.

    -   **all8hip** Analyzes data targeting the hippocampal region of the brain.
