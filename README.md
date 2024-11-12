
# textAnnotatoR: Interactive Text Annotation Tool for R

[![R-CMD-check](https://github.com/chaoliu-cl/textAnnotatoR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/chaoliu-cl/textAnnotatoR/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/textAnnotatoR)](https://CRAN.R-project.org/package=textAnnotatoR)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

## Overview

textAnnotatoR is a comprehensive text annotation tool built with Shiny,
designed to facilitate qualitative data analysis through an intuitive
graphical user interface. It provides researchers, analysts, and
qualitative data scientists with a robust environment for coding text
documents, managing code hierarchies, creating memos, and analyzing
coding patterns. The package supports collaborative research through
standardized annotation formats and provides powerful tools for
comparing two coding sets, analyzing code co-occurrences, and
visualizing coding patterns.

## Key Features

- Interactive text selection and annotation
- Hierarchical code organization with themes
- Real-time memo creation and linking
- Code co-occurrence analysis and visualization
- Multi-coder comparison tools
- Project management capabilities
- Export options for annotated text and analysis results

## Installation

You can install the development version of textAnnotatoR from GitHub
using:

``` r
# Install textAnnotatoR
- From CRAN: 
install.packages('textAnnotatoR')
- From Github:
remotes::install_github("chaoliu-cl/textAnnotatoR")
```

## Quick Start Example

Here’s a basic example of how to launch the annotation interface and
start coding your text:

``` r
library(textAnnotatoR)

# Launch the annotation interface
annotate_gui()
```

This will open the Shiny application in your default web browser. From
there, you can:

1.  Import your text document (supports .txt, .docx, and .pdf)
2.  Select text using the cursor tool
3.  Apply codes and create memos
4.  Organize codes into themes
5.  Analyze coding patterns and co-occurrences

## Package Components

The package is structured around several main components:

### Core Interface

- `annotate_gui()`: The main function that launches the interactive
  interface
- Project management tools for saving and loading annotation projects
- Text import and display functionality

### Code Management

- Hierarchical code organization with themes
- Code merging and renaming capabilities
- Color-coded visualization of annotations

### Analysis Tools

- Code frequency analysis
- Co-occurrence analysis with network and heatmap visualizations
- Pattern recognition for code sequences
- Multi-coder comparison tools

### Export Capabilities

- Export annotations in CSV or JSON format
- Save annotated text with highlighting
- Generate analysis reports and visualizations

## Documentation

For more detailed information, please refer to the package vignettes:

``` r
# View available vignettes
browseVignettes("textAnnotatoR")
```

Key vignettes include: - Getting Started with textAnnotatoR - Managing
Code Hierarchies - Analyzing Coding Patterns - Comparing Multiple Coders

## Ecosystem Integration

textAnnotatoR is designed to work seamlessly with the broader R
ecosystem for qualitative data analysis:

- Imports text from common document formats using `readtext`
- Utilizes `data.tree` for efficient hierarchy management
- Leverages `shiny` and `shinydashboard` for the interactive interface
- Integrates with `DT` for data display and manipulation

The package fills a gap in the R qualitative analysis ecosystem by
providing a user-friendly GUI while maintaining programmatic access to
all functionality.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.
For major changes, please open an issue first to discuss what you would
like to change.

## License

This project is licensed under the GPL-3 License.

## Citation

If you use textAnnotatoR in your research, please cite it as:

``` r
citation("textAnnotatoR")
```

## Contact

- Issues: Please report issues on the [GitHub issues
  page](https://github.com/chaoliu-cl/textAnnotatoR/issues)
- Email: <chaoliu@cedarville.edu>
- X: [@X](https://x.com/ChaoLiu77600168)
