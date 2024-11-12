## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  fig.align = 'center',
  fig.path = "/man/figures/"
)
library(textAnnotatoR)

## ----eval=FALSE---------------------------------------------------------------
# library(textAnnotatoR)
# annotate_gui()

## ----echo=FALSE, out.width="100%", fig.cap="Main Interface Overview"----------
knitr::include_graphics("../man/figures/interface_overview.png")

## ----echo=FALSE, out.width="80%", fig.cap="New Project Dialog"----------------
knitr::include_graphics("../man/figures/new_project.png")

## ----echo=FALSE, out.width="80%", fig.cap="File Import Interface"-------------
knitr::include_graphics("../man/figures/file_import.png")

## ----eval=FALSE---------------------------------------------------------------
# # Example of code application via the API (if available)
# annotate_text(selected_text, code = "important_theme")

## ----eval=FALSE---------------------------------------------------------------
# # Example of code hierarchy structure
# codes <- list(
#   "Methods" = c("Qualitative", "Quantitative"),
#   "Results" = c("Findings", "Discussion")
# )

## ----eval=FALSE---------------------------------------------------------------
# # Example of code merging logic
# merge_codes(c("code1", "code2"), new_code = "merged_code")

## ----eval=FALSE---------------------------------------------------------------
# # Example of frequency analysis
# analyze_code_frequency(annotations)

## ----echo=FALSE, out.width="90%", fig.cap="Code Co-occurrence Analysis"-------
knitr::include_graphics("../man/figures/co_occurrence.png")

## ----eval=FALSE---------------------------------------------------------------
# # Example of summary statistics
# summary_stats <- analyze_text(text, annotations)
# print(summary_stats)

## ----eval=FALSE---------------------------------------------------------------
# # Example of saving annotations
# save_annotations(annotations, "output.csv")

