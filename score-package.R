
score_package <- function(
    risk_breaks = c(0.3, 0.7),
    open_dirs = TRUE,
    overwrite = TRUE
){

  # Setup Directories
  score_dir <- tempfile(pattern = "score_dir")
  fs::dir_create(score_dir)
  message(glue::glue("Scoring Directory: {score_dir}"))

  # Open results directory
  if(isTRUE(open_dirs))  rstudioapi::filesPaneNavigate(score_dir)

  pkg_dir <- file.path(score_dir, "pkg_dir")
  out_dir <- file.path(score_dir, "out_dir")
  fs::dir_create(pkg_dir)
  fs::dir_create(out_dir)

  # Build package
  tar_file <- devtools::build(pkg = ".", path = pkg_dir, quiet = TRUE)

  # Score package
  message("Scoring package...")
  results_dir <- mpn.scorecard::score_pkg(
    pkg = tar_file, out_dir = out_dir,
    overwrite = overwrite
  )

  # Traceability Matrix
  message("Making traceability matrix...")
  library(tidyr)
  trac_matrix <- mpn.scorecard::make_traceability_matrix(
    tar_file, results_dir = results_dir
  )

  # Scorecard
  message("Rendering PDF...")
  pdf_path <- mpn.scorecard::render_scorecard(
    results_dir,
    risk_breaks = risk_breaks,
    overwrite = overwrite,
    add_traceability = "auto"
  )

  # Browse PDF
  if(isTRUE(open_dirs)) browseURL(pdf_path)

  return(
    list(
      score_dir = score_dir,
      pdf_path = pdf_path,
      trac_matrix = trac_matrix
    )
  )
}

overwrite_scorecard <- function(pdf_path){
  devtools::load_all()
  scorecard_dir <- system.file("scorecard", package = "pythonR", mustWork = TRUE)
  fs::file_copy(pdf_path, scorecard_dir, overwrite = TRUE)
}

# Create scorecard
score_card <- score_package()
# rstudioapi::filesPaneNavigate(score_card$score_dir)

# Overwrite scorecard
# overwrite_scorecard(score_card$pdf_path)

# Deploy pkgdown site
# devtools::install()
# pkgdown::deploy_to_branch()
