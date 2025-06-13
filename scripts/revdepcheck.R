## revdepcheck::revdep_reset(".")

local({
  dev_revdep <- function(
    pkg = ".",
    ...,
    num_workers = 6,
    timeout_mins = 30,
    all = TRUE,
    width = as.numeric(system("tput cols", intern = TRUE)) - 5,
    install_deps = TRUE
  ) {
    if (system.file(package = "remotes") == "") install.packages("remotes")
    if (system.file(package = "withr") == "") install.packages("withr")
    if (system.file(package = "pkgload") == "") install.packages("pkgload")
    if (system.file(package = "revdepcheck") == "")
      remotes::install_github("r-lib/revdepcheck")
    if (isTRUE(install_deps)) {
      if (system.file(package = "remotes") == "") remotes::install_cran("desc")

      # make sure all direct deps are from CRAN (not remotes) and the latest version
      remotes::install_cran(
        setdiff(
          desc::desc_get_deps(basename(normalizePath(pkg)))$package,
          c(
            "R",
            unname(installed.packages(priority = "base")[,
              "Package",
              drop = TRUE
            ])
          )
        )
      )
    }

    pkg_path <- pkgload::pkg_path(pkg)

    # run in non-interactive process to avoid personalized setup
    callr::r(
      function(pkg_path_, num_workers_, timeout_mins_, ..., width_, all_) {
        withr::with_options(list(width = width_), {
          revdepcheck::revdep_check(
            pkg = pkg_path_,
            ...,
            num_workers = num_workers_,
            timeout = as.difftime(timeout_mins_, units = "mins")
          )
          capture.output({
            revdepcheck::revdep_report_cran(pkg_path_)
          }) %>%
            paste0(collapse = "\n") %>%
            cat(file = file.path(pkg_path_, "revdep/revdep_cran.md"))
          if (isTRUE(all_)) {
            message("Saving all report information (this may take a minute)")
            revdepcheck::revdep_report(pkg_path_, all = all_)
          }
        })
      },
      list(
        pkg_path_ = pkg_path,
        num_workers_ = num_workers,
        timeout_mins_ = timeout_mins,
        ...,
        width_ = width,
        all_ = all
      ),
      show = TRUE
    )
  }

  # revdepcheck
  dev_revdep()
})
