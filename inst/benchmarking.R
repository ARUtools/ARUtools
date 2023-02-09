library(bench)
library(fs)

project_dir <- "../ARUtools - Extra/ARUtools_file_examples"

# fs package faster
bench::mark(t1 <- list.files(project_dir, recursive = TRUE),
            t2 <- fs::dir_ls(project_dir, all = FALSE, recurse = TRUE, type = "file"),
            check = FALSE)


# pattern checking faster in dir_ls rather than after - For selecting small
bench::mark(t1 <- fs::dir_ls(project_dir, all = FALSE, recurse = TRUE, type = "file",
                             regexp = "P71") %>% as.character(),
            t2 <- fs::dir_ls(project_dir, all = FALSE, recurse = TRUE, type = "file") %>%
              stringr::str_subset("P71")
)

# no real different if selecting large
bench::mark(t1 <- fs::dir_ls(project_dir, all = FALSE, recurse = TRUE, type = "file",
                             regexp = "P71", invert = TRUE) %>% as.character(),
            t2 <- fs::dir_ls(project_dir, all = FALSE, recurse = TRUE, type = "file") %>%
              stringr::str_subset("P71", negate = TRUE)
)
