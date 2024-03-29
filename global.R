

pkg_setup <- function(pkg){
  new_pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new_pkg))
    install.packages(new_pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
