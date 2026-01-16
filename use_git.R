install.packages(c("usethis", "renv", "gh"))

usethis::use_git()

usethis::use_github()

usethis::use_git_config(
  user.name  = "Sebastian Egana",
  user.email = "sebaegana@gmail.com"
)