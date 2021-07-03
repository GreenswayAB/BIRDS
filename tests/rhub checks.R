library(rhub)
validate_email()
mycheck <- check(path = ".", platform = "solaris-x86-patched")
mycheck$browse()
mycheck$print()
mycheck$livelog()
mycheck$urls()

r-oldrel-windows-ix86+x86_64
r-patched-solaris-x86

check_for_cran()
