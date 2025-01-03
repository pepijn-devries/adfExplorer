library(testthat)
library(adfExplorer) |> suppressMessages()

test_check("adfExplorer")
## make sure to close any unclosed devices and files for test that result in error(s)
close_all_devices()
closeAllConnections()