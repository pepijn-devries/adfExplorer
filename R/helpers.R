.check_dev <- function(dev, path) {
  path <- unclass(path)
  if (!lapply(path$device, identical, dev) |> unlist() |> all())
    stop("Virtual path needs to point to a path on the same device as 'dev'")
}