.check_dev <- function(dev, virtual_path) {
  virtual_path <- unclass(virtual_path)
  if (!lapply(virtual_path$device, identical, dev) |> unlist() |> all())
    stop("Virtual path needs to point to a path on the same device as 'dev'")
}