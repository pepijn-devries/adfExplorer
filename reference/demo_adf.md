# Connect with a demonstration ADF file

Opens a connection to a virtual device for demonstration purposes It is
used in examples.

## Usage

``` r
demo_adf(write_protected = TRUE)
```

## Arguments

- write_protected:

  A `logical` value. When `TRUE` you can only read from the virtual
  device. If `FALSE` the demonstration disk will be copied to the
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) and you can write
  to it.

## Value

Returns a connection to a virtual device of class `adf_device`
