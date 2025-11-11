# Transfer binary data to and from connections

These methods mask the identical functions in the `base` package (see
[`base::readBin()`](https://rdrr.io/r/base/readBin.html),
[`base::readLines()`](https://rdrr.io/r/base/readLines.html),
[`base::readChar()`](https://rdrr.io/r/base/readChar.html),
[`base::writeBin()`](https://rdrr.io/r/base/readBin.html),
[`base::writeLines()`](https://rdrr.io/r/base/writeLines.html) and
[`base::writeChar()`](https://rdrr.io/r/base/readChar.html). They behave
exactly the same as their base counterpart, with the exception that they
can read and write to connections opened with
[`adf_file_con()`](https://pepijn-devries.github.io/adfExplorer/reference/adf_file_con.md).

## Usage

``` r
readBin(
  con,
  what,
  n = 1L,
  size = NA_integer_,
  signed = TRUE,
  endian = .Platform$endian
)

# Default S3 method
readBin(
  con,
  what,
  n = 1L,
  size = NA_integer_,
  signed = TRUE,
  endian = .Platform$endian
)

# S3 method for class 'adf_file_con'
readBin(
  con,
  what,
  n = 1L,
  size = NA_integer_,
  signed = TRUE,
  endian = .Platform$endian
)

readLines(
  con,
  n = -1L,
  ok = TRUE,
  warn = TRUE,
  encoding = "unknown",
  skipNul = FALSE
)

# Default S3 method
readLines(
  con = stdin(),
  n = -1L,
  ok = TRUE,
  warn = TRUE,
  encoding = "unknown",
  skipNul = FALSE
)

# S3 method for class 'adf_file_con'
readLines(
  con,
  n = -1L,
  ok = TRUE,
  warn = TRUE,
  encoding = "unknown",
  skipNul = FALSE
)

writeBin(
  object,
  con,
  size = NA_integer_,
  endian = .Platform$endian,
  useBytes = FALSE
)

# Default S3 method
writeBin(
  object,
  con,
  size = NA_integer_,
  endian = .Platform$endian,
  useBytes = FALSE
)

# S3 method for class 'adf_file_con'
writeBin(
  object,
  con,
  size = NA_integer_,
  endian = .Platform$endian,
  useBytes = FALSE
)

writeLines(text, con, sep = "\n", useBytes = FALSE)

# Default S3 method
writeLines(text, con = stdout(), sep = "\n", useBytes = FALSE)

# S3 method for class 'adf_file_con'
writeLines(text, con = stdout(), sep = "\n", useBytes = FALSE)
```

## Arguments

- con:

  A connection to a file on a virtual ADF device. Such a connection can
  be established with
  [`adf_file_con()`](https://pepijn-devries.github.io/adfExplorer/reference/adf_file_con.md).

- what:

  Either an object whose mode will give the mode of the vector to be
  read, or a character vector of length one describing the mode: one of
  `"numeric"`, `"double"`, `"integer"`, `"int"`, `"logical"`,
  `"complex"`, `"character"`, `"raw"`.

- n:

  numeric. The (maximal) number of records to be read. You can use an
  over-estimate here, but not too large as storage is reserved for `n`
  items.

- size:

  integer. The number of bytes per element in the byte stream. The
  default, `NA_integer_`, uses the natural size. Size changing is not
  supported for raw and complex vectors.

- signed:

  logical. Only used for integers of sizes 1 and 2, when it determines
  if the quantity on file should be regarded as a signed or unsigned
  integer.

- endian:

  The endianness (`"big"` or `"little"`) of the target system for the
  file. Using `"swap"` will force swapping endianness.

- ok:

  logical. Is it OK to reach the end of the connection before `n > 0`
  lines are read? If not, an error will be generated.

- warn:

  logical. Warn if a text file is missing a final or if there are
  embedded s in the file.

- encoding:

  encoding to be assumed for input strings. It is used to mark character
  strings as known to be in Latin-1, UTF-8 or to be bytes: it is not
  used to re-encode the input. To do the latter, specify the encoding as
  part of the connection `con` or via
  [`options`](https://rdrr.io/r/base/options.html)`(encoding=)`: see the
  examples and ‘Details’.

- skipNul:

  logical: should s be skipped?

- object:

  An R object to be written to the connection.

- useBytes:

  See [`writeLines`](https://rdrr.io/r/base/writeLines.html).

- text:

  a character vector.

- sep:

  character string. A string to be written to the connection after each
  line of text.

## Value

Returns `NULL` invisibly
