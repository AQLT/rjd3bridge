# Convert 'RJDemetra' (v2) Objects to 'rjd3' (v3) Format

Converts seasonal adjustment models, outputs, or specification objects
from legacy 'RJDemetra' ('JDemetra+' v2) to the modern 'rjd3' ecosystem
(such as packages `rjd3x13` or `rjd3tramoseats` for 'JDemetra+' v3).

## Usage

``` r
as_rjd3(x, new_spec = NULL, ...)

# S3 method for class 'SA_spec'
as_rjd3(x, new_spec = NULL, ...)

# S3 method for class 'regarima'
as_rjd3(x, new_spec = NULL, ...)

# S3 method for class 'SA'
as_rjd3(x, new_spec = NULL, ...)

# S3 method for class 'regarima_spec'
as_rjd3(x, new_spec = NULL, ...)
```

## Arguments

- x:

  An object from the 'RJDemetra' package (v2, e.g., of class `SA`,
  `SA_spec`).

- new_spec:

  An optional specification object from the 'rjd3' ecosystem (v3) to be
  used as a base for conversion. If `NULL`, a default specification will
  be created based on the type of model being converted.

- ...:

  Additional arguments passed to specific methods.

## Value

An equivalent object compatible with the 'rjd3' ecosystem (v3
structure).

A list with two elements:

- `context`: An object of class
  [`rjd3toolkit::modelling_context()`](https://rjdverse.github.io/rjd3toolkit/reference/modelling_context.html)
  containing any user-defined variables.

- `result`: The converted object in the 'rjd3' format.

## Details

The parameters `ub1` and `ub2` are not translated since it seems to have
a different convention between versions.

## Examples

``` r
# \donttest{
y <- RJDemetra::ipi_c_eu[, "FR"]
model_v2 <- RJDemetra::x13(y, spec = "RSA5c")
model_v3 <- as_rjd3(model_v2)

# Convert only a specification object
spec_v2 <- RJDemetra::regarima_spec_tramoseats()
spec_v3 <- as_rjd3(spec_v2)
# }
```
