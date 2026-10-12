# Convert 'rjd3' Objects to 'RJDemetra' (v2) Format

Converts seasonal adjustment models, outputs, or specifications from the
modern 'rjd3' ecosystem ('JDemetra+' v3) back to legacy 'RJDemetra'
('JDemetra+' v2) objects for backward compatibility and legacy reporting
workflows.

## Usage

``` r
as_rjd2(x, ...)

# S3 method for class 'JD3_TRAMOSEATS_SPEC'
as_rjd2(x, context = NULL, ...)

# S3 method for class 'JD3_TRAMO_SPEC'
as_rjd2(x, context = NULL, ...)

# S3 method for class 'JD3_TRAMOSEATS_OUTPUT'
as_rjd2(
  x,
  context = NULL,
  specification = c("estimation_spec", "result_spec"),
  ...
)

# S3 method for class 'JD3_TRAMO_OUTPUT'
as_rjd2(
  x,
  context = NULL,
  specification = c("estimation_spec", "result_spec"),
  y = NULL,
  ...
)

# S3 method for class 'JD3_X13_SPEC'
as_rjd2(x, context = NULL, ...)

# S3 method for class 'JD3_REGARIMA_SPEC'
as_rjd2(x, context = NULL, ...)

# S3 method for class 'JD3_X11_SPEC'
as_rjd2(x, context = NULL, ...)

# S3 method for class 'JD3_X13_OUTPUT'
as_rjd2(
  x,
  context = NULL,
  specification = c("estimation_spec", "result_spec"),
  ...
)

# S3 method for class 'JD3_REGARIMA_OUTPUT'
as_rjd2(
  x,
  context = NULL,
  specification = c("estimation_spec", "result_spec"),
  y = NULL,
  ...
)
```

## Arguments

- x:

  An object from the 'rjd3' ecosystem (e.g., from packages `rjd3x13` or
  `rjd3tramoseats`).

- ...:

  Additional arguments passed to specific methods.

- context:

  Optional
  [`rjd3toolkit::modelling_context()`](https://rjdverse.github.io/rjd3toolkit/reference/modelling_context.html)
  containing user-defined variables.

- specification:

  The specification of the seasonal adjustment model to use for the
  conversion. Can be `"estimation_spec"` (the default, specification
  used to estimate the model), or `"result_spec"` (the specification
  used to produce the final results).

- y:

  Input time series data. Needed for `as_rjd2.JD3_REGARIMA_OUTPUT()` and
  `as_rjd2.JD3_TRAMO_OUTPUT()`. If not provided, the default year will
  by default be equal to 1.

## Value

An equivalent object compatible with the 'RJDemetra' package (v2
structure).

## Details

The parameters `ub1` and `ub2` are not translated since it seems to have
a different convention between versions.

## Examples

``` r
spec_v3 <- rjd3x13::x13_spec()
spec_v2 <- as_rjd2(spec_v3)

# Specify the modelling context with user-defined variables
regs_td <- rjd3toolkit::td(
  s = y,
  groups = c(1, 2, 0, 4, 5, 6, 3),
  contrasts = TRUE
)
#> Error: object 'y' not found

variables <- list(r = list(
  Monday = regs_td[, 1],
  Tuesday = regs_td[, 2],
  Wednesday = regs_td[, 3],
  Thursday = regs_td[, 4],
  Friday = regs_td[, 5],
  Saturday = regs_td[, 6]
))
#> Error: object 'regs_td' not found
context <- rjd3toolkit::modelling_context(variables = variables)
#> Error: object 'variables' not found
spec_v3 <- rjd3tramoseats::tramoseats_spec() |>
  rjd3toolkit::set_tradingdays(
    option = "UserDefined",
    uservariable = c("r.Monday", "r.Tuesday", "r.Wednesday",
                     "r.Thursday", "r.Friday", "r.Saturday"),
    test = "None"
  )
spec_v2 <- as_rjd2(spec_v3, context = context)
#> Error: object 'context' not found
# \donttest{
# For preprocessing models (RegARIMA or TRAMO),
# the input time series `y` should be provided
# to avoid defaulting the start date of the time series to year 1
y <- rjd3toolkit::ABS$X0.2.09.10.M
model_v3 <- rjd3tramoseats::tramo(y)
as_rjd2(model_v3)
#> y = regression model + arima (0, 1, 2, 0, 1, 1)
#> Log-transformation: yes
#> Coefficients:
#>           Estimate Std. Error
#> Theta(1)   -1.1304      0.047
#> Theta(2)    0.3088      0.047
#> BTheta(1)  -0.6523      0.037
#> 
#>             Estimate Std. Error
#> Mean      -3.044e-05          0
#> Week days -6.643e-04          0
#> 
#> 
#> Residual standard error: 0.00614 on 406 degrees of freedom
#> Log likelihood =  1510, aic = -1410 aicc = -1409, bic(corrected for length) = -10.11
#> 
as_rjd2(model_v3, y = y)
#> y = regression model + arima (0, 1, 1, 0, 1, 1)
#> Log-transformation: yes
#> Coefficients:
#>           Estimate Std. Error
#> Theta(1)   -0.8278      0.028
#> BTheta(1)  -0.4255      0.045
#> 
#>               Estimate Std. Error
#> Monday      -0.0109445      0.003
#> Tuesday      0.0048940      0.004
#> Wednesday    0.0001761      0.003
#> Thursday     0.0132928      0.004
#> Friday      -0.0024801      0.004
#> Saturday     0.0153509      0.004
#> Leap year    0.0410667      0.010
#> Easter [6]   0.0503888      0.007
#> AO (7-2000) -0.1972355      0.030
#> AO (6-2000)  0.1681679      0.030
#> 
#> 
#> Residual standard error: 0.03616 on 399 degrees of freedom
#> Log likelihood = 781.4, aic =  4199 aicc =  4199, bic(corrected for length) = -6.464
#> 
# }
```
