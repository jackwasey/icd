# Contributing to `icd`

I'm delighted to welcome code, documentation, tests and bug report from the many `icd` package users. I love feedback on how people are using the package. Feel free to email me or write soemthing in github. If you want `icd` to work differently, or do new things, then the best way is to file a [github issue](https://github.com/jackwasey/icd/issues): there are probably others who have the same or similar needs, and they, or I, may have already thought about them.

A substantial amount of code has now been contributed to the package. Contributions of any kind to `icd` are very welcome. See the [GitHub issues page](https://github.com/jackwasey/icd/issues) to see open issues and feature requests. Documentation, vignettes and examples are very welcome, especially if accompanied by real-world data.

To build `icd`, `Rcpp` must be compiled from source. This happens automatically on Linux, but on Mac and Windows, the following may sometimes be required, especially after upgrading R itself. This is a limitation of the R build system.

```{r eval = FALSE, echo = TRUE}
install.packages("Rcpp", type = "source")
```

## Issues

If you find that an ICD code doesn't give the comorbidity or description you expect, this is an issue! If some code runs too slowly to be useful, this is also an issue. As people run more and more real world data through `icd`, more problems will undoubtedly pop up, and may be trivial to fix, so please share these experiences with other users by making [github issues](https://github.com/jackwasey/icd/issues).

The [github issues](https://github.com/jackwasey/icd/issues) also sketch out future plans and suggestions for the package.

## Coding standards

Contributions should follow the coding standard seen elsewhere in the package, which is defined by both the [general recommendations](http://adv-r.had.co.nz/Style.html) of Hadley Wickham, and the `.lintr` file in the project root directory. I'd prefer that [new dependencies](http://www.tinyverse.org) aren't included unless absolutely necessary.

## Testing

New user-facing code should be covered by the test suite. Code which generates data off-line for inclusion in the package ideally has some unit tests on the code itself, or its output data.

## Security vulnerabilities

In the unlikely event of a security vulnerability, which is not due to R itself, please email me directly and do not file a github issue immediately.

Thanks!

Jack Wasey
