# StepReg NEWS

## Version 1.6.0 (2025-09-29)

### New Features

- **Strata Variables for Cox Regression**: Added support for `strata()` function in Cox regression formulas. This allows users to fit stratified Cox models where separate baseline hazard functions are estimated for different groups while sharing regression coefficients across strata.

- **Continuous-Nested-Within-Class Effects**: Added support for continuous-nested-within-class effects using the `:` operator in formulas. This allows modeling how continuous variables' effects vary across different levels of categorical variables.

 - **train and test validation**: this feature is used for valid inference when `test_ratio` is set between 0-1.

- **feature ratio**:  Proportion of candidate features sampled uniformly at random during forward selection (default = 1). This randomized selection helps identify the best variables while reducing the risk of overfitting, and is only valid when strategy is "forward"..


### Enhancements

- Updated validation functions to properly handle interaction terms with different variable orders (e.g., `X:A` vs `A:X`)
- Enhanced formula parsing to support complex nested effects
- Improved documentation with comprehensive examples for new features
- update vote() to performance() which return a summary of performance of all final models.

### Documentation

- Added new section "Advanced Features" to the vignette with detailed examples
- Updated function documentation with new examples
- Enhanced README with quick start examples for new features
- Updated package description to highlight new capabilities

## Version 1.5.8

- Bug fixes and performance improvements
- Enhanced multicollinearity detection
- Updated documentation

## Version 1.0.0

- Initial CRAN release
- Basic stepwise regression functionality
- Support for linear regression types
- Multiple selection strategies and metrics 