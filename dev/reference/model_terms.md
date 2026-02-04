# Model term names

Retrieve either the response variable names, the beta variable names, or
beta variable names. If the model is an object of class 'lm', by
default, the beta variable names will include anova significance stars.

## Usage

``` r
model_response_variables(model, data = broom::augment(model))

model_beta_variables(model, data = broom::augment(model))

model_beta_label(model, data = broom::augment(model), lmStars = TRUE)
```

## Arguments

- model:

  model in question

- data:

  equivalent to `broom::augment(model)`

- lmStars:

  boolean that determines if stars are added to labels

## Value

character vector of names
