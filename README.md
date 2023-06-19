# rRegressor

This project was undertaken as part of the course **STA3005: Statistical Computing** during Term 2, 2022/2023 at The Chinese University of Hong Kong, Shenzhen (CUHK-SZ). Click this [link](https://dboesono.github.io/rRegressor/articles/rRegressor_vignette.html) to view the package's full documentation.

## Project Overview

`rRegressor` is a R package containing several flexible and simple regression models for supervised learning. With this package, you can easily fit linear, polynomial, ridge, lasso, and k-nearest neighbor regression models, as well as generate predictions and calculate Mean Squared Errors (MSE). This package also contains functions to compare regression models based on their MSE. The package also provides functions to plot the correlation between actual value and predicted value and plot the correlation between all variables of the data.

`rRegressor` is built with efficiency and flexibility in mind, allowing you to fit models with custom parameters. With its clear and concise documentation and intuitive API, the package is designed to be user-friendly and accessible to all levels of R users. Whether you're working on a small-scale project or a large-scale data analysis, this package provides the tools you need to get the job done quickly and accurately. We hope that this guide will help you get started with `rRegressor` and unleash the full power of regression analysis in R.

## Team Members

| Student ID |         Student Name         |                                  Email                                   |
|:----------:|:----------------------------:|:------------------------------------------------------------------------:|
| 120040005  |       Celline Williem        | [120040005\@link.cuhk.edu.cn](mailto:120040005@link.cuhk.edu.cn)|
| 120040022  |        Darren Boesono        | [120040022\@link.cuhk.edu.cn](mailto:120040022@link.cuhk.edu.cn)|
| 120040023  | Jefferson Joseph Tedjojuwono | [120040023\@link.cuhk.edu.cn](mailto:120040023@link.cuhk.edu.cn)|

## Installation

To install `rRegressor`, you can use the `install.packages()` function in R. However, since the package is in `tar.gz` file format, you will need to specify the path to the file using the `repos = NULL` argument. Run the following R code to install the package:

``` r
install.packages("/path/to/rRegressor_0.1.0.tar.gz", repos = NULL, type = "source")
```

Note that you will need to replace `/path/to/` with the actual path to the rRegressor package on your local machine. To install the package directly from this Github repository you will need the `devtools` package installed in R. Run the following R code to install the `devtools` package in R:

``` r
install.packages("devtools")
```

Then run the following R code to install the `rRegressor` package:

``` r
devtools::install_github("dboesono/rRegressor")
```

## Load Package

To use `rRegressor`, you first need to load the library into your R session using the `library()` function. Here's an example:

``` r
library(rRegressor)
```

Once you've loaded the library, you can start using the functions it provides. In the next section, we'll take a closer look at the various regression functions that ther package provides, and how to use them to fit and evaluate regression models.

## Load Dataset

One of the dataset included in the package is the Boston dataset, which contains information about housing values in suburbs of Boston. To load this dataset, you can simply call the Boston function in R:

``` r
data("boston_df")
```

This will load the Boston dataset into your R session as a data frame named `boston_df`. The Boston dataset contains the following variables:

-   `crim`: per capita crime rate by town
-   `zn`: proportion of residential land zoned for lots over 25,000 sq.ft.
-   `indus`: proportion of non-retail business acres per town
-   `chas`: Charles River dummy variable (1 if tract bounds river; 0 otherwise)
-   `nox`: nitric oxides concentration (parts per 10 million)
-   `rm`: average number of rooms per dwelling
-   `age`: proportion of owner-occupied units built prior to 1940
-   `dis`: weighted distances to five Boston employment centers
-   `rad`: index of accessibility to radial highways
-   `tax`: full-value property-tax rate per \$10,000
-   `ptratio`: pupil-teacher ratio by town
-   `black`: 1000(Bk - 0.63)\^2 where Bk is the proportion of blacks by town
-   `lstat`: lower status of the population (percent)
-   `medv`: median value of owner-occupied homes in \$1000s
