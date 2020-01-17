# sc1-svm-example-package

This is a small package implementing soft and hard margin SVMs with abitrary kernels using the quadratic programming library quadprog


## Installation

```
library(devtools)
install_github("alessio-b-zak/sv1-svm-example-package")
library(alessiosvm)
```

## Usage

This package can be used by invoking the following function:

```
svm(X=X, 
    classes=classes, 
    C=C, 
    margin_type=margin_type, 
    kernel_function=kernel_function, 
    feature_map=feature_map)
```

where:
  - `X` is a data matrix with observations on rows
  - `classes` are the labels associated with each observation of the data matrix (either `1` or `-1`)
  - `C` is the numeric cost associated with the soft margin classifier (not needed if `margin_type == "hard")
  - `margin_type` is either `hard` or `soft`
  - `kernel_function` is the kernel function to build the kernel matrix (not needed if `margin_type == "hard")
  - `feature_map` is the map corresponding the the induced feature space of the kernel function (not needed if `margin_type == "hard")

`svm()` returns a model object which contains `$prediction_function` which classifies a new data point and `$params` which contains the `w` and `b` parameters associated with `predition_function`
