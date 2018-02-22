## drawExpression: Visual representation of R abstract syntax and data structure

**Author:** Sylvain Loiseau<br/>
**License:** [BSD_3_clause](https://opensource.org/licenses/BSD-3-Clause)

This package provides a function for drawing graphical visualisations of common
R data structures as well as for drawing steps by step the evaluation of an expression.

# Installation

```{r}
install.packages("drawExpression")
```

```{r}
devtools::install_github("sylvainloiseau/drawExpression")
```

# Usage

```{r}
drawExpression("c(1, 2, 3:5)")
```
