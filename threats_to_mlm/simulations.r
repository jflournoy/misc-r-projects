---
title: "Threats to inference from MLMs"
author: "John C. Flournoy"
date: "October 12, 2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

The purpose of these simulations is to get a sense for threats to interpretting a very simple null model under two conditions. The first condition is when one estimates the association between two variables by computing the first variable by extracting the grouping-level random effects from a hierarchical linear model and estimate the correlation between that variable and the second variable of interest. The seconds