# Project_Distribution-of-Stock-Returns
**Done in R**

* It is a common assumption that stock returns have normal
distribution.
* The use of normal distribution in financial models is a
convenient simplification, as it allows the application of a
multitude of mathematical and statistical methods.
* Ex. Normal Distribution of Returns is one of the
Black-Scholes Model Assumptions.

**Functions used:**
1. tabs <- getURL(url)
2. t <- readHTMLTable(tabs)
3. getSymbols()
4. quantmode::dailyReturn()
5. tseries::jarque.bera.test()
6. par()
7. barplot()
