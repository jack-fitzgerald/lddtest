{smcl}
{it:v. 1.0.0} 


{title:lddtest}

{p 4 4 2}
{bf:tsti} Logarithmic density discontinuity equivalence testing for regression discontinuity designs in Stata

{title:lddtest}

{title:Syntax}

{p 8 8 2} {bf:lddtest} {it:estimate} {it:se} {it:rope_lb} {it:rope_ub} [, df({it:real}) alpha({it:real}) power({it:real})]


{p 4 4 2}{bf:Arguments}

{col 5}{it:Argument}{col 21}{it:Description}
{space 4}{hline}
{col 5}{it:estimate}{col 21}The estimate of interest.
{col 5}{it:se}{col 21}The standard error of the estimate of interest. Must be > 0.
{col 5}{it:rope_lb}{col 21}Lower bound of the region of practical equivalence (ROPE). Must be < rope_ub.
{col 5}{it:rope_lb}{col 21}Upper bound of the region of practical equivalence (ROPE). Must be > rope_lb.
{space 4}{hline}

{p 4 4 2}{bf:Options}

{col 5}{it:Option}{col 21}{it:Description}
{space 4}{hline}
{col 5}df({it:real}){col 21}Degrees of freedom of the estimate of interest. If specified, must be > 0.
{col 5}{col 21}Exact (rather than asymptotically approximate) bounds and testing results are produced if this option is specified.
{col 5}alpha({it:real}){col 21}The significance level of the test. Defaults to 0.05. If specified, it must be true that 0 < alpha < 0.5.
{col 5}power({it:real}){col 21}The power level of the test. Defaults to 0.8. If specified, it must be true that 0.5 < power < 1.
{space 4}{hline}

{title:Author}

{p 4 4 2}
Jack Fitzgerald     {break}
Vrije Universiteit Amsterdam     {break}
j.f.fitzgerald@vu.nl    {break}
{browse "https://jack-fitzgerald.github.io":https://jack-fitzgerald.github.io} 

{References}
Fitzgerald, Jack (2024). "Manipulation Tests in Regression Discontinuity Design: The Need for Equivalence Testing in Economics". Working paper. https://jack-fitzgerald.github.io/files/RDD_Equivalence.pdf.
Manipulation of the running variable in the regression discontinuity design: A density test. Journal of Econometrics 142(2), 698-714.
