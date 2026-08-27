# Third-party dependencies and attribution

The repository contains the dissertation's analysis code. It does not vendor package source, proprietary Bloomberg software/data, copyrighted papers, or external report templates.

The implementation calls public R/Python packages through their documented APIs, including Rcpp, Bessel, GIGrvg, GeneralizedHyperbolic, NumPy, pandas, SciPy and PyArrow. Those packages remain governed by their own licences and are installed separately; listing a dependency does not relicense it.

The C++ terminal evaluator is project-specific source compiled through Rcpp. The strict-interior OU–GH numerical code cites algorithms and literature in the dissertation but does not bundle `mpmath`; that package was used only as an external numerical validation reference.

No software licence is currently asserted. The author may select one before or after publication and should then confirm that every retained production module is covered by that choice. Absent a licence, copyright remains with the author and no reuse licence is granted.
