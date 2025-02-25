Extension to several output quantities
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Equation (4) above can also be written as follows:

:math:`u^{2}(y) = \sum_{i}^{}{\sum_{j}^{}{\frac{\partial G}{\partial x_{i}}\frac{\partial G}{\partial x_{j}}u\left( x_{i},x_{j} \right)}}\ `.


Furthermore, by extending this to the case of linear unfolding for more
than one output quantity (y then gets indices :math:`l` and :math:`k`)
this equation becomes:

:math:`u^{2}\left( y_{l},y_{k} \right) = \sum_{i}^{}{\sum_{j}^{}{\frac{\partial Y_{l}}{\partial x_{i}}\frac{\partial Y_{k}}{\partial x_{j}}u\left( x_{i},x_{j} \right)}}\ `
,

There exists an equivalent of this equation in matrix algebra notation,
which indeed is applied in UR. It assumes an *n*-Vector
:math:`\mathbf{X}` and an *m*-Vector **Y**, associated with an
*n*\ ×\ *n* covariance matrix :math:`\mathbf{U}_{\mathbf{x}}` and an
*m*\ ×\ *m* covariance matrix :math:`\mathbf{U}_{\mathbf{y}}`,
respectively. Introducing furthermore an *m*\ ×\ *n* matrix
:math:`\mathbf{Q}` with elements
:math:`Q_{i,k} = \partial Y_{i}/\partial x_{k}\ `, i.e., partial
derivatives,

:math:`\mathbf{Q =}\left\lbrack \begin{array}{r}
\frac{\partial Y_{1}}{\partial x_{1}}\mathbf{\ \ }\frac{\partial Y_{1}}{\partial x_{2}}\mathbf{\ \ \ \ldots\ }\frac{\partial Y_{1}}{\partial x_{n}} \\
\frac{\partial Y_{2}}{\partial x_{1}}\mathbf{\ \ }\frac{\partial Y_{2}}{\partial x_{2}}\mathbf{\ \ \ \ldots\ }\frac{\partial Y_{2}}{\partial x_{2}} \\
\mathbf{\vdots} \\
\frac{\partial Y_{m}}{\partial x_{1}}\mathbf{\ \ }\frac{\partial Y_{m}}{\partial x_{2}}\mathbf{\ \ \ \ldots\ }\frac{\partial Y_{m}}{\partial x_{n}}
\end{array} \right\rbrack` ,

allows writing Eq. (6) as follows:

:math:`\mathbf{U}_{\mathbf{y}} = \mathbf{Q\ }\mathbf{U}_{\mathbf{x}}\mathbf{\ }\mathbf{Q}^{\mathbf{T}}`
.

This equation represents the way of doing uncertainty propagation, which
is used especially when partial derivatives can be calculated
numerically. Note that this equation is one which can, e.g., well be
used within Excel.

Now consider the simple case, that *m*\ =1 and *n*\ =2 holds. This may
occur when doing linear regression with 1 equation and 2 unknowns, where
:math:`\mathbf{U}_{\mathbf{x}}` is the covariance matrix of the two
fitted parameters :math:`x_{1}` and :math:`x_{2}`. The associated
equation is :math:`Y_{1} = x_{1} + x_{2}z` . Then, Eq. (8) becomes (note
that an uncertainty of :math:`z` would require an additional propagation
term):

:math:`\mathbf{Q\ }\mathbf{U}_{\mathbf{x}}\mathbf{\ }\mathbf{Q}^{\mathbf{T}} = \left( \frac{\partial Y_{1}}{\partial x_{1}}\mathbf{,\ \ }\frac{\partial Y_{1}}{\partial x_{2}} \right)\begin{pmatrix}
U_{x,1,1} & U_{x,1,2} \\
U_{x,2,1} & U_{x,2,2}
\end{pmatrix}\left( \begin{array}{r}
\frac{\partial Y_{1}}{\partial x_{1}} \\
\mathbf{\ }\frac{\partial Y_{1}}{\partial x_{2}}
\end{array} \right)\ `

:math:`= \left( \frac{\partial Y_{1}}{\partial x_{1}}\mathbf{,\ \ }\frac{\partial Y_{1}}{\partial x_{2}} \right)\left( \begin{array}{r}
U_{x,1,1}\frac{\partial Y_{1}}{\partial x_{1}}\mathbf{+}U_{x,1,2}\frac{\partial Y_{1}}{\partial x_{2}} \\
\mathbf{\ }U_{x,2,1}\frac{\partial Y_{1}}{\partial x_{1}}\mathbf{+}U_{x,2,2}\frac{\partial Y_{1}}{\partial x_{2}}
\end{array} \right)`

:math:`= U_{x,1,1}\frac{\partial Y_{1}}{\partial x_{1}}\frac{\partial Y_{1}}{\partial x_{1}}\mathbf{+}U_{x,1,2}\frac{\partial Y_{1}}{\partial x_{2}}\frac{\partial Y_{1}}{\partial x_{1}}\mathbf{+}U_{x,2,1}\frac{\partial Y_{1}}{\partial x_{1}}\frac{\partial Y_{1}}{\partial x_{2}}\mathbf{+}U_{x,2,2}\frac{\partial Y_{1}}{\partial x_{2}}\frac{\partial Y_{1}}{\partial x_{2}}`

:math:`= u^{2}\left( x_{1} \right)\left( \frac{\partial Y_{1}}{\partial x_{1}} \right)^{2}\mathbf{+}u\left( x_{1},x_{2} \right)\frac{\partial Y_{1}}{\partial x_{2}}\frac{\partial Y_{1}}{\partial x_{1}}\mathbf{+}u\left( x_{2},x_{1} \right)\frac{\partial Y_{1}}{\partial x_{1}}\frac{\partial Y_{1}}{\partial x_{2}}\mathbf{+}u^{2}\left( x_{2} \right)\left( \frac{\partial Y_{1}}{\partial x_{2}} \right)^{2}`

From this one recognizes the equation (4) being separated into single
terms.