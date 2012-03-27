\name{D}
\alias{antiD}
\alias{D}
\alias{makeAntiDfun}
\alias{numerical.integration}
\title{Derivative and Anti-derivative operators}
\usage{
  D(formula, ..., .hstep = NULL, add.h.control = FALSE)

  antiD(formula, ..., Const = 0)

  makeAntiDfun(.function, .wrt, from, to, .tol, Const)

  numerical.integration(f, wrt, av, args)
}
\arguments{
  \item{formula}{A formula. The right side specifies the
  variable(s) with which to carry out the integration or
  differentiation.  On the left side should be an
  expression or a function that returns a numerical vector
  of the same length as its argument.  The expression can
  contain unbound variables.}

  \item{\dots}{Default values to be given to unbound
  variables in the expression \code{expr}. See examples.#'
  Note that in creating anti-derivative functions, default
  values of "from" and "to" can be assigned.  They are to
  be written with the name of the variable as a prefix,
  e.g. \code{y.from}.}

  \item{Const}{Numerical value for the constant of
  integration.}

  \item{.hstep}{horizontal distance between points used for
  secant slope calculation in numerical derivatives.}

  \item{add.h.control}{logical indicating whether the
  returned derivative function should have an additional
  parameter for setting .hstep.  Meaningful only for
  numerical derivatives.}

  \item{.function}{function to be integrated}

  \item{.wrt}{character string naming the variable of
  integration}

  \item{from}{default value for the lower bound of the
  integral region}

  \item{to}{default value for the upper bound of the
  integral region}

  \item{.tol}{tolerance of the numerical integrator (not
  yet implemented)}

  \item{f}{a function}

  \item{wrt}{character string naming a variable: the var.
  of integration}

  \item{av}{a list of the arguments passed to the function
  calling this}

  \item{args}{default values (if any) for parameterss}
}
\value{
  For derivatives, the return value is a function of the
  variable(s) of differentiation, as well as any other
  symbols used in the expression.  Thus, \code{D(A*x^2 +
  B*y ~ x + y)} will compute the mixed partial with respect
  to x then y (that is, \eqn{\frac{d^2
  f}{dy\;dx}}{d2f/dydx}).  The returned value will be a
  function of x and y, as well as A and B.  In evaluating
  the returned function, it's best to use the named form of
  arguments, to ensure the order is correct.

  a function of the same arguments as the original
  expression, but with the integration variable split into
  "from" and "to" prefaced by the name of the variable,
  e.g. \code{y.from} and \code{y.to}.
}
\description{
  Operators for computing derivatives and anti-derivatives
  as functions.
}
\details{
  \code{D} attempts to find a symbolic derivative for
  simple expressions, but will provide a function that is a
  numerical derivative if the attempt at symbolic
  differentiation is unsuccessful.  The symbolic derivative
  can be of any order (although the expression may become
  unmanageably complex).  The numerical derivative is
  limited to first or second-order partial derivatives
  (including mixed partials). \code{antiD} always does
  numerical integration.

  \code{antiD} returns a function with arguments \code{to}
  and \code{from=0}, the upper and lower bounds of the
  interval of integration w.r.t. the variable of
  integration. There is also an argument, \code{initVal},
  that plays the role of the constant of integration. The
  numerical value of the integral or derivative can be
  found by evaluating that function.
}
\note{
  This function is not intended for direct use.  It
  packages up the numerical anti-differentiation process so
  that the contents of functions produced by \code{antiD}
  look nicer to human readers.
}
\examples{
D(sin(t) ~ t)
D(A*sin(t) ~ t )
D(A*sin(2*pi*t/P) ~ t, A=2, P=10) # default values for parameters.
f <- D(A*x^3 ~ x + x, A=1) # 2nd order partial -- note, it's a function of x
f(x=2)
f(x=2,A=10) # override default value of parameter A
g <- D(f(x=t, A=1)^2 ~ t)  # note: it's a function of t
g(t=1)
gg <- D(f(x=t, A=B)^2 ~ t, B=10)  # note: it's a function of t and B
gg(t=1)
gg(t=1, B=100)
F <- antiD( A*exp(-k*t^2 ) ~ t, A=1, k=0.1)
F(t.from=-Inf, t.to=0)
F(t.from=-Inf, t.to=Inf)
one = makeFun(1~x&y)
by.x = antiD( one(x=x, y=y) ~x )
by.xy = antiD(by.x(x.from=-sqrt(1-y^2), x.to=sqrt(1-y^2), y=y)~y)
by.xy(y.from=-1, y.to=1)
vel <- antiD( -9.8 ~ t  )
pos <- antiD( vel( t.to=t, initVal=v0)~t, Const=50)
pos(0:5, v0=10)
pos(0:5, v0=10, initVal=100)
}

