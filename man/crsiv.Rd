\name{crsiv}
\alias{crsiv}

\title{
Nonparametric Instrumental Regression
}
\description{
\code{crsiv} computes nonparametric estimation of an instrumental
regression function \eqn{\varphi}{phi} defined by conditional moment
restrictions stemming from a structural econometric model: \eqn{E [Y -
\varphi (Z,X) | W ] = 0}{E [Y - phi (Z,X) | W ] = 0}, and involving
endogenous variables \eqn{Y} and \eqn{Z}, exogenous variables \eqn{X},
and instruments \eqn{W}. The function \eqn{\varphi}{phi} is the solution
of an ill-posed inverse problem.

When \code{method="Tikhonov"}, \code{crsiv} uses the approach of
Darolles, Fan, Florens and Renault (2011) modified for regression
splines (Darolles et al use local constant kernel weighting). When
\code{method="Landweber-Fridman"}, \code{crsiv} uses the approach of
Horowitz (2011) using the regression spline methodology implemented in
the \pkg{crs} package.
}
\usage{
crsiv(y,
      z,
      w,
      x = NULL,
      zeval = NULL,
      weval = NULL,
      xeval = NULL,
      alpha = NULL,
      alpha.min = 1e-10,
      alpha.max = 1e-01,
      alpha.tol = .Machine$double.eps^0.25,
      deriv = 0,
      iterate.max = 1000,
      iterate.diff.tol = 1.0e-08,
      constant = 0.5,
      penalize.iteration = TRUE,
      smooth.residuals = TRUE,
      start.from = c("Eyz","EEywz"),
      starting.values = NULL,
      stop.on.increase = TRUE,
      method = c("Landweber-Fridman","Tikhonov"),
      opts = list("MAX_BB_EVAL"=10000,
                  "EPSILON"=.Machine$double.eps,
                  "INITIAL_MESH_SIZE"="r1.0e-01",
                  "MIN_MESH_SIZE"=paste("r",sqrt(.Machine$double.eps),sep=""),
                  "MIN_POLL_SIZE"=paste("r",1,sep=""),
                  "DISPLAY_DEGREE"=0),
      ...)
}

\arguments{
  \item{y}{
    a one (1) dimensional numeric or integer vector of dependent data, each
    element \eqn{i} corresponding to each observation (row) \eqn{i} of
    \code{z}
  }

  \item{z}{
    a \eqn{p}-variate data frame of endogenous predictors. The data
    types may be continuous, discrete (unordered and ordered factors),
    or some combination thereof
  }
  \item{w}{
    a \eqn{q}-variate data frame of instruments. The data types may be
    continuous, discrete (unordered and ordered factors), or some
    combination thereof
  }
  \item{x}{
    an \eqn{r}-variate data frame of exogenous predictors. The data
    types may be continuous, discrete (unordered and ordered factors),
    or some combination thereof
  }
  \item{zeval}{
    a \eqn{p}-variate data frame of endogenous predictors on which the
    regression will be estimated (evaluation data). By default, evaluation
    takes place on the data provided by \code{z}
  }
  \item{weval}{
    a \eqn{q}-variate data frame of instruments on which the regression
    will be estimated (evaluation data). By default, evaluation
    takes place on the data provided by \code{w}
  }
  \item{xeval}{
    an \eqn{r}-variate data frame of exogenous predictors on which the
    regression will be estimated (evaluation data). By default,
    evaluation takes place on the data provided by \code{x}
  }
  \item{alpha}{
    a numeric scalar that, if supplied, is used rather than numerically
    solving for \code{alpha}, when using \code{method="Tikhonov"}
    }
  \item{alpha.min}{
    minimum of search range for \eqn{\alpha}{alpha}, the Tikhonov
    regularization parameter, when using \code{method="Tikhonov"}
}
  \item{alpha.max}{
    maximum of search range for \eqn{\alpha}{alpha}, the Tikhonov
    regularization parameter, when using  \code{method="Tikhonov"}
  }
\item{alpha.tol}{
  the search tolerance for \code{optimize} when solving for
  \eqn{\alpha}{alpha}, the Tikhonov regularization parameter, 
  when using \code{method="Tikhonov"}
}
\item{iterate.max}{
  an integer indicating the maximum number of iterations permitted
  before termination occurs when using \code{method="Landweber-Fridman"}
}
\item{iterate.diff.tol}{
  the search tolerance for the difference in the stopping rule from
  iteration to iteration when using \code{method="Landweber-Fridman"}
    (disable by setting to zero)
}
\item{constant}{
  the constant to use when using  \code{method="Landweber-Fridman"}
}
\item{method}{
  the regularization method employed (default
   \code{"Landweber-Fridman"}, see Horowitz (2011); see Darolles, Fan,
   Florens and Renault (2011) for details for \code{"Tikhonov"})
 }
\item{penalize.iteration}{ a logical value indicating whether to
    penalize the norm by the number of iterations or not (default
    \code{TRUE})
}    
\item{smooth.residuals}{
  a logical value (defaults to \code{TRUE}) indicating whether to
  optimize bandwidths for the regression of \eqn{y-\varphi(z)}{y-phi(z)}
  on \eqn{w} or for the regression of \eqn{\varphi(z)}{phi(z)} on
  \eqn{w} during iteration
}
\item{start.from}{ a character string indicating whether to start from
  \eqn{E(Y|z)} (default, \code{"Eyz"}) or from \eqn{E(E(Y|z)|z)} (this can
  be overridden by providing \code{starting.values} below)
}  
\item{starting.values}{ a value indicating whether to commence
  Landweber-Fridman assuming
  \eqn{\varphi_{-1}=starting.values}{phi[-1]=starting.values} (proper
  Landweber-Fridman) or instead begin from \eqn{E(y|z)} (defaults to
  \code{NULL}, see details below)
}
\item{stop.on.increase}{
  a logical value (defaults to \code{TRUE}) indicating whether to halt
  iteration if the stopping criterion (see below) increases over the
  course of one iteration (i.e. it may be above the iteration tolerance
  but increased)
}
  \item{opts}{
    arguments passed to the NOMAD solver (see \code{\link{snomadr}} for
    further details)
  }
  \item{deriv}{ an integer \code{l} (default \code{deriv=0}) specifying
  whether to compute the univariate \code{l}th partial derivative for
  each continuous predictor (and difference in levels for each
  categorical predictor) or not and if so what order. Note that if
  \code{deriv} is higher than the spline degree of the associated
  continuous predictor then the derivative will be zero and a warning
  issued to this effect (see important note below)
  }
\item{...}{
  additional arguments supplied to \code{\link{crs}}
}
}
\note{
  Using the option \code{deriv=} computes (effectively) the analytical
  derivative of the estimated \eqn{\varphi(Z,X)}{phi(Z,X)} and not that
  using \code{\link{crsivderiv}}, which instead uses the method of
  Florens and Racine (2012). Though both are statistically consistent,
  practitioners may desire one over the other hence we provide both.
 }
\details{
  Tikhonov regularization requires computation of weight matrices of
  dimension \eqn{n\times n}{n x n} which can be computationally costly
  in terms of memory requirements and may be unsuitable
  (i.e. unfeasible) for large datasets. Landweber-Fridman will be
  preferred in such settings as it does not require construction and
  storage of these weight matrices while it also avoids the need for
  numerical optimization methods to determine \eqn{\alpha}{alpha},
  though it does require iteration that may be equally or even more
  computationally demanding in terms of total computation time.

  When using \code{method="Landweber-Fridman"}, an optimal stopping rule
  based upon \eqn{||E(y|w)-E(\varphi_k(z,x)|w)||^2
  }{||E(y|w)-E(phi(z,x)|w)||^2 } is used to terminate
  iteration. However, if local rather than global optima are encountered
  the resulting estimates can be overly noisy. To best guard against
  this eventuality set \code{nmulti} to a larger number than the default
  \code{nmulti=5} for \code{\link{crs}} when using \code{cv="nomad"} or
  instead use \code{cv="exhaustive"} if possible (this may not be
  feasible for non-trivial problems).

  When using \code{method="Landweber-Fridman"}, iteration will terminate
  when either the change in the value of
  \eqn{||(E(y|w)-E(\varphi_k(z,x)|w))/E(y|w)||^2
  }{||(E(y|w)-E(phi(z,x)|w))/E(y|w)||^2 } from iteration to iteration is
  less than \code{iterate.diff.tol} or we hit \code{iterate.max} or
  \eqn{||(E(y|w)-E(\varphi_k(z,x)|w))/E(y|w)||^2
  }{||(E(y|w)-E(phi(z,x)|w))/E(y|w)||^2 } stops falling in value and
  starts rising.

  When your problem is a simple one (e.g. univariate \eqn{Z}, \eqn{W},
  and \eqn{X}) you might want to avoid \code{cv="nomad"} and instead use
  \code{cv="exhaustive"} since exhaustive search may be feasible (for
  \code{degree.max} and \code{segments.max} not overly large). This will
  guarantee an exact solution for each iteration (i.e. there will be no
  errors arising due to numerical search).

  \code{demo(crsiv)}, \code{demo(crsiv_exog)}, and
  \code{demo(crsiv_exog_persp)} provide flexible interactive
  demonstrations similar to the example below that allow you to modify
  and experiment with parameters such as the sample size, method, and so
  forth in an interactive session.
  
}
\value{
  
  \code{crsiv} returns a \code{\link{crs}} object.  The generic
    functions \code{\link{fitted}} and \code{\link{residuals}} extract
    (or generate) estimated values and residuals. Furthermore, the
    functions \code{\link{summary}}, \code{\link{predict}}, and
    \code{\link{plot}} (options \code{mean=FALSE}, \code{deriv=i} where
    \eqn{i} is an integer, \code{ci=FALSE},
    \code{plot.behavior=c("plot","plot-data","data")}) support objects
    of this type.
    
    See \code{\link{crs}} for details on the return object components.

    In addition to the standard \code{\link{crs}} components,
    \code{crsiv} returns components \code{phi} and either \code{alpha}
    when \code{method="Tikhonov"} or \code{phi}, \code{phi.mat},
    \code{num.iterations}, \code{norm.stop}, \code{norm.value} and
    \code{convergence} when \code{method="Landweber-Fridman"}.
    
}
  
\references{
  
  Carrasco, M. and J.P. Florens and E. Renault (2007), \dQuote{Linear
  Inverse Problems in Structural Econometrics Estimation Based on
  Spectral Decomposition and Regularization,} In: James J. Heckman and
  Edward E. Leamer, Editor(s), Handbook of Econometrics, Elsevier, 2007,
  Volume 6, Part 2, Chapter 77, Pages 5633-5751

  Darolles, S. and Y. Fan and J.P. Florens and E. Renault (2011),
  \dQuote{Nonparametric Instrumental Regression,} Econometrica, 79,
  1541-1565.
  
  Feve, F. and J.P. Florens (2010), \dQuote{The Practice of
  Non-parametric Estimation by Solving Inverse Problems: The Example of
  Transformation Models,} Econometrics Journal, 13, S1-S27.

  Florens, J.P. and J.S. Racine (2012), \dQuote{Nonparametric
  Instrumental Derivatives,} Working Paper.

  Fridman, V. M. (1956), \dQuote{A Method of Successive Approximations
  for Fredholm Integral Equations of the First Kind,} Uspeskhi,
  Math. Nauk., 11, 233-334, in Russian.

  Horowitz, J.L. (2011), \dQuote{Applied Nonparametric Instrumental
  Variables Estimation,} Econometrica, 79, 347-394.
  
  Landweber, L. (1951), \dQuote{An Iterative Formula for Fredholm
  Integral Equations of the First Kind,} American Journal of
  Mathematics, 73, 615-24.

  Li, Q. and J.S. Racine (2007), \emph{Nonparametric Econometrics:
  Theory and Practice,} Princeton University Press.

}
\author{
  Jeffrey S. Racine \email{racinej@mcmaster.ca}, Samuele Centorrino
 \email{samuele.centorrino@univ-tlse1.fr}
}
\note{
This function should be considered to be in \sQuote{beta test} status until further notice.
}

\seealso{
\code{\link[np]{npreg}}, \code{\link{crs}}
}
\examples{
\dontrun{
## This illustration was made possible by Samuele Centorrino
## <samuele.centorrino@univ-tlse1.fr>

set.seed(42)
n <- 1500

## The DGP is as follows:

## 1) y = phi(z) + u

## 2) E(u|z) != 0 (endogeneity present)

## 3) Suppose there exists an instrument w such that z = f(w) + v and
## E(u|w) = 0

## 4) We generate v, w, and generate u such that u and z are
## correlated. To achieve this we express u as a function of v (i.e. u =
## gamma v + eps)

v <- rnorm(n,mean=0,sd=0.27)
eps <- rnorm(n,mean=0,sd=0.05)
u <- -0.5*v + eps
w <- rnorm(n,mean=0,sd=1)

## In Darolles et al (2011) there exist two DGPs. The first is
## phi(z)=z^2 and the second is phi(z)=exp(-abs(z)) (which is
## discontinuous and has a kink at zero).

fun1 <- function(z) { z^2 }
fun2 <- function(z) { exp(-abs(z)) }

z <- 0.2*w + v

## Generate two y vectors for each function.

y1 <- fun1(z) + u
y2 <- fun2(z) + u

## You set y to be either y1 or y2 (ditto for phi) depending on which
## DGP you are considering:

y <- y1
phi <- fun1

## Create an evaluation dataset sorting on z (for plotting)

evaldata <- data.frame(y,z,w)
evaldata <- evaldata[order(evaldata$z),]

## Compute the non-IV regression spline estimator of E(y|z)

model.noniv <- crs(y~z,opts=opts)
mean.noniv <- predict(model.noniv,newdata=evaldata)

## Compute the IV-regression spline estimator of phi(z)

model.iv <- crsiv(y=y,z=z,w=w)
phi.iv <- predict(model.iv,newdata=evaldata)

## For the plots, restrict focal attention to the bulk of the data
## (i.e. for the plotting area trim out 1/4 of one percent from each
## tail of y and z)

trim <- 0.0025

curve(phi,min(z),max(z),
      xlim=quantile(z,c(trim,1-trim)),
      ylim=quantile(y,c(trim,1-trim)),
      ylab="Y",
      xlab="Z",
      main="Nonparametric Instrumental Spline Regression",
      sub=paste("Landweber-Fridman: iterations = ", model.iv$num.iterations,sep=""),
      lwd=1,lty=1)

points(z,y,type="p",cex=.25,col="grey")

lines(evaldata$z,evaldata$z^2 -0.325*evaldata$z,lwd=1,lty=1)

lines(evaldata$z,phi.iv,col="blue",lwd=2,lty=2)

lines(evaldata$z,mean.noniv,col="red",lwd=2,lty=4)

legend(quantile(z,trim),quantile(y,1-trim),
       c(expression(paste(varphi(z),", E(y|z)",sep="")),
         expression(paste("Nonparametric ",hat(varphi)(z))),
         "Nonparametric E(y|z)"),
       lty=c(1,2,4),
       col=c("black","blue","red"),
       lwd=c(1,2,2))
} % end dontrun       
}
\keyword{ instrument }
