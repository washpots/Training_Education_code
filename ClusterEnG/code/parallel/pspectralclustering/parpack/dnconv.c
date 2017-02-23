/* dnconv.f -- translated by f2c (version 20050501).
   You must link the resulting object file with libf2c:
	on Microsoft Windows system, link with libf2c.lib;
	on Linux or Unix systems, link with .../path/to/libf2c.a -lm
	or, if you install libf2c.a in a standard place, with -lf2c -lm
	-- in that order, at the end of the command line, as in
		cc *.o -lf2c -lm
	Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

		http://www.netlib.org/f2c/libf2c.zip
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    integer logfil, ndigit, mgetv0, msaupd, msaup2, msaitr, mseigt, msapps, 
	    msgets, mseupd, mnaupd, mnaup2, mnaitr, mneigh, mnapps, mngets, 
	    mneupd, mcaupd, mcaup2, mcaitr, mceigh, mcapps, mcgets, mceupd;
} debug_;

#define debug_1 debug_

struct {
    integer nopx, nbx, nrorth, nitref, nrstrt;
    real tsaupd, tsaup2, tsaitr, tseigt, tsgets, tsapps, tsconv, tnaupd, 
	    tnaup2, tnaitr, tneigh, tngets, tnapps, tnconv, tcaupd, tcaup2, 
	    tcaitr, tceigh, tcgets, tcapps, tcconv, tmvopx, tmvbx, tgetv0, 
	    titref, trvec;
} timing_;

#define timing_1 timing_

/* Table of constant values */

static doublereal c_b3 = .66666666666666663;

/* ----------------------------------------------------------------------- */
/* \BeginDoc */

/* \Name: dnconv */

/* \Description: */
/*  Convergence testing for the nonsymmetric Arnoldi eigenvalue routine. */

/* \Usage: */
/*  call dnconv */
/*     ( N, RITZR, RITZI, BOUNDS, TOL, NCONV ) */

/* \Arguments */
/*  N       Integer.  (INPUT) */
/*          Number of Ritz values to check for convergence. */

/*  RITZR,  Double precision arrays of length N.  (INPUT) */
/*  RITZI   Real and imaginary parts of the Ritz values to be checked */
/*          for convergence. */
/*  BOUNDS  Double precision array of length N.  (INPUT) */
/*          Ritz estimates for the Ritz values in RITZR and RITZI. */

/*  TOL     Double precision scalar.  (INPUT) */
/*          Desired backward error for a Ritz value to be considered */
/*          "converged". */

/*  NCONV   Integer scalar.  (OUTPUT) */
/*          Number of "converged" Ritz values. */

/* \EndDoc */

/* ----------------------------------------------------------------------- */

/* \BeginLib */

/* \Local variables: */
/*     xxxxxx  real */

/* \Routines called: */
/*     second  ARPACK utility routine for timing. */
/*     dlamch  LAPACK routine that determines machine constants. */
/*     dlapy2  LAPACK routine to compute sqrt(x**2+y**2) carefully. */

/* \Author */
/*     Danny Sorensen               Phuong Vu */
/*     Richard Lehoucq              CRPC / Rice University */
/*     Dept. of Computational &     Houston, Texas */
/*     Applied Mathematics */
/*     Rice University */
/*     Houston, Texas */

/* \Revision history: */
/*     xx/xx/92: Version ' 2.1' */

/* \SCCS Information: @(#) */
/* FILE: nconv.F   SID: 2.3   DATE OF SID: 4/20/96   RELEASE: 2 */

/* \Remarks */
/*     1. xxxx */

/* \EndLib */

/* ----------------------------------------------------------------------- */

/* Subroutine */ int dnconv_(integer *n, doublereal *ritzr, doublereal *ritzi,
	 doublereal *bounds, doublereal *tol, integer *nconv)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;

    /* Builtin functions */
    double pow_dd(doublereal *, doublereal *);

    /* Local variables */
    static integer i__;
    static real t0, t1;
    static doublereal eps23, temp;
    extern doublereal dlapy2_(doublereal *, doublereal *), dlamch_(char *, 
	    ftnlen);
    extern /* Subroutine */ int second_(real *);


/*     %----------------------------------------------------% */
/*     | Include files for debugging and timing information | */
/*     %----------------------------------------------------% */


/* \SCCS Information: @(#) */
/* FILE: debug.h   SID: 2.3   DATE OF SID: 11/16/95   RELEASE: 2 */

/*     %---------------------------------% */
/*     | See debug.doc for documentation | */
/*     %---------------------------------% */

/*     %------------------% */
/*     | Scalar Arguments | */
/*     %------------------% */

/*     %--------------------------------% */
/*     | See stat.doc for documentation | */
/*     %--------------------------------% */

/* \SCCS Information: @(#) */
/* FILE: stat.h   SID: 2.2   DATE OF SID: 11/16/95   RELEASE: 2 */



/*     %-----------------% */
/*     | Array Arguments | */
/*     %-----------------% */

/*     %---------------% */
/*     | Local Scalars | */
/*     %---------------% */


/*     %--------------------% */
/*     | External Functions | */
/*     %--------------------% */

/*     %-----------------------% */
/*     | Executable Statements | */
/*     %-----------------------% */

/*     %-------------------------------------------------------------% */
/*     | Convergence test: unlike in the symmetric code, I am not    | */
/*     | using things like refined error bounds and gap condition    | */
/*     | because I don't know the exact equivalent concept.          | */
/*     |                                                             | */
/*     | Instead the i-th Ritz value is considered "converged" when: | */
/*     |                                                             | */
/*     |     bounds(i) .le. ( TOL * | ritz | )                       | */
/*     |                                                             | */
/*     | for some appropriate choice of norm.                        | */
/*     %-------------------------------------------------------------% */

    /* Parameter adjustments */
    --bounds;
    --ritzi;
    --ritzr;

    /* Function Body */
    second_(&t0);

/*     %---------------------------------% */
/*     | Get machine dependent constant. | */
/*     %---------------------------------% */

    eps23 = dlamch_("Epsilon-Machine", (ftnlen)15);
    eps23 = pow_dd(&eps23, &c_b3);

    *nconv = 0;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* Computing MAX */
	d__1 = eps23, d__2 = dlapy2_(&ritzr[i__], &ritzi[i__]);
	temp = max(d__1,d__2);
	if (bounds[i__] <= *tol * temp) {
	    ++(*nconv);
	}
/* L20: */
    }

    second_(&t1);
    timing_1.tnconv += t1 - t0;

    return 0;

/*     %---------------% */
/*     | End of dnconv | */
/*     %---------------% */

} /* dnconv_ */

