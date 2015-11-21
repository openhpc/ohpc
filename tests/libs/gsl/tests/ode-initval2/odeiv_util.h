#define DBL_MEMCPY(dest,src,n) memcpy((dest),(src),(n)*sizeof(double))
#define DBL_ZERO_MEMSET(dest,n) memset((dest),0,(n)*sizeof(double))

/* Error estimation safety coefficient for methods that use step
 * doubling for error estimates. Error estimates are multiplied by
 * this constant to ensure that the error of a step is not
 * underestimated. 
 *
 * The default safety value of 8.0 ensures 90% of samples lie within
 * the error (assuming a Gaussian distribution with prior 
 * p(sigma) = 1 / sigma). Value of 1.0 conforms to equation 
 * by Ascher and Petzold (reference: Ascher, U.M., Petzold, L.R.,
 * Computer methods for ordinary differential and
 * differential-algebraic equations, SIAM, Philadelphia, 1998).
 */

#define ODEIV_ERR_SAFETY 8.0
