      subroutine solve_for_zbar0(zbar0,area,f,k,lambda,q,dt,ns)
c this program uses the newton raphson method to solve for the roots of eqns
c The user supplied equation is in the function subprogram called 'func'
c The user supplied derivative of the equation is located in 'dfunc'.
      
      implicit none ! 30/03/2004 ITB 

      integer iter, maxit, ns
      integer*8 dt  ! DGT 5/14/12
      real*8 dif, tol, x, xold, fx, dfx
	real*8 zbar0,area(ns),f(ns),k(ns),lambda(ns),q
c*****************Variable Dictionary***********************************
c x = the current best guess for the location of the root.
c xold = the location of the root from the previous iteration.
c func = the user supplied function of x
c dfunc = the derivative of the function (also user supplied)
c dif = the % difference between x and xold (used for convergence criteria)
c tol = tolerance for convergence
c maxit = maximum number of iterations allowed
c***********************************************************************
c  initialize variables
      iter=0
      dif=1.
      tol=.001 
      maxit = 50

c  put in initial guess for root.
      x = zbar0

c  start loop to perform newton-raphson method
15    continue
        xold = x
	  call get_fx_and_dfx(x,area,f,k,lambda,q,ns,fx,dfx,dt) 
        x= xold - fx/dfx

c  add 1 to # of iterations.  If maximum number of iterations (maxit)
c  is exceeded then stop the program.  
        iter=iter+1
        if(iter .GT. maxit)
     +       write(21,*) 'EXCEEDED MAXIMUM NUMBER OF ITERATIONS'   

c  Check the difference between current guess for the root and the
c  previous (x vs xold).  If % difference is less than the tolerance,
c  convergence is reached.  Otherwise, go back up and repeat the process.
        dif=abs((x-xold)/x)
      if(dif .GT. tol)goto 15
      

	zbar0=x
      end

c*****************Subroutine***********************************
c  user suplied function
	subroutine get_fx_and_dfx(zbar0,area,f,k,lambda,q,ns,fx,dfx,dt)
	
      IMPLICIT NONE ! 30/03/2004 ITB 
      
      integer*8 dt ! DGT 5/14/12
      integer*4 ns,i ! 30/03/2004 itb 
      
      real*8 fx, dfx, temp1
	real*8 zbar0,area(ns),f(ns),k(ns),lambda(ns),q
	
      fx=0.
	dfx=0.
      do 1 i=1,ns
	    temp1=area(i)*k(i)*exp(-lambda(i))*exp(-f(i)*zbar0)
	    fx=fx+temp1/f(i)
	    dfx=dfx-temp1
    1 continue
c at this point q is in units of mm**3/ts but fx is in units of m*mm**2/s
c so divide the flow by the interval length to get compatible units
	fx=fx*1000./3600.-q/dt
	dfx=dfx*1000./3600.
	return
	end
      