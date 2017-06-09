
	subroutine asVecCov1(ia, n, oa)
	! compute oa = ia * ia**T and return as an array
	
	! ia : input array
	!  n : length of input array
	! oa : output array of length n * n
	implicit none
	
	! input/output variables
	integer :: n
	double precision :: ia(n), oa(n*n)
	
	! local variables
	integer :: i, j, ii
	
	! element access of a matrix in column major ordering with 1-based index
	! oa(i + (j-1) * n) = ia(i) * ia(j)
	
	ii = 1
	do j=1,n
		do i=1,n
			oa(ii) = ia(i) * ia(j)
			ii = ii + 1
		end do
	end do
	
	end subroutine asVecCov1
	
	subroutine asVec(im, nr, nc, oa)
	! convert matrix to array
	! reshape might be more efficient?
	
	! ia : input matrix
	! nr : number of rows of im
	! nc : number of columns of im
	! oa : output array of length nr * nc
	implicit none
	
	! input/output variables
	integer :: nr, nc
	double precision :: im(nr,nc), oa(nr * nc)
	
	! local variables
	integer :: i, j
	
	! element access of a matrix in column major ordering with 1-based index
	! oa(i + (j-1) * n) = im(i,j)
	
	do j=1,nc
		do i=1,nr
			oa(i + (j-1) * nr) = im(i,j)
		end do
	end do
		
	end subroutine asVec
	
	subroutine M3(x, mu, nr, nc, C, om)
	! compute the third moment of x
	
	!  x : input matrix of data
	! mu : vector of means to center x
	! nr : number of rows of x
	! nc : number of columns of x
	!  C : temporary matrix of dimension (nc * nc, n)
	! om : output matrix of dimension om(nc, nc * nc)
	
	implicit none
	
	! input/output variables
	integer :: nr, nc
	double precision :: x(nr, nc), mu(nc), om(nc, nc*nc), C(nc*nc, nc)
	
	! local variables
	integer :: i
	double precision :: alpha, beta
	double precision :: centret(nc), tccr(nc * nc)
	
	alpha = 1.d0
	beta = 1.d0
	
	do i=1,nr
		centret = x(i,:) - mu
		! the output we care about here is tccr
		! tccr = as.vector(tcrossprod(centret))
		call asVecCov1(centret, nc, tccr)
		
		! C = tccr * centret**T + C
		! (nc*nc x 1) * (1 x nc) = (nc*nc x nc)
		! C := alpha*op( A )*op( B ) + beta*C
   	! DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
   	! Note that TRANSB="N". B is just an array, so I specify the dimension
   	! of B as 1 x nc with the arguments N, K, and LDB.
		call DGEMM('N', 'N', nc*nc, nc, 1, alpha, tccr, nc*nc, centret, 1, beta, C, nc*nc)
	end do
	
	om = transpose(C) / DBLE(nr)
	
	! C := alpha*op( A )*op( B ) + beta*C
   ! DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
   ! TRANSA : 'N' or 'n',  op( A ) = A; 'T' or 't',  op( A ) = A**T
   ! TRANSB : 'N' or 'n',  op( B ) = A; 'T' or 't',  op( B ) = A**T
   ! M      : number  of rows  of the  matrix op( A )  and of the  matrix  C.
   ! N      : number  of columns of the matrix op( B ) and 
   !          the number of columns of the matrix C.
   ! K      : number of columns of the matrix op( A ) and the number of 
   !          rows of the matrix op( B ).
   ! ALPHA  : specifies the scalar alpha
   ! A      : array of DIMENSION ( LDA, ka ), where ka is k  when  
   !          TRANSA = 'N' or 'n',  and is  m  otherwise.
   ! LDA    : the first dimension of A (i.e. number of rows)
   ! B      : array of DIMENSION ( LDB, kb ), where kb is n  when  
   !          TRANSB = 'N' or 'n',  and is  k  otherwise.
   ! LDB    : first dimension of B (i.e. number of rows of B)
   ! BETA   : specifies the scalar alpha
   ! C      : array of DIMENSION ( LDC, n )
   ! LDC    : first dimension of C
   
	
	end subroutine M3
	
	subroutine M4(x, mu, nr, nc, D, om)
	! compute the fourth moment of x
	
	!  x : input matrix of data
	! mu : vector of means to center x
	! nr : number of rows of x
	! nc : number of columns of x
	!  D : temporary matrix of dimension (nc * nc * nc, n)
	! om : output matrix of dimension om(nc, nc * nc * nc)
	
	implicit none
	
	! input/output variables
	integer :: nr, nc
	double precision :: x(nr, nc), mu(nc), om(nc, nc*nc*nc), D(nc*nc*nc, nc)
	
	! local variables
	integer :: i
	double precision :: alpha, beta, beta1
	double precision :: centret(nc), tccr(nc * nc), tccr2(nc * nc * nc), C(nc*nc, nc)
	
	alpha = 1.d0
	beta = 0.d0
	beta1 = 1.d0
	
	do i=1,nr
		centret = x(i,:) - mu
		! the output we care about here is tccr
		! tccr is an nc^2 x 1 matrix (i.e. 1-d vector or array)
		! tccr = as.vector(tcrossprod(centret))
		call asVecCov1(centret, nc, tccr)
		
      ! C = tcrossprod(tccr, centret)
      call DGEMM('N', 'N', nc*nc, nc, 1, alpha, tccr, nc*nc, centret, 1, beta, C, nc*nc)
		
		! tccr2 is an N^3  x 1 matrix (i.e. 1-d vector or array)
		! convert C to a N^3 array and assign to variable tccr2
		call asVec(C, nc*nc, nc, tccr2)
		
		! D = tccr2 * centret**T + D
		call DGEMM('N', 'N', nc*nc*nc, nc, 1, alpha, tccr2, nc*nc*nc, centret, 1, beta1, D, nc*nc*nc)
	end do
	
	om = transpose(D) / DBLE(nr)
	
	! C := alpha*op( A )*op( B ) + beta*C
   ! DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
   ! TRANSA : 'N' or 'n',  op( A ) = A; 'T' or 't',  op( A ) = A**T
   ! TRANSB : 'N' or 'n',  op( B ) = A; 'T' or 't',  op( B ) = A**T
   ! M      : number  of rows  of the  matrix op( A )  and of the  matrix  C.
   ! N      : number  of columns of the matrix op( B ) and 
   !          the number of columns of the matrix C.
   ! K      : number of columns of the matrix op( A ) and the number of 
   !          rows of the matrix op( B ).
   ! ALPHA  : specifies the scalar alpha
   ! A      : array of DIMENSION ( LDA, ka ), where ka is k  when  
   !          TRANSA = 'N' or 'n',  and is  m  otherwise.
   ! LDA    : the first dimension of A (i.e. number of rows)
   ! B      : array of DIMENSION ( LDB, kb ), where kb is n  when  
   !          TRANSB = 'N' or 'n',  and is  k  otherwise.
   ! LDB    : first dimension of B (i.e. number of rows of B)
   ! BETA   : specifies the scalar alpha
   ! C      : array of DIMENSION ( LDC, n )
   ! LDC    : first dimension of C
   
	
	end subroutine M4
	