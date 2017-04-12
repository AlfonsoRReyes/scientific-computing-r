      subroutine convolvef77 (x, lx, y, ly, xy)
c
c A basic implementation of convolution algorithm for two vectors
c Here we assume that they are the same length just to simplify things
c I use zero-based arrays here.
c
      integer lx, ly, i, j
      double precision x(0:lx-1), y(0:ly-1), xy(0:lx+ly-2)
      do 20 i = 0, (lx-1) 
         do 15 j = 0, (ly-1) 
            xy(i+j) = xy(i+j) + x(i) * y(j) 
  15     continue  
  20  continue 
      end