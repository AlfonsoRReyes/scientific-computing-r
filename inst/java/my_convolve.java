public class my_convolve
{            	
     public static double[] convolve(double[] a, double[] b)
     {
         int n1 = a.length;
         int n2 = b.length;
         int n3 = n1+n1-1;
         
         double[] c = new double[n3];
         for(int i=0; i<n1; i++)
            for(int j=0; j<n2; j++) c[i+j] += a[i]*b[j];
            
         return c;
      }   
    }  