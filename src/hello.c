#include "hdf5.h"
#define H5FILE_NAME "my_first_file.h5"


int main() {
  hid_t     file_id;    /* file identifier */
  herr_t    status;
  
  
  /* filename */
  printf("creating h5 file: ...");
   
  
  /* Create a new file using default properties. */
  file_id = H5Fcreate(H5FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  
  /* Terminate access to the file. */
  status = H5Fclose (file_id);
  printf(" ...done\n");
}