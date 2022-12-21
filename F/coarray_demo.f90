PROGRAM hello
  IMPLICIT none
  PRINT *, "hello from", this_image(), " of ", num_images()
END PROGRAM hello
