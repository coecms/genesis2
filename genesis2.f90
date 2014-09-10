program genesis2
  
  use mod_indata

  implicit none

  call init_indata(1, 1)
  call default_indata(1, 1)

  open(50, file='template.scm')
  read(50,INDATA)

  write(*, INDATA)
  close(50)

end program genesis2
