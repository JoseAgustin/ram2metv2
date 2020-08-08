program test7
  use variables
    call logs("testintg function cuenta")
    open(Unit=10,file="est_rama.txt",status="OLD",action="READ")
    print *,cuenta(10)
end program test7
