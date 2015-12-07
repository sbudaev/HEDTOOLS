!All Fortran I/O is still record based. Non-advancing I/O allows partial reads
!and writes within a record. For many compilers the default record length is
!very large (e.g., 2147483647) giving the appearance of stream I/O. This is
!not true for all compilers however.
!On some compilers it is possible to set the record length as follows:
!open(unit=6, recl = 2147483646)
!On other compilers unit 6 is preconnected and the record length cannot be
!changed. (Thanks to Jon Richards of the USGS for this tip.)

Program record_length
  use, intrinsic :: ISO_FORTRAN_ENV, only : OUTPUT_UNIT

  open(unit=OUTPUT_UNIT)
  inquire(unit=OUTPUT_UNIT, recl=i)
  print *, 'recl =', i

end program record_length
