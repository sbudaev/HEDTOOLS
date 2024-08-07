!*******************************************************************************
! SVN $Id: BASE_STRINGS.f90 16153 2024-07-05 15:26:12Z sbu062 $
!*******************************************************************************
! PURPOSE:
! Fortran Character String Utilities
! A collection of string manipulation routines is contained in the module
! ‘strings’ found in the file stringmod.f90. To obtain this module as well as
! some other string utilities, go to the website
!     http://www.gbenthien.net/strings/index.html.
! To use the routines in the module ‘strings’ the user needs to add the
! statement
!     Use strings
! to the top of the program. These routines were developed primarily to aid
! in the reading and manipulation of input data from an ASCII text file.
!*******************************************************************************
module base_strings

implicit none

! Real kinds
integer, parameter :: kr4=selected_real_kind(6,37)   ! single precision real
integer, parameter :: kr8=selected_real_kind(15,307) ! double precision real

! Integer kinds
integer, parameter :: ki4=selected_int_kind(9)       ! single precision integer
integer, parameter :: ki8=selected_int_kind(18)      ! double precision integer

!Complex kinds
integer, parameter :: kc4 = kr4                      ! single precision complex
integer, parameter :: kc8 = kr8                      ! double precision complex


private :: kr4,kr8,ki4,ki8,kc4,kc8

private :: VALUE_DR,VALUE_SR,VALUE_DI,VALUE_SI
private :: WRITE_DR,WRITE_SR,WRITE_DI,WRITE_SI
private :: WRITEQ_DR,WRITEQ_SR,WRITEQ_DI,WRITEQ_SI

interface VALUE  ! Generic operator for converting a number string to a
                 ! number. Calling syntax is 'call value(numstring,number,ios)'
                 ! where 'numstring' is a number string and 'number' is a
                 ! real number or an integer (single or double precision).
   module procedure VALUE_DR
   module procedure VALUE_SR
   module procedure VALUE_DI
   module procedure VALUE_SI
end interface

interface WRITENUM  ! Generic  interface for writing a number to a string. The
                    ! number is left justified in the string. The calling syntax
                    ! is 'call writenum(number,string,format)' where 'number' is
                    ! a real number or an integer, 'string' is a character string
                    ! containing the result, and 'format' is the format desired,
                    ! e.g., 'e15.6' or 'i5'.
   module procedure WRITE_DR
   module procedure WRITE_SR
   module procedure WRITE_DI
   module procedure WRITE_SI
end interface

interface WRITEQ  ! Generic interface equating a name to a numerical value. The
                  ! calling syntax is 'call writeq(unit,name,value,format)' where
                  ! unit is the integer output unit number, 'name' is the variable
                  ! name, 'value' is the real or integer value of the variable,
                  ! and 'format' is the format of the value. The result written to
                  ! the output unit has the form <name> = <value>.
   module procedure writeq_dr
   module procedure writeq_sr
   module procedure writeq_di
   module procedure writeq_si
end interface


!**********************************************************************

contains

!**********************************************************************

pure subroutine PARSE(str_in,delims,args,nargs)

! Parses the string 'str' into arguments args(1), ..., args(nargs) based on
! the delimiters contained in the string 'delims'. Preceding a delimiter in
! 'str' by a backslash (\) makes this particular instance not a delimiter.
! The integer output variable nargs contains the number of arguments found.

character(len=*), intent(in)                  :: str_in
character(len=*), intent(in)                  :: delims
character(len=*), dimension(:), intent(inout) :: args

integer(ki4), intent(out) :: nargs
character(len=:), allocatable :: str
integer(ki4) :: i, k, lenstr, na

str=str_in

call COMPACT(str)
na=size(args)
do i=1,na
  args(i)=' '
end do
nargs=0
lenstr=len_trim(str)
if(lenstr==0) return
k=0

do
   if(len_trim(str) == 0) exit
   nargs=nargs+1
   call SPLIT(str,delims,args(nargs))
   call removebksl(args(nargs))
end do

end subroutine PARSE

!**********************************************************************

elemental subroutine COMPACT(str)

! Converts multiple spaces and tabs to single spaces; deletes control characters;
! removes initial spaces.

character(len=*), intent(inout):: str

character(len=1) :: ch
character(len=:), allocatable :: outstr
integer(ki4) :: i, k, ich, isp, lenstr

str=adjustl(str)
lenstr=len_trim(str)
outstr=repeat(' ', len_trim(str))
isp=0
k=0

do i=1,lenstr
  ch=str(i:i)
  ich=iachar(ch)

  select case(ich)

    case(9,32)     ! space or tab character
      if(isp==0) then
        k=k+1
        outstr(k:k)=' '
      end if
      isp=1

    case(33:)      ! not a space, quote, or control character
      k=k+1
      outstr(k:k)=ch
      isp=0

  end select

end do

str=adjustl(outstr)

end subroutine COMPACT

!**********************************************************************

elemental subroutine REMOVESP(str)

! Removes spaces, tabs, and control characters in string str

character(len=*), intent(inout):: str

character(len=1):: ch
character(len=:), allocatable ::outstr

integer(ki4) :: i, k, ich, lenstr

str=adjustl(str)
lenstr=len_trim(str)
outstr=repeat(' ', len_trim(str))
k=0

do i=1,lenstr
  ch=str(i:i)
  ich=iachar(ch)
  select case(ich)
    case(0:32)  ! space, tab, or control character
         cycle
    case(33:)
      k=k+1
      outstr(k:k)=ch
  end select
end do

str=adjustl(outstr)

end subroutine REMOVESP

!**********************************************************************

pure subroutine VALUE_DR(str_in,rnum,ios)

! Converts number string to a double precision real number

character(len=*), intent(in) :: str_in
real(kr8), intent(out)       :: rnum
integer, intent(out)         :: ios

character(len=:), allocatable :: str
integer(ki4) :: ipos, ilen

str=str_in

ilen=len_trim(str)
ipos=scan(str,'Ee')
if(.not.is_digit(str(ilen:ilen)) .and. ipos/=0) then
   ios=3
   return
end if
read(str,*,iostat=ios) rnum

end subroutine VALUE_DR

!**********************************************************************

pure subroutine VALUE_SR(str_in,rnum,ios)

! Converts number string to a single precision real number

character(len=*), intent(in) :: str_in
real(kr4), intent(out)       :: rnum
integer(ki4), intent(out)    :: ios

real(kr8) :: rnumd
character(len=:), allocatable :: str

str=str_in

call value_dr(str,rnumd,ios)
if( abs(rnumd) > huge(rnum) ) then
  ios=15
  return
end if
if( abs(rnumd) < tiny(rnum) ) rnum=0.0_kr4
rnum=rnumd

end subroutine VALUE_SR

!**********************************************************************

pure subroutine VALUE_DI(str_in,inum,ios)

! Converts number string to a double precision integer value

character(len=*), intent(in) ::str_in
integer(ki8), intent(out)    :: inum
integer(ki4), intent(out)    :: ios

real(kr8) :: rnum
character(len=:), allocatable :: str

str=str_in

call VALUE_DR(str,rnum,ios)
if(abs(rnum)>huge(inum)) then
  ios=15
  return
end if
inum=nint(rnum,ki8)

end subroutine VALUE_DI

!**********************************************************************

pure subroutine VALUE_SI(str_in,inum,ios)

! Converts number string to a single precision integer value

character(len=*), intent(in) :: str_in
integer(ki4), intent(out)    :: inum
integer(ki4), intent(out)    :: ios

real(kr8) :: rnum
character(len=:), allocatable :: str

str=str_in

call VALUE_DR(str,rnum,ios)
if(abs(rnum)>huge(inum)) then
  ios=15
  return
end if
inum=nint(rnum,ki4)

end subroutine VALUE_SI

!**********************************************************************

elemental subroutine SHIFTSTR(str,n)

! Shifts characters in in the string 'str' n positions (positive values
! denote a right shift and negative values denote a left shift). Characters
! that are shifted off the end are lost. Positions opened up by the shift
! are replaced by spaces.

character(len=*), intent(inout) :: str
integer(ki4), intent(in)        :: n

integer(ki4) :: nabs, lenstr

lenstr=len(str)
nabs=iabs(n)
if(nabs>=lenstr) then
  str=repeat(' ',lenstr)
  return
end if
if(n<0) str=str(nabs+1:)//repeat(' ',nabs)  ! shift left
if(n>0) str=repeat(' ',nabs)//str(:lenstr-nabs)  ! shift right
return

end subroutine SHIFTSTR

!**********************************************************************

elemental subroutine INSERTSTR(str,strins,loc)

! Inserts the string 'strins' into the string 'str' at position 'loc'.
! Characters in 'str' starting at position 'loc' are shifted right to
! make room for the inserted string. Trailing spaces of 'strins' are
! removed prior to insertion

character(len=*), intent(inout) :: str
character(len=*), intent(in)    :: strins
integer(ki4), intent(in)        :: loc 

character(len=len(str))::tempstr

integer(ki4) :: lenstrins

lenstrins=len_trim(strins)
tempstr=str(loc:)
call shiftstr(tempstr,lenstrins)
tempstr(1:lenstrins)=strins(1:lenstrins)
str(loc:)=tempstr
return

end subroutine INSERTSTR

!**********************************************************************

elemental subroutine DELSUBSTR(str,substr)

! Deletes first occurrence of substring 'substr' from string 'str' and
! shifts characters left to fill hole. Trailing spaces or blanks are
! not considered part of 'substr'.

character(len=*), intent(inout) :: str
character(len=*), intent(in)    :: substr

integer(ki4) :: ipos, lensubstr

lensubstr=len_trim(substr)
ipos=index(str,substr)
if(ipos==0) return
if(ipos == 1) then
   str=str(lensubstr+1:)
else
   str=str(:ipos-1)//str(ipos+lensubstr:)
end if
return

end subroutine DELSUBSTR

!**********************************************************************

elemental subroutine DELALL(str,substr)

! Deletes all occurrences of substring 'substr' from string 'str' and
! shifts characters left to fill holes.

character(len=*), intent(inout) :: str
character(len=*), intent(in)    :: substr

integer(ki4) :: ipos, lensubstr

lensubstr=len_trim(substr)
do
   ipos=index(str,substr)
   if(ipos == 0) exit
   if(ipos == 1) then
      str=str(lensubstr+1:)
   else
      str=str(:ipos-1)//str(ipos+lensubstr:)
   end if
end do
return

end subroutine DELALL

!**********************************************************************

pure function UPPERCASE(str) result(ucstr)

! convert string to upper case

character (len=*), intent(in) :: str
character (len=:), allocatable :: ucstr

integer(ki4) :: i, iqc, iav, iquote, ioffset, ilen

ilen=len_trim(str)
ioffset=iachar('A')-iachar('a')
iquote=0
ucstr=repeat(' ', len_trim(str))
ucstr=str
do i=1,ilen
  iav=iachar(str(i:i))
  if(iquote==0 .and. (iav==34 .or.iav==39)) then
    iquote=1
    iqc=iav
    cycle
  end if
  if(iquote==1 .and. iav==iqc) then
    iquote=0
    cycle
  end if
  if (iquote==1) cycle
  if(iav >= iachar('a') .and. iav <= iachar('z')) then
    ucstr(i:i)=achar(iav+ioffset)
  else
    ucstr(i:i)=str(i:i)
  end if
end do
return

end function UPPERCASE

!**********************************************************************

pure function LOWERCASE(str) result(lcstr)

! convert string to lower case

character (len=*), intent(in) :: str
character (len=:), allocatable :: lcstr

integer(ki4) :: iqc, iav, i, iquote, ioffset, ilen

ilen=len_trim(str)
ioffset=iachar('A')-iachar('a')
iquote=0
lcstr=repeat(' ', len_trim(str))
lcstr=str
do i=1,ilen
  iav=iachar(str(i:i))
  if(iquote==0 .and. (iav==34 .or.iav==39)) then
    iquote=1
    iqc=iav
    cycle
  end if
  if(iquote==1 .and. iav==iqc) then
    iquote=0
    cycle
  end if
  if (iquote==1) cycle
  if(iav >= iachar('A') .and. iav <= iachar('Z')) then
    lcstr(i:i)=achar(iav-ioffset)
  else
    lcstr(i:i)=str(i:i)
  end if
end do
return

end function LOWERCASE

!**********************************************************************

impure subroutine GETFLINE(nunitr,line,ios)

! Reads line from unit=nunitr, ignoring blank lines
! and deleting comments beginning with an exclamation point(!)
! NOTE: This subroutine was named readline originally, but there
!       is another function readline in CSV_IO, so the name is
!       changed to avoid name conflicts, requiring
!       use  CSV_IO, READLINE_LN => READLINE

integer(ki4), intent(in)       :: nunitr
character (len=*), intent(out) :: line
integer(ki4), intent(out)      :: ios

integer(ki4) :: ipos

do
  read(nunitr,'(a)', iostat=ios) line      ! read input line
  if(ios /= 0) return
  line=adjustl(line)
  ipos=index(line,'!')
  if(ipos == 1) cycle
  if(ipos /= 0) line=line(:ipos-1)
  if(len_trim(line) /= 0) exit
end do
return

end subroutine GETFLINE

!**********************************************************************

pure function MATCH(str, ipos) result (imatch)

! Sets imatch to the position in string of the delimiter matching the delimiter
! in position ipos. Allowable delimiters are (), [], {}, <>.
! err is optional error code:
!   1 = not a valid delimiter
!   2 = has no matching delimiter 

character(len=*), intent(in)        :: str
integer(ki4), intent(in)            :: ipos
integer(ki4)                        :: imatch

character :: delim1,delim2,ch
integer(ki4) :: i, isum, inc, istart, iend, idelim2, lenstr

lenstr=len_trim(str)
delim1=str(ipos:ipos)
select case(delim1)
   case('(')
      idelim2=iachar(delim1)+1
      istart=ipos+1
      iend=lenstr
      inc=1
   case(')')
      idelim2=iachar(delim1)-1
      istart=ipos-1
      iend=1
      inc=-1
   case('[','{','<')
      idelim2=iachar(delim1)+2
      istart=ipos+1
      iend=lenstr
      inc=1
   case(']','}','>')
      idelim2=iachar(delim1)-2
      istart=ipos-1
      iend=1
      inc=-1
   case default
      imatch=-1
      !write(*,*) delim1,' is not a valid delimiter'
      return
end select
if(istart < 1 .or. istart > lenstr) then
   imatch=-1
   !write(*,*) delim1,' has no matching delimiter'
   return
end if
delim2=achar(idelim2) ! matching delimiter

isum=1
do i=istart,iend,inc
   ch=str(i:i)
   if(ch /= delim1 .and. ch /= delim2) cycle
   if(ch == delim1) isum=isum+1
   if(ch == delim2) isum=isum-1
   if(isum == 0) exit
end do
if(isum /= 0) then
   imatch=-1  
   !write(*,*) delim1,' has no matching delimiter'
   return
end if
imatch=i

end function MATCH

!**********************************************************************

pure subroutine WRITE_DR(rnum,str,fmt)

! Writes double precision real number rnum to string str using format fmt

real(kr8), intent(in)         :: rnum
character(len=*), intent(out) :: str
character(len=*), intent(in)  :: fmt
character(len=80) :: formt

formt='('//trim(fmt)//')'
write(str,formt) rnum
str=adjustl(str)

end subroutine WRITE_DR

!***********************************************************************

pure subroutine WRITE_SR(rnum,str,fmt)

! Writes single precision real number rnum to string str using format fmt

real(kr4), intent(in)         :: rnum
character(len=*), intent(out) :: str
character(len=*), intent(in)  :: fmt

character(len=80) :: formt

formt='('//trim(fmt)//')'
write(str,formt) rnum
str=adjustl(str)

end subroutine WRITE_SR

!***********************************************************************

pure subroutine WRITE_DI(inum,str,fmt)

! Writes double precision integer inum to string str using format fmt

integer(ki8), intent(in)      :: inum
character(len=*), intent(out) :: str
character(len=*), intent(in)  :: fmt

character(len=80) :: formt

formt='('//trim(fmt)//')'
write(str,formt) inum
str=adjustl(str)

end subroutine WRITE_DI

!***********************************************************************

pure subroutine write_si(inum,str,fmt)

! Writes single precision integer inum to string str using format fmt

integer(ki4), intent(in)      :: inum
character(len=*), intent(out) :: str
character(len=*), intent(in)  :: fmt

character(len=80) :: formt

formt='('//trim(fmt)//')'
write(str,formt) inum
str=adjustl(str)

end subroutine write_si

!***********************************************************************

elemental subroutine TRIMZERO(str)

! Deletes nonsignificant trailing zeroes from number string str. If number
! string ends in a decimal point, one trailing zero is added.

character(len=*), intent(inout) :: str
character :: ch
character(len=10) :: exp

integer(ki4) :: i, lstr, ipos

ipos=scan(str,'eE')
if(ipos>0) then
   exp=str(ipos:)
   str=str(1:ipos-1)
endif
lstr=len_trim(str)
do i=lstr,1,-1
   ch=str(i:i)
   if(ch=='0') cycle
   if(ch=='.') then
      str=str(1:i)//'0'
      if(ipos>0) str=trim(str)//trim(exp)
      exit
   endif
   str=str(1:i)
   exit
end do
if(ipos>0) str=trim(str)//trim(exp)

end subroutine TRIMZERO

!**********************************************************************

impure subroutine WRITEQ_DR(unit,namestr,value,fmt)

! Writes a string of the form <name> = value to unit

integer, intent(in)          :: unit
character(len=*), intent(in) :: namestr
real(kr8),intent(in)         :: value
character(len=*), intent(in) :: fmt

character(len=32) :: tempstr

call writenum(value,tempstr,fmt)
call trimzero(tempstr)
write(unit,*) trim(namestr)//' = '//trim(tempstr)

end subroutine WRITEQ_DR

!**********************************************************************

impure subroutine WRITEQ_SR(unit,namestr,value,fmt)

! Writes a string of the form <name> = value to unit

integer, intent(in)          :: unit
character(len=*), intent(in) :: namestr
real(kr4), intent(in)        :: value
character(len=*), intent(in) :: fmt

character(len=32) :: tempstr

call writenum(value,tempstr,fmt)
call trimzero(tempstr)
write(unit,*) trim(namestr)//' = '//trim(tempstr)

end subroutine WRITEQ_SR

!**********************************************************************

impure subroutine WRITEQ_DI(unit,namestr,ivalue,fmt)

! Writes a string of the form <name> = ivalue to unit

integer, intent(in)          :: unit
character(len=*), intent(in) :: namestr
integer(ki8), intent(in)     :: ivalue
character(len=*), intent(in) :: fmt

character(len=32) :: tempstr

call writenum(ivalue,tempstr,fmt)
call trimzero(tempstr)
write(unit,*) trim(namestr)//' = '//trim(tempstr)

end subroutine WRITEQ_DI

!**********************************************************************

impure subroutine WRITEQ_SI(unit,namestr,ivalue,fmt)

! Writes a string of the form <name> = ivalue to unit
integer, intent(in)          :: unit
character(len=*), intent(in) :: namestr
integer(ki4), intent(in)     :: ivalue
character(len=*), intent(in) :: fmt

character(len=32) :: tempstr
call writenum(ivalue,tempstr,fmt)
call trimzero(tempstr)
write(unit,*) trim(namestr)//' = '//trim(tempstr)

end subroutine WRITEQ_SI

!**********************************************************************

elemental function IS_LETTER(ch) result(res)

! Returns .true. if ch is a letter and .false. otherwise

character, intent(in) :: ch
logical :: res

select case(ch)
case('A':'Z','a':'z')
  res=.true.
case default
  res=.false.
end select
return

end function IS_LETTER

!**********************************************************************

elemental function IS_DIGIT(ch) result(res)

! Returns .true. if ch is a digit (0,1,...,9) and .false. otherwise

character, intent(in) :: ch
logical :: res

select case(ch)
case('0':'9')
  res=.true.
case default
  res=.false.
end select
return

end function IS_DIGIT

!**********************************************************************

pure subroutine SPLIT(str,delims,before,sep)

! Routine finds the first instance of a character from 'delims' in the
! the string 'str'. The characters before the found delimiter are
! output in 'before'. The characters after the found delimiter are
! output in 'str'. The optional output character 'sep' contains the
! found delimiter. A delimiter in 'str' is treated like an ordinary
! character if it is preceded by a backslash (\). If the backslash
! character is desired in 'str', then precede it with another backslash.

character(len=*),   intent(inout) :: str
character(len=*),   intent(in)    :: delims
character(len=*),   intent(out)   :: before
character,optional, intent(out)   :: sep

logical :: pres
character :: ch,cha
	
integer(ki4) :: i, k, ipos, iposa, ibsl, lenstr

pres=present(sep)
str=adjustl(str)
call COMPACT(str)
lenstr=len_trim(str)
if(lenstr == 0) return        ! string str is empty
k=0
ibsl=0                        ! backslash initially inactive
before=' '
do i=1,lenstr
   ch=str(i:i)
   if(ibsl == 1) then          ! backslash active
      k=k+1
      before(k:k)=ch
      ibsl=0
      cycle
   end if
   if(ch == '\') then          ! backslash with backslash inactive
      k=k+1
      before(k:k)=ch
      ibsl=1
      cycle
   end if
   ipos=index(delims,ch)
   if(ipos == 0) then          ! character is not a delimiter
      k=k+1
      before(k:k)=ch
      cycle
   end if
   if(ch /= ' ') then          ! character is a delimiter that is not a space
      str=str(i+1:)
      if(pres) sep=ch
      exit
   end if
   cha=str(i+1:i+1)            ! character is a space delimiter
   iposa=index(delims,cha)
   if(iposa > 0) then          ! next character is a delimiter
      str=str(i+2:)
      if(pres) sep=cha
      exit
   else
      str=str(i+1:)
      if(pres) sep=ch
      exit
   end if
end do
if(i >= lenstr) str=''
str=adjustl(str)              ! remove initial spaces
return

end subroutine SPLIT

!**********************************************************************

elemental subroutine REMOVEBKSL(str)

! Removes backslash (\) characters. Double backslashes (\\) are replaced
! by a single backslash.

character(len=*), intent(inout):: str

character(len=1) :: ch
character(len=:), allocatable ::outstr

integer(ki4) :: i, k, ibsl, lenstr

str=adjustl(str)
lenstr=len_trim(str)
outstr=repeat(' ', len_trim(str))
k=0
ibsl=0                        ! backslash initially inactive

do i=1,lenstr
  ch=str(i:i)
  if(ibsl == 1) then          ! backslash active
   k=k+1
   outstr(k:k)=ch
   ibsl=0
   cycle
  end if
  if(ch == '\') then          ! backslash with backslash inactive
   ibsl=1
   cycle
  end if
  k=k+1
  outstr(k:k)=ch              ! non-backslash with backslash inactive
end do

str=adjustl(outstr)

end subroutine REMOVEBKSL

!**********************************************************************
!**********************************************************************
!**********************************************************************

elemental function IS_NUMERIC(string, include_blank) result (num_flag)
!*******************************************************************************
! IS_NUMERIC
! PURPOSE: Checks if a string is a string representation of a number, i.e.
!          consists only of digits and ".," characters.
! CALL PARAMETERS:
!    Character string
!    Logical flag to include some non-numeric space characters: space and
!     horizontal tab.
! Author: Sergey Budaev
!*******************************************************************************

  character(len=*), intent(in)  :: string
  logical, optional, intent(in) :: include_blank
  logical :: num_flag

  ! Local
  integer :: i
  logical :: include_blank_here
  character, parameter :: TAB =achar(9)
  character, parameter :: DQT =achar(34) ! double quote "
  character, parameter :: SQT =achar(39) ! single quote '


  num_flag = .TRUE.

  if (len_trim(string)==0) then
    num_flag=.FALSE.
    return
  end if

  if(present(include_blank)) then
    include_blank_here = include_blank
  else
    include_blank_here = .FALSE.
  end if

  if (include_blank_here) then
    do i = 1, len_trim(string)
      if ( .not. is_digit(string(i:i)) .and.                                  &
            string(i:i)/="-" .and.                                            &
            string(i:i)/="E" .and.                                            &
            string(i:i)/="." .and.                                            &
            string(i:i)/="," .and.                                            &
            string(i:i)/=" " .and.                                            &
            string(i:i)/=DQT .and.                                            &
            string(i:i)/=SQT .and.                                            &
            string(i:i)/=TAB ) num_flag = .FALSE.
    end do
  else
    do i = 1, len_trim(string)
      if ( .not. is_digit(string(i:i)) .and.                                  &
            string(i:i)/="-" .and.                                            &
            string(i:i)/="E" .and.                                            &
            string(i:i)/="." .and.                                            &
            string(i:i)/="," ) num_flag = .FALSE.
    end do
  end if

end function IS_NUMERIC

!**********************************************************************

elemental subroutine OUTSTRIP(string,set)
!*******************************************************************************
! OUTSTRIP
! PURPOSE: removes (strips) a set of characters from a string
!
! CALL PARAMETERS:
!    Character string to be stripped
!    Character string containing the set of characters to be stripped
!
! Author: Rosetta code
!         https://www.rosettacode.org/wiki/Strip_a_set_of_characters_from_a_string#Fortran
!*******************************************************************************

  character(len=*), intent(inout) :: string
  character(len=*), intent(in)    :: set
  integer                         :: old, new, stride

  old = 1; new = 1
  do
    stride = scan( string( old : ), set )
    if ( stride > 0 ) then
      string( new : new+stride-2 ) = string( old : old+stride-2 )
      old = old+stride
      new = new+stride-1
    else
      string( new : ) = string( old : )
      return
    end if
  end do
end subroutine OUTSTRIP

end module base_strings
