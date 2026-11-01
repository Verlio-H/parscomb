program main
   use parscomb_mod
   use sml_parser_mod
   use iso_fortran_env, only: real64
   implicit none (type, external)
 
   class(parser), pointer :: parse
   class(parse_result), allocatable :: res
   character(:), allocatable :: input_str, fname
   character, allocatable, target :: input(:)
   integer(size_t) :: fname_size
 
   print *, 'Starting parser build'
   call sml_build()
   parse => sml_parser
   print *, 'Parser built'
 
   if (command_argument_count() /= 1) then
      error stop 'Expected a single argument'
   end if

   call get_command_argument(1, length=fname_size)
   allocate(character(fname_size) :: fname)
   call get_command_argument(1, value=fname)
 
   call read_file(input_str, fname)
   if (.not.allocated(input_str)) then
      error stop 'Failed to read file'
   end if
   
   input = transfer(input_str, [character ::])
   res = parse%run(input)
   select type (res)
   type is (parse_success)
      print *, 'successful parse'
      select type (rem => res%remaining)
      type is (character(*))
         print *, 'remaining: ', rem
      end select
      call print_sml(res%result, 0)
   type is (parse_failure)
      print *, 'unsuccessful parse'
      print '(A, A, I0, A, I0, A, A)', &
         fname, ':', count(input(:res%location) == achar(10)) + 1, ':', &
         res%location - findloc(input(:res%location), achar(10), back=.true.), ': ', &
         res%error 
   end select
contains
    recursive subroutine print_sml(input, indent)
       class(*), allocatable, intent(in) :: input
       integer, value, intent(in) :: indent
 
       integer(size_t) :: i
 
       if (.not.allocated(input)) then
          !print '(A, A)', repeat('  ', indent), 'null'
          return
       end if
       select type (input)
       class default
          print '(A, A)', repeat('  ', indent), 'unknown type'
       type is (integer(size_t))
          print '(A, I0)', repeat('  ', indent), input
       type is (real(real64))
          print '(A, F0.5)', repeat('  ', indent), input
       type is (character(*))
          print '(A, A, A, A)', repeat('  ', indent), '"', input, '"'
       type is (pair)
          call print_sml(input%a, indent)
          call print_sml(input%b, indent)
       type is (tag)
          print '(A, A, A)', repeat('  ', indent), input%tag, ':'
          call print_sml(input%val, indent + 1)
       end select
    end subroutine
 
    subroutine read_file(result, fname)
       character(:), allocatable, intent(out) :: result
       character(*), intent(in) :: fname
 
       integer(size_t) :: file_size
       integer :: unit, iostat
 
       iostat = 0
       open(newunit=unit, file=fname, access='stream',  form='unformatted', status='old', iostat=iostat)
       if (iostat /= 0) return
 
       inquire(file=fname, size=file_size)
       
       if (file_size >= 0) then
          allocate(character(file_size) :: result)
          read(unit) result
       end if
 
       close(unit)
    end subroutine
end program main
