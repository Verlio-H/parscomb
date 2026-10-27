module parscomb_maps_mod
   use parscomb_mod, only: pair, size_t
   use iso_fortran_env, only: real64
   implicit none (type, external)

   character(*), parameter :: default_str = '_'

   type :: box
      class(*), allocatable :: val
   end type

   type :: box_array
      type(box), allocatable :: val(:)
   end type
contains
   recursive function count_leaf_chars(input) result(result)
      class(*), allocatable, intent(in) :: input
      integer(size_t) :: result

      if (.not.allocated(input)) then
         result = 0
         return
      end if

      select type (input)
      class default
         result = len(default_str)
      type is (character(*))
         result = len(input)
      type is (pair)
         result = count_leaf_chars(input%a) + count_leaf_chars(input%b)
      end select
   end function

   recursive function count_leaves(input) result(result)
      class(*), allocatable, intent(in) :: input
      integer(size_t) :: result

      if (.not.allocated(input)) then
         result = 0
         return
      end if

      select type (input)
      class default
         result = 1
      type is (pair)
         result = count_leaves(input%a) + count_leaves(input%b)
      end select
   end function

   recursive subroutine string_reduction_helper(input, result)
      class(*), allocatable, intent(in) :: input
      character(:), pointer, intent(inout) :: result

      if (.not.allocated(input)) return

      select type (input)
      class default
         result(:len(default_str)) = default_str
         result => result(len(default_str) + 1:)
      type is (character(*))
         result(:len(input)) = input
         result => result(len(input) + 1:)
      type is (pair)
         call string_reduction_helper(input%a, result)
         call string_reduction_helper(input%b, result)
      end select
   end subroutine

   subroutine string_reduction(input)
      class(*), allocatable, intent(inout) :: input

      character(:), target, allocatable :: true_result
      character(:), pointer :: true_result_ptr
      integer(size_t) :: leaf_count

      leaf_count = count_leaf_chars(input)
      allocate(character(leaf_count) :: true_result)
      true_result_ptr => true_result
      call string_reduction_helper(input, true_result_ptr)
      call move_alloc(true_result, input)
   end subroutine

   recursive subroutine arrayify_helper(input, result)
      class(*), allocatable, intent(inout) :: input
      type(box), pointer, intent(inout) :: result(:)

      if (.not.allocated(input)) return

      select type (val => input)
      class default
         call move_alloc(input, result(1)%val)
         result => result(2:)
      type is (pair)
         call arrayify_helper(val%a, result)
         call arrayify_helper(val%b, result)
      end select
   end subroutine

   subroutine arrayify(input)
      class(*), allocatable, intent(inout) :: input

      type(box_array), target, allocatable :: true_result
      type(box), pointer :: true_result_ptr(:)
      integer(size_t) :: leaf_count

      leaf_count = count_leaves(input)
      true_result = box_array()
      allocate(true_result%val(leaf_count))
      true_result_ptr => true_result%val
      call arrayify_helper(input, true_result_ptr)
      call move_alloc(true_result, input)
   end subroutine

   subroutine atoi(str)
      class(*), allocatable, intent(inout) :: str

      integer(size_t) :: result
      integer :: ios

      if (.not.allocated(str)) return

      select type (s => str)
      type is (character(*))
         read(s, *, iostat=ios) result
         if (ios /= 0) then
            deallocate(str)
            return
         end if
         str = result
      end select
   end subroutine

   subroutine htoi(str)
      class(*), allocatable, intent(inout) :: str

      integer(size_t) :: result
      integer :: ios

      if (.not.allocated(str)) return

      select type (s => str)
      type is (character(*))
         read(s, '(Z)', iostat=ios) result
         if (ios /= 0) then
            deallocate(str)
            return
         end if
         str = result
      end select
   end subroutine

   subroutine itoc(input)
      class(*), allocatable, intent(inout) :: input

      if (.not.allocated(input)) return

      select type (i => input)
      type is (integer(size_t))
         if (i < 0 .or. i > 255) then
            deallocate(input)
         else
            input = achar(i)
         end if
      end select
   end subroutine

   subroutine atof(input)
      class(*), allocatable, intent(inout) :: input
      
      real(real64) :: result
      integer(size_t) :: loc
      integer :: ios

      if (.not.allocated(input)) return
      select type (str => input)
      type is (character(*))
         if (str(:1) == '~') str(:1) = '-'
         loc = index(str, '~', kind=size_t)
         if (loc /= 0_size_t) str(loc:loc) = '-'
         read(str, *, iostat=ios) result
         if (ios /= 0) then
            deallocate(input)
            return
         end if
         input = result
      class default
         error stop
      end select
   end subroutine

end module
