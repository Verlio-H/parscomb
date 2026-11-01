module parscomb_mod
   use iso_c_binding, only: size_t => c_size_t
   use predicate_mod, only: predicate
   implicit none (type, external)

   type, abstract :: parse_result
   end type

   type, extends(parse_result) :: parse_success
      class(*), allocatable :: result
      class(*), pointer :: remaining(:)
      integer(size_t) :: consumed
   end type

   type, extends(parse_result) :: parse_failure
      character(:), allocatable :: error
      integer(size_t) :: location
   end type

   type :: tag
      character(:), allocatable :: tag
      class(*), allocatable :: val
   end type

   type, abstract :: parser
   contains
      procedure(run_interface), deferred :: run
   end type

   abstract interface
      ! unfortunately parsers cannot be marked pure
      ! this is because they return an allocatable polymorphic result
      recursive function run_interface(this, stream) result(result)
         import parser, parse_result
         class(parser), intent(in) :: this
         class(*), target, intent(in) :: stream(:)
         class(parse_result), allocatable :: result
      end function
   end interface

   interface operator(**)
      procedure :: parser_chain
   end interface

   interface operator(*)
      procedure :: parser_copy
   end interface

   interface operator(.or.)
      procedure :: parser_or
   end interface

   interface operator(.rep.)
      procedure :: parser_rep
   end interface

   interface operator(.oneplus.)
      procedure :: parser_rep1
   end interface

   interface operator(>)
      procedure :: parser_right_chain
   end interface

   interface operator(<)
      procedure :: parser_left_chain
   end interface

   interface operator(.sepby.)
      procedure :: parser_separated_by
   end interface

   interface operator(.opt.)
      procedure :: parser_opt
   end interface

   interface operator(.error.)
      procedure :: parser_error
   end interface

   interface operator(.annotate.)
      procedure :: parser_annotate
   end interface

   interface operator(.no.)
      procedure :: parser_not
   end interface

   interface operator(.nonempty.)
      procedure :: parser_nonempty
   end interface

   interface operatoR(.ifempty.)
      procedure :: parser_ifempty
   end interface

   interface operator(.ref.)
      procedure :: parser_ref
   end interface

   interface operator(.tag.)
      procedure :: parser_tag
   end interface

   interface operator(.has.)
      procedure :: parser_lookahead
   end interface

   interface operator(.lassoc.)
      procedure :: parser_lassoc_unary
      procedure :: parser_lassoc_binary
   end interface

   interface operator(.until.)
      procedure :: parser_until
   end interface

   type :: pair
      class(*), allocatable :: a
      class(*), allocatable :: b
   end type

   type, extends(parser) :: char_parser
      character(:), allocatable :: char
   contains
      procedure :: run => run_char_parser
   end type

   type, extends(parser) :: option_parser
      character(:), allocatable :: options
   contains
      procedure :: run => run_option_parser
   end type

   type, extends(parser) :: predicate_parser
      procedure(predicate), nopass, pointer :: predicate
      character(:), allocatable :: failure_message
   contains
      procedure :: run => run_predicate_parser
   end type


   type, extends(parser) :: or_parser
      class(parser), allocatable :: a
      class(parser), allocatable :: b
   contains
      procedure :: run => run_or_parser
   end type

   type, extends(parser) :: succeed_parser
   contains
      procedure :: run => run_succeed_parser
   end type

   type, extends(parser) :: chain_parser
      class(parser), allocatable :: a
      class(parser), allocatable :: b
   contains
      procedure :: run => run_chain_parser
   end type

   type, extends(parser) :: right_parser
      class(parser), allocatable :: a
      class(parser), allocatable :: b
   contains
      procedure :: run => run_right_parser
   end type

   type, extends(parser) :: left_parser
      class(parser), allocatable :: a
      class(parser), allocatable :: b
   contains
      procedure :: run => run_left_parser
   end type

   type, extends(parser) :: repitition_parser
      class(parser), allocatable :: sub_parser
   contains
      procedure :: run => run_repitition_parser
   end type

   type, extends(parser) :: error_parser
      class(parser), allocatable :: sub_parser
      character(:), allocatable :: new_error
      logical :: keep_original = .false.
   contains
      procedure :: run => run_error_parser
   end type

   type, extends(parser) :: stub_parser
      class(parser), allocatable :: sub_parser
   contains
      procedure :: run => run_stub_parser
   end type

   type, extends(parser) :: ref_parser
      class(parser), pointer :: sub_parser => NULL()
   contains
      procedure :: run => run_ref_parser
   end type

   type, extends(parser) :: str_parser
      character(:), allocatable :: str
   contains
      procedure :: run => run_str_parser
   end type

   type, extends(parser) :: not_parser
      class(parser), allocatable :: sub_parser
   contains
      procedure :: run => run_not_parser
   end type

   type, extends(parser) :: non_empty_parser
      class(parser), allocatable :: sub_parser
      character(:), allocatable :: error
   contains
      procedure :: run => run_non_empty_parser
   end type

   type, extends(parser) :: copy_parser
      integer :: count
      class(parser), allocatable :: sub_parser
   contains
      procedure :: run => run_copy_parser
   end type

   type, extends(parser) :: tag_parser
      class(parser), allocatable :: sub_parser
      character(:), allocatable :: tag
   contains
      procedure :: run => run_tag_parser
   end type

   type, extends(parser) :: lassoc_parser
      class(parser), allocatable :: sub_parser
      character(:), allocatable :: tag
   contains
      procedure :: run => run_lassoc_parser
   end type

   type, extends(parser) :: lookahead_parser
      class(parser), allocatable :: sub_parser
   contains
      procedure :: run => run_lookahead_parser
   end type

   type, extends(parser) :: ternary_parser
      class(parser), allocatable :: test_parser
      class(parser), allocatable :: if_parser
      class(parser), allocatable :: else_parser
   contains
      procedure :: run => run_ternary_parser
   end type

   type, extends(parser) :: until_parser
      class(parser), allocatable :: repeated_parser
      class(parser), allocatable :: end_parser
   contains
      procedure :: run => run_until_parser
   end type

   abstract interface
      subroutine map_input(a)
         class(*), allocatable, intent(inout) :: a
      end subroutine
   end interface

   type, extends(parser) :: map_parser
      class(parser), allocatable :: sub_parser
      procedure(map_input), nopass, pointer :: map_func => NULL()
   contains
      procedure :: run => run_map_parser
   end type
contains
   function parser_chain(a, b) result(result)
      class(parser), intent(in) :: a, b
      type(chain_parser) :: result
      result = chain_parser(a, b)
   end function

   function parser_left_chain(a, b) result(result)
      class(parser), intent(in) :: a, b
      type(left_parser) :: result
      result = left_parser(a, b)
   end function

   function parser_right_chain(a, b) result(result)
      class(parser), intent(in) :: a, b
      type(right_parser) :: result
      result = right_parser(a, b)
   end function

   function parser_or(a, b) result(result)
      class(parser), intent(in) :: a, b
      type(or_parser) :: result
      result = or_parser(a, b)
   end function

   function parser_separated_by(a, b) result(result)
      class(parser), intent(in) :: a, b
      type(chain_parser) :: result
      result = a ** .rep.(b > a)
   end function

   function parser_opt(a) result(result)
      class(parser), intent(in) :: a
      type(or_parser) :: result
      result = a .or. succeed_parser()
   end function

   function parser_error(a, b) result(result)
      class(parser), intent(in) :: a
      character(*), intent(in) :: b
      type(error_parser) :: result
      result = error_parser(a, b)
   end function

   function parser_annotate(a, b) result(result)
      class(parser), intent(in)  :: a
      character(*), intent(in) :: b
      type(error_parser) :: result
      result = error_parser(a, b, .true.)
   end function

   function parser_rep1(a) result(result)
      class(parser), intent(in) :: a
      type(non_empty_parser) :: result
      result = .nonempty.(.rep.a)
   end function

   function parser_not(a) result(result)
      class(parser), intent(in) :: a
      type(not_parser) :: result
      result = not_parser(a)
   end function

   function parser_nonempty(a) result(result)
      class(parser), intent(in) :: a
      type(non_empty_parser) :: result
      result = non_empty_parser(a, 'Missing symbol')
   end function

   function parser_ifempty(a, error) result(result)
      class(parser), intent(in) :: a
      character(*), intent(in) :: error
      type(non_empty_parser) :: result
      result = non_empty_parser(a, error)
   end function

   function parser_ref(a) result(result)
      class(parser), target, intent(in) :: a
      type(ref_parser) :: result
      class(parser), pointer :: ptr
      ptr => a
      result = ref_parser(ptr)
   end function

   function parser_copy(x, a) result(result)
      integer, value, intent(in) :: x
      class(parser), intent(in) :: a
      type(copy_parser) :: result
      result = copy_parser(x, a)
   end function

   function parser_tag(a, t) result(result)
      class(parser), intent(in) :: a
      character(*), intent(in) :: t
      type(tag_parser) :: result
      result = tag_parser(a, t)
   end function

   function parser_lookahead(a) result(result)
      class(parser), intent(in) :: a
      type(lookahead_parser) :: result
      result = lookahead_parser(a)
   end function

   function map(a, b) result(result)
      class(parser), intent(in) :: a
      procedure(map_input) :: b
      type(map_parser) :: result
      result = map_parser(a, b)
   end function

   function parser_rep(input) result(result)
      class(parser), intent(in) :: input
      type(repitition_parser) :: result
      result = repitition_parser(input)
   end function

   function parser_lassoc_unary(input) result(result)
      class(parser), intent(in) :: input
      type(lassoc_parser) :: result
      result = lassoc_parser(input)
   end function

   function parser_lassoc_binary(input, t) result(result)
      class(parser), intent(in) :: input
      character(*), intent(in) :: t
      type(lassoc_parser) :: result
      result = lassoc_parser(input, t)
   end function

   function parser_until(rep, end) result(result)
      class(parser), intent(in) :: rep, end
      type(until_parser) :: result
      result = until_parser(rep, end)
   end function

   function tern(a, b, c) result(result)
      class(parser), intent(in) :: a, b, c
      type(ternary_parser) :: result
      result = ternary_parser(a, b, c)
   end function

   recursive function run_char_parser(this, stream) result(result)   
      class(char_parser), intent(in) :: this
      class(*), target, intent(in) :: stream(:)
      class(parse_result), allocatable :: result

      if (size(stream) == 0) then
         result = parse_failure('End of input reached', 1)
      else
         select type (stream)
         class default
            result = parse_failure('Expected stream of characters', 1)
         type is (character(*))
            if (stream(1) == this%char) then
               result = parse_success(this%char, stream(2:), 1)
            else
               result = parse_failure('Invalid character in input', 1)
            end if
         end select
      end if
   end function

   recursive function run_option_parser(this, stream) result(result)
      class(option_parser), intent(in) :: this
      class(*), target, intent(in) :: stream(:)
      class(parse_result), allocatable :: result

      if (.not.allocated(this%options)) then
         result = parse_failure('Options parser not allocated', 1)
         return
      end if

      if (size(stream) == 0) then
         result = parse_failure('End of input reached', 1)
      else
         select type (stream)
         class default
            result = parse_failure('Expected stream of characters', 1)
         type is (character(*))
            if (index(this%options, stream(1)) /= 0) then
               result = parse_success(stream(1), stream(2:), 1)
            else
               result = parse_failure('Invalid character in input', 1)
            end if
         end select
      end if
   end function

   recursive function run_non_empty_parser(this, stream) result(result)
      class(non_empty_parser), intent(in) :: this
      class(*), target, intent(in) :: stream(:)
      class(parse_result), allocatable :: result

      if (.not.allocated(this%sub_parser)) then
         result = parse_failure('Non-empty parser not allocated', 1)
         return
      end if

      result = this%sub_parser%run(stream)
      select type (res => result)
      type is (parse_success)
         if (.not.allocated(res%result)) then
            result = parse_failure(this%error, 1)
         end if
      end select
   end function

   recursive function run_error_parser(this, stream) result(result)
      class(error_parser), intent(in) :: this
      class(*), target, intent(in) :: stream(:)
      class(parse_result), allocatable :: result

      if (.not.allocated(this%sub_parser)) then
         result = parse_failure('Error parser not allocated', 1)
         return
      end if

      result = this%sub_parser%run(stream)
      select type (result)
      type is (parse_failure)
         if (this%keep_original) then
            result%error = this%new_error//result%error
         else
            result%error = this%new_error
         end if
      end select
   end function

   recursive function run_lookahead_parser(this, stream) result(result)
      class(lookahead_parser), intent(in) :: this
      class(*), target, intent(in) :: stream(:)
      class(parse_result), allocatable :: result

      if (.not.allocated(this%sub_parser)) then
         result = parse_failure('Lookahead parser not allocated', 1)
         return
      end if

      result = this%sub_parser%run(stream)
      select type (result)
      type is (parse_success)
         result%consumed = 0
         result%remaining => stream
      end select
   end function

   recursive function run_tag_parser(this, stream) result(result)
      class(tag_parser), intent(in) :: this
      class(*), target, intent(in) :: stream(:)
      class(parse_result), allocatable :: result

      if (.not.allocated(this%sub_parser)) then
         result = parse_failure('Tag parser not allocated', 1)
         return
      end if

      result = this%sub_parser%run(stream)
      select type (result)
      type is (parse_success)
         result%result = tag(this%tag, result%result)
      end select
   end function

   recursive function run_not_parser(this, stream) result(result)
      class(not_parser), intent(in) :: this
      class(*), target, intent(in) :: stream(:)
      class(parse_result), allocatable :: result

      class(parse_result), allocatable :: tmp_result

      if (.not.allocated(this%sub_parser)) then
         result = parse_failure('Not parser not allocated', 1)
         return
      end if

      tmp_result = this%sub_parser%run(stream)
      select type (tmp_result)
      type is (parse_success)
         result = parse_failure('Extraneous symbols', 1)
      type is (parse_failure)
         result = parse_success(NULL(), stream, 0)
      end select
   end function

   recursive function run_stub_parser(this, stream) result(result)
      class(stub_parser), intent(in) :: this
      class(*), target, intent(in) :: stream(:)
      class(parse_result), allocatable :: result

      if (.not.allocated(this%sub_parser)) then
         result = parse_failure('Stub parser not allocated', 1)
         return
      end if

      result = this%sub_parser%run(stream)
   end function

   recursive function run_ref_parser(this, stream) result(result)
      class(ref_parser), intent(in) :: this
      class(*), target, intent(in) :: stream(:)
      class(parse_result), allocatable :: result

      if (.not.associated(this%sub_parser)) then
         result = parse_failure('Ref parser not associated', 1)
         return
      end if

      result = this%sub_parser%run(stream)
   end function

   recursive function run_str_parser(this, stream) result(result)
      class(str_parser), intent(in) :: this
      class(*), target, intent(in) :: stream(:)
      class(parse_result), allocatable :: result

      integer(size_t) :: i

      if (.not.allocated(this%str)) then
         result = parse_failure('Str parser string not allocated', 1)
         return
      end if

      select type (stream)
      class default
         result = parse_failure('Input stream is not character stream', 1)
      type is (character(*))
         do i = 1, len(this%str, kind=size_t)
            if (i > size(stream, kind=size_t)) then
               result = parse_failure('Expected: '//this%str, 1)
               return
            end if
            if (stream(i) /= this%str(i:i)) then
               result = parse_failure('Expected: '//this%str, 1)
               return
            end if
         end do
         result = parse_success(this%str, stream(len(this%str) + 1:), len(this%str))
      end select
   end function

   recursive function run_predicate_parser(this, stream) result(result)
      class(predicate_parser), intent(in) :: this
      class(*), target, intent(in) :: stream(:)
      class(parse_result), allocatable :: result

      if (size(stream) == 0) then
         result = parse_failure('End of input reached', 1)
      else
         if (this%predicate(stream(1))) then
            result = parse_success(stream(1), stream(2:), 1)
         else
            result = parse_failure(this%failure_message, 1)
         end if
      end if
   end function

   recursive function run_map_parser(this, stream) result(result)
      class(map_parser), intent(in) :: this
      class(*), target, intent(in) :: stream(:)
      class(parse_result), allocatable :: result

      if (.not.allocated(this%sub_parser)) then
         result = parse_failure('Map parser not allocated', 1)
         return
      end if

      result = this%sub_parser%run(stream)
      select type (result)
      type is (parse_success)
         call this%map_func(result%result)
      end select
   end function
   recursive function run_or_parser(this, stream) result(result)
      class(or_parser), intent(in) :: this
      class(*), target, intent(in) :: stream(:)
      class(parse_result), allocatable :: result

      result = this%a%run(stream)
      select type (res => result)
      type is (parse_failure)
         result = this%b%run(stream)
      end select
   end function

   recursive function run_succeed_parser(this, stream) result(result)
      class(succeed_parser), intent(in) :: this
      class(*), target, intent(in) :: stream(:)
      class(parse_result), allocatable :: result

      result = parse_success(NULL(), stream, 0)
   end function

   recursive function run_chain_parser(this, stream) result(result)
      class(chain_parser), intent(in) :: this
      class(*), target, intent(in) :: stream(:)
      class(parse_result), allocatable :: result

      type(parse_success), allocatable :: true_result
      type(pair), allocatable :: result_pair
      class(parse_result), allocatable :: result1, result2

      if (.not.allocated(this%a) .or. .not.allocated(this%b)) then
         result = parse_failure('Chain parser missing parsers', 1)
         return
      end if

      result1 = this%a%run(stream)
      select type (res1 => result1)
      type is (parse_success)
         result_pair = pair()
         call move_alloc(res1%result, result_pair%a)
         result2 = this%b%run(res1%remaining)
         select type (res2 => result2)
         type is (parse_success)
            call move_alloc(res2%result, result_pair%b)
            true_result = parse_success(NULL(), res2%remaining, res1%consumed + res2%consumed)
            call move_alloc(result_pair, true_result%result)
            call move_alloc(true_result, result)
         type is (parse_failure)
            res2%location = res2%location + res1%consumed
            call move_alloc(result2, result)
         end select
      type is (parse_failure)
         call move_alloc(result1, result)
      end select
   end function

   recursive function run_right_parser(this, stream) result(result)
      class(right_parser), intent(in) :: this
      class(*), target, intent(in) :: stream(:)
      class(parse_result), allocatable :: result

      class(parse_result), allocatable :: result1, result2

      if (.not.allocated(this%a) .or. .not.allocated(this%b)) then
         result = parse_failure('Right parser missing parsers', 1)
         return
      end if

      result1 = this%a%run(stream)
      select type (res1 => result1)
      type is (parse_success)
         result2 = this%b%run(res1%remaining)
         select type (res2 => result2)
         type is (parse_success)
            res2%consumed = res2%consumed + res1%consumed
            call move_alloc(result2, result)
         type is (parse_failure)
            res2%location = res2%location + res1%consumed
            call move_alloc(result2, result)
         end select
      type is (parse_failure)
         call move_alloc(result1, result)
      end select
   end function

   recursive function run_left_parser(this, stream) result(result)
      class(left_parser), intent(in) :: this
      class(*), target, intent(in) :: stream(:)
      class(parse_result), allocatable :: result

      type(parse_success), allocatable :: true_result
      class(parse_result), allocatable :: result1, result2

      if (.not.allocated(this%a) .or. .not.allocated(this%b)) then
         result = parse_failure('Left parser missing parsers', 1)
         return
      end if

      result1 = this%a%run(stream)
      select type (res1 => result1)
      type is (parse_success)
         result2 = this%b%run(res1%remaining)
         select type (res2 => result2)
         type is (parse_success)
            true_result = parse_success(NULL(), res2%remaining, res1%consumed + res2%consumed)
            call move_alloc(res1%result, true_result%result)
            call move_alloc(true_result, result)
         type is (parse_failure)
            res2%location = res2%location + res1%consumed
            call move_alloc(result2, result)
         end select
      type is (parse_failure)
         call move_alloc(result1, result)
      end select
   end function

   recursive function run_copy_parser_helper(this, stream, count) result(result)
      class(copy_parser), intent(in) :: this
      class(*), target, intent(in) :: stream(:)
      integer, value, intent(in) :: count
      class(parse_result), allocatable :: result

      class(parse_result), allocatable :: result1, result2
      type(parse_success), allocatable :: true_result
      type(pair), allocatable :: result_pair

      if (count <= 0) then
         result = parse_success(NULL(), stream, 0)
         return
      end if

      result1 = this%sub_parser%run(stream)
      select type (res1 => result1)
      type is (parse_success)
         result2 = run_copy_parser_helper(this, res1%remaining, count - 1)
         select type (res2 => result2)
         type is (parse_success)
            result_pair = pair()
            call move_alloc(res1%result, result_pair%a)
            call move_alloc(res2%result, result_pair%b)
            true_result = parse_success(NULL(), res2%remaining, res1%consumed + res2%consumed)
            call move_alloc(result_pair, true_result%result)
            call move_alloc(true_result, result)
         type is (parse_failure)
            res2%location = res2%location + res1%consumed
            call move_alloc(result2, result)
         end select
      type is (parse_failure)
         call move_alloc(result1, result)
      end select
   end function

   recursive function run_copy_parser(this, stream) result(result)
      class(copy_parser), intent(in) :: this
      class(*), target, intent(in) :: stream(:)
      class(parse_result), allocatable :: result

      class(parse_result), allocatable :: result1, result2
      type(parse_success), allocatable :: true_result
      type(pair), allocatable :: result_pair

      if (.not.allocated(this%sub_parser)) then
         result = parse_failure('Copy parser not allocated', 1)
         return
      end if

      result = run_copy_parser_helper(this, stream, this%count)
   end function

   recursive subroutine pair_flip(dest, src, t)
      class(*), allocatable, intent(inout) :: dest, src
      character(:), allocatable, intent(in) :: t

      type(pair), allocatable :: tmp_pair
      type(tag), allocatable :: tmp_tag

      if (.not.allocated(src)) return

      select type (s => src)
      class default
         if (.not.allocated(dest)) then
            call move_alloc(src, dest)
         else
            tmp_pair = pair()
            call move_alloc(dest, tmp_pair%a)
            call move_alloc(src, tmp_pair%b)
            if (allocated(t)) then
               tmp_tag = tag(t)
               call move_alloc(tmp_pair, tmp_tag%val)
               call move_alloc(tmp_tag, dest)
            else
               call move_alloc(tmp_pair, dest)
            end if
         end if
      type is (pair)
         call pair_flip(dest, s%a, t)
         call pair_flip(dest, s%b, t)
      end select
   end subroutine

   recursive function run_lassoc_parser(this, stream) result(result)
      class(lassoc_parser), intent(in) :: this
      class(*), target, intent(in) :: stream(:)
      class(parse_result), allocatable :: result

      class(parse_result), allocatable :: tmp_result
      type(parse_success), allocatable :: true_result

      if (.not.allocated(this%sub_parser)) then
         result = parse_failure('Lassoc parser not allocated', 1)
         return
      end if

      tmp_result = this%sub_parser%run(stream)
      select type (res => tmp_result)
      type is (parse_success)
         true_result = parse_success(NULL(), res%remaining, res%consumed)
         call pair_flip(true_result%result, res%result, this%tag)
         call move_alloc(true_result, result)
      type is (parse_failure)
         call move_alloc(tmp_result, result)
      end select
   end function

   recursive function run_repitition_parser(this, stream) result(result)
      class(repitition_parser), intent(in) :: this
      class(*), target, intent(in) :: stream(:)
      class(parse_result), allocatable :: result

      class(parse_result), allocatable :: result1, result2
      type(parse_success), allocatable :: true_result
      type(pair), allocatable :: result_pair

      if (.not.allocated(this%sub_parser)) then
         result = parse_failure('Repitition parser not allocated', 1)
         return
      end if

      result1 = this%sub_parser%run(stream)
      select type (res1 => result1)
      type is (parse_success)
         if (res1%consumed == 0) then
            result = parse_failure('Infinite recursion in repitition parser', 1)
            return
         end if
         result2 = this%run(res1%remaining)
         result_pair = pair()
         call move_alloc(res1%result, result_pair%a)
         select type (res2 => result2)
         type is (parse_success)
            call move_alloc(res2%result, result_pair%b)
            true_result = parse_success(NULL(), res2%remaining, res1%consumed + res2%consumed)
            call move_alloc(result_pair, true_result%result)
            call move_alloc(true_result, result)
         type is (parse_failure) ! in theory should be impossible...
            error stop
         end select
      type is (parse_failure)
         result = parse_success(NULL(), stream, 0)
      end select
   end function

   recursive function run_ternary_parser(this, stream) result(result)
      class(ternary_parser), intent(in) :: this
      class(*), target, intent(in) :: stream(:)
      class(parse_result), allocatable :: result
      class(parse_result), allocatable :: tmp_result

      if (.not.allocated(this%test_parser)) then
         result = parse_failure('Ternary test parser not allocated', 1)
         return
      end if

      tmp_result = this%test_parser%run(stream)
      select type (tmp_result)
      type is (parse_failure)
         if (.not.allocated(this%else_parser)) then
            result = parse_failure('Ternary else parser not allocated', 1)
            return
         end if
         result = this%else_parser%run(stream)
      type is (parse_success)
         if (.not.allocated(this%if_parser)) then
            result = parse_failure('Ternary if parser not allocated', tmp_result%consumed + 1)
            return
         end if
         result = this%if_parser%run(tmp_result%remaining)
         select type (result)
         type is (parse_success)
            result%consumed = result%consumed + tmp_result%consumed
         type is (parse_failure)
            result%location = result%location + tmp_result%consumed
         end select
      end select
   end function

   recursive function run_until_parser(this, stream) result(result)
      class(until_parser), intent(in) :: this
      class(*), target, intent(in) :: stream(:)
      class(parse_result), allocatable :: result

      class(parse_result), allocatable :: tmp_result, tmp_result2, end_result
      type(pair), allocatable :: result_pair
      type(parse_success), allocatable :: true_result 

      if (.not.allocated(this%repeated_parser) .or. .not.allocated(this%end_parser)) then
         result = parse_failure('Until parsers not allocated', 1)
         return
      end if

      end_result = this%end_parser%run(stream)
      select type (end_res => end_result)
      type is (parse_failure)
         tmp_result = this%repeated_parser%run(stream)
         select type (res1 => tmp_result)
         type is (parse_success)
            if (res1%consumed == 0) then
               result = parse_failure('Infinite recursion in until parser', 1)
               return
            end if
            result_pair = pair()
            call move_alloc(res1%result, result_pair%a)
            tmp_result2 = this%run(res1%remaining)
            select type (res2 => tmp_result2)
            type is (parse_success)
               call move_alloc(res2%result, result_pair%b)
               true_result = parse_success(NULL(), res2%remaining, res1%consumed + res2%consumed)
               call move_alloc(result_pair, true_result%result)
               call move_alloc(true_result, result)
            type is (parse_failure)
               res2%location = res2%location + res1%consumed
               call move_alloc(tmp_result2, result)
            end select
         type is (parse_failure)
            call move_alloc(tmp_result, result)
         end select
      type is (parse_success)
         call move_alloc(end_result, result)
      end select
   end function
end module
