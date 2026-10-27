module common_parsers_mod
   use parscomb_mod
   use predicate_mod, only: is_digit, is_hex_digit, true_predicate, false_predicate, is_alpha
   use parscomb_maps_mod, only: string_reduction, atoi, htoi
   implicit none (type, external)
contains
   function dec_digit_parser() result(result)
      type(predicate_parser) :: result
      result = predicate_parser(is_digit, 'Character is not decimal digit')
   end function

   function hex_digit_parser() result(result)
      type(predicate_parser) :: result
      result = predicate_parser(is_hex_digit, 'Character is not hex digit')
   end function

   function dec_parser() result(result)
      type(map_parser) :: result
      result = &
         map_parser( &
            map_parser(.oneplus.dec_digit_parser(), string_reduction), &
            atoi &
         )
   end function

   function hex_parser() result(result)
      type(map_parser) :: result
      result = &
         map_parser( &
            map_parser(.oneplus.hex_digit_parser(), string_reduction), &
            htoi &
         )
   end function

   function alpha_parser() result(result)
      type(predicate_parser) :: result
      result = predicate_parser(is_alpha, 'Character is not alphabetical')
   end function

   function any_parser() result(result)
      type(predicate_parser) :: result
      result = predicate_parser(true_predicate, '')
   end function

   function whitespace_char_parser() result(result)
      type(or_parser) :: result
      result = char_parser(' ') .or. char_parser(achar(10)) .or. char_parser(achar(9)) .or. &
         char_parser(achar(12)) .or.  char_parser(achar(13))
   end function

   function whitespace_parser() result(result)
      type(map_parser) :: result
      result = map_parser( &
         .rep.whitespace_char_parser(), &
         string_reduction &
      )
   end function

   function eof_parser() result(result)
      type(not_parser) :: result
      result = .no.any_parser()
   end function

   function fail_parser(error) result(result)
      type(non_empty_parser) :: result
      character(*), intent(in) :: error
      result = succeed_parser() .ifempty. error
   end function

   ! Only handles common single letter escape characters
   ! Ones common to both c and sml
   function escape_seq_parser() result(result)
      type(map_parser) :: result
      result = map_parser( &
         char_parser('\') > option_parser('abfnrv\"'), &
         escape_seq_map &
      )
   end function

   subroutine escape_seq_map(input)
      class(*), allocatable, intent(inout) :: input

      select type (c => input)
      type is (character(*))
         ! default means char is unchanged
         select case (c)
         case ('a')
            input = achar(7)
         case ('b')
            input = achar(8)
         case ('f')
            input = achar(12)
         case ('n')
            input = achar(10)
         case ('r')
            input = achar(13)
         case ('t')
            input = achar(9)
         case ('v')
            input = achar(11)
         end select
      end select
   end subroutine
end module
