module predicate_mod
   implicit none (type, external)

   abstract interface
      function predicate(element) result(conclusion)
         class(*), intent(in) :: element
         logical :: conclusion
      end function
   end interface
contains
   function true_predicate(element) result(conclusion)
      class(*), intent(in) :: element
      logical :: conclusion
      conclusion = .true.
   end function

   function false_predicate(element) result(conclusion)
      class(*), intent(in) :: element
      logical :: conclusion
      conclusion = .false.
   end function

   function is_digit(element)
      class(*), intent(in) :: element
      logical :: is_digit

      is_digit = .false.
      select type (element)
      type is (character(*))
         is_digit = element >= '0' .and. element <= '9'
      end select
   end function

   function is_hex_digit(element)
      class(*), intent(in) :: element
      logical :: is_hex_digit

      is_hex_digit = .false.
      select type (element)
      type is (character(*))
         is_hex_digit = &
            element >= '0' .and. element <= '9' .or. &
            element >= 'a' .and. element <= 'f' .or. &
            element >= 'A' .and. element <= 'F'
      end select
   end function

   function is_alpha(element)
      class(*), intent(in) :: element
      logical :: is_alpha

      is_alpha = .false.
      select type (element)
      type is (character(*))
         is_alpha = &
            element >= 'A' .and. element <= 'Z' .or. &
            element >= 'a' .and. element <= 'z'
      end select
   end function
end module
