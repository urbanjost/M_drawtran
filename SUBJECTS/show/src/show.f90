module show
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, show!"
  end subroutine say_hello
end module show
