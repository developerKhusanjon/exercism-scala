object ReverseString {
  def reverse(str: String): String = {
    str.foldRight("")((letter, s) => s + letter)
  }
}
