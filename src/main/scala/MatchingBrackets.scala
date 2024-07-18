object MatchingBrackets {
  def isPaired(brackets: String): Boolean = {
    var front = 0
    var back = brackets.length - 1

    while (back >= front) {
      brackets.charAt(front) match {
        case '{' if (brackets.charAt(back) != '}' && brackets.charAt(front + 1) != '}') => return false
        case '}' if (front != 0 || brackets.charAt(front - 1) != '{') => return false
        case '(' if (brackets.charAt(back) != ')' && brackets.charAt(front + 1) != ')') => return false
        case ')' if (front != 0 || brackets.charAt(front - 1) != '(') => return false
        case '[' if (brackets.charAt(back) != ']' && brackets.charAt(front + 1) != ']') => return false
        case ']' if (front != 0 || brackets.charAt(front - 1) != '[') => return false
        case _ =>
      }

      front = front + 1
      back = back -1
    }

    true
  }
}
