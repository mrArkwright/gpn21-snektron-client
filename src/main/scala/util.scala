extension (a: Int)
  def %%(b: Int): Int = ((a % b) + b) % b
