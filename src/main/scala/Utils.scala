object Utils:
  extension (e: Either.type)
    def leftIf[T](cond: => Boolean, left: => T): Either[T, Unit] =
      Either.cond(cond, right = (), left = left)
