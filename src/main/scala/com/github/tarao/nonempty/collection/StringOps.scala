package com.github.tarao.nonempty.collection

import scala.collection.immutable.WrappedString

/** Methods inherited from `StringOps` that preserve non-emptiness. */
trait StringOps[+A, +C <: Iterable[A]] extends Any {
  self: NonEmpty[A, C] =>

  /** Returns a new string containing the chars from this string
    * followed by the chars from the right hand operand.
    *
    * @param suffix the string to append.
    * @return       a new string which contains all chars
    *               of this string followed by all chars of `suffix`.
    * @see [[scala.collection.StringOps!.concat(suffix:String)*]]
    */
  @inline final def concat(suffix: String)(implicit
    coll: C => WrappedString
  ): NonEmpty[Char, WrappedString] =
    unsafeApply[Char, WrappedString](coll(value).unwrap.concat(suffix))

  /** Alias for `concat` */
  @inline final def ++(suffix: String)(implicit
    coll: C => WrappedString
  ): NonEmpty[Char, WrappedString] = concat(suffix)

  /** A copy of the string with an element prepended
    * @see [[scala.collection.StringOps!.prepended(c:Char)*]]
    */
  def prepended(c: Char)(implicit
    coll: C => WrappedString
  ): NonEmpty[Char, WrappedString] =
    unsafeApply[Char, WrappedString](coll(value).unwrap.prepended(c))

  /** Alias for `prepended` */
  @inline final def +:(c: Char)(implicit
    coll: C => WrappedString
  ): NonEmpty[Char, WrappedString] = prepended(c)

  /** A copy of the string with an element prepended
    * @see [[scala.collection.StringOps!.prependedAll(prefix:String)*]]
    */
  def prependedAll(prefix: String)(implicit
    coll: C => WrappedString
  ): NonEmpty[Char, WrappedString] =
    unsafeApply[Char, WrappedString](prefix + coll(value).unwrap)

  /** Alias for `prependedAll` */
  @inline final def ++: (prefix: String)(implicit
    coll: C => WrappedString
  ): NonEmpty[Char, WrappedString] = prependedAll(prefix)

  /** A copy of the string with an element appended
    * @see [[scala.collection.StringOps!.appended(c:Char)*]]
    */
  def appended(c: Char)(implicit
    coll: C => WrappedString
  ): NonEmpty[Char, WrappedString] =
    unsafeApply[Char, WrappedString](coll(value).unwrap.appended(c))

  /** Alias for `appended` */
  @inline final def :+(c: Char)(implicit
    coll: C => WrappedString
  ): NonEmpty[Char, WrappedString] = appended(c)

  /** A copy of the string with another string appended
    * @see [[scala.collection.StringOps!.appendedAll(suffix:String)*]]
    */
  @inline final def appendedAll(suffix: String)(implicit
    coll: C => WrappedString
  ): NonEmpty[Char, WrappedString] =
    unsafeApply[Char, WrappedString](coll(value).unwrap + suffix)

  /** Alias for `appendedAll` */
  @inline final def :++ (suffix: String)(implicit
    coll: C => WrappedString
  ): NonEmpty[Char, WrappedString] = appendedAll(suffix)

  /** Returns a string with a char appended until a given target length
    * is reached.
    *
    * @param  len   the target length
    * @param  elem  the padding value
    * @return a string consisting of this string followed by the
    *         minimal number of occurrences of `elem` so that the
    *         resulting string has a length of at least `len`.
    */
  def padTo(len: Int, elem: Char)(implicit
    coll: C => WrappedString
  ): NonEmpty[Char, WrappedString] =
    unsafeApply[Char, WrappedString](coll(value).unwrap.padTo(len, elem))

  /** A copy of this string with one single replaced element.
    * @param  index  the position of the replacement
    * @param  elem   the replacing element
    * @return a new string which is a copy of this string with the element at position `index` replaced by `elem`.
    * @throws scala.IndexOutOfBoundsException if `index` does not satisfy `0 <= index < length`.
    * @see [[scala.collection.StringOps!.updated]]
    */
  def updated(index: Int, elem: Char)(implicit
    coll: C => WrappedString
  ): NonEmpty[Char, WrappedString] =
    unsafeApply[Char, WrappedString](coll(value).unwrap.updated(index, elem))

  /** Return the current string concatenated `n` times */
  def *(n: Int)(implicit
    coll: C => WrappedString
  ): NonEmpty[Char, WrappedString] =
    unsafeApply[Char, WrappedString](coll(value).unwrap * n)

  /** Returns this string with first character converted to upper case.
    * If the first character of the string is capitalized, it is
    * returned unchanged.  This method does not convert characters
    * outside the Basic Multilingual Plane (BMP).
    * @see [[scala.collection.StringOps!.capitalize]]
    */
  def capitalize(implicit
    coll: C => WrappedString
  ): NonEmpty[Char, WrappedString] =
    unsafeApply[Char, WrappedString](coll(value).unwrap.capitalize)

  /** Uses the underlying string as a pattern (in a fashion similar to
    * printf in C), and uses the supplied arguments to fill in the
    * holes.
    *
    * The interpretation of the formatting patterns is described in
    * `java.util.Formatter`, with the addition that classes deriving
    * from `ScalaNumber` (such as [[scala.BigInt]] and
    * [[scala.BigDecimal]]) are unwrapped to pass a type which
    * `Formatter` understands.
    *
    * @param args the arguments used to instantiating the pattern.
    * @throws scala.IllegalArgumentException
    * @see [[scala.collection.StringOps!.format]]
    */
  def format(args: Any*)(implicit
    coll: C => WrappedString
  ): NonEmpty[Char, WrappedString] =
    unsafeApply[Char, WrappedString](coll(value).unwrap.format(args: _*))

  /** Like `format(args*)` but takes an initial `Locale` parameter
    * which influences formatting as in `java.lang.String`'s format.
    *
    * The interpretation of the formatting patterns is described in
    * `java.util.Formatter`, with the addition that classes deriving
    * from `ScalaNumber` (such as `scala.BigInt` and
    * `scala.BigDecimal`) are unwrapped to pass a type which
    * `Formatter` understands.
    *
    * @param l    an instance of `java.util.Locale`
    * @param args the arguments used to instantiating the pattern.
    * @throws scala.IllegalArgumentException
    * @see [[scala.collection.StringOps!.formatLocal]]
    */
  def formatLocal(l: java.util.Locale, args: Any*)(implicit
    coll: C => WrappedString
  ): NonEmpty[Char, WrappedString] =
    unsafeApply[Char, WrappedString](coll(value).unwrap.formatLocal(l, args: _*))

  /** Converts all of the characters in this `String` to lower
    * case using the rules of the default locale. This is equivalent to calling
    * `toLowerCase(Locale.getDefault())`.
    *
    * Note: This method is locale sensitive, and may produce
    * unexpected results if used for strings that are intended to be
    * interpreted locale independently.  Examples are programming
    * language identifiers, protocol keys, and HTML tags.  For
    * instance, `"TITLE".toLowerCase()` in a Turkish locale returns
    * `"t\u005Cu0131tle"`, where '\u005Cu0131' is the LATIN SMALL
    * LETTER DOTLESS I character.  To obtain correct results for
    * locale insensitive strings, use `toLowerCase(Locale.ROOT)`.
    *
    * @return  the `String}`, converted to lowercase.
    */
  def toLowerCase()(implicit
    coll: C => WrappedString
  ): NonEmpty[Char, WrappedString] =
    unsafeApply[Char, WrappedString](coll(value).unwrap.toLowerCase)

  /** Converts all of the characters in this `String` to lower case
    * using the rules of the given `Locale`.  Case mapping is based on
    * the Unicode Standard version specified by the
    * `java.lang.Character` class. Since case mappings are not always
    * 1:1 char mappings, the resulting `String` may be a different
    * length than the original `String`.
    *
    * @param locale use the case transformation rules for this locale
    * @return the `String`, converted to lowercase.
    */
  def toLowerCase(locale: java.util.Locale)(implicit
    coll: C => WrappedString
  ): NonEmpty[Char, WrappedString] =
    unsafeApply[Char, WrappedString](coll(value).unwrap.toLowerCase(locale))

  /** Converts all of the characters in this `String` to upper
    * case using the rules of the default locale. This method is equivalent to
    * `toUpperCase(Locale.getDefault())`.
    *
    * Note: This method is locale sensitive, and may produce
    * unexpected results if used for strings that are intended to be
    * interpreted locale independently.  Examples are programming
    * language identifiers, protocol keys, and HTML tags.  For
    * instance, `"title".toUpperCase()` in a Turkish locale returns
    * `"T\u005Cu0130TLE"`, where '\u005Cu0130' is the LATIN CAPITAL
    * LETTER I WITH DOT ABOVE character.  To obtain correct results
    * for locale insensitive strings, use `toUpperCase(Locale.ROOT)`.
    *
    * @return  the `String`, converted to uppercase.
    */
  def toUpperCase()(implicit
    coll: C => WrappedString
  ): NonEmpty[Char, WrappedString] =
    unsafeApply[Char, WrappedString](coll(value).unwrap.toUpperCase)

  /** Converts all of the characters in this `String` to upper case
    * using the rules of the given `Locale`. Case mapping is based on
    * the Unicode Standard version specified by the
    * `java.lang.Character` class. Since case mappings are not always
    * 1:1 char mappings, the resulting `String` may be a different
    * length than the original `String`.
    *
    * @param locale use the case transformation rules for this locale
    * @return the `String`, converted to uppercase.
    */
  def toUpperCase(locale: java.util.Locale)(implicit
    coll: C => WrappedString
  ): NonEmpty[Char, WrappedString] =
    unsafeApply[Char, WrappedString](coll(value).unwrap.toUpperCase(locale))

  // We are not providing `reverse` and `grouped` here because it
  // conflicts with `SeqOps.reverse` and `IterableOps.grouped`.
}
