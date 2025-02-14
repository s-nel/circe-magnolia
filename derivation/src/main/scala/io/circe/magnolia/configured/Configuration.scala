package io.circe.magnolia.configured

import java.util.regex.Pattern

/**
  * Configuration allowing customisation of the JSON produced when encoding, or expected when decoding.
  *
  * @param transformMemberNames Transforms the names of any case class members in the JSON allowing, for example,
  *                             formatting or case changes.
  *                             If there are collisions in transformed member names, an exception will be thrown
  *                             during derivation (runtime)
  * @param useDefaults Whether to allow default values as specified for any case-class members.
  * @param discriminator Optional key name that, when given, will be used to store the name of the constructor of an ADT
  *                      in a nested field with this name. If not given, the name is instead stored as a key under which
  *                      the contents of the ADT are stored as an object.
  *                      If the discriminator conflicts with any of the keys of a case class, an exception will be thrown
  *                      during derivation (runtime)
  * @param transformConstructorNames Transforms the value of any constructor names in the JSON allowing, for example,
  *                                  formatting or case changes.
  *                                  If there are collisions in transformed constructor names, an exception will be thrown
  *                                  during derivation (runtime)
  * @param strictDecoding When true, raises a decoding error when there are any extraneous fields in the given JSON
  *                       that aren't present in the case class.
  */
final case class Configuration(
  transformMemberNames: String => String,
  transformConstructorNames: String => String,
  useDefaults: Boolean,
  discriminator: Option[String],
  strictDecoding: Boolean = false
) {
  def withSnakeCaseMemberNames: Configuration = copy(
    transformMemberNames = Configuration.snakeCaseTransformation
  )

  def withKebabCaseMemberNames: Configuration = copy(
    transformMemberNames = Configuration.kebabCaseTransformation
  )

  def withSnakeCaseConstructorNames: Configuration = copy(
    transformConstructorNames = Configuration.snakeCaseTransformation
  )

  def withKebabCaseConstructorNames: Configuration = copy(
    transformConstructorNames = Configuration.kebabCaseTransformation
  )

  def withDefaults: Configuration = copy(useDefaults = true)
  def withDiscriminator(discriminator: String): Configuration = copy(discriminator = Some(discriminator))
  def withStrictDecoding: Configuration = copy(strictDecoding = true)
}

final object Configuration {

  val default: Configuration = Configuration(Predef.identity, Predef.identity, false, None)
  private val basePattern: Pattern = Pattern.compile("([A-Z]+)([A-Z][a-z])")
  private val swapPattern: Pattern = Pattern.compile("([a-z\\d])([A-Z])")

  val snakeCaseTransformation: String => String = s => {
    val partial = basePattern.matcher(s).replaceAll("$1_$2")
    swapPattern.matcher(partial).replaceAll("$1_$2").toLowerCase
  }

  val kebabCaseTransformation: String => String = s => {
    val partial = basePattern.matcher(s).replaceAll("$1-$2")
    swapPattern.matcher(partial).replaceAll("$1-$2").toLowerCase
  }
}

final object defaults {
  implicit val defaultGenericConfiguration: Configuration = Configuration.default
}

