package io.circe.magnolia

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.either._
import cats.syntax.list._
import io.circe.{Decoder, DecodingFailure, HCursor}
import io.circe.Decoder.{AccumulatingResult, Result}
import io.circe.magnolia.configured.Configuration
import magnolia._

private[magnolia] object MagnoliaDecoder {

  private[magnolia] def combine[T](
    caseClass: CaseClass[Decoder, T]
  )(implicit configuration: Configuration): Decoder[T] = {
    val paramJsonKeyLookup: Map[String, String] = caseClass.parameters.map { p =>
      val jsonKeyAnnotation = p.annotations.collectFirst {
        case ann: JsonKey => ann
      }
      jsonKeyAnnotation match {
        case Some(ann) => p.label -> ann.value
        case None => p.label -> configuration.transformMemberNames(p.label)
      }
    }.toMap

    if (paramJsonKeyLookup.values.toList.distinct.length != caseClass.parameters.length) {
      throw new DerivationError(
        "Duplicate key detected after applying transformation function for case class parameters"
      )
    }

    val nonStrictDecoder = if (configuration.useDefaults) {
      new Decoder[T] {
        override def apply(c: HCursor): Result[T] = {
          decodeAccumulating(c).toEither.leftMap(_.head)
        }

        override def decodeAccumulating(c: HCursor): AccumulatingResult[T] = {
          caseClass.constructEither { p =>
            val key = paramJsonKeyLookup.getOrElse(
              p.label,
              throw new IllegalStateException(
                "Looking up a parameter label should always yield a value. This is a bug"
              )
            )
            val keyCursor = c.downField(key)
            keyCursor.focus match {
              case Some(_) => p.typeclass.tryDecodeAccumulating(keyCursor).toEither
              case None =>
                p.default.fold {
                  // Some decoders (in particular, the default Option[T] decoder) do special things when a key is missing,
                  // so we give them a chance to do their thing here.
                  p.typeclass.tryDecodeAccumulating(keyCursor).toEither
                }(x => Right(x))
            }
          } match {
            case Right(a) => Valid(a)
            case Left(head :: tail) =>
              val errsNel = tail.foldLeft(head) {
                case (acc, nel) => acc.concatNel(nel)
              }
              Invalid(errsNel)
            case Left(Nil) => throw new IllegalStateException("Decoding failed, but empty error list. This is a bug")
          }
        }
      }
    } else {
      new Decoder[T] {
        override def apply(c: HCursor): Result[T] = {
          decodeAccumulating(c).toEither.leftMap(_.head)
        }

        override def decodeAccumulating(c: HCursor): AccumulatingResult[T] = {
          caseClass.constructEither { p =>
            p.typeclass
              .tryDecodeAccumulating(
                c.downField(
                  paramJsonKeyLookup.getOrElse(
                    p.label,
                    throw new IllegalStateException(
                      "Looking up a parameter label should always yield a value. This is a bug"
                    )
                  )
                )
              )
              .toEither
          } match {
            case Right(a) => Valid(a)
            case Left(head :: tail) =>
              val errsNel = tail.foldLeft(head) {
                case (acc, nel) => acc.concatNel(nel)
              }
              Invalid(errsNel)
            case Left(Nil) => throw new IllegalStateException("Decoding failed, but empty error list. This is a bug")
          }
        }
      }
    }

    if (configuration.strictDecoding) {
      val expectedFields = paramJsonKeyLookup.values
      new Decoder[T] {
        override def apply(c: HCursor): Result[T] = decodeAccumulating(c).toEither.leftMap(_.head)

        override def decodeAccumulating(c: HCursor): AccumulatingResult[T] = {
          val accResult = nonStrictDecoder.decodeAccumulating(c)
          val maybeUnexpectedErrors = (for {
            json <- c.focus
            jsonKeys <- json.hcursor.keys
            unexpected = jsonKeys.toSet -- expectedFields
            decodingFailuresNel <- unexpected.toList.map { unexpectedField =>
              DecodingFailure(s"Unexpected field: [$unexpectedField]. Valid fields: ${expectedFields.mkString(",")}", c.history)
            }.toNel
          } yield {
            Invalid(decodingFailuresNel)
          }).getOrElse(Valid(()))

          maybeUnexpectedErrors.product(accResult).map(_._2)
        }
      }
    } else {
      nonStrictDecoder
    }
  }

  private[magnolia] def dispatch[T](
    sealedTrait: SealedTrait[Decoder, T]
  )(implicit configuration: Configuration): Decoder[T] = {
    val constructorLookup: Map[String, Subtype[Decoder, T]] =
      sealedTrait.subtypes.map { s =>
        configuration.transformConstructorNames(s.typeName.short) -> s
      }.toMap

    if (constructorLookup.size != sealedTrait.subtypes.length) {
      throw new DerivationError(
        "Duplicate key detected after applying transformation function for case class parameters"
      )
    }

    configuration.discriminator match {
      case Some(discriminator) => new DiscriminatedDecoder[T](discriminator, constructorLookup)
      case None => new NonDiscriminatedDecoder[T](constructorLookup)
    }
  }

  private[magnolia] class NonDiscriminatedDecoder[T](constructorLookup: Map[String, Subtype[Decoder, T]])
    extends Decoder[T] {
    private val knownSubTypes = constructorLookup.keys.toSeq.sorted.mkString(",")

    override def apply(c: HCursor): Result[T] = {
      decodeAccumulating(c).toEither.leftMap(_.head)
    }

    override def decodeAccumulating(c: HCursor): AccumulatingResult[T] = {
      c.keys match {
        case Some(keys) if keys.size == 1 =>
          val key = keys.head
          constructorLookup.get(key) match {
            case Some(theSubtype) =>
              theSubtype.typeclass.tryDecodeAccumulating(c.downField(key))
            case None =>
              Validated.invalidNel(
                DecodingFailure(
                  s"""Can't decode coproduct type: couldn't find matching subtype.
                   |JSON: ${c.value},
                   |Key: $key
                   |Known subtypes: $knownSubTypes\n""".stripMargin,
                  c.history
                )
              )
          }
        case _ =>
          Validated.invalidNel(
            DecodingFailure(
              s"""Can't decode coproduct type: zero or several keys were found, while coproduct type requires exactly one.
                 |JSON: ${c.value},
                 |Keys: ${c.keys.map(_.mkString(","))}
                 |Known subtypes: $knownSubTypes\n""".stripMargin,
              c.history
            )
          )
      }
    }
  }

  private[magnolia] class DiscriminatedDecoder[T](
    discriminator: String,
    constructorLookup: Map[String, Subtype[Decoder, T]]
  ) extends Decoder[T] {
    val knownSubTypes = constructorLookup.keys.toSeq.sorted.mkString(",")

    override def apply(c: HCursor): Result[T] = {
      decodeAccumulating(c).toEither.leftMap(_.head)
    }

    override def decodeAccumulating(c: HCursor): AccumulatingResult[T] = {
      c.downField(discriminator).as[String] match {
        case Left(_) =>
          Validated.invalidNel(
            DecodingFailure(
              s"""
                 |Can't decode coproduct type: couldn't find discriminator or is not of type String.
                 |JSON: ${c.value}
                 |discriminator key: discriminator
              """.stripMargin,
              c.history
            )
          )
        case Right(ctorName) =>
          constructorLookup.get(ctorName) match {
            case Some(subType) => subType.typeclass.tryDecodeAccumulating(c)
            case None =>
              Validated.invalidNel(
                DecodingFailure(
                  s"""
                     |Can't decode coproduct type: constructor name not found in known constructor names
                     |JSON: ${c.value}
                     |Allowed discriminators: $knownSubTypes
              """.stripMargin,
                  c.history
                )
              )
          }
      }
    }
  }
}
