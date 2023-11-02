object Main extends App {

  sealed trait Json
  final case class JsObject(get: Map[String, Json]) extends Json
  final case class JsString(get: String) extends Json
  final case class JsNumber(get: Double) extends Json
  final case object JsNull extends Json
// The "serialize to JSON" behaviour is encoded in this trait
  trait JsonWriter[A] {
    def write(value: A): Json
  }

  final case class Person(name: String, email: String)
  object JsonWriterInstances {
    implicit val stringWriter: JsonWriter[String] =
      new JsonWriter[String] {
        def write(value: String): Json =
          JsString(value)
      }
    implicit val personWriter: JsonWriter[Person] =
      new JsonWriter[Person] {
        def write(value: Person): Json =
          JsObject(
            Map(
              "name" -> JsString(value.name),
              "email" -> JsString(value.email)
            )
          )
      }
    implicit def optionWriter[A](implicit
        writer: JsonWriter[A]
    ): JsonWriter[Option[A]] =
      new JsonWriter[Option[A]] {
        def write(option: Option[A]): Json = {
          option match {
            case None        => JsNull
            case Some(value) => writer.write(value)
          }
        }
      }
// etc...
  }

  /*object Json {
    def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
      w.write(value)
  }*/

  import JsonWriterInstances._
  /*println(Json.toJson(Person("a", "b")))*/

  object JsonSyntax {
    implicit class JsonWriterOps[A](value: A) {
      def toJson(implicit w: JsonWriter[A]): Json =
        w.write(value)
    }
  }

  import JsonSyntax._

  // println(Person("Dave", "diver@mail.com").toJson)

  // println((Some("ab"): Option[String]).toJson)

}
