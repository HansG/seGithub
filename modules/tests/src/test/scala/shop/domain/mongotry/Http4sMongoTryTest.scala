package shop.domain.mongotry

import cats.effect.IO
import munit.CatsEffectSuite

class Http4sMongoTryTest extends CatsEffectSuite {
  import Http4sMongoTry._

  test("mongo save") {
    countryServiceFromConnectionString[IO](tablename = "countryT").use { cs =>
      cs.save(List(Country("NGER", "NewGermany"), Country("OGER", "OldGermany")))
    }

  }

}
