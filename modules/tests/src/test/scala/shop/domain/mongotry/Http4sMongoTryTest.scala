package shop.domain.mongotry

import cats.effect.IO
import cats.implicits.catsSyntaxTuple2Parallel
import munit.CatsEffectSuite
import natchez.Trace.Implicits.noop
import cats.effect.std.Console

class Http4sMongoTryTest extends CatsEffectSuite {
  import Http4sMongoTry._

  test("mongo save") {
    countryServiceFromConnectionString[IO](tablename = "countryT").use { cs =>
      cs.save(List(Country("NGER", "NewGermany"), Country("OGER", "OldGermany")))
    }
  }


  test("transfer") {
    (countryServiceP[IO], countryServiceFromConnectionString[IO](tablename = "countryT"))
      .parMapN((_, _))
      .use { sp =>
        val csQuelle = sp._1
        val csZiel =  sp._2
        for {
          st <- csQuelle.all
          cst = st.chunkN(10, true)
          u <- cst
            .evalTap(chunk => Console[IO].println(s"Current chunk: $chunk"))
            .evalMap(chunk => csZiel.save(chunk.toList))
            .evalTap(ids => Console[IO].println(s"Current Ids: $ids"))
            .compile
            .drain
        } yield u


      }


  }

}
