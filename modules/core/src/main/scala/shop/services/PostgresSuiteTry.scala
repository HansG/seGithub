package shop.services

import org.scalacheck.Gen
import org.scalacheck.Prop._
import shop.domain.brand.{Brand, BrandId, BrandName}
import cats.{Applicative, Monad}
import cats.effect._
import cats.effect.std.Console
import cats.implicits.{catsSyntaxApplicativeError, catsSyntaxApply, catsSyntaxOptionId, none, toFlatMapOps, toFoldableOps, toFunctorOps, toTraverseOps}
import fs2.Stream
import mongo4cats.circe.deriveCirceCodecProvider
import mongo4cats.client.MongoClient
import mongo4cats.collection.GenericMongoCollection
import mongo4cats.database.GenericMongoDatabase
import monocle.Iso
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import skunk._
import skunk.implicits._
import skunk.codec.all._
import natchez.Trace.Implicits.noop
import org.scalacheck.effect.PropF
import shop.domain.ID
import shop.domain.auth.{EncryptedPassword, UserName}
import shop.effects.GenUUID
import shop.optics.IsUUID
import shop.services.StartPostgres.Res

import java.time.OffsetDateTime
import java.util.UUID



class PostgresSuiteTry extends CatsEffectSuite {
  import StartPostgres._


  case class BrandIdT(value: UUID)
  object BrandIdT {
    implicit val isuuid: IsUUID[BrandIdT] = new IsUUID[BrandIdT] {
      val _UUID: Iso[UUID, BrandIdT] = Iso[UUID, BrandIdT](BrandIdT(_))(_.value)
    }
  }
  case class BrandNameT(value: String)

  case class BrandT(uuid: BrandIdT, name: BrandNameT)
//codecs
  val brandIdT: Codec[BrandIdT]     = uuid.imap[BrandIdT](BrandIdT(_))(_.value)
  val brandNameT: Codec[BrandNameT] = varchar.imap[BrandNameT](BrandNameT(_))(_.value)
  val brandT: Codec[BrandT] =
    (brandIdT ~ brandNameT).imap {
      case i ~ n => BrandT(i, n)
    }(b => b.uuid ~ b.name)

  val selectAll: Query[Void, BrandT] =
    sql"""
        SELECT * FROM brands
       """.query(brandT)

  val insertBrand: Command[BrandT] =
    sql"""
        INSERT INTO brands
        VALUES ($brandT)
        """.command

  trait BrandsT[F[_]] {
    def findAll: F[List[BrandT]]
    def create(name: BrandNameT): F[BrandIdT]
  }

  object BrandsT {
    def make[F[_]: GenUUID: MonadCancelThrow](
        session:  Session[F]
      //  postgres: Resource[F, Session[F]]
    ): BrandsT[F] =
      new BrandsT[F] {

        def findAll: F[List[BrandT]] =
          session.execute(selectAll)
        //  postgres.use(_.execute(selectAll))

        def create(name: BrandNameT): F[BrandIdT] =
       //   postgres.use { session =>
            session.prepare(insertBrand).flatMap { cmd =>
              ID.make[F, BrandIdT].flatMap { id =>
                cmd.execute(BrandT(id, name)).as(id)
              }
            }
      //    }
      }

    def makeMG[F[_]: GenUUID: MonadCancelThrow: Sync](
        client: MongoClient[F]
      //  postgres: Resource[F, Session[F]]
    ): Brands[F] =
      new Brands[F] {
        lazy val collF: F[GenericMongoCollection[F, Brand, Stream[F, *]]] =
        for {
          db  <- client.getDatabase("store")
          coll  <- db.getCollectionWithCodec[Brand]("brands")
        } yield coll

        def findAll: F[List[Brand]] = {
          for {
            coll  <- collF
            //        persons = 1.to(5).map( i => Person("Bib"+i, "Bloggs" +i+"berg", Address("München", "GER"), Instant.now()))
            //        _    <- coll.insertMany(persons)
            docs <- coll.find.stream.compile.toList
          } yield docs
        }
        //  postgres.use(_.execute(selectAll))

        def create(name: BrandName): F[BrandId] =
       //   postgres.use { session =>
            session.prepare(insertBrand).flatMap { cmd =>
              ID.make[F, BrandIdT].flatMap { id =>
                cmd.execute(BrandT(id, name)).as(id)
              }
            }
      //    }
      }
  }

  def idGen[A](f: UUID => A): Gen[A] =
    Gen.uuid.map(f)

  lazy val brandIdTGen: Gen[BrandIdT] =
    idGen(BrandIdT.apply)

  lazy val nonEmptyStringGen: Gen[String] =
    Gen
      .chooseNum(7, 15)
      .flatMap { n =>
        Gen.buildableOfN[String, Char](n, Gen.alphaChar)
      }
      .map(_.toLowerCase.capitalize)

  def nesGen[A](f: String => A): Gen[A] =
    nonEmptyStringGen.map(f)

  lazy val brandNameTGen: Gen[BrandNameT] =
    nesGen(BrandNameT.apply)

  lazy val brandTGen: Gen[BrandT] =
    for {
      i <- brandIdTGen
      n <- brandNameTGen
    } yield BrandT(i, n)

  val brandTLGen: Gen[List[BrandT]] =
    Gen
      .chooseNum(1, 10)
      .flatMap { n =>
        Gen.buildableOfN[List[BrandT], BrandT](n, brandTGen)
      }

  val flushTables: Command[Void] = sql"DELETE FROM #brands".command

  def findCreate2(res : Res, brand: BrandT): IO[Unit] =
    res.flatTap(withTempTable).map(BrandsT.make(_)(GenUUID[IO], MonadCancelThrow[IO]))
    .use { bs =>
      for {
        x <- bs.findAll
        _ <- bs.create(brand.name)
        y <- bs.findAll
        z <- bs.create(brand.name).attempt
      } yield println(
        x.toString() + "\n" + (y.count(_.name == brand.name) == 1) + "\n" + (z
          .fold(_.printStackTrace(), "Neu: " + _.toString))
      )
    }

  def brandTFromSession(s : Session[IO]) = BrandsT.make[IO](s)
  val brand: BrandT = brandTGen.sample.get
  val brandL: List[BrandT] = brandTLGen.sample.get

  test("single brand") {
    findCreate2(singleSession, brand)
  }

  test("list brand") {
    brandL.traverse(br =>  findCreate2(singleSession, br))
  }

  test("list brand") {
   PropF.forAllF(brandTGen) { brand =>
    //   forAll(brandTGen) { brand =>
      findCreate2(singleSession, brand).as(())
    }
  }

  //res.unsafeRunSync()



  //gen zu stream + parMap mit pooledSessions

}



object StartPostgres extends App {
  type Res = Resource[IO, Session[IO]]

  // os.proc("""C:\se\prog\postgresql\12\bin\pg_ctl.exe""", "runservice",  """-N "postgresql-x64-12"""", """-D "C:\se\prog\postgresql\12\data"""",  "-w")

//    Sync[F].delay(new Configuration(system))
//      .flatMap(configure)
//      .flatTap(GlobalTracer.registerTracer[F])

  Resource.make( IO(os.proc("""C:\se\prog\postgresql\12\pgAdmin 4\bin\pgAdmin4.exe""").call() ) )(c => IO(println(c.toString()) ))
    .useForever
//    .map { new JaegerEntryPoint[F](_, uriPrefix) }

  // a resource that creates and drops a temporary table
  def withTempTable(s: Session[IO]): Resource[IO, Unit] = {
    val alloc = s.execute(sql"CREATE TEMP TABLE teta (name varchar, age int2)".command).void
    val free  = s.execute(sql"DROP TABLE teta".command).void
    Resource.make(alloc)(_ => free)
  }


  lazy val pooledSessions: Resource[IO, Resource[IO, Session[IO]]] =
    Session.pooled[IO](
      host = "localhost",
      port = 5432,
      user = "postgres",
      password = Some("postgres"),
      database = "store",
      max = 10
    )

  lazy val singleSession: Res =
    Session.single[IO](
      host = "localhost",
      port = 5432,
      user = "postgres",
      password = Some("postgres"),
      database = "store"
    )

  lazy val mongodbIO = MongoClient.fromConnectionString[IO]("mongodb://localhost:27017").map(client =>   client.getDatabase("testdb"))

}


object UserTry {
  case class UserT(id: UUID, name: String, pwd: String)

  val codec = (uuid ~ varchar ~ varchar).imap {
    case id ~ name ~ pwd => UserT(id, name, pwd)
  } {
    case u => u.id ~ u.name ~ u.pwd
  }
  val insertSql: Command[((UUID, String), String)] = sql"insert into users VALUES ($uuid, $varchar, $varchar)".command
  val insertSqlC = sql"insert into users VALUES $codec".command

  def insert(dbres: Res, username: String, password: String) =
    dbres.use(s => insertS(s,username, password))
  def insertS(s: Session[IO], username: String, password: String) =
  //  s.prepare(insertSqlC).flatMap(pc => pc.execute(UserT(UUID.randomUUID(), username, password)))
    s.prepare(insertSql).flatMap(pc => pc.execute((UUID.randomUUID() ~ username ~ password)))

  val findSql: Query[String, UserT] = sql"select * from users where name = $varchar".query(codec)

  def findUser[A]( where : Fragment[A]): Query[A, UserT] = sql"select * from users $where".query(codec)
  val frag : Fragment[String] =  sql"name = $varchar"
  def findUserByNameQ: Query[String, UserT] = findUser(frag)
  val findAllUserQ: Query[Void, UserT] = findUser(Fragment.empty)


  def findS(s: Session[IO], uname: String): IO[Option[UserT]] =
    s.prepare(findUserByNameQ).flatMap { q =>
      q.option(uname).map {
        case Some(u) => u.some
        case _       => none[UserT]
      }
    }

  def findAllUserS(s : Session[IO]) =  s.prepare(findAllUserQ).flatMap { q =>
    q.stream(Void,32).evalTap(u => IO(println(u))).compile.toList
  }

  def findAllUserS(s : Session[IO], uname: String) =  s.prepare(findUserByNameQ).flatMap { pq =>
    pq.stream(uname, 32).evalTap(u => IO(println(u))).compile.toList
  }


  def find(dbres: Res, uname: String) = dbres.use { s => findS(s, uname) }

  def insertEtFind(dbres: Res, username: String, password: String) =
    for {
      d <- insert(dbres, username, password)
      x <- find(dbres, username)
    } yield x.count(_.id == d)

 def insertEtFindS(s: Session[IO], username: String, password: String): IO[Int] =
    for {
      d <- insertS(s, username, password)
      x <- findAllUserS(s, username)
    } yield x.count(_.id == d)

}


class TestTry extends CatsEffectSuite {
  import UserTry._


  test("findUser") {
    StartPostgres.singleSession.use {  findAllUserS }
  }

  test("single Session") {
    StartPostgres.singleSession.use( s =>
      List(("Ha", "aH"), ("Sa", "aS"), ("Mo", "om"), ("Ad", "da")).traverse((vn) => UserTry.insertEtFindS(s, vn._1, vn._2).flatTap(n => IO.println(s"Gefnden mit id $n")) )
    )
    //    UserTry.test(singleSession, "Ha", "aH") .as(ExitCode.Success)
    //    UserTry.test(singleSession, "Sa", "aS") .as(ExitCode.Success)
    //    UserTry.test(singleSession, "Mo", "om") .as(ExitCode.Success)
    //    UserTry.test(singleSession, "Ad", "da") .as(ExitCode.Success)
  }

  test("single SessionRes") {
      List(("Ha", "aH"), ("Sa", "aS"), ("Mo", "om"), ("Ad", "da")).traverse((vn) => UserTry.insertEtFind(StartPostgres.singleSession, vn._1, vn._2))
    //    UserTry.test(singleSession, "Ha", "aH") .as(ExitCode.Success)
    //    UserTry.test(singleSession, "Sa", "aS") .as(ExitCode.Success)
    //    UserTry.test(singleSession, "Mo", "om") .as(ExitCode.Success)
    //    UserTry.test(singleSession, "Ad", "da") .as(ExitCode.Success)
  }

  test("SessionPool") {
    StartPostgres.pooledSessions.use { resS =>
      List(("Ha", "aH"), ("Sa", "aS"), ("Mo", "om"), ("Ad", "da")).traverse((vn) => UserTry.insertEtFind(resS, vn._1, vn._2))
    }
  }


  /*
  https://stackoverflow.com/questions/60438969/postgresql-npgsql-returning-42601-syntax-error-at-or-near-1
  using (Npgsql.NpgsqlConnection conn = new Npgsql.NpgsqlConnection(DBManager.GetConnectionString()))
          {
              conn.Open();
              Logger.Info("connection opened for adding column");
              using (Npgsql.NpgsqlCommand addColumnQuery = new Npgsql.NpgsqlCommand(@"ALTER TABLE @tableName ADD COLUMN IF NOT EXISTS @columnName  @columnType;", conn))
              {
                  addColumnQuery.Parameters.AddWithValue("@tableName", tableName);
                  addColumnQuery.Parameters.AddWithValue("@columnName", columnName);
                  addColumnQuery.Parameters.AddWithValue("@columnType", columnType);
                  addColumnQuery.ExecuteNonQuery();
              }
          }
   */


}







