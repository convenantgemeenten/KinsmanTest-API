package convenantgemeenten.kinsmantest.endpoint

import java.time.LocalDate

import com.twitter.finagle.Service
import com.twitter.finagle.http.{Request, Response, Status}
import convenantgemeenten.kinsmantest.ns.KinsmanTest
import io.finch.{Application, Bootstrap, Input}
import lspace.codec
import lspace.codec.ActiveContext
import lspace.codec.argonaut.{nativeDecoder, nativeEncoder}
import lspace.codec.json.jsonld.JsonLDEncoder
import lspace.graphql.QueryResult
import lspace.ns.vocab.schema
import lspace.provider.detached.DetachedGraph
import lspace.provider.mem.MemGraph
import lspace.services.LApplication
import lspace.structure.Graph
import lspace.util.SampleGraph
import monix.eval.Task
import org.scalatest.{AsyncWordSpec, BeforeAndAfterAll, FutureOutcome, Matchers}
import shapeless.{:+:, CNil}

class KinsmanTestEndpointSpec
    extends AsyncWordSpec
    with Matchers
    with BeforeAndAfterAll {

  import lspace.Implicits.Scheduler.global
  import lspace.encode.EncodeJson
  import lspace.encode.EncodeJson._
  import lspace.encode.EncodeJsonLD
  import lspace.encode.EncodeJsonLD._
  import lspace.services.codecs.Encode._

  lazy val dataGraph: Graph = MemGraph("ApiServiceSpec")
  lazy val testsDataGraph: Graph = MemGraph("ApiServiceSpec")
  implicit val encoderJsonLD = JsonLDEncoder.apply(nativeEncoder)
  implicit val decoderJsonLD =
    lspace.codec.json.jsonld.JsonLDDecoder.apply(DetachedGraph)(nativeDecoder)
  implicit val decoderGraphQL = codec.graphql.Decoder
  import lspace.Implicits.AsyncGuide.guide
  implicit lazy val activeContext = KinsmanTestEndpoint.activeContext

  val testsEndpoint =
    KinsmanTestEndpoint(dataGraph,
                        testsDataGraph,
                        "http://example.org/agetests/")

  lazy val service: com.twitter.finagle.Service[Request, Response] = Bootstrap
    .configure(enableMethodNotAllowed = true, enableUnsupportedMediaType = true)
    .serve[LApplication.JsonLD :+: Application.Json :+: CNil](testsEndpoint.api)
    .serve[LApplication.JsonLD :+: Application.Json :+: CNil](
      testsEndpoint.graphql)
    .serve[LApplication.JsonLD :+: Application.Json :+: CNil](
      testsEndpoint.librarian)
    .toService

  lazy val initTask = (for {
    sample <- SampleGraph.loadSocial(dataGraph)
    _ <- for {
      _ <- sample.persons.Gray.person --- schema.parent --- sample.persons.Levi.person
      _ <- sample.persons.Levi.person --- schema.parent --- sample.persons.Garrison.person
    } yield ()
  } yield sample).memoizeOnSuccess

  override def withFixture(test: NoArgAsyncTest): FutureOutcome = {
    new FutureOutcome(initTask.runToFuture flatMap { result =>
      super.withFixture(test).toFuture
    })
  }
  import lspace.Implicits.AsyncGuide.guide
  "A KinsmanEndpoint" should {
    "test positive for a family-relation between Gray and Levi" in {
      (for {
        sample <- initTask
        gray = sample.persons.Gray.person
        levi = sample.persons.Levi.person
        test = KinsmanTest(Set(gray.iri, levi.iri), Some(1))
        node <- test.toNode
      } yield {
        val input = Input
          .post("/")
          .withBody[LApplication.JsonLD](node)
        testsEndpoint
          .create(input)
          .awaitOutput()
          .map { output =>
            output.isRight shouldBe true
            val response = output.right.get
            response.status shouldBe Status.Ok
            response.value
              .out(KinsmanTest.keys.resultBoolean)
              .head shouldBe true
          }
          .getOrElse(fail("endpoint does not match"))
      }).runToFuture
    }
    "test positive for a family-relation between Levi and Garrison" in {
      (for {
        sample <- initTask
        levi = sample.persons.Levi.person
        garrison = sample.persons.Garrison.person
        test = KinsmanTest(Set(levi.iri, garrison.iri), Some(1))
        node <- test.toNode
      } yield {
        val input = Input
          .post("/")
          .withBody[LApplication.JsonLD](node)
        testsEndpoint
          .create(input)
          .awaitOutput()
          .map { output =>
            output.isRight shouldBe true
            val response = output.right.get
            response.status shouldBe Status.Ok
            response.value
              .out(KinsmanTest.keys.resultBoolean)
              .head shouldBe true
          }
          .getOrElse(fail("endpoint does not match"))
      }).runToFuture
    }
    "test negative for a family-relation between Gray and Stan" in {
      (for {
        sample <- initTask
        gray = sample.persons.Gray.person
        stan = sample.persons.Stan.person
        test = KinsmanTest(Set(gray.iri, stan.iri), Some(1))
        node <- test.toNode
      } yield {
        val input = Input
          .post("/")
          .withBody[LApplication.JsonLD](node)
        testsEndpoint
          .create(input)
          .awaitOutput()
          .map { output =>
            output.isRight shouldBe true
            val response = output.right.get
            response.status shouldBe Status.Ok
            response.value
              .out(KinsmanTest.keys.resultBoolean)
              .head shouldBe false
          }
          .getOrElse(fail("endpoint does not match"))
      }).runToFuture
    }
    "test negative for a family-relation between Gray and Garrison (via Levi) with degree 1" in {
      (for {
        sample <- initTask
        gray = sample.persons.Gray.person
        garrison = sample.persons.Garrison.person
        test = KinsmanTest(Set(gray.iri, garrison.iri), Some(1))
        node <- test.toNode
      } yield {
        val input = Input
          .post("/")
          .withBody[LApplication.JsonLD](node)
        testsEndpoint
          .create(input)
          .awaitOutput()
          .map { output =>
            output.isRight shouldBe true
            val response = output.right.get
            response.status shouldBe Status.Ok
            response.value
              .out(KinsmanTest.keys.resultBoolean)
              .head shouldBe false
          }
          .getOrElse(fail("endpoint does not match"))
      }).runToFuture
    }
    "test positive for a family-relation between Gray and Garrison (via Levi) with degree 2" in {
      (for {
        sample <- initTask
        gray = sample.persons.Gray.person
        garrison = sample.persons.Garrison.person
        test = KinsmanTest(Set(gray.iri, garrison.iri), Some(2))
        node <- test.toNode
      } yield {
        val input = Input
          .post("/")
          .withBody[LApplication.JsonLD](node)
        testsEndpoint
          .create(input)
          .awaitOutput()
          .map { output =>
            output.isRight shouldBe true
            val response = output.right.get
            response.status shouldBe Status.Ok
            response.value
              .out(KinsmanTest.keys.resultBoolean)
              .head shouldBe true
          }
          .getOrElse(fail("endpoint does not match"))
      }).runToFuture
    }
  }
  import lspace.services.util._
  "A compiled KinsmanEndpoint" should {
    "test positive for a family-relation between Gray and Levi" in {
      (for {
        sample <- initTask
        gray = sample.persons.Gray.person
        levi = sample.persons.Levi.person
        test = KinsmanTest(Set(gray.iri, levi.iri), Some(2))
        node <- test.toNode
        input = Input
          .post("/")
//          .withBody[Application.Json](node)
          .withBody[LApplication.JsonLD](node)
        _ <- Task
          .fromFuture(service(input.request))
          .flatMap { r =>
            r.status shouldBe Status.Ok
            decoderJsonLD
              .stringToNode(r.contentString)(activeContext)
              .map(_.out(KinsmanTest.keys.resultBoolean).head shouldBe true)
          }
      } yield succeed).runToFuture
    }
    "test negative for a family-relation between Gray and Stan" in {
      (for {
        sample <- initTask
        gray = sample.persons.Gray.person
        stan = sample.persons.Stan.person
        test = KinsmanTest(Set(gray.iri, stan.iri), Some(2))
        node <- test.toNode
        input = Input
          .post("/")
          .withBody[LApplication.JsonLD](node)
        _ <- Task
          .fromFuture(service(input.request))
          .flatMap { r =>
            r.status shouldBe Status.Ok
            decoderJsonLD
              .stringToNode(r.contentString)(activeContext)
              .map(_.out(KinsmanTest.keys.resultBoolean).head shouldBe false)
          }
      } yield succeed).runToFuture
    }
    "test negative for a family-relation between Gray and Garrison (via Levi) with degree 1" in {
      (for {
        sample <- initTask
        gray = sample.persons.Gray.person
        garrison = sample.persons.Garrison.person
        test = KinsmanTest(Set(gray.iri, garrison.iri), Some(1))
        node <- test.toNode
        input = Input
          .post("/")
          .withBody[LApplication.JsonLD](node)
        _ <- Task
          .fromFuture(service(input.request))
          .flatMap { r =>
            r.status shouldBe Status.Ok
            decoderJsonLD
              .stringToNode(r.contentString)(activeContext)
              .map(_.out(KinsmanTest.keys.resultBoolean).head shouldBe false)
          }
      } yield succeed).runToFuture
    }
    "test positive for a family-relation between Gray and Garrison (via Levi) with degree 2" in {
      (for {
        sample <- initTask
        gray = sample.persons.Gray.person
        garrison = sample.persons.Garrison.person
        test = KinsmanTest(Set(gray.iri, garrison.iri), Some(2))
        node <- test.toNode
        input = Input
          .post("/")
          .withBody[LApplication.JsonLD](node)
        _ <- Task
          .fromFuture(service(input.request))
          .flatMap { r =>
            r.status shouldBe Status.Ok
            decoderJsonLD
              .stringToNode(r.contentString)(activeContext)
              .map(_.out(KinsmanTest.keys.resultBoolean).head shouldBe true)
          }
      } yield succeed).runToFuture
    }
  }
}
