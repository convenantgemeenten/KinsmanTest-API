package convenantgemeenten.kinsmantest.service

import java.time.format.DateTimeFormatter

import cats.effect.IO
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.{Http, Service}
import com.twitter.server.TwitterServer
import convenantgemeenten.kinsmantest.endpoint.KinsmanTestEndpoint
import io.finch.{Application, Bootstrap, Endpoint, Text}
import lspace.Label.D._
import lspace.Label.P._
import lspace._
import lspace.codec.json.jsonld.JsonLDEncoder
import lspace.codec.{ActiveContext, ActiveProperty}
import lspace.encode.{EncodeJson, EncodeJsonLD}
import lspace.ns.vocab.schema
import lspace.provider.detached.DetachedGraph
import lspace.services.LService
import lspace.services.codecs.{Application => LApplication}
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import shapeless.{:+:, CNil}

import scala.collection.immutable.ListMap
import scala.concurrent.Await
import scala.util.Try

object KinsmanTestService extends LService with TwitterServer {
  lazy val kinsmanGraph: Graph = Graph("kinsmanGraph")
  lazy val kinsmanTestGraph: Graph = Graph("kinsmanTestGraph")

  import lspace.codec.argonaut._
  implicit val ec: Scheduler = lspace.Implicits.Scheduler.global

  implicit val encoderJsonLD = JsonLDEncoder.apply(nativeEncoder)
  implicit val decoderJsonLD =
    lspace.codec.json.jsonld.JsonLDDecoder.apply(DetachedGraph)(nativeDecoder)
  implicit val decoderGraphQL = codec.graphql.Decoder
  import lspace.Implicits.AsyncGuide.guide
  implicit lazy val activeContext = KinsmanTestEndpoint.activeContext

  lazy val kinsmantestEndpoint =
    KinsmanTestEndpoint(
      kinsmanGraph,
      kinsmanTestGraph,
      "http://demo.convenantgemeenten.nl/kinsmantest/") //TODO: get from config

  object UtilsApi extends Endpoint.Module[IO] {
    import io.finch._

    def reset(): Task[Unit] =
      for {
//      SampleData.loadSample(graph).forkAndForget.runToFuture(monix.execution.Scheduler.global)
        _ <- { //partnerdataset
          import com.github.tototoshi.csv._

          import scala.io.Source

          val csvIri = "testset_gbav.csv"
          val source = Source.fromResource(csvIri)

          implicit object MyFormat extends DefaultCSVFormat {
            override val delimiter = ','
          }
          val reader = CSVReader.open(source)

          val data = reader.allWithHeaders

          val formatter = DateTimeFormatter.ofPattern("M/d/yyyy")

          val subjectBSN = "01.01.20"
          val ouder1BSN = "02.01.20"
          val ouder2BSN = "03.01.20"
          val kindBSN = "09.01.20"

          println(data.size)
          Observable
            .fromIterable(data)
            .map(_.filter(_._2.nonEmpty))
            .mapEval { record =>
              val ssn = record.get("01.01.20")
              val ssnOuder1 = record.get("02.01.20")
              val ssnOuder2 = record.get("03.01.20")
              val ssnKind = record.get("09.01.20")
              for {
                _ <- ssn
                  .map(
                    ssn =>
                      kinsmanGraph.nodes
                        .upsert(s"https://data.gov.example/person/nl_${ssn}",
                                schema.Person)
                        .flatMap { person =>
                          Task.gather(Seq(
                            ssnOuder1.map(ssn =>
                              for {
                                ouder <- kinsmanGraph.nodes.upsert(
                                  s"https://data.gov.example/person/nl_${ssn}",
                                  schema.Person)
                                _ <- person --- schema.parent --> ouder
                              } yield ()),
                            ssnOuder2.map(ssn =>
                              for {
                                ouder <- kinsmanGraph.nodes.upsert(
                                  s"https://data.gov.example/person/nl_${ssn}",
                                  schema.Person)
                                _ <- person --- schema.parent --> ouder
                              } yield ()),
                            ssnKind.map(ssn =>
                              for {
                                kind <- kinsmanGraph.nodes.upsert(
                                  s"https://data.gov.example/person/nl_${ssn}",
                                  schema.Person)
                                _ <- person --- schema.children --> kind
                              } yield ())
                          ).flatten)
                      })
                  .getOrElse(Task.unit)
              } yield ()
            }
            .onErrorHandle { f =>
              scribe.error(f.getMessage); throw f
            }
            .completedL
        }
        _ <- lspace.g.N.count().withGraph(kinsmanGraph).headF.foreachL { l =>
          println(s"loaded $l nodes")
        }
      } yield ()

    val resetGraphs: Endpoint[IO, String] = get(path("reset")) {
      (for {
        _ <- purge()
        _ <- reset()
      } yield ()).runToFuture(monix.execution.Scheduler.global)

      Ok("resetting now, building graphs...")
    }

    def custompath(name: String) = path(name)
    def purge() = kinsmanTestGraph.purge
    val clearGraphs: Endpoint[IO, String] = get(path("clear")) {
      purge.startAndForget
        .runToFuture(monix.execution.Scheduler.global)
      Ok("clearing now")
    }

    val persist: Endpoint[IO, Unit] = get("_persist") {
      scribe.info("persisting all graphs")
      kinsmanTestGraph.persist
      io.finch.NoContent[Unit]
    }
  }

//  SampleData.loadSample(graph).runSyncUnsafe()(monix.execution.Scheduler.global, CanBlock.permit)
  UtilsApi.reset.runToFuture
//  println(SigmaJsVisualizer.visualizeGraph(graph))

  lazy val service: Service[Request, Response] = {

    import EncodeJson._
    import EncodeJsonLD._

    import lspace.services.codecs.Encode._
    import EncodeJson._
    import EncodeJsonLD._
    import io.finch.Encode._
    import io.finch.ToResponse._
    import io.finch.fs2._

    Bootstrap
      .configure(enableMethodNotAllowed = true,
                 enableUnsupportedMediaType = false)
      .serve[LApplication.JsonLD :+: Application.Json :+: CNil](
        kinsmantestEndpoint.api)
      .serve[LApplication.JsonLD :+: Application.Json :+: CNil](
        kinsmantestEndpoint.graphql)
      .serve[LApplication.JsonLD :+: Application.Json :+: CNil](
        kinsmantestEndpoint.librarian)
      .serve[Text.Plain :+: CNil](
        UtilsApi.clearGraphs :+: UtilsApi.resetGraphs :+: UtilsApi.persist)
      .serve[Text.Html](App.appService.api)
      .toService
  }

  def main(): Unit = {
    val server = Http.server
//      .configured(Stats(statsReceiver))
      .serve(
        s":8080",
        service
      )

    import scala.concurrent.duration._
    onExit {
      println(s"close wedding-planner-server")
      Await.ready(
        Task
          .sequence(
            Seq(
              Task.gatherUnordered(
                Seq(
                  kinsmanGraph.persist,
                  kinsmanTestGraph.persist
                )),
              Task.gatherUnordered(
                Seq(
                  kinsmanGraph.close,
                  kinsmanTestGraph.close
                ))
            ))
          .runToFuture(monix.execution.Scheduler.global),
        20 seconds
      )

      server.close()
    }

    com.twitter.util.Await.ready(adminHttpServer)
  }
}
