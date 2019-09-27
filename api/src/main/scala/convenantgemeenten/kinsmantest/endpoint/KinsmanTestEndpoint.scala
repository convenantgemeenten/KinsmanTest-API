package convenantgemeenten.kinsmantest.endpoint

import cats.effect.IO
import convenantgemeenten.kinsmantest.ns.KinsmanTest
import io.finch._
import lspace.Label.D._
import lspace._
import lspace.codec._
import lspace.codec.json.jsonld.JsonLDDecoder
import lspace.datatype.SetType
import lspace.decode.{DecodeJson, DecodeJsonLD}
import lspace.librarian.task.AsyncGuide
import lspace.ns.vocab.schema
import lspace.services.rest.endpoints.util.MatchParam
import lspace.services.rest.endpoints.{GraphqlApi, LabeledNodeApi, LibrarianApi}
import monix.eval.Task
import monix.execution.Scheduler
import shapeless.{:+:, CNil, HNil}

import scala.collection.immutable.ListMap

object KinsmanTestEndpoint {
  def apply[Json](ageGraph: Graph, ageTestGraph: Graph, baseUrl: String = "")(
      implicit activeContext: ActiveContext = ActiveContext(),
      decoderJsonLD: JsonLDDecoder[Json],
      ecoderGraphQL: codec.graphql.Decoder,
      guide: AsyncGuide,
      scheduler: Scheduler): KinsmanTestEndpoint[Json] =
    new KinsmanTestEndpoint(ageGraph, ageTestGraph, baseUrl)

  lazy val activeContext = ActiveContext(
    `@prefix` = ListMap(
      "subjects" -> KinsmanTest.keys.subjects.iri,
      "degree" -> KinsmanTest.keys.degree.iri,
      "executedOn" -> KinsmanTest.keys.executedOn.iri,
      "result" -> KinsmanTest.keys.result.iri
    ),
    definitions = Map(
      KinsmanTest.keys.subjects.iri -> ActiveProperty(
        `@type` = SetType(schema.Thing) :: Nil,
        property = KinsmanTest.keys.subjects)(),
      KinsmanTest.keys.degree.iri -> ActiveProperty(
        `@type` = `@int` :: Nil,
        property = KinsmanTest.keys.degree)(),
      KinsmanTest.keys.executedOn.iri -> ActiveProperty(
        `@type` = `@datetime` :: Nil,
        property = KinsmanTest.keys.executedOn)(),
      KinsmanTest.keys.result.iri -> ActiveProperty(`@type` = `@boolean` :: Nil,
                                                    property =
                                                      KinsmanTest.keys.result)()
    )
  )
}
class KinsmanTestEndpoint[Json](kinsmanGraph: Graph,
                                kinsmanTestGraph: Graph,
                                baseUrl: String)(
    implicit activeContext: ActiveContext = ActiveContext(),
    decoderJsonLD: JsonLDDecoder[Json],
    ecoderGraphQL: codec.graphql.Decoder,
    guide: AsyncGuide,
    scheduler: Scheduler)
    extends Endpoint.Module[IO] {

  import lspace.services.codecs.Decode._

  lazy val nodeApi =
    LabeledNodeApi(kinsmanTestGraph, KinsmanTest.ontology, baseUrl)
  lazy val librarianApi = LibrarianApi(kinsmanTestGraph)
  lazy val graphQLApi = GraphqlApi(kinsmanTestGraph)

  /**
    * tests if a kinsman path exists between two persons
    * TODO: update graph with latest (remote) data
    */
  val create: Endpoint[IO, Node] = {
    import shapeless.::

    implicit val bodyJsonldTyped = DecodeJsonLD
      .bodyJsonldTyped(KinsmanTest.ontology, KinsmanTest.fromNode)

    implicit val jsonToNodeToT = DecodeJson
      .jsonToNodeToT(KinsmanTest.ontology, KinsmanTest.fromNode)

    post(body[
      Task[KinsmanTest],
      lspace.services.codecs.Application.JsonLD :+: Application.Json :+: CNil])
      .mapOutputAsync {
        case task =>
          task
            .map { test =>
              test.copy(
                subjects = test.subjects.map(
                  iri =>
                    scala.util
                      .Try(iri.toInt)
                      .toOption
                      .map(id => "https://data.gov.example/person/nl_" + id)
                      .getOrElse(iri)))
            }
            .flatMap {
              case kinsmanTest: KinsmanTest
                  if !kinsmanTest.degree.exists(_ > 0) =>
                Task.now(NotAcceptable(new Exception("degree must be > 0")))
              case kinsmanTest: KinsmanTest if kinsmanTest.result.isDefined =>
                Task.now(
                  NotAcceptable(
                    new Exception("result should not yet be defined")))
              case kinsmanTest: KinsmanTest =>
                for {
                  separationPaths <- kinsmanTest.toLibrarian
                    .withGraph(kinsmanGraph)
                    .toListF
                    .map(_.asInstanceOf[List[List[List[Any]]]])
                  testAsNode <- kinsmanTest
                    .copy(result =
                            Some(if (separationPaths.isEmpty) false else true),
                          id = Some(
                            baseUrl + java.util.UUID
                              .randomUUID()
                              .toString + scala.math.random()))
                    .toNode
                  persistedNode <- kinsmanTestGraph.nodes ++ testAsNode
                } yield
                  Ok(persistedNode).withHeader("Location" -> persistedNode.iri)
              case _ =>
                Task.now(NotAcceptable(new Exception("invalid parameters")))
            }
            .to[IO]
      }
  }

  lazy val api = nodeApi.context :+: nodeApi.byId :+: nodeApi.list :+: create :+: nodeApi.removeById
  lazy val graphql = MatchParam[IO]("query") :: graphQLApi.list(
    KinsmanTest.ontology)
  lazy val librarian = librarianApi.filtered.list(KinsmanTest.ontology)
}
