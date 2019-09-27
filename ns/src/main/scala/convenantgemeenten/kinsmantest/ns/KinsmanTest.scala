package convenantgemeenten.kinsmantest.ns

import java.time.Instant

import convenantgemeenten.ns.Test
import lspace.datatype.{ListType, NodeURLType, SetType}
import lspace.librarian.traversal.Traversal
import lspace.ns.vocab.schema
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import lspace.{Label, g}
import monix.eval.Task
import shapeless.{HList, HNil}

import scala.annotation.tailrec

object KinsmanTest
    extends OntologyDef(
      "https://ns.convenantgemeenten.nl/kinsmantest",
      label = "Kinsman test",
      comment =
        "A kinsman test is an assertion whether two people are related to some extend",
      labels = Map("nl" -> "Bloedverwantschapsproef"),
      comments = Map(
        "nl" -> ("Een bloedverwantschapsproef toets of twee mensen familie zijn tot op bepaalde hoogte " +
          "(ouders of kinderen -> broers of zussen -> grootouders of kleinkinderen -> neven of nichten)."))
    ) {
  object keys extends Test.Properties {
    object subjects
        extends PropertyDef(ontology.iri + "/subjects",
                            label = "subjects",
                            `@range` =
                              () => SetType(schema.Thing.ontology) :: Nil)
    lazy val subjectsThingList
      : TypedProperty[Set[Node]] = subjects.property as SetType(
      schema.Thing.ontology)

    object degree
        extends PropertyDef(ontology.iri + "/degree",
                            label = "degree",
                            comment = "Degrees of separation",
                            `@range` = () => Label.D.`@int` :: Nil)
    lazy val degreeInt: TypedProperty[Int] = degree as Label.D.`@int`

    object result
        extends PropertyDef(
          ontology.iri + "/result",
          label = "result",
          `@extends` =
            () =>
              Property.properties.getOrCreate("https://schema.org/result",
                                              Set()) :: Nil,
          `@range` = () => Label.D.`@boolean` :: Nil
        )
    lazy val resultBoolean
      : TypedProperty[Boolean] = result as Label.D.`@boolean`
  }
  override lazy val properties
    : List[Property] = keys.subjects.property :: keys.degree.property :: keys.result.property :: Test.properties
  trait Properties extends Test.Properties {
    lazy val subjects = keys.subjects
    lazy val subjectsThingList = keys.subjectsThingList
    lazy val degree = keys.degree
    lazy val degreeInt = keys.degreeInt
    lazy val result = keys.result
    lazy val resultBoolean = keys.resultBoolean
  }

  def fromNode(node: Node): KinsmanTest = {
    KinsmanTest(
      node.outE(keys.subjects).head.to.value.asInstanceOf[Set[Node]].map(_.iri),
      node.out(keys.degreeInt).headOption,
      node.out(keys.executedOn as lspace.Label.D.`@datetime`).headOption,
      node.out(keys.resultBoolean).headOption,
      if (node.iri.nonEmpty) Some(node.iri) else None
    )
  }

  implicit def toNode(cc: KinsmanTest): Task[Node] = {
    for {
      node <- cc.id
        .map(DetachedGraph.nodes.upsert(_, ontology))
        .getOrElse(DetachedGraph.nodes.create(ontology))
      subjects <- Task.gather(cc.subjects.map(DetachedGraph.nodes.upsert(_)))
      _ <- node.addOut(keys.subjectsThingList, subjects)
//      _ <- node --- keys.subjects --> subjects
      _ <- cc.degree
        .map(degree => node --- keys.degree --> degree)
        .getOrElse(Task.unit)
      _ <- cc.executedOn
        .map(node --- keys.executedOn --> _)
        .getOrElse(Task.unit)
      _ <- cc.result
        .map(result => node --- keys.result --> result)
        .getOrElse(Task.unit)
    } yield node
  }

}
case class KinsmanTest(subjects: Set[String],
                       degree: Option[Int],
                       executedOn: Option[Instant] = None,
                       result: Option[Boolean] = None,
                       id: Option[String] = None) {
  lazy val toNode: Task[Node] = this

  def toLibrarian = //: Traversal[ClassType[Any], ListType[List[Any]], HList] =
    subjects.size match {
      case 2 =>
        if (degree.exists(_ > 1)) {
          g.N
            .hasIri(subjects.head)
            .repeat(
              _.union(_.out(schema.parent, schema.children),
                      _.in(schema.parent, schema.children)).dedup(),
              degree.map(_ - 1).getOrElse(0),
              false,
              true
            )(_.hasIri(subjects.tail))
            .hasIri(subjects.tail)
            .path
        } else {
          g.N
            .hasIri(subjects.head)
            .union(_.out(schema.parent, schema.children),
                   _.in(schema.parent, schema.children))
            .dedup()
            .hasIri(subjects.tail)
            .path
        }
      case 1 => g.N.coin(0).path
      case 0 => g.N.coin(0).path
      case _ =>
        @tailrec
        def build[S <: HList](
            person: String,
            persons: Set[String],
            traversals: List[Traversal[NodeURLType[Node], ClassType[Any], S]] =
              List())
          : List[Traversal[NodeURLType[Node], ClassType[Any], HList]] = {
          def traversal =
            if (degree.exists(_ > 1)) {
              lspace
                .__[NodeURLType[Node], ClassType[Any]]
                .hasIri(persons.head)
                .repeat(_.union(_.out(schema.parent, schema.children),
                                _.in(schema.parent, schema.children)).dedup(),
                        degree.map(_ - 1).getOrElse(0),
                        false,
                        true)(_.hasIri(persons - person))
                .hasIri(persons - person)
            } else {
              lspace
                .__[NodeURLType[Node], ClassType[Any]]
                .hasIri(persons.head)
                .union(_.out(schema.parent, schema.children),
                       _.in(schema.parent, schema.children))
                .dedup()
                .hasIri(persons - person)
            }
          if (persons.nonEmpty) {
            build(persons.head, persons.tail, traversal :: traversals)
          } else traversals
        }
        (build(subjects.head, subjects.tail) match {
          case Nil =>
            g.N.limit(0)
          case traversal :: Nil =>
            g.N ++ traversal
          case traversals =>
            g.N.union(_ => traversals.head,
                      traversals.tail.map(
                        t =>
                          (traversal: Traversal[NodeURLType[Node],
                                                NodeURLType[Node],
                                                HNil]) => t): _*)
        }).path
    }

  //  def toSPARQL
  //  def toSQL

}
