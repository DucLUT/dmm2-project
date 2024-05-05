error id: jar:file://<HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/modules/scala-xml_2.12/2.2.0/scala-xml_2.12-2.2.0-sources.jar!/scala/xml/ScalaVersionSpecificReturnTypes.scala:[1339..1343) in Input.VirtualFile("jar:file://<HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/modules/scala-xml_2.12/2.2.0/scala-xml_2.12-2.2.0-sources.jar!/scala/xml/ScalaVersionSpecificReturnTypes.scala", "/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.xml

/*
 Unlike other Scala-version-specific things, this class is not filling any gaps in capabilities
 between different versions of Scala; instead, it mostly documents the types that different versions of the
 Scala compiler inferred in the unfortunate absence of the explicit type annotations.
 What should have been specified explicitly is given in the comments;
 next time we break binary compatibility the types should be changed in the code and this class removed.
 */
private[xml] object ScalaVersionSpecificReturnTypes { // should be
  type ExternalIDAttribute = MetaData                 // Null.type
  type NoExternalIDId = scala.Null
  type NodeNoAttributes = MetaData                    // Null.type
  type NullFilter = MetaData                          // Null.type
  type NullGetNamespace = scala.Null
  type NullNext = scala.Null
  type NullKey = scala.Null
  type NullValue = scala.Null
  type NullApply1 = scala.collection.Seq[Node]        // scala.Null
  type NullApply3 = scala.Null
  type NullRemove = Null.type
  type SpecialNodeChild = Nil.type
  type GroupChild = Nothing
}
")
jar:file://<HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/modules/scala-xml_2.12/2.2.0/scala-xml_2.12-2.2.0-sources.jar!/scala/xml/ScalaVersionSpecificReturnTypes.scala
jar:file://<HOME>/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/modules/scala-xml_2.12/2.2.0/scala-xml_2.12-2.2.0-sources.jar!/scala/xml/ScalaVersionSpecificReturnTypes.scala:34: error: expected identifier; obtained type
  type SpecialNodeChild = Nil.type
  ^
#### Short summary: 

expected identifier; obtained type