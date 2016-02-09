package codegen

import slick.ast.ColumnOption
import slick.driver.MySQLDriver
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * This customizes the Slick code generator. We only do simple name mappings.
  * For a more advanced example see https://github.com/cvogt/slick-presentation/tree/scala-exchange-2013
  */
abstract class PlaySlickCodeGenerator extends Config {

  implicit class CamelStringImplicity(value: String) {

    def toPropertyName = uncapitalize(propertyName(value))

    def toClassName = propertyName(value).capitalize

    private def uncapitalize(value: String): String = {
      if (value == null) null
      else if (value.length == 0) ""
      else if (value.charAt(0).isLower) value
      else {
        val chars = value.toCharArray
        chars(0) = chars(0).toLower
        new String(chars)
      }
    }

    private def toCamelCase(name: String): String = {
      "[- _]([a-zA-Z0-9])".r.replaceAllIn(name, m => m.group(1).toUpperCase)
    }

    private def propertyName(columnName: String): String = {
      if (useCamelCase) {
        toCamelCase(columnName)
      } else {
        columnName
      }
    }
  }

  def codegen = {

    val db = MySQLDriver.api.Database.forURL(url, driver = jdbcDriver, password = password, user = user)

    val tableModels = db.run {
      val tables = MySQLDriver.defaultTables
      tables.filter(t => !excludedTables.contains(t)).flatMap(
        t =>
          MySQLDriver.createModelBuilder(t, false).buildModel
      )
    }

    tableModels.map { model =>

      new slick.codegen.SourceCodeGenerator(model) {

        val jodaImports = Seq(
          "import org.joda.time._",
          "import com.github.tototoshi.slick.GenericJodaSupport",
          "object PortableJodaSupport extends GenericJodaSupport(slick.driver.MySQLDriver)",
          "import PortableJodaSupport._"
        )

        val jsonImports = Seq("import play.api.libs.json.Json")

        def generateFormater(tableOrClassName: String) = {
          val caseClassName = tableOrClassName.toClassName
          val varName = tableOrClassName.toPropertyName
          s"implicit val ${varName}Format = Json.format[$caseClassName]"
        }

        lazy val jsonFormaters = model.tables.map {
          t =>
            val mainClassFormatter = generateFormater(t.name.table)
            val additionalFormaters = if (nestedCaseClassTables.contains(t.name.table)) {
              nestedCaseClassTables(t.name.table).keys.map(generateFormater(_)).mkString("\n")
            } else ""
            additionalFormaters + "\n" + mainClassFormatter
        }

        val imports = {
          additionalImports ++
            (if (useJodaForTimestamp) jodaImports else Seq()) ++
            (if (generateJsonFormaters) jsonImports else Seq())
        }

        override def code = {
          imports.mkString("\n") + "\n" + super.code + "\n" + jsonFormaters.mkString("\n")
        }

        // customize Scala entity name (case class, etc.)
        override def entityName = {
          dbTableName => dbTableName.toClassName
        }

        // customize Scala table name (table class, table values, ...)
        override def tableName = dbTableName => entityName(dbTableName) + "Table"

        // override generator responsible for tables
        override def Table = new Table(_) {
          table =>

          override def autoIncLastAsOption = autoIncAsOption

          def nestedCaseClassEnabled = nestedCaseClassTables.contains(model.name.table)

          lazy val allNestedColumns: Seq[String] = nestedCaseClassTables.get(model.name.table).getOrElse(Map[String, Seq[String]]()).flatMap {
            case (e, c) => c
          }.to[Seq]

          lazy val pkList = model.primaryKey match {
            case Some(primaryKey) =>
              primaryKey.columns.map(_.name)
            case None =>
              model.columns.find(
                c => c.options.find(
                  o => o match {
                    case ColumnOption.PrimaryKey => true
                    case _ => false
                  }
                ).isDefined
              ).map(c => Seq(c.name)).getOrElse(Seq("None!"))
          }

          lazy val nonPkList = model.columns.filter(
            c => !c.options.find(
              o => o match {
                case ColumnOption.PrimaryKey => true
                case _ => false
              }
            ).isDefined && !allNestedColumns.contains(c.name.toPropertyName)
          ).map(c => c.name)

          lazy val nestedTupleDefs = nestedCaseClassTables.get(model.name.table).getOrElse(Map[String, Seq[String]]())

          lazy val nestedTupleDefsStr = nestedTupleDefs.map {
            case (e, c) => (e, "(" + c.mkString(",") + ")")
          }

          lazy val nonPkColumnsStr = (if (nonPkList.isEmpty) "" else nonPkList.map(_.toPropertyName).mkString(","))
          lazy val pkStr = pkList.map(_.toPropertyName).mkString(",")
          lazy val nestedPartClasses = nestedTupleDefs.keySet
          lazy val nestedPartVars = nestedPartClasses.map(c => c.toPropertyName)
          lazy val classVarPairs = (nestedPartClasses zip nestedPartVars)
          lazy val caseClassFieldsAsTupleStr = s"($pkStr, $nonPkColumnsStr,\n" + nestedTupleDefsStr.values.mkString(",\n") + ")"
          lazy val simpleColumns: Seq[Column] = columns.filter(c => !allNestedColumns.contains(c.name))

          override def mappingEnabled = nestedCaseClassEnabled || super.mappingEnabled

          override def PlainSqlMapper = new PlainSqlMapper {

            def generateMapper(entity: String, params: Seq[Column], nested: Set[String] = Set()) = {
              //val dependencies = params.map(_.exposedType).distinct.zipWithIndex.map{ case (t,i) => s"""e$i: GR[$t]"""}.mkString(", ")
              val positionalParams = compoundValue(
                columnsPositional.filter(c => params.contains(c))
                  .map(c => (if (c.fakeNullable || c.model.nullable) s"<<?[${c.rawType}]" else s"<<[${c.rawType}]")) ++
                  nested.map(e => s"GetResult$e(prs)")
              )
              s"""
implicit def GetResult$entity = GR{
  prs => import prs._
    $entity.tupled($positionalParams)
}
            """
            }

            def generateWholeMapper = {
              generateMapper(EntityType.rawName, simpleColumns, nestedPartClasses)
            }

            def generatePartsMapper = {
              nestedTupleDefs.map {
                case (e, cols) =>
                  val nestedCols = columns.filter(c => cols.contains(c.model.name)).toSeq
                  generateMapper(e, nestedCols)
              }.mkString("\n")
            }

            def nestedCaseClassMapperCode = {
              generateWholeMapper + "\n" + generatePartsMapper
            }

            override def code = {
              if (nestedCaseClassEnabled) {
                nestedCaseClassMapperCode
              } else {
                super.code
              }
            }
          }

          def createCaseClass(name: String, args: Seq[String], parents: Seq[String]) = {
            val argsStr = args.mkString(", ")
            val prns = (parents.take(1).map(" extends " + _) ++ parents.drop(1).map(" with " + _)).mkString("")
            s"""case class $name($argsStr)$prns"""
          }

          def mapToArgDeclaration(c: Column) = {
            c.default.map(v =>
              s"${c.model.name.toPropertyName}: ${c.exposedType} = $v"
            ).getOrElse(
              s"${c.model.name.toPropertyName}: ${c.exposedType}"
            )
          }

          def NestedCaseClassesDef: Seq[Def] = {
            val res = nestedCaseClassTables.get(model.name.table).getOrElse(Map[String, Seq[String]]()).map {
              case (e, c) =>
                val caseClassArgs = columns.filter(col => c.contains(col.model.name)).map(mapToArgDeclaration)
                new Def {
                  def doc = s"nested case classes $e"

                  def code = createCaseClass(e, caseClassArgs, Seq())

                  def rawName = ???
                }
            }.toSeq
            res
          }

          override def definitions: Seq[Def] = NestedCaseClassesDef ++ super.definitions

          override def EntityType = new EntityType {

            override def parents = if (caseClassExtends.contains(model.name.table)) caseClassExtends(model.name.table) else defaultCaseClassExtends

            override def doc = {
              if (nestedCaseClassEnabled) {
                s"Entity class storing rows of table ${TableValue.name}\n" +
                  simpleColumns.map(c => "@param " + c.name + " " + c.doc).mkString("\n") + "\n" +
                  classVarPairs.map {
                    case (c, v) =>
                      s"@param $v nested $c"
                  }.mkString("\n")
              } else super.doc
            }

            override def code = {

              if (nestedCaseClassEnabled) {
                val simpleArgs = simpleColumns.map(mapToArgDeclaration)

                val nestedClassArgs = nestedCaseClassTables.get(model.name.table).getOrElse(Map[String, Seq[String]]()).map {
                  case (e, c) =>
                    e.toPropertyName + ": " + e
                }

                val args = (simpleArgs ++ nestedClassArgs)

                createCaseClass(name, args, parents)
              } else {
                super.code
              }
            }
          }

          override def TableValue = new TableValue {
            override def rawName = "Table$".r.replaceAllIn(super.rawName.uncapitalize, "Query")
          }

          override def compoundValue(values: Seq[String]): String = {
            if (nestedCaseClassEnabled)
              s"""(${values.mkString(", ")})"""
            else if (hlistEnabled) values.mkString(" :: ") + " :: HNil"
            else if (values.size == 1) values.head
            else if (values.size <= 22)
              s"""(${values.mkString(", ")})"""
            else throw new Exception("Cannot generate tuple for > 22 columns, please set hlistEnable=true or override compound.")
          }

          override def TableClass = new TableClass() {

            override def parents = if (tableClassExtends.contains(model.name.table)) tableClassExtends(model.name.table) else defaultTableClassExtends

            def NestedUnapplyDef = new Def {
              def doc = "Unapply for nested case classes."

              def code = {
                classVarPairs.map {
                  case (c, v) => s"def unapply$c($v : $c) = $c.unapply($v).get\n"
                }.mkString
              }

              def rawName = ???
            }

            override def definitions: Seq[Seq[Def]] = Seq(Seq(NestedUnapplyDef)) ++ super.definitions

            def starNestedCaseClass = {
              val columnsInOrder = if (autoIncLastAsOption) nonPkList ++ nestedPartVars ++ pkList else pkList ++ nonPkList ++ nestedPartVars
              val nestedApplyStrs = classVarPairs.map { case (c, v) => s"$c.tupled.apply($v)" }
              val nestedUnapplyStrs = classVarPairs.map { case (c, v) => s"unapply$c(w.$v)" }
              val applyStrs = if (autoIncLastAsOption) nonPkList ++ nestedApplyStrs ++ pkList else pkList ++ nonPkList ++ nestedApplyStrs
              val unapplyStrs = if (autoIncLastAsOption) nonPkList.map("w." + _) ++ nestedUnapplyStrs ++ pkList.map("w." + _) else (pkList ++ nonPkList).map("w." + _) ++ nestedUnapplyStrs
              s"def * = $caseClassFieldsAsTupleStr.shaped <> ({ case (${columnsInOrder.mkString(",")}) =>\n" +
                s"${elementType}(${applyStrs.mkString(",")}" +
                s")}, { w: ${elementType} =>\n" +
                s"Some((${unapplyStrs.mkString(",")}))})"
            }

            override def star = {
              if (nestedCaseClassEnabled) {
                starNestedCaseClass
              } else {
                super.star
              }
            }

            def optionNestedCaseClass: String = {
              val structMap = { c: Column => if (c.model.nullable) s"${c.model.name.toPropertyName}" else s"Rep.Some(${c.model.name.toPropertyName})" }
              val struct = compoundValue(
                simpleColumns.map(structMap) ++
                  nestedTupleDefs.map { case (e, c) => "\n(" + columns.filter { col => c.contains(col.name) }.map(structMap).mkString(",") + ")" }
              )
              val rhs = s"""$struct.shaped.<>({case $caseClassFieldsAsTupleStr =>\n $optionFactoryNestedCaseClass}, (_:Any) =>  throw new Exception("Inserting into ? projection not supported."))"""
              s"def ? = $rhs"
            }

            def optionFactoryNestedCaseClass = {
              val acessorMap = { c: Column => if (c.fakeNullable || c.model.nullable) c.name.toPropertyName else s"${c.name.toPropertyName}.get" }
              s"${elementType}(" +
                simpleColumns.map(acessorMap).mkString(",") + "," +
                nestedTupleDefs.map {
                  case (e, c) =>
                    val nestedCols = columns.filter { col => c.contains(col.name) }.map(acessorMap)
                    s"\n$e.tupled.apply((" + nestedCols.mkString(",") + "))"

                }.mkString(",") + ")"
            }

            override def option = {
              if (nestedCaseClassEnabled) {
                optionNestedCaseClass
              } else {
                super.option
              }
            }

          }

          override def Column = new Column(_) {

            override def rawName = {
              model.name
            }

            //
            //          override def code = s"""val ${name.toPropertyName}: Rep[$actualType] = column[$actualType]("${model.name}"${options.map(", "+_).mkString("")})"""

            override def defaultCode = v => {
              def raw(v: Any) = rawType match {
                case "String" => "\"" + v + "\""
                case "Long" => v + "L"
                case "Float" => v + "F"
                case "Char" => "'" + v + "'"
                case "scala.math.BigDecimal" => s"new scala.math.BigDecimal(new java.math.BigDecimal($v))"
                case "Byte" | "Short" | "Int" | "Double" | "Boolean" => v.toString
              }
              v match {
                case Some(x) => s"Some(${raw(x)})"
                case None => "None"
                case x => raw(x)
              }
            }

            // munge rawType -> SQL column type HERE (scaladoc in Slick 2.1.0 is outdated or incorrect, GeneratorHelpers#mapJdbcTypeString does not exist)
            // you can filter on model.name for the column name or model.tpe for the column type
            // your IDE won't like the String here but don't worry, the return type the compiler expects here is String
            override def rawType = if (useJodaForTimestamp) {
              model.tpe match {
                case "java.sql.Timestamp" => "DateTime"
                case "java.sql.Date" => "DateTime"
                case "java.sql.Time" => "LocalTime"
                case "java.sql.DateTime" => "DateTime"
                case _ => super.rawType
              }
            } else {
              super.rawType
            }
          }
        }
      }
    }
  }
}