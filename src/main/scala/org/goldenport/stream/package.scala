package org.goldenport

import scalaz._, Scalaz._
import scalaz.concurrent.Task
import scalaz.stream._
import scalaz.stream.Cause, Cause._
import scala.io.{Codec, Source}
import java.sql._
import java.io.InputStream
import java.nio.charset.Charset
import com.asamioffice.goldenport.text.UString
import scodec.bits.ByteVector

/*
 * @since   Jun.  9, 2014
 *  version Jul. 25, 2014
 *  version Dec. 30, 2014
 *  version Jun.  8, 2015
 *  version Oct.  6, 2015
 *  version Dec. 21, 2015
 *  version Feb. 29, 2016
 *  version Aug. 29, 2017
 * @version May.  2, 2025
 * @author  ASAMI, Tomoharu
 */
package object stream {
  object io {
    def fromIterator[T](
      iter: Iterator[T]
    ): Process[Task, T] = {
      def acquire = Task.now(iter)
      def release(src: Iterator[T]) = Task.now(())
      def execute(src: Iterator[T]) = Task delay {
        if (src.hasNext)
          src.next
        else
          throw Cause.Terminated(Cause.End)
      }
      scalaz.stream.io.resource(acquire)(release)(execute)
    }

    // def resultsets(sql: String, params: Seq[Any] = Nil)(c: => Connection): Process[Task, ResultSet] = {
    //   val query = sql.format(params: _*)
    //   def acquire = Task.delay {
    //     val conn = c
    //     val stmt = conn.createStatement()
    //     val rs = stmt.executeQuery(query)
    //     SqlContext(conn, stmt, rs)
    //   }
    //   def release(src: SqlContext): Task[Unit] = Task.delay {
    //     src.close()
    //   }
    //   scalaz.stream.io.resource(acquire)(release) { src =>
    //     Task.delay {
    //       if (src.resultset.next())
    //         src.resultset
    //       else
    //         throw Cause.Terminated(Cause.End)
    //     }
    //   }
    // }

    // def sqlrows(
    //   sql: String,
    //   params: Seq[Any] = Nil,
    //   withinconnection: Boolean = false,
    //   epilogue: SqlRowContext => Unit = _ => ()
    // )(c: => Connection): Process[Task, SqlRow] = {
    //   val query = sql.format(params: _*)
    //   def acquire = Task.delay {
    //     val conn = c
    //     val stmt = conn.createStatement()
    //     val rs = stmt.executeQuery(query)
    //     val meta = anorm.Sql.metaData(rs)
    //     SqlRowContext(conn, stmt, rs, meta)
    //   }
    //   def release(src: SqlRowContext): Task[Unit] = Task.delay {
    //     epilogue(src)
    //     if (withinconnection)
    //       src.commit()
    //     else
    //       src.close()
    //   }
    //   scalaz.stream.io.resource(acquire)(release) { src =>
    //     Task.delay {
    //       if (src.resultset.next())
    //         SqlUtils.resultSet2SqlRow(src.meta, src.resultset)
    //       else
    //         throw Cause.Terminated(Cause.End)
    //     }
    //   }
    // }

    // def sqlrowsWithinConnection(
    //   sql: String,
    //   params: Seq[Any] = Nil,
    //   epilogue: SqlRowContext => Unit = _ => ()
    // )(c: => Connection): Process[Task, SqlRow] = {
    //   sqlrows(sql, params, true, epilogue)(c)
    // }

    // def wrappedresultset(
    //   sql: String, params: Seq[Any] = Nil,
    //   epilogue: WrappedResultSetContext => Unit = _ => ()
    // )(c: => Connection): Process[Task, WrappedResultSet] = {
    //   val query = sql.format(params: _*)
    //   def acquire = Task.delay {
    //     val conn = c
    //     val stmt = conn.createStatement()
    //     val rs = stmt.executeQuery(query)
    //     new WrappedResultSetContext(conn, stmt, rs)
    //   }
    //   def release(src: WrappedResultSetContext): Task[Unit] = Task.delay {
    //     epilogue(src)
    //     src.close()
    //   }
    //   scalaz.stream.io.resource(acquire)(release) { src =>
    //     Task.delay {
    //       if (src.resultset.next())
    //         WrappedResultSet(src.resultset, new ResultSetCursor(src.position), src.position)
    //       else
    //         throw Cause.Terminated(Cause.End)
    //     }
    //   }
    // }

    // def records(
    //   sql: String,
    //   params: Seq[Any] = Nil,
    //   schema: Schema = NullSchema,
    //   epilogue: SqlRowContext => Unit = _ => ()
    // )(c: => Connection): Process[Task, Record] = {
    //   sqlrows(sql, params, false, epilogue)(c).pipe(stream.process1.sqlRow2Record(schema))
    // }

    // def recordsWithinConnection(
    //   sql: String,
    //   params: Seq[Any] = Nil,
    //   schema: Schema = NullSchema,
    //   epilogue: SqlRowContext => Unit = _ => ()
    // )(c: => Connection): Process[Task, Record] = {
    //   sqlrowsWithinConnection(sql, params, epilogue)(c).pipe(stream.process1.sqlRow2Record(schema))
    // }

    // def recordsWithHeader(
    //   sql: String,
    //   params: Seq[Any] = Nil,
    //   schema: Schema = NullSchema,
    //   useDynamicHeader: Boolean = true,
    //   epilogue: RecordContext => Unit = _ => ()
    // )(c: => Connection): Process[Task, Record] = {
    //   records_with_header(sql, params, schema, useDynamicHeader, false, epilogue)(c).flatMap(Process.emitAll)
    // }

    // def recordsWithHeaderWithinConnection(
    //   sql: String,
    //   params: Seq[Any] = Nil,
    //   schema: Schema = NullSchema,
    //   useDynamicHeader: Boolean = true,
    //   epilogue: RecordContext => Unit = _ => ()
    // )(c: => Connection): Process[Task, Record] = {
    //   records_with_header(sql, params, schema, useDynamicHeader, true, epilogue)(c).flatMap(Process.emitAll)
    // }

    // protected def records_with_header(
    //   sql: String,
    //   params: Seq[Any] = Nil,
    //   schema: Schema = NullSchema,
    //   useDynamicHeader: Boolean = true,
    //   withinconnection: Boolean = false,
    //   epilogue: RecordContext => Unit = _ => ()
    // )(c: => Connection): Process[Task, Seq[Record]] = {
    //   val query = sql.format(params: _*)
    //   def acquire = Task.delay {
    //     val conn = c
    //     val stmt = conn.createStatement()
    //     val rs = stmt.executeQuery(query)
    //     val meta = anorm.Sql.metaData(rs)
    //     val metacolumns = meta.ms.map(_.column)
    //     val statickeys: Vector[String] = schema.columns.map(_.name).toVector
    //     val dynamiccolumns = metacolumns.filterNot(x =>
    //       statickeys.element(x.qualified) ||
    //         x.alias.cata(statickeys.element, false))
    //     val dynamickeys: Seq[String] =
    //       useDynamicHeader ?? dynamiccolumns.map(_.qualified)
    //     val keys = statickeys ++ dynamickeys
    //     val header = {
    //       val staticheader = schema.columns.map(x => x.label getOrElse UString.capitalize(x.name))
    //       val dynamicheader = dynamiccolumns.map { x =>
    //         val s = x.alias | x.qualified
    //         UString.capitalize(s.stripPrefix("."))
    //       }
    //       staticheader ++ dynamicheader
    //     }
    //     new RecordContext(conn, stmt, rs, meta, keys, header)
    //   }
    //   def release(src: RecordContext): Task[Unit] = Task.delay {
    //     epilogue(src)
    //     if (withinconnection)
    //       src.commit()
    //     else
    //       src.close()
    //   }
    //   scalaz.stream.io.resource(acquire)(release) { src =>
    //     Task.delay {
    //       if (src.resultset.next()) {
    //         val row = SqlUtils.resultSet2SqlRow(src.meta, src.resultset)
    //         val record = SqlUtils.sqlRow2Record(row, src.keys)
    //         if (src.isHeader) {
    //           src.isHeader = false
    //           Vector(src.headerRecord, record)
    //         } else {
    //           Vector(record)
    //         }
    //       } else {
    //         throw Cause.Terminated(Cause.End)
    //       }
    //     }
    //   }
    // }

    // case class SqlContext(
    //   connection: Connection,
    //   statement: Statement,
    //   resultset: ResultSet
    // ) {
    //   def close() {
    //     resultset.close()
    //     statement.close()
    //     connection.close()
    //   }
    // }

    // case class SqlRowContext(
    //   connection: Connection,
    //   statement: Statement,
    //   resultset: ResultSet,
    //   meta: MetaData
    // ) {
    //   def commit() {
    //     connection.commit()
    //   }

    //   def close() {
    //     resultset.close()
    //     statement.close()
    //     connection.close()
    //   }
    // }

    // class WrappedResultSetContext(
    //   val connection: Connection,
    //   val statement: Statement,
    //   val resultset: ResultSet
    // ) {
    //   var position = 1 // TODO

    //   def next(): Boolean = {
    //     position += 1
    //     resultset.next()
    //   }

    //   def close() {
    //     resultset.close()
    //     statement.close()
    //     connection.close()
    //   }
    // }

    // class RecordContext(
    //   val connection: Connection,
    //   val statement: Statement,
    //   val resultset: ResultSet,
    //   val meta: MetaData,
    //   val keys: Seq[String],
    //   val header: Seq[String]
    // ) {
    //   var isHeader = true

    //   def commit() {
    //     connection.commit()
    //   }

    //   def close() {
    //     resultset.close()
    //     statement.close()
    //     connection.close()
    //   }

    //   def headerRecord = Record.create(keys.zip(header))
    // }

    def stringsR(src: Source, size: Int): Process[Task, String] =
      scalaz.stream.io.resource(Task.delay(src))(src => Task.delay(src.close)) { src =>
        lazy val chunks = src.grouped(size) // A stateful iterator
        Task.delay {
          if (chunks.hasNext)
            chunks.next.mkString
          else
            throw Cause.Terminated(Cause.End)
        }
      }

    def stringsR(in: InputStream, size: Int)(implicit codec: Codec): Process[Task, String] =
      stringsR(Source.fromInputStream(in), size)
  }

  object process1 {
    import scalaz.stream.process1.lift

    // val sqlRow2Record: Process1[SqlRow, Record] = {
    //   lift(SqlUtils.sqlRow2Record)
    // }

    // def sqlRow2Record(schema: Schema): Process1[SqlRow, Record] = {
    //   sqlRow2Record(schema.columns.map(_.name))
    // }

    // def sqlRow2Record(columns: Seq[String]): Process1[SqlRow, Record] = {
    //   lift(SqlUtils.sqlRow2Record(_, columns))
    // }

    // val record2csv: Process1[Record, String] = {
    //   lift { x =>
    //     import au.com.bytecode.opencsv.CSVWriter
    //     val buf = new java.io.StringWriter()
    //     val writer = new au.com.bytecode.opencsv.CSVWriter(
    //       buf,
    //       CSVWriter.DEFAULT_SEPARATOR,
    //       CSVWriter.DEFAULT_QUOTE_CHARACTER,
    //       CSVWriter.DEFAULT_ESCAPE_CHARACTER,
    //       "\r\n"
    //     )
    //     writer.writeNext(RecordUtils.toStringValues(x).toArray)
    //     writer.flush()
    //     writer.close()
    //     buf.toString()
    //   }
    // }

    def textEncode(encoding: String): Process1[String, ByteVector] = {
      val charset = Charset.forName(encoding)
      lift(s => ByteVector.view(s.getBytes(charset)))
    }

  //   def stringToChanelBuffer(
  //     charset: Charset
  //   ): Process1[String, ChannelBuffer] = {
  //     lift { s =>
  //       val buf = new DynamicChannelBuffer(8192)
  //       buf.writeBytes(s.getBytes(charset))
  //       buf
  //     }
  //   }

  //   def stringsToChanelBuffer(
  //     charset: Charset = Platform.charset.UTF8,
  //     newline: String = "\n"
  //   ): Process1[Seq[String], ChannelBuffer] = {
  //     lift(Arrow.stringsToChanelBuffer(charset, newline))
  //   }
  }
}
