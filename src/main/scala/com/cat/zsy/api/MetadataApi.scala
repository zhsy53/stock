package com.cat.zsy.api
import com.cat.zsy.domain.MetadataElement
import org.apache.poi.ss.usermodel.WorkbookFactory

import java.io.{BufferedReader, File, FileReader}
import java.net.URI
import java.nio.file.Paths
import scala.collection.convert.ImplicitConversions.`iterable AsScalaIterable`
import scala.util.Using

object MetadataApi {
  private def getFileFromClasspath(filename: String): URI = this.getClass.getResource("/" + filename).toURI

  class SafeFileReader(file: File)(implicit manager: Using.Manager) extends BufferedReader(new FileReader(file)) {
    def this(fileName: String)(implicit manager: Using.Manager) = this(new File(fileName))

    manager.acquire(this)
  }

  // TODO
  def getData: Seq[MetadataElement] = {
    // val files = Paths.get(getDirFromClasspath).toFile.listFiles().filter(_.isFile).filter(_.getName.matches(".*\\.xls.?$"))

    def getSheet(filename: String) = WorkbookFactory.create(Paths.get(getFileFromClasspath(filename)).toFile).getSheetAt(0)

    val sh = getSheet("sh.xls").map(row => MetadataElement(row.getCell(2).getStringCellValue, row.getCell(1).getStringCellValue)).toSeq

    val sz = getSheet("sz.xlsx").map(row => MetadataElement(row.getCell(4).getStringCellValue, row.getCell(5).getStringCellValue)).toSeq

    sh ++ sz
  }
}
