package com.cat.zsy.api
import com.cat.zsy.domain.MetadataElement
import com.cat.zsy.util.FileUtils
import org.apache.poi.ss.usermodel._

import scala.jdk.CollectionConverters._
import scala.util.Using

object MetadataApi {
  def getData: Seq[MetadataElement] = Using.Manager { use =>
    def workbookToData(workbook: Workbook, rowParser: Row => MetadataElement): Seq[MetadataElement] = workbook.getSheetAt(0).iterator.asScala.map(rowParser).toSeq
    val w1 = use(getWorkbook("sh.xls"))
    val w2 = use(getWorkbook("sz.xlsx"))
    val sh = workbookToData(w1, r => MetadataElement(r.getCell(2).getStringCellValue, r.getCell(1).getStringCellValue))
    val sz = workbookToData(w2, r => MetadataElement(r.getCell(4).getStringCellValue, r.getCell(5).getStringCellValue))

    sh ++ sz
  }.get

  private def getWorkbook(filename: String): Workbook = WorkbookFactory.create(FileUtils.getFileFromClasspath(filename))
}
