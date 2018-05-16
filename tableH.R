tableH <- function (data,tablename,filename)
{
  require(ReporteRs)
  docxH <- docx()
  docxH <- addTitle(docxH,tablename,level=1)
  Tabdoc <- data
  Tabela <- vanilla.table(Tabdoc,add.rownames = TRUE)
  docxH <- addFlexTable(docxH,Tabela)
  writeDoc (docxH, file = paste(filename,".docx"))
}