lo_path = "/usr/bin/libreoffice"
docx_dir = "~/Downloads/"
doc_file = "~/Documents/kassandra/data/raw/2019-03-05/1-03.doc"

add_on = "-env:LD_LIBRARY_PATH=:/usr/lib/libreoffice/program:/usr/lib/x86_64-linux-gnu/"

cmd <- sprintf("\"%s\" --convert-to docx:\"MS Word 2007 XML\" --headless --outdir \"%s\" \"%s\"", 
               lo_path, docx_dir, doc_file)

cmd2 <- sprintf('%s --convert-to docx:"MS Word 2007 XML" --headless --outdir %s %s', 
               lo_path, docx_dir, doc_file)

cat(cmd)
cat(cmd2)


system(cmd, intern = TRUE)
system(cmd2, intern = TRUE)

?system

library(docxtractr)
set_libreoffice_path("/usr/bin/libreoffice")

tbl = docxtractr::read_docx("~/Downloads/1-11.doc")
