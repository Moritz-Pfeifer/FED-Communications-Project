import io
from pdfminer.converter import TextConverter
from pdfminer.pdfinterp import PDFPageInterpreter
from pdfminer.pdfinterp import PDFResourceManager
from pdfminer.pdfpage import PDFPage
import os
import glob
from pathlib import Path
import database as _db
db = _db.Database("fed.db")


def read_files():
    pdfs = []
    for file in glob.glob(os.path.join('ocr', '*.pdf')):
        pdfs.append(file)
    return pdfs


def extract_text():
    pdfs = read_files()
    for pdf in pdfs:
        print(f'Reading file: {pdf}...')
        with open(pdf, 'rb') as fh:
            text = ""
            for page in PDFPage.get_pages(fh, caching=True, check_extractable=True):
                resource_manager = PDFResourceManager()
                fake_file_handle = io.StringIO()
                converter = TextConverter(resource_manager, fake_file_handle)
                page_interpreter = PDFPageInterpreter(resource_manager, converter)
                page_interpreter.process_page(page)
                text += fake_file_handle.getvalue() + " "
                # print(text)

                # close open handles
                converter.close()
                fake_file_handle.close()

            # get uuid used for database record marking
            uuid = Path(pdf).name.split('.')[0]
            # print(f'uuid: {uuid}')

            try:
                db.cursor.execute("UPDATE speeches SET content = ? WHERE pdfId = ?", (text, uuid))
                # c.execute("UPDATE movies SET rating = ? WHERE name = ?", (8.7, "'Allo 'Allo! (1982)"))
                # qry =
                # qry = "update speeches set content = '" + text + "' WHERE pdfId ='" + uuid + "'"
                # print(qry)
                # db.query("update speeches set content = '" + text + "' WHERE pdfId ='" + uuid + "'")
                print(f'updated content for uuid: {uuid}')
            except Exception as e:
                print(f'unable to update content for uuid: {uuid}')
                print(e)

            # update the database record and save content/text


try:
    extract_text()
except Exception as exp:
    print(exp)
finally:
    db.close()
