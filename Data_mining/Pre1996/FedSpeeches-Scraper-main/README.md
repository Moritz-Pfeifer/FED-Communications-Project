# FedSpeeches-Scraper
Scraper to get speeches from fraser.stlouisfed.org


### How to use:
1. Install all dependencies from `requirements.txt`
2. Run `main.py` and let it collect data and PDFs from the fed website
3. Once #2 is done, all the downloaded PDFs will be stored in `downloads` folder
4. OCR the downloaded pdfs and save the OCR'd files under the `ocr` folder
5. Run `ocr.py`; it will read the OCR'd pdfs and update the database records
