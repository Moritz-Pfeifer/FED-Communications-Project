import time
import settings
import chromedriver
import csv
from colorama import Fore, Style
import database as _db
import os
from pathlib import Path
import requests
import uuid
import warnings
warnings.filterwarnings("ignore", category=DeprecationWarning)


def download_file(url, uuid):
    try:
        path = os.path.join('downloads', uuid + '.pdf')
        filename = Path(path)
        print(f'Downloading file: {uuid}.pdf ....')
        response = requests.get(url)
        filename.write_bytes(response.content)
        print(f'Download completed: {path} ....')
    except Exception as e:
        print(f'Unable to download file: {uuid}, error: {e}')


def extract_location(str):
    str = str.lower()
    possibles = ["Alaska", "Alabama", "Arkansas", "American Samoa", "Arizona", "California", "Colorado",
                 "Connecticut", "District ", "of Columbia", "Delaware", "Florida", "Georgia", "Guam", "Hawaii",
                 "Iowa", "Idaho", "Illinois", "Indiana", "Kansas", "Kentucky", "Louisiana", "Massachusetts",
                 "Maryland", "Maine", "Michigan", "Minnesota", "Missouri", "Mississippi", "Montana",
                 "North Carolina", "North Dakota", "Nebraska", "New Hampshire", "New Jersey", "New Mexico",
                 "Nevada", "New York", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Puerto Rico", "Rhode Island",
                 "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Virginia", "Virgin Islands",
                 "Vermont", "Washington", "Wisconsin", "West Virginia", "Wyoming", "Washington, D.C."]

    for p in possibles:
        if p.lower() in str:
            return p

    return ''


# read settings
settings = settings.load_settings('settings.json')
driver = chromedriver.get_driver(settings["chromeDriverLocation"], settings["headlessDriver"])

# Create database if not exists
db = _db.Database("fed.db")

try:
    db.query('CREATE TABLE speeches ('
             'id INTEGER PRIMARY KEY AUTOINCREMENT,'
             'title TEXT,'
             'location TEXT,'
             'date TEXT,'
             'partOf TEXT,'
             'author TEXT,'
             'contributingAuthor TEXT,'
             'pdfUrl TEXT,'
             'pdfId TEXT,'
             'url TEXT,'
             'content TEXT'
             ')')
except Exception as e:
    print("DB Error")
    print(e)

# temp vars
all_titles = []
titles_seen = []
titles_per_link = []
links = []


# read urls from csv
with open('links.csv', newline='') as csvfile:
    reader = csv.reader(csvfile, delimiter=',', quotechar='|')
    for row in reader:
        links.append(row[0])

try:
    for link_count, fed_link in enumerate(links):
        all_titles = []
        titles_seen = []
        db_data = []
        print(Fore.YELLOW + f'Fetching data for link {link_count + 1}/{len(links)}....' + Style.RESET_ALL)

        driver.get(fed_link)
        time.sleep(2)

        years = driver.find_elements_by_xpath("//nav[@class='browse-by-jump-to']/ul[1]/li")
        print("years: ", len(years))

        # get all links and titles
        for year in years:
            # print("activating ", year.text)
            year.click()
            time.sleep(1)
            titles = driver.find_elements_by_xpath("//a[@class='list-item']")
            # print("titles=", len(titles))
            for i, title in enumerate(titles):
                # print(i)
                # print(title.text)
                # print(title.get_attribute('href'))
                # print("\n")
                if title.text not in titles_seen:
                    all_titles.append({"name": title.text, "url": title.get_attribute('href')})
                    titles_seen.append(title.text)
                # else:
                #     print("duplicate: ",title.text)

        for title in all_titles:
            driver.get(title["url"])
            metadata = driver.find_element_by_xpath("/html[1]/body[1]/div[1]/div[3]/div[3]/div[2]")
            # print(metadata.text)
            date_issued = metadata.find_element_by_xpath("//span[@class='dateIssued']").text
            part_of = metadata.find_element_by_xpath("//li[@class='partOf']").text
            pdf_download = metadata.find_element_by_xpath("//p[@class='metadata-buttons']/a[1]").get_attribute('href')
            all_authors = metadata.find_elements_by_xpath("//li[@class='authors']")
            main_author = all_authors[0].text
            contributing_authors = ",".join([a.text for a in all_authors[1:]])
            location = extract_location(title["name"])
            txt_url = metadata.find_element_by_xpath("//p[@class='metadata-buttons']/a[2]").get_attribute('href')
            # driver.get(txt_url)
            # content = driver.find_element_by_xpath("//pre").text
            # content = content.replace('\t', ' ').replace('\n', ' ').replace('¡*r“-------------------',' ').replace('r _ -------------------',' ')
            pdf_id = str(uuid.uuid4())
            download_file(pdf_download, pdf_id)

            # print(date_issued, part_of, pdf_download, main_author, contributing_authors, location)

            # dump to db
            db_data.append(
                [
                    title["name"], location, date_issued, part_of, main_author,
                    contributing_authors, pdf_download, pdf_id, title["url"], ""
                ]
            )

        db.write_many("speeches (title, location, date, partOf, author, contributingAuthor, pdfUrl, pdfId, url, content)",
                      "?,?,?,?,?,?,?,?,?,?",
                      db_data)

        titles_per_link.append(len(all_titles))
except Exception as e:
    print("Error occurred:")
    print(e)
    db.close()

try:
    db.close()
except Exception as dbe:
    pass


print("******************************************")
print(sum(titles_per_link))

driver.quit()
