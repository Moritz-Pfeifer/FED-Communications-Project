from sys import platform
from selenium import webdriver


def get_driver(driver_path, headless=False):
    if platform == "win32":
        # windows
        driver_instance = webdriver.Chrome(executable_path=driver_path)
        return driver_instance
    else:
        # linux or macOS
        chrome_options = webdriver.ChromeOptions()
        if headless:
            chrome_options.add_argument('--headless')
            chrome_options.add_argument('--no-sandbox')
            chrome_options.add_argument('--disable-gpu')
            chrome_options.add_argument('--window-size=1280x1696')
            chrome_options.add_argument('--hide-scrollbars')
            chrome_options.add_argument('--enable-logging')
            chrome_options.add_argument('--log-level=0')
            chrome_options.add_argument('--v=99')
            chrome_options.add_argument('--single-process')
            chrome_options.add_argument('--ignore-certificate-errors')
            chrome_options.add_argument(
                'user-agent=Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.100 Safari/537.36')
        driver_instance = webdriver.Chrome(chrome_options=chrome_options, executable_path=driver_path)
        return driver_instance