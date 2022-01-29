import requests
from bs4 import BeautifulSoup
from openpyxl import Workbook

url = 'https://www.beeradvocate.com/beer/top-styles/'

response = requests.get(url)
response.raise_for_status()
html = response.text
soup = BeautifulSoup(html, 'html.parser')
tbodys = soup.select('div.stylebreak > ul li')
datas = []
for tbody in tbodys:
    link = tbody.find('a').get('href')
    datas.append([link])
#trs = tbody.select('tr')

#p = re.compile('/beer/profile/')
#m = p.match()

#for link in tbody.findAll('a'):
#    links = link.get('href')
#    datas.append([links])

write_wb = Workbook()
write_ws = write_wb.create_sheet('결과')
for data in datas:
    write_ws.append(data)
write_wb.save(r'/Users/yemoonkim/Downloads/style_url_list.xlsx')