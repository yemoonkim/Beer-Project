import requests
from bs4 import BeautifulSoup
from openpyxl import Workbook

#스크래핑할 style url 리스트 가져오기
import csv
data = list()
f = open("/Users/yemoonkim/PycharmProjects/pythonProject/style_url_list.csv", 'r')
rea = csv.reader(f)
for row in rea:
    data.append(row)

datas = []

for url in data:
    print(url[0])
    response = requests.get(url[0])
    response.raise_for_status()
    html = response.text
    soup = BeautifulSoup(html, 'html.parser')
    tbody = soup.select_one('table')
#trs = tbody.select('tr')

#p = re.compile('/beer/profile/')
#m = p.match()

    for link in tbody.findAll('a'):
        links = link.get('href')
        datas.append([links])

write_wb = Workbook()
write_ws = write_wb.create_sheet('결과')
for data in datas:
    write_ws.append(data)
write_wb.save(r'/Users/yemoonkim/Downloads/beer_url_list.xlsx')