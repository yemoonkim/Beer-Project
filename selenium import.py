import requests
from bs4 import BeautifulSoup
from openpyxl import Workbook

#스크래핑할 url 리스트 가져오기
import csv
data = list()
f = open("/Users/yemoonkim/PycharmProjects/pythonProject/beer_url_list6.csv", 'r')
rea = csv.reader(f)
for row in rea:
    data.append(row)
#빈 리스트 생성
datas = []
#url 하나씩 들어가서 파싱
for url in data:
    print(url[0])
    response = requests.get(url[0])
    response.raise_for_status()
    html = response.text
    soup = BeautifulSoup(html, 'html.parser')
    #맥주 이름 파싱
    title = soup.select_one('head > title').get_text()
    title_split = title.split(sep = ' | ')
    #리뷰 파싱
    trs = soup.find("div", id="rating_fullview_content_2")
    #맥주이름, 회사, 리뷰 datas에 저장
    for tr in trs:
        name = title_split[0]
        company = title_split[1]
        review = tr.get_text()
        datas.append([name, company, review])

write_wb = Workbook()
write_ws = write_wb.create_sheet('결과')
for data in datas:
    write_ws.append(data)
write_wb.save(r'/Users/yemoonkim/Downloads/Beer_parsing_total6.xlsx')