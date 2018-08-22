import scrapy
import csv
from datetime import date

class WSJcraper(scrapy.Spider):
    name="WSJ_spider"
    start_urls=['http://www.wsj.com/mdc/public/page/2_3021-newhinyse-newhighs-20180716.html?mod=mdc_pastcalendar']

    def parse(self,response):
        value=response.xpath('//td[@colspan="6"]/text()')
        high=value[0].extract()
        low=value[1].extract()
        n_high=len(high)
        n_low=len(low)
        
        high=high[(n_high-2):]
        low=low[(n_low-2):]
        
        print("**************************")
        print(high)
        print("**************************")
        print(low)

        x=[str(date.today()),high, low]
        
        writer=csv.writer(open("HighLowTable.csv",'a'))
        writer.writerow(x)








