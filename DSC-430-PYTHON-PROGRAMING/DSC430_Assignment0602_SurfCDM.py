#Author: Wilson Wu
#Date:2019.10.29
#Honor statement: “I have not given or received any unauthorized assistance on this assignment”,
#Video link: https://youtu.be/WzGwal-DXOo
import urllib.request
from urllib.request import urlopen
from urllib.parse import urljoin
from html.parser import HTMLParser

class Collector(HTMLParser):
    'collects hyperlink URLs into a list'

    def __init__(self, url):
        'initializes parser, the url, and a list'
        HTMLParser.__init__(self)
        self.url = 'https://www.cdm.depaul.edu/'
        self.links = []
        self.tag=None
        self.attrs=None
        self.inLink = False
        self.count=0
        self.dataname=[]

    def handle_starttag(self, tag, attrs):
        'collects hyperlink URLs in their absolute format'
        self.inLink = False
        if tag == 'a':
            for attr in attrs:
                if attr[0] == 'href':
                    # construct absolute URL
                    absolute = urljoin(self.url, attr[1].strip().replace('\n', ''))
                    if absolute[:4] == 'http': # collect HTTP URLs
                        if absolute not in self.links:
                            self.links.append(absolute)
                            self.tag=tag
                            self.attrs=absolute
                            self.inLink = True
    
    def handle_endtag(self, tag):
        if tag == "a":
            self.inlink = False
                    
    def handle_data(self, data):
        if self.tag == 'a' and self.inLink and data.strip():
            if self.url in self.attrs:
                data= data.replace('\n','').replace('\r','').strip()
                self.dataname.append(data)
             
    def getLinks(self):
        'returns hyperlinks URLs in their absolute format'
        return self.links

    def getcount(self):
        return self.dataname

def analyze(url):
    
    #print('done')           # for testing

    # obtain links in the web page
    content = urlopen(url).read().decode()
    collector = Collector(url)
    collector.feed(content)
    urls = collector.getLinks()          # get list of links

    # compute word frequencies
    data = collector.getcount()
    freq = frequency(data)
    freqword=frequencyword(data)
#    print(data)

    # print the frequency of every text data word in web page
#    print('\n{:45} {:10} {:5}'.format('URL', 'word', 'count'))
#    for word in freq:
#        print('{:45} {:10} {:5}'.format(url, word, freq[word]))

    # print the http links found in web page
#    print('\n{:45} {:10}'.format('URL', 'link'))
#    for link in urls:
#        print('{:45} {:10}'.format(url, link))
#
    return urls


dicword={}
def frequencyword(data):
    global dicword
    for i in data:
        for word in i.split():
            if word not in dicword:
                dicword[word]=1
            else:
                dicword[word]+=1
    return dicword

dic={}
def frequency(data):
    global dic
    for i in data:
        if i not in dic:
            dic[i]=1
        else:
            dic[i]+=1
    return dic
    
visited = set()# initialize visited to an empty set

countdone=0
def crawl2(url):
    '''a recursive web crawler that calls analyze()
       on every visited web page'''

    if 'https://www.cdm.depaul.edu/' not in url:
        pass
    else:
    # add url to set of visited pages
        global visited     # warns the programmer 
        visited.add(url)
        global dic
        global dicword
        global countdone
        sorted_dic = sorted(dic.items(), key=lambda x: x[1], reverse=True)
        sorted_dicword = sorted(dicword.items(), key=lambda x: x[1], reverse=True)
        if countdone%20==0:
#           print(str(countdone)+'times')
            print(sorted_dic[:25])
            print(sorted_dicword[:25])
#            print(visited)

    # analyze() returns a list of hyperlink URLs in web page url 
        links = analyze(url)
        countdone+=1
    # recursively continue crawl from every link in links
        for link in links:
        # follow link only if not visited
            if link not in visited:
                try:
                    crawl2(link)
                except:
                    pass
        
def main():
    url="https://www.cdm.depaul.edu/Pages/default.aspx"
    crawl2(url)

'''
content=urlopen(url).read().decode()
c=Collector(url)
c.feed(content)
link=c.getLinks()
cdmlinks=[]
for eachlink in link:
    if 'https://www.cdm.depaul.edu/' in eachlink:
        cdmlinks.append(eachlink)
for cdmlink in cdmlinks:
    try:
        crawl2(cdmlink)
    except:
        pass
'''   
'''
content=urlopen(url).read().decode()

c=Collector(url)
c.feed(content)
link=c.getLinks()
cdmlink=[]
for eachlink in link:
    if 'https://www.cdm.depaul.edu/' in eachlink:
        cdmlink.append(eachlink)
print(len(cdmlink))
print(len(c.getcount()))
'''
