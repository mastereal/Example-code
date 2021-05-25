import os
import re
import googlemaps
import pandas as pd
from pandas import DataFrame, Series
from urllib.request import urlopen,quote
import json
from bs4 import BeautifulSoup
import requests
#import coordinateTransform


def geocode(address):
    url = 'http://api.map.baidu.com/geocoding/v3/'
    output = 'json'
    ak = 'AqaaOjrMdKWReUtRMgU1SDjk0sQc9oem'
    #completed url
    add = quote(address)
    url2 = url + '?' + 'address=' + add  + '&output=' + output + '&ak=' + ak#+'&callback=showLocation'
    req = urlopen(url2)
    res  = req.read().decode()
    temp = json.loads(res)
    print(temp)
    try:
        lng = temp['result']['location']['lng']  # 获取经度
        lat = temp['result']['location']['lat']  # 获取纬度
        conf=temp['result']['confidence']
        list1=[lng,lat,conf]
    except:
        list1=[0,0,0]
    return list1


checklist=[]
with open("reform.txt",'r',encoding="utf-8-sig") as f:
    linelist = f.readlines()

#open the text file


for ele in linelist:
    record=re.match(r"(\d\d\d\d)(.*)",ele)
    if re.match(r"(\d\d\d\d)(.*)",ele)!=None:
                #print(record)
        year=str(record.group(1))
        county=str(record.group(2))
                #print(year)
                #print(county)
        countyrelist=county.split()
                #print(countyrelist) #extract county name from txt file
        checklist.extend(countyrelist)

achecklist=list(set(checklist)) #remove duplicated item
#print(achecklist)

        #print(df_t)
        #df_t.to_excel(dfname)

dirlist=os.listdir(path="./pointlist/") # name and coordinate data from CHGIS

falselist=[]
latitude=[]
longtitude=[]
namelist=[]
bchecklist=[]

for recounty in achecklist:
    c=0
    for filename in dirlist:
        path="./pointlist/"+filename
        fcounty = pd.read_excel(path,header=0,sheet_name=0)
        #print(fcounty)
        countylist=list(fcounty.NAME_CH.str.strip()) #extract county name
        #print(countylist)
        count=0
        for n,countypy in enumerate(countylist):
            if re.match(recounty+"(.*)",countypy)!=None:
                namelist.append(recounty)
                xcoord=fcounty.iloc[n,1]
                ycoord=fcounty.iloc[n,2]
                longtitude.append(xcoord)
                latitude.append(ycoord)
                print(recounty,xcoord,ycoord)
                count=1
                c=1
            else:
                #bchecklist.append(recounty)
                continue
        if count==0:
            continue
        else:
            break
    if c==0:
        bchecklist.append(recounty)

unichecklist=list(set(bchecklist))             

apikey=open("APIkey.txt").readline()
gmaps=googlemaps.Client(key=apikey)
# Setup python client for geocoding API

count=0
for city in unichecklist:
    citycoding=gmaps.geocode(city)
    count+=1
    #print(count)
    #print(citycoding)
    #keylist=citycoding[0].keys()
    #print(keylist)
    if not citycoding==[]:
        city_lat=citycoding[0]["geometry"]["location"]["lat"] # Extract latitude from result   
        city_lng=citycoding[0]["geometry"]["location"]["lng"] # Extract longtitude from result
        if 3<city_lat<60 and 70<city_lng<140:   # Select results that located in China
            latitude.append(city_lat)
            longtitude.append(city_lng)
            namelist.append(city)
        else:
            print(city) # Check with city that failed to code and the index of this city
            index_ll=count-1
            print(index_ll)
            #latitude.append(0)
            #longtitude.append(0)
            falselist.append(city)
    else:   # Check with city that failed to code and the index of this city
        print(city)
        index_ll=count-1
        print(index_ll)
        #latitude.append(0)
        #longtitude.append(0)
        falselist.append(city)

afterfaillist=[]
count=0
for city in falselist:
    citycoding=geocode(city) # Use baidu API to recheck
    count+=1
    #print(count)
    #print(citycoding)
    #keylist=citycoding[0].keys()
    #print(keylist)
    if not citycoding==[]:
        city_lat=citycoding[1] # Extract latitude from result   
        city_lng=citycoding[0] # Extract longtitude from result
        conf=citycoding[2]
        if 3<city_lat<60 and 70<city_lng<140:   # Select results that located in China
            latitude.append(city_lat)
            longtitude.append(city_lng)
            namelist.append(city)
        else:
            print(city) # Check with city that failed to code and the index of this city
            index_ll=count-1
            print(index_ll)
            #latitude.append(0)
            #longtitude.append(0)
            afterfaillist.append(city)
    else:   # Check with city that failed to code and the index of this city
        print(city)
        index_ll=count-1
        print(index_ll)
        #latitude.append(0)
        #longtitude.append(0)
        afterfaillist.append(city)

datadict={"city":namelist,"latitude":latitude,"longtitude":longtitude} # Setup dictionary to apply DataFrame method
databef=DataFrame(datadict, columns=["city","latitude","longtitude"])
datareconstruct=databef[["latitude","longtitude"]].groupby([databef["city"]]).mean()
print(datareconstruct)

#datareconstruct.to_csv("./geocoding_flood_drought_CN.csv")
datareconstruct.to_excel("./geocoding_CN.xlsx") #export geocoding result


tf2= open("checkf.txt","a",encoding="utf-8-sig")
for i in afterfaillist:
    tf2.write(str(i)+'\n')
tf2.close()
#list city name which cannot identified through programme