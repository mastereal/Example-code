import os
import re
import json
from aip import AipNlp

APP_ID=open("AppID.txt").readline()
API_KEY=open("API Key.txt").readline()
SECRET_KEY=open("Secret Key.txt").readline()
client = AipNlp(APP_ID, API_KEY, SECRET_KEY) #Activate Baidu API for word analysis

with open("whole.txt", 'r',encoding="utf-8-sig") as fp:
    texta=fp.readline()
    fp.close()

nbtext=re.sub(r"[(].*?[)]"," ",texta) 
year=re.compile(r"\d\d\d\d",re.S)
yearlist=year.findall(nbtext)
for t in yearlist:
    nt="\n"+t
    nbtext=re.sub(t,nt,nbtext) # Split the content by year
print(nbtext)

tfa= open("yearsplit.txt","a",encoding="utf-8-sig") 
tfa.write(nbtext)
tfa.close()

with open("yearsplit.txt", 'r',encoding="utf-8-sig") as fpb:
    textlist=fpb.readlines()
    fpb.close()

print(textlist)
contentlist=[]

for i in textlist:
    textb=i.encode("gbk","ignore") # bite type after encoding (ignore content which cannot be encoded by gbk)
    text=textb.decode("gbk") # Decode into string based on gbk
    resultitem=client.lexer(text)
    #print(resultitem)
    try:
        result=resultitem["items"]
    except KeyError:
        print(resultitem["error_msg"])
        continue
    wordlist=[]
    for items in result: #identify the location in the text and save them in the form [year, name]
        if items["ne"]=="TIME" and re.match(year,items["basic_words"][0])!=None:
            item=items["item"]
            wordlist.append(item) 
        elif items["ne"]=="LOC":
            item=items["item"]
            #item=itema.encode("utf-8-sig")
            wordlist.append(item)
        else:
            continue
    textcon=" ".join(wordlist)
    contentlist.append(textcon)

tf= open("new_whole.txt","a",encoding="utf-8-sig") #export result
for i in contentlist:
    tf.write(str(i)+'\n')
tf.close()

