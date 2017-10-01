#!/usr/bin/python3

import sys
import re
import csv

printFlag=0
word2Dictionary={}
word3Dictionary={}
subject=""
sequence=""
sequenceDictionary={}

#check condition, set the drug name and extreme length
extreme_length=5
condition=sys.argv[2]

if condition == "DarkApoLow":
    drug=sys.argv[1][:-10]
    
elif condition == "DarkPTZ":
    drug=sys.argv[1][:-7]
      
elif condition == "DarkApoHigh":
    drug=sys.argv[1][:-11]

elif condition == "Dark":
    drug=sys.argv[1][:-4]
    
elif condition == "Light":
    drug=sys.argv[1][:-5]
    
elif condition == "LightDark":
    drug=sys.argv[1][:-9]
    
#make dictionary for each stratification
for dicty in range(2,extreme_length+2):
    word2Dictionary[dicty]={}
for dicty in range(3,extreme_length+2):
    word3Dictionary[dicty]={}
    

#accumulate the sequences per 5-minute recording
with open(sys.argv[1],'r') as seqFile:

    for line in seqFile:
        if line.find('>')!=-1 and line.find(sys.argv[3])!=-1:
            res=re.search("Individual:..",line)        
            if res:            
                subject=res.group(0)
            printFlag=1
            continue
        if printFlag==1:
            if subject in sequenceDictionary:            
                sequenceDictionary[subject].append(line.rstrip().replace('L','').replace('R',''))
            else:
                sequenceDictionary[subject]=[line.rstrip().replace('L','').replace('R','')]
            printFlag=0




drug+='_'+sys.argv[3]

no_subjects=len(sequenceDictionary)

for dicty in range(2,extreme_length+1):
    subjectOrder=0
    
    for subject in sorted(sequenceDictionary):
    
        #set time of recording factor to zero, increase with each recording, going from 1 to 13
        timeOfRecording=0
            
        #go through all fish
        for recording in sequenceDictionary[subject]:
        
            #extract all the bouts
            boutsList=re.findall('b.*?(?=b)',recording)
            #stratify
            boutsList=[x for x in boutsList if len(x)-1==dicty]
            if len(boutsList)>0:
                for bout in boutsList:
                    if len(bout)>2:
                        for pos in range(1,len(bout)-1):
                            if bout[pos:pos+2] in word2Dictionary[dicty]:
                                word2Dictionary[dicty][bout[pos:pos+2]][timeOfRecording][subjectOrder]=word2Dictionary[dicty][bout[pos:pos+2]][timeOfRecording][subjectOrder]+1
                            else:
                                word2Dictionary[dicty][bout[pos:pos+2]]=[[0 for x in range(no_subjects)] for y in range(13)]
                                word2Dictionary[dicty][bout[pos:pos+2]][timeOfRecording][subjectOrder]=1
                    if len(bout)>3:
                        for pos in range(1,len(bout)-2):
                            if bout[pos:pos+3] in word3Dictionary[dicty]:
                                word3Dictionary[dicty][bout[pos:pos+3]][timeOfRecording][subjectOrder]=word3Dictionary[dicty][bout[pos:pos+3]][timeOfRecording][subjectOrder]+1
                            else:
                                word3Dictionary[dicty][bout[pos:pos+3]]=[[0 for x in range(no_subjects)] for y in range(13)]
                                word3Dictionary[dicty][bout[pos:pos+3]][timeOfRecording][subjectOrder]=1
    
    
            
            #increase the time of recording and subject
            timeOfRecording+=1
        subjectOrder+=1
    
   
#extreme length
subjectOrder=0
dicty=extreme_length+1
for subject in sorted(sequenceDictionary):

    #set time of recording factor to zero, increase with each recording, going from 1 to 13
    timeOfRecording=0
        
    #go through all fish
    for recording in sequenceDictionary[subject]:

        #extract all the bouts
        boutsList=re.findall('b.*?(?=b)',recording)
        #stratify
        boutsList=[x for x in boutsList if len(x)-1>=dicty]
        if len(boutsList)>0:
            for bout in boutsList:
                if len(bout)>2:
                    for pos in range(1,len(bout)-1):
                        if bout[pos:pos+2] in word2Dictionary[dicty]:
                            word2Dictionary[dicty][bout[pos:pos+2]][timeOfRecording][subjectOrder]=word2Dictionary[dicty][bout[pos:pos+2]][timeOfRecording][subjectOrder]+1
                        else:
                            word2Dictionary[dicty][bout[pos:pos+2]]=[[0 for x in range(no_subjects)] for y in range(13)]
                            word2Dictionary[dicty][bout[pos:pos+2]][timeOfRecording][subjectOrder]=1
                if len(bout)>3:
                    for pos in range(1,len(bout)-2):
                        if bout[pos:pos+3] in word3Dictionary[dicty]:
                            word3Dictionary[dicty][bout[pos:pos+3]][timeOfRecording][subjectOrder]=word3Dictionary[dicty][bout[pos:pos+3]][timeOfRecording][subjectOrder]+1
                        else:
                            word3Dictionary[dicty][bout[pos:pos+3]]=[[0 for x in range(no_subjects)] for y in range(13)]
                            word3Dictionary[dicty][bout[pos:pos+3]][timeOfRecording][subjectOrder]=1


        
        #increase the time of recording and subject
        timeOfRecording+=1
    subjectOrder+=1
 
#print(word2Dictionary[2]["ss"])

''' 
for key,value in word2Dictionary.items():
    print(key)
    print(value)

for key,value in word3Dictionary.items():
    print(key)
    print(value)
'''
#word2

#turn into proportions of all transitions for each strata each time frame for each subject
#dont take mean for all subjects in each time frame, but melt

for dicty in range(2,extreme_length+2):
    
    word2_values=[ v for v in sorted(word2Dictionary[dicty].values()) ]
    word2_values_t=list(zip(*word2_values))
    for time_frame in range(0,13):
        word2_values_t_t=list(zip(*word2_values_t[time_frame]))
        for subj in range(0,len(word2_values_t_t)):
            if sum(word2_values_t_t[subj]) != 0:
                   word2_values_t_t[subj]=[float(x)/sum(word2_values_t_t[subj]) for x in word2_values_t_t[subj]]
        word2_values_t[time_frame]=list(zip(*word2_values_t_t))
    
        word2_values=list(zip(*word2_values_t))
        dict_value=0
        for keys in sorted(word2Dictionary[dicty], key=word2Dictionary[dicty].get):
            word2Dictionary[dicty][keys]=word2_values[dict_value]
            dict_value+=1
            
    #make time factor variable
    word2Dictionary[dicty]["TimeFactor"]=[[y for x in range(no_subjects)] for y in range(1,14)]
    

#print(word2Dictionary[2]["ss"])
#print(word2Dictionary[2]["TimeFactor"])

#word3
for dicty in range(3,extreme_length+2):
    
    word3_values=[ v for v in sorted(word3Dictionary[dicty].values()) ]
    word3_values_t=list(zip(*word3_values))
    for time_frame in range(0,13):
        word3_values_t_t=list(zip(*word3_values_t[time_frame]))
        for subj in range(0,len(word3_values_t_t)):
            if sum(word3_values_t_t[subj]) != 0:
                   word3_values_t_t[subj]=[float(x)/sum(word3_values_t_t[subj]) for x in word3_values_t_t[subj]]
        word3_values_t[time_frame]=list(zip(*word3_values_t_t))
    
        word3_values=list(zip(*word3_values_t))
        dict_value=0
        for keys in sorted(word3Dictionary[dicty], key=word3Dictionary[dicty].get):
            word3Dictionary[dicty][keys]=word3_values[dict_value]
            dict_value+=1
            
    #make time factor variable
    word3Dictionary[dicty]["TimeFactor"]=[[y for x in range(no_subjects)] for y in range(1,14)]


# take mean for all subjects in each time frame in each strata
for dicty in range(2,extreme_length+2):

    for key,value in word2Dictionary[dicty].items():
        word2Dictionary[dicty][key]=[sum(x_list)/len(x_list) for x_list in value]

for dicty in range(3,extreme_length+2):

    for key,value in word3Dictionary[dicty].items():
    
        word3Dictionary[dicty][key]=[sum(x_list)/len(x_list) for x_list in value]

for dicty in range(2,extreme_length+2):

    with open('../../../../processed_data/simple_word_search/B1/'+condition+'/Natural_'+drug+ '_' + condition +'_word2_strata_'+str(dicty),'wt') as word2File:

        writerword2File = csv.writer(word2File, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
        writerword2File.writerow(sorted(word2Dictionary[dicty], key=word2Dictionary[dicty].get))
        for value in list(zip(*sorted(word2Dictionary[dicty].values()))):
            writerword2File.writerow(value)
    

for dicty in range(3,extreme_length+2):

    with open('../../../../processed_data/simple_word_search/B1/'+condition+'/Natural_'+drug+ '_' + condition +'_word3_strata_'+str(dicty),'wt') as word3File:

        writerword3File = csv.writer(word3File, delimiter=',',quotechar='', quoting=csv.QUOTE_NONE)
        writerword3File.writerow(sorted(word3Dictionary[dicty], key=word3Dictionary[dicty].get))
        for value in list(zip(*sorted(word3Dictionary[dicty].values()))):
            writerword3File.writerow(value)
