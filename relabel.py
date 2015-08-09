import csv as csv
import numpy as np
csv_file_object = csv.reader(open('train.csv', 'rb')) 	# Load in the csv file
#header = csv_file_object.next()    # Skip the fist line as it is a header
data=[]
count=0
for row in csv_file_object:  # Skip through each row in the csv file,
    if count==0:
        header=row
        count=1
        #print row
    else:
        data.append(row[0:])    # adding each row to the data variable
data = np.array(data) 	     # Then convert from a list to an array.
countrow=0
tt=np.zeros([15120,56],float)
for line in data:
    countcol=0
    for point in line:
        tt[countrow][countcol]=point
        countcol+=1
        #print countcol
    countrow+=1
wildset=0.0
soilset=0.0
nt=np.zeros([15120,14],float)
for i in range(0,countrow):
    wildcheck=0.0
    soilcheck=0.0
    for j in range(11,15):
        #print i, tt[i][j]
        wildcheck+=1.0
        if tt[i][j]==1.0:
           wildset=wildcheck
    #print wildset
    for j in range(15,55):
        soilcheck+=1.0
        if tt[i][j]==1.0:
            soilset=soilcheck
    #print soilset
    for j in range(0,11):
        nt[i][j]=tt[i][j]
    nt[i][11]=wildset
    nt[i][12]=soilset
    nt[i][13]=tt[i][55]

with open('relabeltrain.csv', 'wb') as csvfile:
    fileout = csv.writer(csvfile)
    fileout.writerow(header)
    for i in range(0,countrow):
        fileout.writerow(nt[i])
