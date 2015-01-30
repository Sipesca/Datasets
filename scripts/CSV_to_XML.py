#!/usr/bin/python

import sys, getopt, csv, json, re
from lxml import etree

def main(argv):
   inputfile = ''
   outputfile = ''
   try:
      opts, args = getopt.getopt(argv,"hi:o:",["ifile=","ofile="])
   except getopt.GetoptError:
      print 'CSV_to_XML.py -i <inputfile> -o <outputfile>'
      sys.exit(2)
   for opt, arg in opts:
      if opt == '-h':
         print 'CSV_to_XML.py -i <inputfile> -o <outputfile>'
         sys.exit()
      elif opt in ("-i", "--ifile"):
         inputfile = arg
      elif opt in ("-o", "--ofile"):
         outputfile = arg
   
   csvData = csv.reader(open(inputfile))
   xmlData = open(outputfile, 'w')
   xmlData.write('<?xml version="1.0"?>' + "\n")
   xmlData.write('<csv_data>' + "\n")

   if "TS" in inputfile :
       tags = ("TS","VALUE")
   else:
       tags = ("VALUE",)

   for row in csvData:
      xmlData.write('<row>' + "\n")
      for i in range(len(tags)):
         xmlData.write('    ' + '<' + tags[i] + '>' \
                          + row[i] + '</' + tags[i] + '>' + "\n")
         xmlData.write('</row>' + "\n")

   xmlData.write('</csv_data>' + "\n")
   xmlData.close()




if __name__ == "__main__":
   main(sys.argv[1:])