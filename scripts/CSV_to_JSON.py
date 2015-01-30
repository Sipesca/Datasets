#!/usr/bin/python

import sys, getopt, csv, json, re

def main(argv):
   inputfile = ''
   outputfile = ''
   try:
      opts, args = getopt.getopt(argv,"hi:o:",["ifile=","ofile="])
   except getopt.GetoptError:
      print 'CSV_to_JSON.py -i <inputfile> -o <outputfile>'
      sys.exit(2)
   for opt, arg in opts:
      if opt == '-h':
         print 'CSV_to_JSON.py -i <inputfile> -o <outputfile>'
         sys.exit()
      elif opt in ("-i", "--ifile"):
         inputfile = arg
      elif opt in ("-o", "--ofile"):
         outputfile = arg
   csvfile = open(inputfile,'r')
   jsonfile = open(outputfile,'w')

   if "TS" in inputfile :
      fieldnames = ("TS","VALUE")
   else:
      fieldnames = ("VALUE",)
   
   reader = csv.DictReader( csvfile, fieldnames)

   for row in reader:
      json.dump(row, jsonfile)
      jsonfile.write('\n')

if __name__ == "__main__":
   main(sys.argv[1:])