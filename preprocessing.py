#!/usr/bin/python
# -*- coding: utf-8 -*-
"""preprocessing.py"""

import sys
import csv
import os

def main():

    if len(sys.argv) == 2:
        args = sys.argv
        theCSV = args[1]
        theOutputFile = os.path.splitext(args[1])[0]+"_processed.csv"
        with open (theCSV, newline='') as csvFile, open (theOutputFile, 'w', newline='') as csvOutputFile:
            csvReader = csv.DictReader(csvFile)

            variables = ['Timestamp', 'Girl', 'Ambient', 'Women_talks',
            'Talks_about_w', 'Talks_gendergap', 'Professors', 'Interest_eng',
            'Maths', 'Physics', 'Tech', 'Computer_science', 'Years_tech',
            'Years_computer_science', 'Interest_new_tech', 'Jobs',
            'Future_studies', 'Social_acceptance', 'Wealth', 'Creative_job',
            'Easy_job', 'Good_schedule', 'Job_impact', 'Engineering',
            'Eng_easy_access', 'Eng_easy_study', 'Eng_im_capable',
            'Eng_opportunities_good', 'Eng_job_fast', 'Eng_for_women_good',
            'Eng_for_men_good', 'Eng_for_geeks_good', 'Eng_not_easy_access',
            'Eng_not_easy_study', 'Eng_im_not_capable', 'Eng_opportunities_bad',
            'Eng_job_not_fast', 'Eng_for_women_bad', 'Eng_for_men_bad',
            'Eng_for_geeks_bad']

            csvWtriter = csv.DictWriter(theOutputFile, fieldnames = variables)

            #for row in csvReader:
                #print(', '.join(row))
    else:
        print ("Usa el método así: python preprocessing.py nombredelarchivo.csv")
        sys.exit()

if __name__=="__main__":
        main()
