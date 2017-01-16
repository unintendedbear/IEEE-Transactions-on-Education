#!/usr/bin/python
# -*- coding: utf-8 -*-
"""preprocessing.py"""

import sys
import csv
import os

# Utility functions
def obtain_variables_from_response(instance):
    variableList = []
    reasonsYES = [value.strip() for value in instance['Si has respondido que SÍ te los planteas, ¿por qué? (marca todas las que correspondan)'].split(',')]
    reasonsNO = [value.strip() for value in instance['Si has respondido que NO te los planteas, ¿por qué? (marca todas las que correspondan)'].split(',')]
    possibleYESanswers = ['Es fácil acceder a ellas', 'Son fáciles de acabar',
    'Soy capaz de estudiarlas', 'Tienen muchas salidas profesionales',
    'Sirven para encontrar trabajo rápidamente', 'Son carreras para mujeres',
    'Son carreras para hombres', "Son carreras para 'frikis'"]
    possibleNOanswers = ['Es difícil acceder a ellas', 'Son difíciles de acabar',
    'No soy capaz de estudiarlas', 'No tienen muchas salidas profesionales',
    'No sirven para encontrar trabajo rápidamente', 'No me interesa',
    'Ya he hecho una Ingeniería/Ingeniería Técnica/Grado',
    'Son carreras para mujeres', 'Son carreras para hombres',
    "Son carreras para 'frikis'"]
    variablesYES = ['Eng_easy_access', 'Eng_easy_study', 'Eng_im_capable',
    'Eng_opportunities_good', 'Eng_job_fast', 'Eng_for_women_good',
    'Eng_for_men_good', 'Eng_for_geeks_good']
    variablesNO = ['Eng_not_easy_access', 'Eng_not_easy_study',
    'Eng_im_not_capable', 'Eng_opportunities_bad', 'Eng_job_not_fast',
    'Eng_not_interested', 'Eng_already', 'Eng_for_women_bad', 'Eng_for_men_bad',
    'Eng_for_geeks_bad']

    if ([y for y in reasonsYES if y != '']):
        variableList.append([variablesYES[j] for j in obtain_array_indexes(reasonsYES, possibleYESanswers)])

    if ([x for x in reasonsNO if x != '']):
        variableList.append([variablesNO[i] for i in obtain_array_indexes(reasonsNO, possibleNOanswers)])

    return variableList

def obtain_array_indexes(responses, database):
    indexes = []

    for response in responses:
        indexes.append(database.index(response))

    return indexes

def fill_value(column, aList):
    if aList:
        if len(aList) > 1:
            return 1 if column in aList[0] or column in aList[1] else 0
        else:
            return 1 if column in aList[0] else 0
    else: return 0

def find_interest(instance):
    questionESO = '¿Cuánto dirías que te interesa la ingeniería?'
    questionOthers = '¿Cuánto dirías que te interesaba la ingeniería durante la E.S.O?'
    if questionESO in instance.keys():
        return instance[questionESO]
    else:
        return instance[questionOthers]

def main():

    if len(sys.argv) == 2:
        args = sys.argv
        theCSV = args[1]
        theOutputFile = os.path.splitext(args[1])[0]+"_processed.csv"
        with open (theCSV, newline='') as csvFile, open (theOutputFile, 'w', newline='') as csvOutputFile:
            csvReader = csv.DictReader(csvFile) # No salen en orden
            ##csvReader = csv.reader(csvFile, delimiter=',', quotechar='"') #Salen en orden

            variables = ['Timestamp', 'Girl', 'Class', 'Women_talks',
            'Talks_about_w', 'Talks_gendergap', 'Professors', 'Interest_eng',
            'Maths', 'Physics', 'Tech', 'Computer_science', 'Years_tech',
            'Years_computer_science', 'Interest_new_tech', 'Jobs',
            'Future_studies', 'Social_acceptance', 'Wealth', 'Creative_job',
            'Easy_job', 'Good_schedule', 'Job_impact', 'Engineering',
            'Eng_easy_access', 'Eng_easy_study', 'Eng_im_capable',
            'Eng_opportunities_good', 'Eng_job_fast', 'Eng_for_women_good',
            'Eng_for_men_good', 'Eng_for_geeks_good', 'Eng_not_easy_access',
            'Eng_not_easy_study', 'Eng_im_not_capable', 'Eng_opportunities_bad',
            'Eng_job_not_fast', 'Eng_not_interested', 'Eng_already',
            'Eng_for_women_bad', 'Eng_for_men_bad', 'Eng_for_geeks_bad']

            csvWtriter = csv.DictWriter(csvOutputFile, fieldnames = variables)
            csvWtriter.writeheader()

            for row in csvReader:
                theList = obtain_variables_from_response(row)
                csvWtriter.writerow({'Timestamp': row['Timestamp'],
                'Girl': row['¿Eres mujer?'],
                'Class': row['¿Qué proporción de chicos y chicas había en tu clase de tecnología/informática durante tu último curso?'],
                'Women_talks': row['Indica si algunas de las siguientes actividades han tenido lugar en tu colegio o instituto [Charlas dadas por mujeres científicas o ingenieras]'],
                'Talks_about_w': row['Indica si algunas de las siguientes actividades han tenido lugar en tu colegio o instituto [Charlas o discusiones sobre el trabajo de mujeres científicas o ingenieras]'],
                'Talks_gendergap': row['Indica si algunas de las siguientes actividades han tenido lugar en tu colegio o instituto [Charlas o discusiones sobre la diferencia entre el número de hombres y mujeres en la ciencia o la ingeniería]'],
                'Professors': row['¿Cómo han sido tus profesores/as en las asignaturas de tecnología/informática?'],
                'Interest_eng': find_interest(row),
                'Maths': row['Indica tus nota media en las siguientes asignaturas (si alguna no la has tenido, no marques nada en esa asignatura) [Matemáticas]'],
                'Physics': row['Indica tus nota media en las siguientes asignaturas (si alguna no la has tenido, no marques nada en esa asignatura) [Física]'],
                'Tech': row['Indica tus nota media en las siguientes asignaturas (si alguna no la has tenido, no marques nada en esa asignatura) [Tecnología]'],
                'Computer_science': row['Indica tus nota media en las siguientes asignaturas (si alguna no la has tenido, no marques nada en esa asignatura) [Informática]'],
                'Years_tech': row['¿Cuántos años has cursado la asignatura de Tecnología?'],
                'Years_computer_science': row['¿Cuántos años has cursado la asignatura de Informática?'],
                'Interest_new_tech': row['¿Te interesan las nuevas tecnologías? (Móviles, televisores inteligentes, relojes inteligentes, etc.)'],
                'Jobs': row['¿En qué medida conoces las salidas profesionales que tienen las ingenierías?'],
                'Future_studies': row['Si tuvieses que decidir ahora qué harás después de este curso, ¿qué responderías?'],
                'Social_acceptance': row['¿Qué opinas de los informáticos? [Tienen prestigio social]'],
                'Wealth': row['¿Qué opinas de los informáticos? [Ganan mucho dinero]'],
                'Creative_job': row['¿Qué opinas de los informáticos? [Hacen trabajos variados y creativos]'],
                'Easy_job': row['¿Qué opinas de los informáticos? [Su trabajo es muy fácil]'],
                'Good_schedule': row['¿Qué opinas de los informáticos? [Tienen un buen horario laboral]'],
                'Job_impact': row['¿Qué opinas de los informáticos? [Su trabajo tiene un gran impacto en la sociedad]'],
                'Engineering': row['¿Te planteas los estudios superiores en una carrera tecnológica?'],
                'Eng_easy_access': fill_value('Eng_easy_access', theList),
                'Eng_easy_study': fill_value('Eng_easy_study', theList),
                'Eng_im_capable': fill_value('Eng_im_capable', theList),
                'Eng_opportunities_good': fill_value('Eng_opportunities_good', theList),
                'Eng_job_fast': fill_value('Eng_job_fast', theList),
                'Eng_for_women_good': fill_value('Eng_for_women_good', theList),
                'Eng_for_men_good': fill_value('Eng_for_men_good', theList),
                'Eng_for_geeks_good': fill_value('Eng_for_geeks_good', theList),
                'Eng_not_easy_access': fill_value('Eng_not_easy_access', theList),
                'Eng_not_easy_study': fill_value('Eng_not_easy_study', theList),
                'Eng_im_not_capable': fill_value('Eng_im_not_capable', theList),
                'Eng_opportunities_bad': fill_value('Eng_opportunities_bad', theList),
                'Eng_job_not_fast': fill_value('Eng_job_not_fast', theList),
                'Eng_not_interested': fill_value('Eng_not_interested', theList),
                'Eng_already': fill_value('Eng_already', theList),
                'Eng_for_women_bad': fill_value('Eng_for_women_bad', theList),
                'Eng_for_men_bad': fill_value('Eng_for_men_bad', theList),
                'Eng_for_geeks_bad': fill_value('Eng_for_geeks_bad', theList)})
    else:
        print ("Usa el método así: python preprocessing.py nombredelarchivo.csv")
        sys.exit()

if __name__=="__main__":
        main()
